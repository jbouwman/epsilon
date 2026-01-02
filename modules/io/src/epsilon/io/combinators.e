;;;; epsilon.io.combinators - Stream composition utilities
;;;;
;;;; Provides composable wrappers for building stream pipelines:
;;;; - Limiting: bound the number of bytes
;;;; - Teeing: duplicate data to multiple sinks
;;;; - Chaining: concatenate multiple sources
;;;; - Counting: track bytes transferred
;;;; - Transforming: modify bytes in transit
;;;;
;;;; Design: Each combinator returns a new stream that wraps others.
;;;; Composition is explicit and type-safe.

(package epsilon.io.combinators
  (import (epsilon.io.protocol proto)
          (epsilon.io.conditions cond)))

(defun check-open (stream operation)
  "Signal closed-error if STREAM is not open."
  (unless (proto:open-p stream)
    (error 'cond:closed-error :operation operation :stream stream)))

;;; ============================================================================
;;; Limit Reader - Bound the number of bytes readable
;;; ============================================================================

(defstruct (limit-reader (:constructor %make-limit-reader))
  "Reader that yields at most N bytes from source.

   After the limit is reached, returns EOF.
   Does not close the underlying source when exhausted."
  (source nil)
  (remaining 0 :type fixnum)
  (closed-p nil :type boolean))

(defun make-limit-reader (source limit)
  "Create a reader that yields at most LIMIT bytes from SOURCE."
  (%make-limit-reader :source source :remaining limit))

(defmethod proto:read-into ((reader limit-reader) buffer &key (start 0) (end (length buffer)))
  (check-open reader :read)
  (if (zerop (limit-reader-remaining reader))
      0
      (let* ((requested (- end start))
             (allowed (min requested (limit-reader-remaining reader)))
             (n (proto:read-into (limit-reader-source reader) buffer
                                 :start start
                                 :end (+ start allowed))))
        (decf (limit-reader-remaining reader) n)
        n)))

(defmethod proto:close* ((reader limit-reader))
  (unless (limit-reader-closed-p reader)
    (setf (limit-reader-closed-p reader) t)
    (proto:close* (limit-reader-source reader))
    t))

(defmethod proto:open-p ((reader limit-reader))
  (not (limit-reader-closed-p reader)))

(defun limit-reader-exhausted-p (reader)
  "Return T if the limit has been reached."
  (zerop (limit-reader-remaining reader)))

;;; ============================================================================
;;; Limit Writer - Bound the number of bytes writable
;;; ============================================================================

(defstruct (limit-writer (:constructor %make-limit-writer))
  "Writer that accepts at most N bytes.

   After the limit, signals limit-exceeded-error."
  (sink nil)
  (remaining 0 :type fixnum)
  (closed-p nil :type boolean))

(defun make-limit-writer (sink limit)
  "Create a writer that accepts at most LIMIT bytes."
  (%make-limit-writer :sink sink :remaining limit))

(defmethod proto:write-from ((writer limit-writer) buffer &key (start 0) (end (length buffer)))
  (check-open writer :write)
  (let ((count (- end start)))
    (when (> count (limit-writer-remaining writer))
      (error 'limit-exceeded-error
             :limit (- (limit-writer-remaining writer) count)
             :attempted count
             :stream writer))
    (let ((n (proto:write-from (limit-writer-sink writer) buffer :start start :end end)))
      (decf (limit-writer-remaining writer) n)
      n)))

(defmethod proto:flush ((writer limit-writer))
  (proto:flush (limit-writer-sink writer))
  writer)

(defmethod proto:close* ((writer limit-writer))
  (unless (limit-writer-closed-p writer)
    (setf (limit-writer-closed-p writer) t)
    (proto:close* (limit-writer-sink writer))
    t))

(defmethod proto:open-p ((writer limit-writer))
  (not (limit-writer-closed-p writer)))

;;; ============================================================================
;;; Tee Reader - Duplicate reads to a writer
;;; ============================================================================

(defstruct (tee-reader (:constructor %make-tee-reader))
  "Reader that writes everything read to a secondary writer.

   Useful for logging, checksumming, or progress tracking."
  (source nil)
  (writer nil)
  (closed-p nil :type boolean))

(defun make-tee-reader (source writer)
  "Create a reader that writes all read data to WRITER."
  (%make-tee-reader :source source :writer writer))

(defmethod proto:read-into ((reader tee-reader) buffer &key (start 0) (end (length buffer)))
  (check-open reader :read)
  (let ((n (proto:read-into (tee-reader-source reader) buffer :start start :end end)))
    (when (plusp n)
      (proto:write-all (tee-reader-writer reader) buffer :start start :end (+ start n)))
    n))

(defmethod proto:close* ((reader tee-reader))
  (unless (tee-reader-closed-p reader)
    (setf (tee-reader-closed-p reader) t)
    (proto:close* (tee-reader-source reader))
    (proto:flush (tee-reader-writer reader))
    t))

(defmethod proto:open-p ((reader tee-reader))
  (not (tee-reader-closed-p reader)))

;;; ============================================================================
;;; Multi Writer - Write to multiple sinks
;;; ============================================================================

(defstruct (multi-writer (:constructor %make-multi-writer))
  "Writer that writes to multiple underlying writers.

   All writes go to all sinks. Flush flushes all."
  (sinks nil :type list)
  (closed-p nil :type boolean))

(defun make-multi-writer (&rest sinks)
  "Create a writer that writes to all SINKS."
  (%make-multi-writer :sinks sinks))

(defmethod proto:write-from ((writer multi-writer) buffer &key (start 0) (end (length buffer)))
  (check-open writer :write)
  (let ((count (- end start)))
    (dolist (sink (multi-writer-sinks writer))
      (proto:write-all sink buffer :start start :end end))
    count))

(defmethod proto:flush ((writer multi-writer))
  (dolist (sink (multi-writer-sinks writer))
    (proto:flush sink))
  writer)

(defmethod proto:close* ((writer multi-writer))
  (unless (multi-writer-closed-p writer)
    (setf (multi-writer-closed-p writer) t)
    (dolist (sink (multi-writer-sinks writer))
      (proto:close* sink))
    t))

(defmethod proto:open-p ((writer multi-writer))
  (not (multi-writer-closed-p writer)))

;;; ============================================================================
;;; Chain Reader - Concatenate multiple readers
;;; ============================================================================

(defstruct (chain-reader (:constructor %make-chain-reader))
  "Reader that reads from multiple sources in sequence.

   When one source is exhausted, moves to the next."
  (sources nil :type list)
  (closed-p nil :type boolean))

(defun make-chain-reader (&rest sources)
  "Create a reader that reads from SOURCES in sequence."
  (%make-chain-reader :sources sources))

(defmethod proto:read-into ((reader chain-reader) buffer &key (start 0) (end (length buffer)))
  (check-open reader :read)
  (loop
    (when (null (chain-reader-sources reader))
      (return 0))
    (let ((n (proto:read-into (first (chain-reader-sources reader)) buffer
                        :start start :end end)))
      (if (zerop n)
          ;; Source exhausted, try next
          (progn
            (proto:close* (pop (chain-reader-sources reader))))
          (return n)))))

(defmethod proto:close* ((reader chain-reader))
  (unless (chain-reader-closed-p reader)
    (setf (chain-reader-closed-p reader) t)
    (dolist (source (chain-reader-sources reader))
      (proto:close* source))
    (setf (chain-reader-sources reader) nil)
    t))

(defmethod proto:open-p ((reader chain-reader))
  (not (chain-reader-closed-p reader)))

;;; ============================================================================
;;; Counting Wrappers - Track bytes transferred
;;; ============================================================================

(defstruct (counting-reader (:constructor %make-counting-reader))
  "Reader that tracks total bytes read."
  (source nil)
  (count 0 :type fixnum)
  (closed-p nil :type boolean))

(defun make-counting-reader (source)
  "Create a reader that counts bytes read from SOURCE.
   Use counting-reader-count to get the count."
  (%make-counting-reader :source source))

(defmethod proto:read-into ((reader counting-reader) buffer &key (start 0) (end (length buffer)))
  (check-open reader :read)
  (let ((n (proto:read-into (counting-reader-source reader) buffer :start start :end end)))
    (incf (counting-reader-count reader) n)
    n))

(defmethod proto:close* ((reader counting-reader))
  (unless (counting-reader-closed-p reader)
    (setf (counting-reader-closed-p reader) t)
    (proto:close* (counting-reader-source reader))
    t))

(defmethod proto:open-p ((reader counting-reader))
  (not (counting-reader-closed-p reader)))

(defstruct (counting-writer (:constructor %make-counting-writer))
  "Writer that tracks total bytes written."
  (sink nil)
  (count 0 :type fixnum)
  (closed-p nil :type boolean))

(defun make-counting-writer (sink)
  "Create a writer that counts bytes written to SINK.
   Use counting-writer-count to get the count."
  (%make-counting-writer :sink sink))

(defmethod proto:write-from ((writer counting-writer) buffer &key (start 0) (end (length buffer)))
  (check-open writer :write)
  (let ((n (proto:write-from (counting-writer-sink writer) buffer :start start :end end)))
    (incf (counting-writer-count writer) n)
    n))

(defmethod proto:flush ((writer counting-writer))
  (proto:flush (counting-writer-sink writer))
  writer)

(defmethod proto:close* ((writer counting-writer))
  (unless (counting-writer-closed-p writer)
    (setf (counting-writer-closed-p writer) t)
    (proto:close* (counting-writer-sink writer))
    t))

(defmethod proto:open-p ((writer counting-writer))
  (not (counting-writer-closed-p writer)))

;;; ============================================================================
;;; Transform Reader - Apply function to bytes
;;; ============================================================================

(defstruct (transform-reader (:constructor %make-transform-reader))
  "Reader that applies a transformation to each byte read.

   The transform function takes a byte and returns a byte."
  (source nil)
  (transform nil :type function)
  (closed-p nil :type boolean))

(defun make-transform-reader (source transform)
  "Create a reader that applies TRANSFORM to each byte from SOURCE.
   TRANSFORM: (lambda (byte) -> byte)"
  (%make-transform-reader :source source :transform transform))

(defmethod proto:read-into ((reader transform-reader) buffer &key (start 0) (end (length buffer)))
  (check-open reader :read)
  (let ((n (proto:read-into (transform-reader-source reader) buffer :start start :end end))
        (xform (transform-reader-transform reader)))
    (loop for i from start below (+ start n) do
      (setf (aref buffer i) (funcall xform (aref buffer i))))
    n))

(defmethod proto:close* ((reader transform-reader))
  (unless (transform-reader-closed-p reader)
    (setf (transform-reader-closed-p reader) t)
    (proto:close* (transform-reader-source reader))
    t))

(defmethod proto:open-p ((reader transform-reader))
  (not (transform-reader-closed-p reader)))

;;; ============================================================================
;;; Offset Reader - Skip initial bytes
;;; ============================================================================

(defstruct (offset-reader (:constructor %make-offset-reader))
  "Reader that skips the first N bytes of source."
  (source nil)
  (skip-remaining 0 :type fixnum)
  (closed-p nil :type boolean))

(defun make-offset-reader (source offset)
  "Create a reader that skips first OFFSET bytes of SOURCE."
  (%make-offset-reader :source source :skip-remaining offset))

(defmethod proto:read-into ((reader offset-reader) buffer &key (start 0) (end (length buffer)))
  (check-open reader :read)
  ;; Skip bytes if needed
  (block nil
    (loop while (plusp (offset-reader-skip-remaining reader)) do
      (let* ((skip-buf (make-array (min 8192 (offset-reader-skip-remaining reader))
                                   :element-type '(unsigned-byte 8)))
             (n (proto:read-into (offset-reader-source reader) skip-buf)))
        (when (zerop n)
          (return 0))
        (decf (offset-reader-skip-remaining reader) n)))
    ;; Normal read
    (proto:read-into (offset-reader-source reader) buffer :start start :end end)))

(defmethod proto:close* ((reader offset-reader))
  (unless (offset-reader-closed-p reader)
    (setf (offset-reader-closed-p reader) t)
    (proto:close* (offset-reader-source reader))
    t))

(defmethod proto:open-p ((reader offset-reader))
  (not (offset-reader-closed-p reader)))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun limit (stream n &key (direction :input))
  "Limit STREAM to N bytes.
   DIRECTION is :input for readers, :output for writers."
  (ecase direction
    (:input (make-limit-reader stream n))
    (:output (make-limit-writer stream n))))

(defun tee (reader writer)
  "Create a reader that writes all read data to WRITER."
  (make-tee-reader reader writer))

(defun chain (&rest readers)
  "Concatenate READERS into a single reader."
  (apply #'make-chain-reader readers))

(defun broadcast (&rest writers)
  "Create a writer that writes to all WRITERS."
  (apply #'make-multi-writer writers))

(defun counting (stream &key (direction :input))
  "Wrap STREAM in a counting wrapper.
   DIRECTION is :input for readers, :output for writers."
  (ecase direction
    (:input (make-counting-reader stream))
    (:output (make-counting-writer stream))))

(defun transform (reader fn)
  "Create a reader that applies FN to each byte."
  (make-transform-reader reader fn))

(defun skip-bytes (reader n)
  "Create a reader that skips first N bytes."
  (make-offset-reader reader n))

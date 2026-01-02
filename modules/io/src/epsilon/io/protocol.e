;;;; epsilon.io.protocol - stream protocols
;;;;
;;;; Defines the fundamental abstractions for IO operations:
;;;; - Reader: byte source
;;;; - Writer: byte sink
;;;; - Closer: resource cleanup
;;;; - Seeker: random access positioning
;;;;
;;;; Each protocol defines one method that must be implemented; all
;;;; others have default implementations built on top.

(package epsilon.io.protocol
  (import (epsilon.io.conditions cond)))

;;; ============================================================================
;;; Reader Protocol
;;; ============================================================================

(defgeneric read-into (reader buffer &key start end)
  (:documentation
   "Read bytes from READER into BUFFER between START and END.

    BUFFER must be a (simple-array (unsigned-byte 8) (*)).
    START defaults to 0, END defaults to (length buffer).

    Returns:
      - Positive integer: number of bytes read
      - 0: end of stream (EOF)

    Signals:
      - io-error: on read failure
      - closed-error: if reader is closed

    This is the ONLY method that must be implemented for Reader protocol.
    All other reader methods have default implementations."))

(defgeneric read-byte* (reader)
  (:documentation
   "Read a single byte from READER.

    Returns:
      - Integer 0-255: the byte read
      - NIL: end of stream (EOF)

    Default implementation calls read-into with a 1-byte buffer.")
  (:method ((reader t))
    (let ((buf (make-array 1 :element-type '(unsigned-byte 8))))
      (declare (dynamic-extent buf))
      (let ((n (read-into reader buf)))
        (if (plusp n)
            (aref buf 0)
            nil)))))

(defgeneric read-exact (reader buffer &key start end)
  (:documentation
   "Read exactly (- END START) bytes into BUFFER.

    Unlike read-into, this either reads all requested bytes or signals
    short-read-error. Useful when you know exactly how many bytes to expect.

    Signals:
      - short-read-error: if EOF before all bytes read")
  (:method ((reader t) buffer &key (start 0) (end (length buffer)))
    (let ((total 0)
          (remaining (- end start)))
      (loop while (plusp remaining) do
        (let ((n (read-into reader buffer :start (+ start total) :end end)))
          (when (zerop n)
            (error 'cond:short-read-error
                   :operation :read-exact
                   :expected remaining
                   :actual total))
          (incf total n)
          (decf remaining n)))
      total)))

(defgeneric read-all (reader &key max-size initial-size)
  (:documentation
   "Read all bytes from READER until EOF.

    Returns a byte vector containing all data.

    MAX-SIZE: if provided, limits total bytes read
    INITIAL-SIZE: initial buffer allocation (default 4096)")
  (:method ((reader t) &key max-size (initial-size 4096))
    (let* ((chunks nil)
           (total 0)
           (chunk-size initial-size))
      (loop
        (when (and max-size (>= total max-size))
          (return))
        (let* ((size (if max-size
                         (min chunk-size (- max-size total))
                         chunk-size))
               (buf (make-array size :element-type '(unsigned-byte 8)))
               (n (read-into reader buf)))
          (when (zerop n)
            (return))
          (push (if (= n size) buf (subseq buf 0 n)) chunks)
          (incf total n)
          ;; Grow chunk size for large reads
          (setf chunk-size (min (* chunk-size 2) 65536))))
      ;; Combine chunks into single buffer
      (if (null chunks)
          (make-array 0 :element-type '(unsigned-byte 8))
          (let ((result (make-array total :element-type '(unsigned-byte 8)))
                (pos 0))
            (dolist (chunk (nreverse chunks))
              (replace result chunk :start1 pos)
              (incf pos (length chunk)))
            result)))))

;;; ============================================================================
;;; Writer Protocol
;;; ============================================================================

(defgeneric write-from (writer buffer &key start end)
  (:documentation
   "Write bytes from BUFFER[START:END] to WRITER.

    BUFFER must be a (simple-array (unsigned-byte 8) (*)).
    START defaults to 0, END defaults to (length buffer).

    Returns:
      - Positive integer: number of bytes written
      - May write fewer bytes than requested (for non-blocking IO)

    Signals:
      - io-error: on write failure
      - closed-error: if writer is closed

    This is the ONLY method that must be implemented for Writer protocol."))

(defgeneric write-byte* (writer byte)
  (:documentation
   "Write a single BYTE to WRITER.

    Returns the number of bytes written (always 1 on success).

    Default implementation calls write-from with a 1-byte buffer.")
  (:method ((writer t) byte)
    (let ((buf (make-array 1 :element-type '(unsigned-byte 8)
                             :initial-element byte)))
      (declare (dynamic-extent buf))
      (write-from writer buf))))

(defgeneric write-all (writer buffer &key start end)
  (:documentation
   "Write all bytes from BUFFER[START:END] to WRITER.

    Unlike write-from, this writes all bytes or signals an error.

    Signals:
      - short-write-error: if write fails partway through")
  (:method ((writer t) buffer &key (start 0) (end (length buffer)))
    (let ((total 0)
          (remaining (- end start)))
      (loop while (plusp remaining) do
        (let ((n (write-from writer buffer :start (+ start total) :end end)))
          (when (zerop n)
            (error 'cond:short-write-error
                   :operation :write-all
                   :expected remaining
                   :actual total))
          (incf total n)
          (decf remaining n)))
      total)))

(defgeneric flush (writer)
  (:documentation
   "Ensure all buffered data is written to the underlying sink.

    For unbuffered writers, this is a no-op.
    For buffered writers, this forces pending data to be written.

    Returns the writer for chaining.")
  (:method ((writer t))
    writer))

;;; ============================================================================
;;; Closer Protocol
;;; ============================================================================

(defgeneric close* (closable)
  (:documentation
   "Release resources associated with CLOSABLE.

    This method is idempotent: calling it multiple times has no
    additional effect after the first call.

    Returns T if resources were released, NIL if already closed.")
  (:method ((closable t))
    nil))

(defgeneric open-p (closable)
  (:documentation
   "Return T if CLOSABLE is open, NIL if closed.")
  (:method ((closable t))
    t))

;;; ============================================================================
;;; Seeker Protocol
;;; ============================================================================

(defgeneric seek* (seeker offset whence)
  (:documentation
   "Set the position within SEEKER.

    OFFSET is a byte offset.
    WHENCE is one of:
      :start   - offset from beginning
      :current - offset from current position
      :end     - offset from end (offset typically negative)

    Returns the new absolute position.

    Signals:
      - io-error: if seeking is not supported or position invalid"))

(defgeneric position* (positionable)
  (:documentation
   "Return the current byte position within POSITIONABLE.

    Returns NIL if position is not available.")
  (:method ((positionable t))
    nil))

(defgeneric size* (sizable)
  (:documentation
   "Return the total size in bytes of SIZABLE.

    Returns NIL if size is unknown or not applicable.")
  (:method ((sizable t))
    nil))

;;; ============================================================================
;;; Convenience Macros
;;; ============================================================================

(defmacro with-open ((var opener &rest args) &body body)
  "Execute BODY with VAR bound to result of (OPENER ARGS...).
   Ensures CLOSE* is called on VAR when BODY exits.

   Example:
     (with-open (f #'open-file \"data.bin\")
       (read-all f))"
  `(let ((,var (funcall ,opener ,@args)))
     (unwind-protect
          (progn ,@body)
       (close* ,var))))

(defmacro with-reader ((var reader) &body body)
  "Execute BODY with VAR bound to READER, closing on exit."
  `(with-open (,var (lambda () ,reader))
     ,@body))

(defmacro with-writer ((var writer) &body body)
  "Execute BODY with VAR bound to WRITER, flushing and closing on exit."
  `(let ((,var ,writer))
     (unwind-protect
          (progn ,@body)
       (flush ,var)
       (close* ,var))))

;;; ============================================================================
;;; Protocol Predicates
;;; ============================================================================

(defun reader-p (obj)
  "Return T if OBJ implements the reader protocol."
  (and (compute-applicable-methods #'read-into (list obj #() :start 0 :end 0))
       t))

(defun writer-p (obj)
  "Return T if OBJ implements the writer protocol."
  (and (compute-applicable-methods #'write-from (list obj #() :start 0 :end 0))
       t))

(defun closer-p (obj)
  "Return T if OBJ implements the closer protocol."
  (and (compute-applicable-methods #'close* (list obj))
       t))

(defun seeker-p (obj)
  "Return T if OBJ implements the seeker protocol."
  (and (compute-applicable-methods #'seek* (list obj 0 :start))
       t))

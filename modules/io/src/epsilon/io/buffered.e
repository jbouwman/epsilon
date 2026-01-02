;;;; epsilon.io.buffered - Buffered reader and writer
;;;;
;;;; Buffered wrappers for readers and writers.

(package epsilon.io.buffered
  (import (epsilon.io.protocol proto)
          (epsilon.io.conditions cond)))

(defun check-open (stream operation)
  "Signal closed-error if STREAM is not open."
  (unless (proto:open-p stream)
    (error 'cond:closed-error :operation operation :stream stream)))

;;; ============================================================================
;;; Buffered Reader
;;; ============================================================================

(defconstant +default-buffer-size+ 8192
  "Default buffer size for buffered streams.")

(defstruct (buffered-reader (:constructor %make-buffered-reader))
  "Buffered wrapper around a reader.

   Provides byte-at-a-time reading by buffering data from
   the underlying source. Also provides line-oriented reading.

   Implements: Reader, Closer protocols."
  (source nil)                              ; underlying reader
  (buffer nil :type (simple-array (unsigned-byte 8) (*)))
  (position 0 :type fixnum)                 ; next byte to read
  (limit 0 :type fixnum)                    ; end of buffered data
  (eof-p nil :type boolean)                 ; true after source EOF
  (closed-p nil :type boolean))

(defun make-buffered-reader (source &key (buffer-size +default-buffer-size+))
  "Create a buffered reader wrapping SOURCE.

   SOURCE must implement the Reader protocol.
   BUFFER-SIZE controls the internal buffer size (default 8192)."
  (%make-buffered-reader
   :source source
   :buffer (make-array buffer-size :element-type '(unsigned-byte 8))))

(defun buffered-reader-fill (reader)
  "Fill the buffer from the source. Returns bytes read."
  (when (buffered-reader-eof-p reader)
    (return-from buffered-reader-fill 0))
  ;; Compact: move unread data to start
  (let* ((buf (buffered-reader-buffer reader))
         (pos (buffered-reader-position reader))
         (lim (buffered-reader-limit reader))
         (remaining (- lim pos)))
    (when (and (plusp remaining) (plusp pos))
      (replace buf buf :start1 0 :start2 pos :end2 lim))
    (setf (buffered-reader-position reader) 0)
    (setf (buffered-reader-limit reader) remaining)
    ;; Fill rest of buffer
    (let* ((space (- (length buf) remaining))
           (n (proto:read-into (buffered-reader-source reader) buf
                         :start remaining
                         :end (length buf))))
      (when (zerop n)
        (setf (buffered-reader-eof-p reader) t))
      (incf (buffered-reader-limit reader) n)
      n)))

(defun buffered-reader-buffered (reader)
  "Return number of bytes available without reading from source."
  (- (buffered-reader-limit reader) (buffered-reader-position reader)))

(defmethod proto:read-into ((reader buffered-reader) buffer &key (start 0) (end (length buffer)))
  "Read bytes into BUFFER, using internal buffer."
  (check-open reader :read)
  (let ((total 0)
        (requested (- end start)))
    (loop while (< total requested) do
      ;; Try to satisfy from buffer
      (let* ((available (buffered-reader-buffered reader))
             (needed (- requested total))
             (from-buf (min available needed)))
        (when (plusp from-buf)
          (replace buffer (buffered-reader-buffer reader)
                   :start1 (+ start total)
                   :end1 (+ start total from-buf)
                   :start2 (buffered-reader-position reader))
          (incf (buffered-reader-position reader) from-buf)
          (incf total from-buf)))
      ;; If still need more, try to fill buffer
      (when (< total requested)
        (if (buffered-reader-eof-p reader)
            (return total)
            (when (zerop (buffered-reader-fill reader))
              (return total)))))
    total))

(defmethod read-byte* ((reader buffered-reader))
  "Read a single byte efficiently."
  (check-open reader :read)
  (when (= (buffered-reader-position reader) (buffered-reader-limit reader))
    (when (zerop (buffered-reader-fill reader))
      (return-from read-byte* nil)))
  (prog1 (aref (buffered-reader-buffer reader) (buffered-reader-position reader))
    (incf (buffered-reader-position reader))))

(defmethod proto:close* ((reader buffered-reader))
  "Close the buffered reader and underlying source."
  (unless (buffered-reader-closed-p reader)
    (setf (buffered-reader-closed-p reader) t)
    (proto:close* (buffered-reader-source reader))
    t))

(defmethod proto:open-p ((reader buffered-reader))
  (not (buffered-reader-closed-p reader)))

;;; ============================================================================
;;; Buffered Reader - Extended Operations
;;; ============================================================================

(defun buffered-reader-peek (reader &optional (offset 0))
  "Peek at byte at OFFSET from current position without consuming.
   Returns NIL if not enough data available (even after refill)."
  (check-open reader :read)
  (loop while (< (buffered-reader-buffered reader) (1+ offset)) do
    (when (zerop (buffered-reader-fill reader))
      (return-from buffered-reader-peek nil)))
  (aref (buffered-reader-buffer reader)
        (+ (buffered-reader-position reader) offset)))

(defun buffered-reader-skip (reader n)
  "Skip N bytes. Returns number of bytes actually skipped."
  (check-open reader :read)
  (let ((skipped 0))
    (loop while (< skipped n) do
      (let* ((available (buffered-reader-buffered reader))
             (to-skip (min available (- n skipped))))
        (when (plusp to-skip)
          (incf (buffered-reader-position reader) to-skip)
          (incf skipped to-skip)))
      (when (< skipped n)
        (if (zerop (buffered-reader-fill reader))
            (return skipped))))
    skipped))

(defun buffered-reader-read-until (reader delimiter)
  "Read bytes until DELIMITER is found.
   Returns (values bytes found-p) where:
   - bytes: vector of bytes read (not including delimiter)
   - found-p: T if delimiter was found, NIL if EOF first"
  (check-open reader :read)
  (let ((chunks nil)
        (total 0))
    (loop
      ;; Ensure we have data to scan
      (when (= (buffered-reader-position reader) (buffered-reader-limit reader))
        (when (zerop (buffered-reader-fill reader))
          ;; EOF without finding delimiter
          (return-from buffered-reader-read-until
            (values (merge-chunks chunks total) nil))))
      ;; Scan for delimiter in buffer
      (let* ((buf (buffered-reader-buffer reader))
             (pos (buffered-reader-position reader))
             (lim (buffered-reader-limit reader))
             (delim-pos (position delimiter buf :start pos :end lim)))
        (if delim-pos
            ;; Found delimiter
            (let* ((chunk-len (- delim-pos pos))
                   (chunk (subseq buf pos delim-pos)))
              (setf (buffered-reader-position reader) (1+ delim-pos))  ; consume delimiter
              (push chunk chunks)
              (return-from buffered-reader-read-until
                (values (merge-chunks chunks (+ total chunk-len)) t)))
            ;; No delimiter found, consume all and continue
            (let* ((chunk-len (- lim pos))
                   (chunk (subseq buf pos lim)))
              (push chunk chunks)
              (incf total chunk-len)
              (setf (buffered-reader-position reader) lim)))))))

(defun merge-chunks (chunks total-size)
  "Merge list of byte chunks into single vector."
  (if (null chunks)
      (make-array 0 :element-type '(unsigned-byte 8))
      (let ((result (make-array total-size :element-type '(unsigned-byte 8)))
            (pos 0))
        (dolist (chunk (nreverse chunks))
          (replace result chunk :start1 pos)
          (incf pos (length chunk)))
        result)))

(defun buffered-reader-read-line (reader &key (encoding :utf-8))
  "Read a line (terminated by LF or CRLF).
   Returns (values string more-p) where:
   - string: the line without newline
   - more-p: T if more lines may follow, NIL at EOF"
  (multiple-value-bind (bytes found-p)
      (buffered-reader-read-until reader (char-code #\Newline))
    ;; Strip trailing CR if present
    (let* ((len (length bytes))
           (end (if (and (plusp len)
                         (= (aref bytes (1- len)) (char-code #\Return)))
                    (1- len)
                    len))
           (trimmed (if (= end len) bytes (subseq bytes 0 end))))
      (values
       (ecase encoding
         (:utf-8 (sb-ext:octets-to-string trimmed :external-format :utf-8))
         (:ascii (map 'string #'code-char trimmed))
         (:latin-1 (sb-ext:octets-to-string trimmed :external-format :latin-1)))
       (or found-p (plusp len))))))

(defmacro do-lines ((var reader &key (encoding :utf-8)) &body body)
  "Iterate over lines from READER."
  (let ((more-p (gensym "MORE-P")))
    `(loop
       (multiple-value-bind (,var ,more-p)
           (buffered-reader-read-line ,reader :encoding ,encoding)
         (unless ,more-p (return))
         ,@body))))

;;; ============================================================================
;;; Buffered Writer
;;; ============================================================================

(defstruct (buffered-writer (:constructor %make-buffered-writer))
  "Buffered wrapper around a writer.

   Batches writes to reduce syscalls. Data is written to the underlying
   sink when the buffer is full or when flush is called.

   Implements: Writer, Closer protocols."
  (sink nil)                                ; underlying writer
  (buffer nil :type (simple-array (unsigned-byte 8) (*)))
  (position 0 :type fixnum)                 ; next write position
  (closed-p nil :type boolean))

(defun make-buffered-writer (sink &key (buffer-size +default-buffer-size+))
  "Create a buffered writer wrapping SINK.

   SINK must implement the Writer protocol.
   BUFFER-SIZE controls the internal buffer size (default 8192)."
  (%make-buffered-writer
   :sink sink
   :buffer (make-array buffer-size :element-type '(unsigned-byte 8))))

(defun buffered-writer-available (writer)
  "Return bytes of buffer space available."
  (- (length (buffered-writer-buffer writer))
     (buffered-writer-position writer)))

(defmethod proto:write-from ((writer buffered-writer) buffer &key (start 0) (end (length buffer)))
  "Write bytes to the buffer, flushing as needed."
  (check-open writer :write)
  (let ((remaining (- end start))
        (written 0))
    (loop while (plusp remaining) do
      (let* ((space (buffered-writer-available writer))
             (to-buffer (min space remaining)))
        (cond
          ;; Write fits entirely in buffer
          ((plusp to-buffer)
           (replace (buffered-writer-buffer writer) buffer
                    :start1 (buffered-writer-position writer)
                    :start2 (+ start written)
                    :end2 (+ start written to-buffer))
           (incf (buffered-writer-position writer) to-buffer)
           (incf written to-buffer)
           (decf remaining to-buffer))
          ;; Buffer is full, need to flush
          (t
           (buffered-writer-flush-buffer writer)))))
    written))

(defmethod write-byte* ((writer buffered-writer) byte)
  "Write a single byte efficiently."
  (check-open writer :write)
  (when (zerop (buffered-writer-available writer))
    (buffered-writer-flush-buffer writer))
  (setf (aref (buffered-writer-buffer writer)
              (buffered-writer-position writer))
        byte)
  (incf (buffered-writer-position writer))
  1)

(defun buffered-writer-flush-buffer (writer)
  "Write buffered data to sink."
  (let ((pos (buffered-writer-position writer)))
    (when (plusp pos)
      (proto:write-all (buffered-writer-sink writer)
                 (buffered-writer-buffer writer)
                 :start 0 :end pos)
      (setf (buffered-writer-position writer) 0))))

(defmethod proto:flush ((writer buffered-writer))
  "Flush buffer to underlying sink, then flush sink."
  (check-open writer :flush)
  (buffered-writer-flush-buffer writer)
  (proto:flush (buffered-writer-sink writer))
  writer)

(defmethod proto:close* ((writer buffered-writer))
  "Flush and close the writer."
  (unless (buffered-writer-closed-p writer)
    (buffered-writer-flush-buffer writer)
    (setf (buffered-writer-closed-p writer) t)
    (proto:close* (buffered-writer-sink writer))
    t))

(defmethod proto:open-p ((writer buffered-writer))
  (not (buffered-writer-closed-p writer)))

;;; ============================================================================
;;; Buffered Writer - Extended Operations
;;; ============================================================================

(defun buffered-writer-write-string (writer string &key (encoding :utf-8))
  "Write STRING encoded as bytes."
  (let ((bytes (ecase encoding
                 (:utf-8 (sb-ext:string-to-octets string :external-format :utf-8))
                 (:ascii (map '(simple-array (unsigned-byte 8) (*))
                              #'char-code string))
                 (:latin-1 (sb-ext:string-to-octets string :external-format :latin-1)))))
    (proto:write-all writer bytes)))

(defun buffered-writer-write-line (writer string &key (encoding :utf-8))
  "Write STRING followed by a newline."
  (buffered-writer-write-string writer string :encoding encoding)
  (write-byte* writer (char-code #\Newline)))

(defun buffered-writer-buffered (writer)
  "Return number of bytes currently buffered."
  (buffered-writer-position writer))

;;; ============================================================================
;;; Convenience Constructors
;;; ============================================================================

(defun wrap-reader (source &key (buffer-size +default-buffer-size+))
  "Wrap SOURCE in a buffered reader if not already buffered."
  (if (typep source 'buffered-reader)
      source
      (make-buffered-reader source :buffer-size buffer-size)))

(defun wrap-writer (sink &key (buffer-size +default-buffer-size+))
  "Wrap SINK in a buffered writer if not already buffered."
  (if (typep sink 'buffered-writer)
      sink
      (make-buffered-writer sink :buffer-size buffer-size)))

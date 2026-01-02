;;;; epsilon.io.byte-stream - In-memory byte readers and writers
;;;;
;;;; Simple implementations of the Reader and Writer protocols for
;;;; working with in-memory byte data. These are the building blocks
;;;; for testing and for wrapping other data sources.

(package epsilon.io.byte-stream
  (import (epsilon.io.protocol proto)
          (epsilon.io.conditions cond)))

(defun check-open (stream operation)
  "Signal closed-error if STREAM is not open."
  (unless (proto:open-p stream)
    (error 'cond:closed-error :operation operation :stream stream)))

;;; ============================================================================
;;; Byte Reader - Read from a byte vector
;;; ============================================================================

(defstruct (byte-reader (:constructor %make-byte-reader))
  "Reader that yields bytes from an in-memory byte vector.

   Implements: Reader, Seeker, Closer protocols."
  (data nil :type (simple-array (unsigned-byte 8) (*)))
  (position 0 :type fixnum)
  (end 0 :type fixnum :read-only t)
  (closed-p nil :type boolean))

(defun make-byte-reader (bytes &key (start 0) (end (length bytes)))
  "Create a byte-reader from BYTES.
   BYTES can be any sequence of integers 0-255; it will be coerced to a byte array.
   Optionally specify START and END to read only a slice."
  (let ((data (if (typep bytes '(simple-array (unsigned-byte 8) (*)))
                  bytes
                  (coerce bytes '(simple-array (unsigned-byte 8) (*))))))
    (%make-byte-reader :data data
                       :position start
                       :end (min end (length data)))))

(defmethod proto:read-into ((reader byte-reader) buffer &key (start 0) (end (length buffer)))
  "Read bytes from the byte-reader into BUFFER."
  (check-open reader :read)
  (let* ((available (- (byte-reader-end reader) (byte-reader-position reader)))
         (requested (- end start))
         (count (min available requested)))
    (when (plusp count)
      (replace buffer (byte-reader-data reader)
               :start1 start
               :end1 (+ start count)
               :start2 (byte-reader-position reader))
      (incf (byte-reader-position reader) count))
    count))

(defmethod proto:read-byte* ((reader byte-reader))
  "Read a single byte from the reader."
  (check-open reader :read)
  (let ((pos (byte-reader-position reader)))
    (if (< pos (byte-reader-end reader))
        (prog1 (aref (byte-reader-data reader) pos)
          (incf (byte-reader-position reader)))
        nil)))

(defmethod proto:close* ((reader byte-reader))
  "Close the byte-reader."
  (if (byte-reader-closed-p reader)
      nil
      (progn
        (setf (byte-reader-closed-p reader) t)
        t)))

(defmethod proto:open-p ((reader byte-reader))
  (not (byte-reader-closed-p reader)))

(defmethod proto:seek* ((reader byte-reader) offset whence)
  "Seek within the byte data."
  (check-open reader :seek)
  (let ((new-pos (ecase whence
                   (:start offset)
                   (:current (+ (byte-reader-position reader) offset))
                   (:end (+ (byte-reader-end reader) offset)))))
    (when (or (< new-pos 0) (> new-pos (byte-reader-end reader)))
      (error 'cond:seek-error :offset offset :whence whence :stream reader))
    (setf (byte-reader-position reader) new-pos)))

(defmethod proto:position* ((reader byte-reader))
  (byte-reader-position reader))

(defmethod proto:size* ((reader byte-reader))
  (byte-reader-end reader))

;;; ============================================================================
;;; Byte Writer - Write to a growable byte vector
;;; ============================================================================

(defstruct (byte-writer (:constructor %make-byte-writer))
  "Writer that accumulates bytes into a growable buffer.

   Implements: Writer, Closer protocols."
  (data nil :type (array (unsigned-byte 8) (*)))
  (position 0 :type fixnum)
  (closed-p nil :type boolean))

(defun make-byte-writer (&key (initial-size 256))
  "Create a byte-writer with initial buffer capacity.
   The buffer grows as needed."
  (%make-byte-writer
   :data (make-array initial-size
                     :element-type '(unsigned-byte 8)
                     :adjustable t
                     :fill-pointer 0)))

(defun byte-writer-ensure-capacity (writer additional)
  "Ensure the writer can accommodate ADDITIONAL more bytes."
  (let* ((data (byte-writer-data writer))
         (current-size (array-total-size data))
         (needed (+ (byte-writer-position writer) additional)))
    (when (> needed current-size)
      (let ((new-size (max needed (* current-size 2))))
        (setf (byte-writer-data writer) (adjust-array data new-size))))))

(defmethod proto:write-from ((writer byte-writer) buffer &key (start 0) (end (length buffer)))
  "Write bytes to the byte-writer."
  (check-open writer :write)
  (let ((count (- end start)))
    (byte-writer-ensure-capacity writer count)
    (let* ((data (byte-writer-data writer))
           (write-pos (byte-writer-position writer))
           (new-end (+ write-pos count)))
      ;; Extend fill-pointer BEFORE replace (replace respects fill-pointer bounds)
      (when (> new-end (fill-pointer data))
        (setf (fill-pointer data) new-end))
      (replace data buffer
               :start1 write-pos
               :end1 new-end
               :start2 start
               :end2 end)
      (setf (byte-writer-position writer) new-end))
    count))

(defmethod proto:write-byte* ((writer byte-writer) byte)
  "Write a single byte."
  (check-open writer :write)
  (byte-writer-ensure-capacity writer 1)
  (setf (aref (byte-writer-data writer) (byte-writer-position writer)) byte)
  (incf (byte-writer-position writer))
  (setf (fill-pointer (byte-writer-data writer))
        (max (fill-pointer (byte-writer-data writer))
             (byte-writer-position writer)))
  1)

(defmethod proto:close* ((writer byte-writer))
  "Close the byte-writer."
  (if (byte-writer-closed-p writer)
      nil
      (progn
        (setf (byte-writer-closed-p writer) t)
        t)))

(defmethod proto:open-p ((writer byte-writer))
  (not (byte-writer-closed-p writer)))

(defmethod proto:position* ((writer byte-writer))
  (byte-writer-position writer))

(defmethod proto:size* ((writer byte-writer))
  (fill-pointer (byte-writer-data writer)))

(defun byte-writer-bytes (writer)
  "Return a copy of the bytes written so far."
  (let* ((data (byte-writer-data writer))
         (len (fill-pointer data))
         (result (make-array len :element-type '(unsigned-byte 8))))
    (replace result data)
    result))

(defun byte-writer-string (writer &key (encoding :utf-8))
  "Return bytes as a decoded string."
  (let ((bytes (byte-writer-bytes writer)))
    (ecase encoding
      (:utf-8 (sb-ext:octets-to-string bytes :external-format :utf-8))
      (:ascii (map 'string #'code-char bytes))
      (:latin-1 (sb-ext:octets-to-string bytes :external-format :latin-1)))))

(defun byte-writer-reset (writer)
  "Reset the writer to empty state (for reuse)."
  (setf (byte-writer-position writer) 0)
  (setf (fill-pointer (byte-writer-data writer)) 0)
  writer)

;;; ============================================================================
;;; Null Streams - Discard/empty streams
;;; ============================================================================

(defstruct null-reader
  "Reader that always returns EOF.")

(defvar *null-reader* (make-null-reader)
  "Singleton null reader that always returns EOF.")

(defmethod proto:read-into ((reader null-reader) buffer &key start end)
  (declare (ignore buffer start end))
  0)

(defmethod proto:read-byte* ((reader null-reader))
  nil)

(defmethod proto:close* ((reader null-reader))
  nil)

(defmethod proto:open-p ((reader null-reader))
  t)

(defstruct null-writer
  "Writer that discards all writes.")

(defvar *null-writer* (make-null-writer)
  "Singleton null writer that discards everything.")

(defmethod proto:write-from ((writer null-writer) buffer &key (start 0) (end (length buffer)))
  (- end start))

(defmethod proto:write-byte* ((writer null-writer) byte)
  (declare (ignore byte))
  1)

(defmethod proto:flush ((writer null-writer))
  writer)

(defmethod proto:close* ((writer null-writer))
  nil)

(defmethod proto:open-p ((writer null-writer))
  t)

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun bytes-to-reader (bytes)
  "Shorthand for make-byte-reader."
  (make-byte-reader bytes))

(defun string-to-reader (string &key (encoding :utf-8))
  "Create a reader from a string."
  (let ((bytes (ecase encoding
                 (:utf-8 (sb-ext:string-to-octets string :external-format :utf-8))
                 (:ascii (map '(simple-array (unsigned-byte 8) (*))
                              #'char-code string))
                 (:latin-1 (sb-ext:string-to-octets string :external-format :latin-1)))))
    (make-byte-reader bytes)))

(defun collect-bytes (reader &key (max-size nil))
  "Read all bytes from READER and return as a vector."
  (proto:read-all reader :max-size max-size))

(defun copy-stream (reader writer &key (buffer-size 8192))
  "Copy all bytes from READER to WRITER.
   Returns total bytes copied."
  (let ((buf (make-array buffer-size :element-type '(unsigned-byte 8)))
        (total 0))
    (loop
      (let ((n (proto:read-into reader buf)))
        (when (zerop n)
          (return total))
        (proto:write-all writer buf :start 0 :end n)
        (incf total n)))))

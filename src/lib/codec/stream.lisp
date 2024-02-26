(in-package #:lib.codec)

(defclass decompressing-stream (fundamental-binary-input-stream)
  ((wrapped-stream :initarg :stream :reader wrapped-stream)

   (dstate :initarg :dstate :reader dstate)
   (dfun :initarg :dfun :reader dfun)

   (input-buffer :initform (->u8 4096)
                 :reader input-buffer)
   (input-buffer-index :initform 0 :accessor input-buffer-index)
   (input-buffer-n-bytes :initform 0 :accessor input-buffer-n-bytes)

   (output-buffer :initform (->u8 4096)
                  :reader output-buffer)
   (output-buffer-index :initform 0 :accessor output-buffer-index)
   (output-buffer-n-bytes :initform 0 :accessor output-buffer-n-bytes)))

(defun make-decompressing-stream (format stream)
  (multiple-value-bind (state dfun)
      (ecase format
        ((:deflate :zlib :gzip)
         (values (make-inflate-state format) #'%inflate))
        ((:bzip2)
         (values (make-bzip2-state) #'%bzip2-decompress)))
    (make-instance 'decompressing-stream
                   :stream stream
                   :dstate state
                   :dfun dfun)))

(defun output-available-p (stream)
  (/= (output-buffer-index stream) (output-buffer-n-bytes stream)))

(defun input-available-p (stream)
  (/= (input-buffer-index stream) (input-buffer-n-bytes stream)))

(defun refill-stream-input-buffer (stream)
  (with-slots (input-buffer wrapped-stream
                            input-buffer-index input-buffer-n-bytes)
      stream
    (let ((n-bytes-read (read-sequence input-buffer wrapped-stream)))
      (setf input-buffer-index 0 input-buffer-n-bytes n-bytes-read)
      (values))))

(defun refill-stream-output-buffer (stream)
  (unless (input-available-p stream)
    (refill-stream-input-buffer stream))
  (multiple-value-bind (bytes-read bytes-output)
      (funcall (the function (dfun stream))
               (dstate stream)
               (input-buffer stream)
               (output-buffer stream)
                :input-start (input-buffer-index stream)
                :input-end (input-buffer-n-bytes stream))
    (setf (output-buffer-index stream) 0
          (output-buffer-n-bytes stream) bytes-output
          (input-buffer-index stream) (+ (input-buffer-index stream) bytes-read))
    (assert (<= (input-buffer-index stream) (input-buffer-n-bytes stream)))))

;;; methods

(defun read-and-decompress-byte (stream)
  (flet ((maybe-done ()
           (when (output-available-p stream)
             (return-from read-and-decompress-byte
               (aref (output-buffer stream)
                     (prog1 (output-buffer-index stream)
                       (incf (output-buffer-index stream))))))))
    ;; several input buffers may be used up before output is available
    ;; => read-byte should refill "something" while at all possible,
    ;; like read-sequence already does.
    (loop initially (maybe-done)
          do (refill-stream-output-buffer stream)
             (maybe-done)
             (unless (input-available-p stream)
               (refill-stream-input-buffer stream))
             ;; If we didn't refill, then we must be all done.
             (unless (input-available-p stream)
               (finish-dstate (dstate stream))
               (return :eof)))))

(defun copy-existing-output (stream seq start end)
  (declare (type ->u8 seq))
  (let ((amount (min (- end start)
                     (- (output-buffer-n-bytes stream)
                        (output-buffer-index stream)))))
    (replace seq (output-buffer stream)
             :start1 start :end1 end
             :start2 (output-buffer-index stream)
             :end2 (output-buffer-n-bytes stream))
    (incf (output-buffer-index stream) amount)
    (+ start amount)))

(defmethod stream-read-sequence ((stream decompressing-stream) seq &optional (start 0) end)
  (unless (typep seq '->u8)
    (return-from stream-read-sequence (call-next-method)))
  (let ((end (or end (length seq))))
    (loop initially (when (output-available-p stream)
                      (setf start (copy-existing-output stream seq
                                                        start end)))
          while (< start end)
          do (unless (input-available-p stream)
               (refill-stream-input-buffer stream))
             ;; If we didn't refill, then we must be all done.
             (unless (input-available-p stream)
               (finish-dstate (dstate stream))
               (loop-finish))
             ;; Decompress directly into the user-provided buffer.
             (multiple-value-bind (bytes-read bytes-output)
                 (funcall (the function (dfun stream))
                          (dstate stream)
                          (input-buffer stream)
                          seq
                          :input-start (input-buffer-index stream)
                          :input-end (input-buffer-n-bytes stream)
                          :output-start start
                          :output-end end)
               (incf (input-buffer-index stream) bytes-read)
               (incf start bytes-output))
          finally (return start))))

(defmethod stream-read-byte ((stream decompressing-stream))
  (read-and-decompress-byte stream))

(defclass compressing-stream (fundamental-binary-output-stream)
  ((openp
    :initform t
    :accessor openp)
   (compressor
    :initarg :compressor
    :accessor compressor))
  (:documentation
   "A stream that transparently compresses its input and writes the
compressed data to another stream."))

(defun make-compressing-stream (compressor-type stream)
  "Return a COMPRESSING-STREAM that transparently compresses its input and
writes it to STREAM. COMPRESSOR-TYPE is a symbol naming the compressor class to
use.

Closing the returned COMPRESSING-STREAM merely finalizes the compression and
does not close STREAM."
  (make-instance
   'compressing-stream
   :compressor (make-instance
                compressor-type
                :callback (make-stream-output-callback stream))))

(defmethod stream-write-byte ((stream compressing-stream) byte)
  (unless (openp stream)
    (error 'stream-closed :stream stream))
  (compress-octet byte (compressor stream))
  byte)

(defmethod stream-write-sequence ((stream compressing-stream) sequence &optional start end)
  (unless (openp stream)
    (error 'stream-closed :stream stream))
  (let ((vector (if (typep sequence 'vector)
                    sequence
                    (coerce sequence 'vector))))
    (compress-u8-vector vector (compressor stream) :start start :end end))
  sequence)

(defmethod stream-element-type ((stream compressing-stream))
  'u8)

(defmethod close ((stream compressing-stream) &key abort)
  (declare (ignore abort))
  (when (openp stream)
    (finish-compression (compressor stream))
    (setf (openp stream) nil)
    t))

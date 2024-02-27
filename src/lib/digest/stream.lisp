(defpackage #:lib.digest.stream
  (:use
   #:cl
   #:sb-gray
   #:lib.type
   #:lib.digest.generic)
  (:export
   #:with-authenticating-stream))

(in-package #:lib.digest.stream)

;;; implementation via Gray streams

;;; These could be specialized for particular implementations by hooking
;;; in directly to the "native" stream methods for the implementation.

(defclass octet-stream ()
  ((buffer :accessor buffer
           :initarg :buffer
           :type ->u8)))

(defmethod sb-gray::stream-element-type ((stream octet-stream))
  '(unsigned-byte 8))

;;; input streams

(defclass octet-input-stream (octet-stream fundamental-binary-input-stream)
  ((index :accessor index :initarg :index :type array-index)
   (end :accessor end :initarg :end :type array-index)))

(defmethod stream-read-byte ((stream octet-input-stream))
  (let ((buffer (buffer stream))
        (index (index stream)))
    (declare (type ->u8 buffer))
    (cond
      ((>= index (end stream)) :eof)
      (t
       (setf (index stream) (1+ index))
       (aref buffer index)))))

(defmethod stream-read-sequence ((stream octet-input-stream) seq &optional (start 0) end)
  (typecase seq
    (->u8
     (let ((end (or end (length seq))))
       (let ((buffer (buffer stream))
             (index (index stream))
             (buffer-end (end stream)))
         (declare (type ->u8 buffer))
         (let* ((remaining (- buffer-end index))
                (length (- end start))
                (amount (min remaining length)))
           (replace seq buffer :start1 start :end1 end :start2 index :end2
                    buffer-end)
           (setf (index stream) (+ index amount))
           (+ start amount)))))
    (t (call-next-method))))

(defun make-octet-input-stream (buffer &optional (start 0) end)
  "As MAKE-STRING-INPUT-STREAM, only with octets instead of characters."
  (declare (type ->u8 buffer)
           (type array-index start)
           (type (or array-index null) end))
  (let ((end (or end (length buffer))))
    (make-instance 'octet-input-stream
                   :buffer buffer :index start :end end)))

(defmacro with-octet-input-stream ((var buffer &optional (start 0) end) &body body)
  `(with-open-stream (,var (make-octet-input-stream ,buffer ,start ,end))
     ,@body))


;;; output streams

(defclass octet-output-stream (octet-stream fundamental-binary-output-stream)
  ((index :accessor index :initform 0 :type array-index)))

(defmethod :stream-write-byte ((stream octet-output-stream) integer)
  (declare (type (unsigned-byte 8) integer))
  (let* ((buffer (buffer stream))
         (length (length buffer))
         (index (index stream)))
    (declare (type ->u8 buffer))
    (when (>= index (length buffer))
      (let ((new-buffer (make-array (* 2 length)
                                    :element-type '(unsigned-byte 8))))
        (declare (type ->u8 new-buffer))
        (replace new-buffer buffer)
        (setf buffer new-buffer
              (buffer stream) new-buffer)))
    (setf (aref buffer index) integer
          (index stream) (1+ index))
    integer))

(defmethod stream-write-sequence ((stream octet-output-stream) seq &optional (start 0) end)
  (typecase seq
    (->u8
     (let ((end (or end (length seq))))
       (let* ((buffer (buffer stream))
              (length (length buffer))
              (index (index stream))
              (amount (- end start)))
         (declare (type ->u8 buffer))
         (when (>= (+ index amount) length)
           (let ((new-buffer
                  (make-array (* 2 (max amount length)) :element-type
                              '(unsigned-byte 8))))
             (declare (type ->u8 new-buffer))
             (replace new-buffer buffer)
             (setf buffer new-buffer
                   (buffer stream) new-buffer)))
         (replace buffer seq :start1 index :start2 start :end2 end)
         (incf (index stream) amount)
         seq)))
    (t (call-next-method))))

(defmethod sb-gray::stream-clear-output ((stream octet-output-stream))
  (setf (index stream) 0)
  nil)

(defun get-output-stream-octets (stream)
  "As GET-OUTPUT-STREAM-STRING, only with an octet output-stream instead
of a string output-stream."
  (let ((buffer (buffer stream))
        (index (index stream)))
    (setf (index stream) 0)
    (subseq buffer 0 index)))

(defun make-octet-output-stream ()
  "As MAKE-STRING-OUTPUT-STREAM, only with octets instead of characters."
  (make-instance 'octet-output-stream
                 :buffer (make-array 128 :element-type '(unsigned-byte 8))))

(defmacro with-octet-output-stream ((var) &body body)
  `(with-open-stream (,var (make-octet-output-stream))
     ,@body
     (get-output-stream-octets ,var)))

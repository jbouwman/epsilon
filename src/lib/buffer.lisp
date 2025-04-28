;;;; In-memory byte-array-valued streams

(defpackage #:epsilon.lib.buffer
  (:use #:cl
        #:sb-gray
        #:epsilon.lib.type)
  (:export #:make-buffer
           #:make-input-stream
           #:make-output-stream
           #:stream-buffer
           #:with-buffer))

(in-package #:epsilon.lib.buffer)

(defun make-buffer (&key (initial-length 32))
  (make-array initial-length :element-type 'u8 
                             :fill-pointer 0 :adjustable t))

(defclass buffer-output-stream (fundamental-binary-output-stream)
  ((buffer :initform (make-buffer)
           :accessor stream-buffer)
   (position :initform 0
             :accessor stream-position)))

(defun make-output-stream ()
  (make-instance 'buffer-output-stream))

(defmethod stream-write-byte ((stream buffer-output-stream) byte)
  (vector-push-extend byte (stream-buffer stream))
  (incf (stream-position stream))
  byte)

(defmethod stream-write-sequence ((stream buffer-output-stream) seq &optional (start 0) end)
  (let* ((buffer (stream-buffer stream))
         (end (or end (length seq)))
         (count (- end start))
         (current-size (length buffer))
         (new-size (+ current-size count)))
    (adjust-array buffer new-size :fill-pointer new-size)
    (replace buffer seq :start1 current-size :start2 start :end2 end)
    (incf (stream-position stream) count))
  seq)

(defclass buffer-input-stream (fundamental-binary-input-stream)
  ((buffer :initarg :buffer             ; array of unsigned-byte 8
           :accessor stream-buffer)
   (position :initform 0
             :accessor stream-position)))

(defun make-input-stream (buffer)
  (make-instance 'buffer-input-stream :buffer buffer))

(defmethod stream-element-type ((stream buffer-input-stream))
  'u8)

(defmethod stream-read-byte ((stream buffer-input-stream))
  (with-slots (buffer position) stream
    (when (< position (length buffer))
      (let ((byte (aref buffer position)))
        (incf position)
        byte))))

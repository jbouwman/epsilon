(in-package :epsilon.net.tls)

(declaim
 (inline
  make-buffer
  buffer-length
  buffer-elt
  set-buffer-elt
  s/b-replace
  b/s-replace))

(defun make-buffer (size)
  (ffi:make-shareable-byte-vector size))

(defun buffer-length (buf)
  (length buf))

(defun buffer-elt (buf index)
  (elt buf index))
(defun set-buffer-elt (buf index val)
  (setf (elt buf index) val))
(defsetf buffer-elt set-buffer-elt)

(defun s/b-replace (seq buf &key (start1 0) end1 (start2 0) end2)
  (replace seq buf :start1 start1 :end1 end1 :start2 start2 :end2 end2))
(defun b/s-replace (buf seq &key (start1 0) end1 (start2 0) end2)
  (replace buf seq :start1 start1 :end1 end1 :start2 start2 :end2 end2))

(defmacro with-pointer-to-vector-data ((ptr buf) &body body)
  `(ffi:with-pointer-to-vector-data (,ptr ,buf)
    ,@body))

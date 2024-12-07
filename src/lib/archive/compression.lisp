(in-package #:epsilon.lib.archive)

(defgeneric make-decompression-state (format))

(defgeneric call-with-decompressed-buffer (function vector start end state))

(defgeneric make-compression-state (format &key buffer))

(defgeneric call-with-compressed-buffer (function vector start end state))

(defgeneric call-with-completed-compressed-buffer (function state))

(defmethod make-decompression-state (format)
  (error 'unsupported-compression-method :compression-method format))

(defmethod make-decompression-state ((format (eql nil)))
  nil)

(defmethod make-decompression-state ((format (eql :store)))
  nil)

(defmethod call-with-decompressed-buffer (function input start end (state (eql nil)))
  (funcall function input start end))

(defclass deflate-state-2 ()
  ((output-buffer :initarg :buffer)))

(defmethod make-decompression-state ((format (eql :deflate)))
  (make-instance 'deflate-state-2 :buffer (make-instance 'fast-output-stream)))

(defmethod call-with-decompressed-buffer (function input start end (state deflate-state-2))
  (with-slots (output-buffer) state
    (epsilon.lib.codec::decompress output-buffer (epsilon.lib.codec::make-dstate :deflate) input
                           :input-state start
                           :input-end end)
    (let ((result (finish-output-stream output-buffer)))
      (funcall function result 0 (length result)))
    (- end start)))

(defmethod make-compression-state ((format (eql nil)) &key buffer)
  (declare (ignore buffer))
  nil)

(defmethod make-compression-state ((format (eql :store)) &key buffer)
  (declare (ignore buffer))
  nil)

(defmethod call-with-compressed-buffer (function vector start end (state null))
  (funcall function vector start end))

(defmethod call-with-completed-compressed-buffer (function (state (eql nil)))
  (funcall function #() 0 0))

(defmethod make-compression-state ((format (eql :deflate)) &key buffer)
  (declare (ignore buffer))
  (make-instance 'epsilon.lib.codec::deflate-compressor))

(defmethod call-with-compressed-buffer (function vector start end (state epsilon.lib.codec::deflate-compressor))
  (setf (epsilon.lib.codec::callback state) (lambda (buffer end) (funcall function buffer 0 end)))
  (epsilon.lib.codec::compress-u8-vector vector state :start start :end end))

(defmethod call-with-completed-compressed-buffer (function (state epsilon.lib.codec::deflate-compressor))
  (setf (epsilon.lib.codec::callback state) (lambda (buffer end) (funcall function buffer 0 end)))
  (epsilon.lib.codec::finish-compression state))

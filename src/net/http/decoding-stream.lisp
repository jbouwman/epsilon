(defpackage #:net.http.decoding-stream
  (:use
   #:cl
   #:sb-gray
   #:lib.char
   #:lib.type)
  (:export
   #:make-decoding-stream
   #:decoding-stream)
  (:documentation "Provides character decoding stream."))

(in-package :net.http.decoding-stream)

(declaim (type fixnum +buffer-size+))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +buffer-size+ 128))

(defclass decoding-stream (fundamental-character-input-stream)
  ((stream :type stream
           :initarg :stream
           :initform (error ":stream is required")
           :accessor decoding-stream-stream)
   (encoding :initarg :encoding
             :initform (error ":encoding is required")
             :accessor decoding-stream-encoding)
   (buffer :type (->u8 #.+buffer-size+)
           :initform (make-array +buffer-size+ :element-type 'u8)
           :accessor decoding-stream-buffer)
   (buffer-position :type fixnum
                    :initform +buffer-size+
                    :accessor decoding-stream-buffer-position)
   (buffer-end-position :type fixnum
                        :initform -1
                        :accessor decoding-stream-buffer-end-position)
   (last-char :type character
              :initform #\Nul
              :accessor decoding-stream-last-char)
   (last-char-size :type fixnum
                   :initform 0
                   :accessor decoding-stream-last-char-size)
   (on-close :type (or null function) :initform nil :initarg :on-close)))

(defmethod initialize-instance :after ((stream decoding-stream) &rest initargs)
  (declare (ignore initargs))
  (with-slots (encoding) stream
    (when (keywordp encoding)
      (setf encoding (get-character-encoding encoding)))))

(defun make-decoding-stream (stream &key (encoding lib.char:*default-character-encoding*)
                                      (on-close))
  (let ((decoding-stream (make-instance 'decoding-stream
                                        :stream stream
                                        :encoding encoding
                                        :on-close on-close)))
    (fill-buffer decoding-stream)
    decoding-stream))

(defun fill-buffer (stream)
  (declare (optimize speed))
  (with-slots (stream buffer buffer-position buffer-end-position) stream
    (declare (type (->u8 #.+buffer-size+) buffer)
             (type fixnum buffer-position))
    (let ((to-read (- +buffer-size+ buffer-position)))
      (declare (type fixnum to-read))
      (replace buffer buffer
               :start1 0
               :start2 buffer-position
               :end2 +buffer-size+)
      (setf buffer-position 0)
      (let ((n (read-sequence buffer stream :start to-read)))
        (declare (type fixnum n))
        (unless (= n +buffer-size+)
          (setf buffer-end-position n))))))

(defun needs-to-fill-buffer-p (stream)
  (declare (optimize speed))
  (when (/= -1 (the fixnum (decoding-stream-buffer-end-position stream)))
    (return-from needs-to-fill-buffer-p nil))

  (with-slots (buffer-position encoding) stream
    (< (- +buffer-size+ (the fixnum buffer-position))
       (the fixnum (enc-max-units-per-char encoding)))))

(defmethod stream-read-char ((stream decoding-stream))
  (declare (optimize speed))
  (when (needs-to-fill-buffer-p stream)
    (fill-buffer stream))

  (when (= (the fixnum (decoding-stream-buffer-end-position stream))
           (the fixnum (decoding-stream-buffer-position stream)))
    (return-from stream-read-char :eof))

  (with-slots (buffer buffer-position encoding last-char last-char-size)
      stream
    (declare (fixnum buffer-position))
    (let* ((mapping (lookup-mapping *string-vector-mappings* encoding))
           (counter (code-point-counter mapping)))
      (declare (type function counter))
      (multiple-value-bind (chars new-end)
          (funcall counter buffer buffer-position +buffer-size+ 1)
        (declare (ignore chars) (fixnum new-end))
        (let ((string (make-string 1 :element-type 'lib.char:unicode-char))
              (size (the fixnum (- new-end buffer-position))))
          (funcall (the function (lib.char:decoder mapping))
                   buffer buffer-position new-end string 0)
          (setf buffer-position new-end
                last-char (aref string 0)
                last-char-size size)
          (aref string 0))))))

(defmethod stream-unread-char ((stream decoding-stream) char)
  (let ((last-char (decoding-stream-last-char stream)))
    (when (char= last-char #\Nul)
      (error "No character to unread from this stream"))
    (unless (char= char last-char)
      (error "Last character read (~S) was different from ~S"
             last-char char))
    (with-slots (buffer-position last-char-size) stream
      (decf buffer-position last-char-size))
    (with-slots (last-char last-char-size) stream
      (setf last-char #\Nul
            last-char-size 0))
    nil))

(defmethod open-stream-p ((stream decoding-stream))
  (open-stream-p (decoding-stream-stream stream)))

(defmethod stream-element-type ((stream decoding-stream))
  'unicode-char)

(defmethod close ((stream decoding-stream) &key abort)
  ;; TODO: modify me to return the connection to the connection pool
  (with-slots (stream) stream
    (when (open-stream-p stream)
      (close stream :abort abort))))

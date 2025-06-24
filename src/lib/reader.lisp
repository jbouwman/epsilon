(defpackage :epsilon.lib.reader
  (:use
   :cl
   :epsilon.lib.syntax
   :epsilon.lib.type)
  (:shadow)
  (:local-nicknames
   (#:char #:epsilon.lib.char))
  (:export
   :make-reader
   :read-string))

(in-package :epsilon.lib.reader)

(defvar *default-eol-style*
  #+:win32 :crlf
  #-:win32 :lf
  "The end-of-line style used by external formats if none is
explicitly given.  Depends on the OS the code is compiled on.")

(defvar *default-little-endian*
  #+:little-endian t
  #-:little-endian nil
  "Whether external formats are little-endian by default
\(i.e. unless explicitly specified).  Depends on the platform
the code is compiled on.")

(declaim (type fixnum +buffer-size+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +buffer-size+ 8192
    "Default size for buffers used for internal purposes."))

(defclass reader (fundamental-character-input-stream)
  ((stream :type stream
           :initarg :stream
           :initform (error ":stream is required")
           :accessor reader-stream)
   (encoding :initarg :encoding
             :initform (error ":encoding is required")
             :accessor reader-encoding)
   (buffer :type (->u8 #.+buffer-size+)
           :initform (->u8 +buffer-size+)
           :accessor reader-buffer)
   (buffer-position :type fixnum
                    :initform +buffer-size+
                    :accessor reader-buffer-position)
   (buffer-end-position :type fixnum
                        :initform -1
                        :accessor reader-buffer-end-position)
   (last-char :type character
              :initform #\Nul
              :accessor reader-last-char)
   (last-char-size :type fixnum
                   :initform 0
                   :accessor reader-last-char-size)
   (on-close :type (or null function) :initform nil :initarg :on-close)))

(defmethod initialize-instance :after ((stream reader) &rest initargs)
  (declare (ignore initargs))
  (with-slots (encoding) stream
    (when (keywordp encoding)
      (setf encoding (char:get-character-encoding encoding)))))

(defun fill-buffer (stream)
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

(defun make-reader (stream &key encoding (on-close))
  (let ((reader (make-instance 'reader
                               :stream stream
                               :encoding encoding
                               :on-close on-close)))
    (fill-buffer reader)
    reader))

(defun needs-to-fill-buffer-p (stream)
  (when (/= -1 (the fixnum (reader-buffer-end-position stream)))
    (return-from needs-to-fill-buffer-p nil))

  (with-slots (buffer-position encoding) stream
    (< (- +buffer-size+ (the fixnum buffer-position))
       (the fixnum (char:enc-max-units-per-char encoding)))))

(defmethod stream-read-char ((stream reader))
  (when (needs-to-fill-buffer-p stream)
    (fill-buffer stream))
  (when (= (the fixnum (reader-buffer-end-position stream))
           (the fixnum (reader-buffer-position stream)))
    (return-from stream-read-char :eof))
  (with-slots (buffer buffer-position encoding last-char last-char-size)
      stream
    (declare (fixnum buffer-position))
    ;; FIXME
    #++
    (let* ((mapping (char:lookup-mapping char:*string-vector-mappings* encoding))
           (counter (char:code-point-counter mapping)))
      (declare (type function counter))
      (multiple-value-bind (chars new-end)
          (funcall counter buffer buffer-position +buffer-size+ 1)
        (declare (ignore chars) (fixnum new-end))
        (let ((string (make-string 1 :element-type 'char:unicode-char))
              (size (the fixnum (- new-end buffer-position))))
          (funcall (the function (char:decoder mapping))
                   buffer buffer-position new-end string 0)
          (setf buffer-position new-end
                last-char (aref string 0)
                last-char-size size)
          (aref string 0))))))

(defmethod open-stream-p ((stream reader))
  (open-stream-p (reader-stream stream)))

(defmethod stream-element-type ((stream reader))
  'character)

(defmethod close ((stream reader) &key abort)
  (with-slots (stream) stream
    (when (open-stream-p stream)
      (close stream :abort abort))))

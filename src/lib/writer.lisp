(defpackage :epsilon.lib.writer
  (:use
   :cl)
  (:shadow
   :write-char
   :write-string)
  (:local-nicknames
   (#:char #:epsilon.lib.char))
  (:export
   :make-writer
   :write-char
   :write-string))

(in-package :epsilon.lib.writer)

(defclass writer ()
  ((stream :initarg :stream
           :reader writer-stream)
   (column :initform 0
           :accessor output-column)
   (row :initform 0
           :accessor output-row)
   (encoding :initform (char:make-encoding :utf-8)
             :initarg :encoding
             :reader stream-encoding)))

(defun make-writer (stream &key (encoding :utf-8))
  (unless (streamp stream)
    (error "not a stream"))
  (unless (open-stream-p stream)
    (error 'stream-closed :stream stream))
  (make-instance 'writer
                 :stream stream
                 :encoding (char:make-encoding encoding)))

(defun start-line-p (writer)
  "Tell if writer is already at start of fresh new line."
  (zerop (output-column writer)))

(defun write-string (writer string)
  (let ((newline-pos (position #\Newline string :from-end t)))
    (when newline-pos
      (setf (output-column writer) (- (length string) newline-pos 1))))
  (write-sequence string (writer-stream writer)))

(defun write-char (writer char)
  (if (char= char #\Newline)
      (setf (output-column writer) 0)
      (incf (output-column writer)))
  (write-char char (writer-streeam writer)))

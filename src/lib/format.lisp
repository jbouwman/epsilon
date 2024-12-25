(defpackage #:epsilon.lib.format
  (:use #:cl #:sb-gray)
  (:export #:format-output-stream
           #:output-column))

(in-package #:epsilon.lib.format)

(defclass format-output-stream (fundamental-character-output-stream)
  ((column :initform 0 :accessor output-column)
   (understream :initarg :understream :initform (error "required!"))))

(defmethod stream-write-sequence ((s format-output-stream) seq &optional start end)
  "Write SEQ to stream S."
  (let ((newline-pos (position #\Newline seq :from-end t)))
    (when newline-pos
      (setf (output-column s) (- (length seq) newline-pos 1))))
  (write-sequence seq (slot-value s 'understream) :start start :end end))

(defmethod stream-line-column ((s format-output-stream))
  "Tell column number that stream S is currently at."
  (output-column s))

(defmethod stream-start-line-p ((s format-output-stream))
  "Tell if stream S is already at start of fresh new line."
  (zerop (output-column s)))

(defmethod stream-write-char ((s format-output-stream) char)
  "Write CHAR to stream S."
  (if (char= char #\Newline)
      (setf (output-column s) 0)
      (incf (output-column s)))
  (write-char char (slot-value s 'understream)))

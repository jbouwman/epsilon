(defpackage #:lib.id
  (:use
   #:cl
   #:lib.type)
  (:export
   #:uuid))

(in-package #:lib.id)

(defvar +uuid-re+
  "[0-9a-fA-F]{8}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{12}")

(defclass uuid ()
  ((bytes :initarg :bytes
          :initform (random-u8 32))))

(defun hex->u8 (hex-char)
  (ecase hex-char
    (#\0 0)
    (#\1 1)
    (#\2 2)
    (#\3 3)
    (#\4 4)
    (#\5 5)
    (#\6 6)
    (#\7 7)
    (#\8 8)
    (#\9 9)
    ((#\a #\A) 10)
    ((#\b #\B) 11)
    ((#\c #\C) 12)
    ((#\d #\D) 13)
    ((#\e #\E) 14)
    ((#\f #\F) 15)))

(defun parse-uuid (s)
  (unless (= 36 (length s))
    (error "UUID has bad length: ~A" s))
  (let ((uuid (->u8 16)))
    (flet ((plink (uuid-index string-index)
             (setf (aref uuid uuid-index)
                   (+ (ash (hex->u8 (aref s string-index)) 4)
                      (hex->u8 (aref s (1+ string-index)))))))
      (plink 0 0)
      (plink 1 2)
      (plink 2 4)
      (plink 3 6)

      (plink 4 9)
      (plink 5 11)

      (plink 6 14)
      (plink 7 16)

      (plink 8 19)
      (plink 9 21)

      (plink 10 24)
      (plink 11 26)
      (plink 12 28)
      (plink 13 30)
      (plink 14 32)
      (plink 15 34))
    uuid))

(defun uuid (&optional s)
  "With no argument, generate a random UUID. With one argument,
  conform that argument to a UUID or raise an exception."
  (if s
      (parse-uuid s)
      (make-instance 'uuid)))

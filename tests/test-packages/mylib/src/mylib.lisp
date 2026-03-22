(defpackage :mylib
  (:use :cl)
  (:export #:greet)
  (:enter t))

(defun greet (name)
  "Return a greeting for NAME"
  (format nil "Hello, ~A!" name))

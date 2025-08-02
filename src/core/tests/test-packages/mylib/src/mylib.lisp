(defpackage :mylib
  (:use :cl)
  (:export #:greet))

(in-package :mylib)

(defun greet (name)
  "Return a greeting for NAME"
  (format nil "Hello, ~A!" name))
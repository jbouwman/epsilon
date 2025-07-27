(defpackage :myapp
  (:use :cl)
  (:local-nicknames (:lib :mylib))
  (:export #:test-greeting))

(in-package :myapp)

(defun test-greeting ()
  "Test the greeting function from mylib"
  (format t "Testing mylib: ~A~%" (lib:greet "World")))
(defpackage :myapp
  (:use :cl)
  (:import (mylib lib))
  (:export #:test-greeting))

(defun test-greeting ()
  "Test the greeting function from mylib"
  (format t "Testing mylib: ~A~%" (lib:greet "World")))

(defpackage #:lib.eval
  (:use
   #:cl)
  (:export
   #:partial))

(in-package #:lib.eval)

(defun partial (fn &rest args)
  (lambda (&rest args2)
    (apply fn (append args args2))))

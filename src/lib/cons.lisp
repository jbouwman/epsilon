(defpackage #:lib.cons
  (:use
   #:cl)
  (:export
   #:ensure-list))

(in-package #:lib.cons)

(defun ensure-list (list)
  (if (listp list)
      list
      (list list)))

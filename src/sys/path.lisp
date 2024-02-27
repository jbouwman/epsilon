(defpackage #:sys.path
  (:use :cl)
  (:export
   #:path
   #:join))

(in-package #:sys.path)

(defgeneric path (a))

(defgeneric join (a b))

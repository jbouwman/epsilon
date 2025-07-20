(defpackage #:epsilon.sys.timeout
  (:use
   #:cl)
  (:export
   #:timeout
   #:with-timeout))

(in-package #:epsilon.sys.timeout)

(deftype timeout ()
  'sb-ext:timeout)

(defmacro with-timeout ((timeout) &body body)
  `(sb-ext:with-timeout ,timeout
     ,@body))

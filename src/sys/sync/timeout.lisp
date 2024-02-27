(defpackage #:sys.sync.timeout
  (:use
   #:cl)
  (:export
   #:timeout
   #:with-timeout))

(in-package #:sys.sync.timeout)

(deftype timeout ()
  'sb-ext:timeout)

(defmacro with-timeout ((timeout) &body body)
  `(sb-ext:with-timeout ,timeout
     ,@body))

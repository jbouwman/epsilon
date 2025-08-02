(defpackage #:epsilon.checksum.generic
  (:use
   #:cl)
  (:export
   #:checksum
   #:update))

(in-package #:epsilon.checksum.generic)

(defgeneric update (state buffer start count))

(defgeneric checksum (state))

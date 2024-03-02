(defpackage #:lib.checksum.generic
  (:use
   #:cl)
  (:export
   #:checksum
   #:update))

(in-package #:lib.checksum.generic)

(defgeneric update (state buffer start count))

(defgeneric checksum (state))

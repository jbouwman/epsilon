(defpackage #:epsilon.lib.checksum.generic
  (:use
   #:cl)
  (:export
   #:checksum
   #:update))

(in-package #:epsilon.lib.checksum.generic)

(defgeneric update (state buffer start count))

(defgeneric checksum (state))

(defpackage #:encode.generic
  (:use #:cl)
  (:export
   #:checksum
   #:update))

(in-package #:encode.generic)

(defgeneric update (state buffer start count))

(defgeneric checksum (state))

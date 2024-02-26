(defpackage #:encode.checksum.adler-32
  (:use #:cl
        #:encode.generic
        #:encode.type)
  (:export
   #:adler-32))

;; https://en.wikipedia.org/wiki/Adler-32

(in-package #:encode.checksum.adler-32)

(defconstant +adler-32-base+ 65521)

(defclass adler-32 ()
  ((high :initform 0)
   (low :initform 1)))

(defmethod checksum ((checksum adler-32))
  (with-slots (high low) checksum
    (+ (ash high 16) low)))

(defmethod update ((checksum adler-32) buffer start count)
  (declare (optimize speed)
           (type ->u8 buffer)
           (type array-index start count))
  (with-slots (high low) checksum
    (declare (type u32 high low))
    (loop :for i :from 0 :below count
          :do (setf low (mod (+ (aref buffer (+ start i)) low) +adler-32-base+)
                    high (mod (+ low high) +adler-32-base+)))))
      

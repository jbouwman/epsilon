(defpackage #:epsilon.lib.checksum.tests
  (:use
   #:cl
   #:epsilon.tool.test
   #:epsilon.lib.checksum.adler-32
   #:epsilon.lib.checksum.generic
   #:epsilon.lib.codec
   #:epsilon.lib.stream
   #:epsilon.lib.type))

(in-package #:epsilon.lib.checksum.tests)

(deftest adler-32 ()
  (let ((adler (make-instance 'adler-32))
        (buffer (->u8 (loop :for i :from 1 :to 32768
                            :collect (mod i 256)))))
    (update adler buffer 0 32768)
    (is (= (checksum adler)
           60605362))))

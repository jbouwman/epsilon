(tool.test:define-test-package #:lib.checksum/tests
  (:use
   #:cl
   #:lib.checksum.adler-32
   #:lib.checksum.generic
   #:lib.codec
   #:lib.stream
   #:lib.type))

(in-package #:lib.checksum/tests)

(deftest adler-32 ()
  (let ((adler (make-instance 'adler-32))
        (buffer (->u8 (loop :for i :from 1 :to 32768
                            :collect (mod i 256)))))
    (update adler buffer 0 32768)
    (is (= (checksum adler)
           60605362))))

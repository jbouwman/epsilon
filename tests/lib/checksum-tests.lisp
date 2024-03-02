(tool.test:define-test-package #:lib.checksum/tests
  (:use
   #:cl
   #:lib.checksum.adler-32
   #:lib.checksum.generic
   #:lib.codec
   #:lib.io
   #:lib.type))

(in-package #:lib.checksum/tests)

(deftest adler-32 ()
  (let ((adler (make-instance 'adler-32))
        (buffer (fixed-u8 32768)))
    (update adler buffer 0 32768)
    (is (= (checksum adler)
           60605362))))

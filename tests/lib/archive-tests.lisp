(tool.test:define-test-package #:lib.archive/tests
  (:use
   #:cl
   #:lib.char
   #:lib.stream
   #:lib.string
   #:lib.type))

(in-package #:lib.archive/tests)

(deftest zipfile ()
  (lib.archive:with-zip-file (file (test-file "nibbles-0.15-a46a67736e07.zip"))
    (let* ((entry (aref (lib.archive:entries file) 12))
           (stream (make-instance 'fast-output-stream)))
      (flet ((output (buffer start end)
               (write-sequence buffer stream :start start :end end)
               end))
        (lib.archive:decode-entry #'output entry)
        (is (starts-with-p (u8-to-string (finish-output-stream stream))
                           ";;;; types.lisp"))))))
    

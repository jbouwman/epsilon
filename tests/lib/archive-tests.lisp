(tool.test:define-test-package #:lib.archive/tests
  (:use
   #:cl
   #:lib.char
   #:lib.stream
   #:lib.string
   #:lib.type))

(in-package #:lib.archive/tests)

(deftest zipfile ()
  (lib.archive:with-zip-file (file (test-file "simple.zip"))
    (let ((content (loop :for entry :across (lib.archive:entries file)
                         :unless (getf (first (lib.archive:attributes entry)) :directory)
                           :collect (let ((stream (make-vector-stream :output t)))
                                      (flet ((output (buffer start end)
                                               (write-sequence buffer stream :start start :end end)
                                               end))
                                        (lib.archive:decode-entry #'output entry)
                                        (u8-to-string (output-stream-vector stream)))))))
      (is (starts-with-p (first content)
                         "abcde")))))
    

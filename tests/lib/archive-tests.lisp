(defpackage #:epsilon.lib.archive.tests
  (:use
   #:cl
   #:epsilon.tool.test
   #:epsilon.lib.char
   #:epsilon.lib.stream
   #:epsilon.lib.string
   #:epsilon.lib.type)
  (:local-nicknames
   (#:uri #:epsilon.lib.uri)))

(in-package #:epsilon.lib.archive.tests)

(deftest zipfile ()
  (let ((nibbles (project-file :epsilon/tests
                               "tests/lib/nibbles-0.15-a46a67736e07.zip")))
    (epsilon.lib.archive:with-zip-file (file nibbles)
      (let* ((entry (aref (epsilon.lib.archive:entries file) 12))
             (stream (make-instance 'fast-output-stream)))
        (flet ((output (buffer start end)
                 (write-sequence buffer stream :start start :end end)
                 end))
          (epsilon.lib.archive:decode-entry #'output entry)
          (is (starts-with-p (u8-to-string (finish-output-stream stream))
                             ";;;; types.lisp")))))))
    

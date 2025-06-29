(defpackage :epsilon.lib.archive-tests
  (:use
   :cl
   :epsilon.tool.test
   :epsilon.lib.type)
  (:local-nicknames
   (:char :epsilon.lib.char)
   (:str :epsilon.lib.string)
   (:stream :epsilon.lib.stream)
   (:uri :epsilon.lib.uri)))

(in-package :epsilon.lib.archive-tests)

(deftest zipfile
  (let ((nibbles (project-file "epsilon.core"
                               "tests/lib/nibbles-0.15-a46a67736e07.zip")))
    (epsilon.lib.archive:with-zip-file (file nibbles)
      (let* ((entry (aref (epsilon.lib.archive:entries file) 12))
             (stream (stream:make-output-stream)))
        (flet ((output (buffer start end)
                 (write-sequence buffer stream :start start :end end)
                 end))
          (epsilon.lib.archive:decode-entry #'output entry)
          (is (str:starts-with-p (char:bytes-to-string (stream:buffer stream))
                             ";;;; types.lisp")))))))
    

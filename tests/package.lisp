(epsilon.tool.unit-test:define-test-package #:epsilon-tests
  (:use
   #:cl
   #:lib.checksum.generic
   #:lib.io
   #:sys.type
   #:encode
   #:lib.checksum.adler-32)
  (:export
   #:run-epsilon-tests))

(in-package #:epsilon-tests)

(defun run-all-tests ()
  (run-package-tests :packages '(:epsilon-tests)
                     :interactive t))

(defun test-file (name)
  (merge-pathnames (format nil "tests/data/~A" name)
                   (asdf:system-source-directory "epsilon")))

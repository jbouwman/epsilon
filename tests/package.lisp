(epsilon.tool.unit-test:define-test-package #:epsilon-tests
  (:use
   #:cl
   #:encode.generic
   #:encode.type
   #:encode
   #:encode.checksum.adler-32)
  (:export
   #:run-epsilon-tests))

(in-package #:epsilon-tests)

(defun run-all-tests ()
  (run-package-tests :packages '(:epsilon-tests)
                     :interactive t))

(defun test-file (name)
  (merge-pathnames (format nil "tests/data/~A" name)
                   (asdf:system-source-directory "epsilon")))

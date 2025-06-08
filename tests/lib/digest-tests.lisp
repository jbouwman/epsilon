(defpackage #:epsilon.lib.digest.tests
  (:use #:cl
        #:epsilon.tool.test))

(in-package #:epsilon.lib.digest.tests)

;; shasum -a 256 tests/lib/shilling.txt
;; b9ed5c0ed11f350f7247678de2d16418b69c7f2ce00eabea7a8990889e54faa5  tests/data/shilling.txt

(deftest sha-2 ()
  (let ((digest (epsilon.lib.digest:make-digest :sha-256))
        (shilling (project-file :epsilon/tests "tests/lib/shilling.txt")))
    (with-open-file (stream shilling :element-type 'unsigned-byte)
      (epsilon.lib.digest:digest-stream digest stream))
    (is (string=
         "b9ed5c0ed11f350f7247678de2d16418b69c7f2ce00eabea7a8990889e54faa5"
         (epsilon.lib.hex:u8-to-hex
          (epsilon.lib.digest:get-digest digest))))))

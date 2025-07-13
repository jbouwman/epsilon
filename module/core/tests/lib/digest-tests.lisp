(defpackage #:epsilon.lib.digest.tests
  (:use #:cl
        #:epsilon.test)
  (:local-nicknames
   (digest epsilon.lib.digest)
   (hex epsilon.lib.hex)))

(in-package #:epsilon.lib.digest.tests)

;; shasum -a 256 tests/lib/shilling.txt
;; b9ed5c0ed11f350f7247678de2d16418b69c7f2ce00eabea7a8990889e54faa5  tests/data/shilling.txt

(deftest sha-2
  (let ((digest (digest:make-digest :sha-256))
        (shilling (project-file "epsilon.core" "tests/lib/shilling.txt")))
    (with-open-file (stream shilling :element-type 'unsigned-byte)
      (digest:digest-stream digest stream))
    (is (string=
         "b9ed5c0ed11f350f7247678de2d16418b69c7f2ce00eabea7a8990889e54faa5"
         (hex:u8-to-hex
          (digest:get-digest digest))))))

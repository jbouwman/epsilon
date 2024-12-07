(epsilon.tool.test:define-test-package #:epsilon.lib.digest/tests)

(in-package #:epsilon.lib.digest/tests)

;; shasum -a 256 tests/data/shilling.txt
;; b9ed5c0ed11f350f7247678de2d16418b69c7f2ce00eabea7a8990889e54faa5  tests/data/shilling.txt

(deftest sha-2 ()
  (let ((digest (epsilon.lib.digest:make-digest :sha-256))
        (shilling (get-test-relative-path 'sha-2 "shilling.txt")))
    (with-open-file (stream shilling :element-type 'unsigned-byte)
      (epsilon.lib.digest:digest-stream digest stream))
    (is (string=
         "b9ed5c0ed11f350f7247678de2d16418b69c7f2ce00eabea7a8990889e54faa5"
         (epsilon.lib.codec.hex:u8-to-hex
          (epsilon.lib.digest:get-digest digest))))))

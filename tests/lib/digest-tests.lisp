(tool.test:define-test-package #:lib.digest/tests)

(in-package #:lib.digest/tests)

;; shasum -a 256 tests/data/shilling.txt
;; b9ed5c0ed11f350f7247678de2d16418b69c7f2ce00eabea7a8990889e54faa5  tests/data/shilling.txt

(deftest sha-2 ()
  (let ((digest (lib.digest:make-digest :sha-256)))
    (with-open-stream (stream (lib.url:input-stream (test-data "shilling.txt")))
      (lib.digest:digest-stream digest stream))
    (is (string=
         "b9ed5c0ed11f350f7247678de2d16418b69c7f2ce00eabea7a8990889e54faa5"
         (lib.codec.hex:u8-to-hex
          (lib.digest:get-digest digest))))))

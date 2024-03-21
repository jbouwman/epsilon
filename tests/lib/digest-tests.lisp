(tool.test:define-test-package #:lib.digest/tests)

(in-package #:lib.digest/tests)

;; shasum -a 256 tests/data/shilling.txt
;; b9ed5c0ed11f350f7247678de2d16418b69c7f2ce00eabea7a8990889e54faa5  tests/data/shilling.txt

(deftest sha-2 ()
  (let ((digest (lib.digest.sha-2::%make-sha256-digest)))
    (is (string=
         "b9ed5c0ed11f350f7247678de2d16418b69c7f2ce00eabea7a8990889e54faa5"
         (lib.codec.hex:u8-to-hex
          (lib.digest.public:digest-file digest
                                         (lib.url:uri-path (test-data "shilling.txt"))))))))

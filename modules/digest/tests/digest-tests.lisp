(defpackage epsilon.digest.tests
  (:use
   cl
   epsilon.test)
  (:local-nicknames
   (digest epsilon.digest)
   (hex epsilon.hex)))

(in-package epsilon.digest.tests)

;; shasum -a 256 tests/lib/shilling.txt
;; b9ed5c0ed11f350f7247678de2d16418b69c7f2ce00eabea7a8990889e54faa5  tests/data/shilling.txt

(deftest sha-2
  (let ((shilling (module-file "epsilon.core" "tests/lib/shilling.txt")))
    (with-open-file (stream shilling :element-type 'unsigned-byte)
      (let ((hasher (digest:make-sha256)))
        (loop for byte = (read-byte stream nil)
              while byte
              do (digest:update hasher (vector byte)))
        (is (string=
             "b9ed5c0ed11f350f7247678de2d16418b69c7f2ce00eabea7a8990889e54faa5"
             (string-downcase (digest:bytes-to-hex (digest:finalize hasher)))))))))

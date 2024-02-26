(asdf:defsystem #:epsilon
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "generic-lo")
                             (:file "type-lo")
                             (:module "checksum"
                              :components ((:file "adler-32")))
                             (:file "package")
                             (:file "generic")
                             (:file "eval")
                             (:file "constant")
                             (:file "type")
                             (:file "io")
                             (:file "checksum/crc")
                             (:file "condition")
                             (:file "gzip")
                             (:file "zlib")
                             (:file "decompress")
                             (:file "inflate")
                             (:file "bzip")
                             (:file "stream")
                             (:file "bitstream")
                             (:file "huffman")
                             (:file "compress")
                             (:file "public"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:epsilon/tests))))

(asdf:defsystem #:epsilon/tests
  :depends-on (#:epsilon
               #:flexi-streams          ; TODO
               #:fiasco)                ; TODO
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :epsilon-tests :run-all-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")        ; TODO
               (:file "checksum")
               (:file "encode")))

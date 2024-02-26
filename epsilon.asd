(asdf:defsystem #:epsilon
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:module "sys"
      :components
      ((:file "type")))
     (:module "lib"
      :components
      ((:file "io")
       (:file "eval")
       (:module "checksum"
        :components
                ((:file "generic")
                 (:file "adler-32")))))
     (:module "tool"
      :components ((:file "unit-test")))
     (:file "package")
     (:file "generic")
     (:file "constant")
     (:file "type")
     (:file "lib/checksum/crc")
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
  :depends-on (#:epsilon)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :epsilon-tests :run-all-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")        ; TODO
               (:file "checksum")
               (:file "encode")))

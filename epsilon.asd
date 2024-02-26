(asdf:defsystem #:epsilon
  :serial t
  :depends-on (:sb-rotate-byte)
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
       (:file "cons")
       (:file "symbol")
       (:file "eval")
       (:module "checksum"
        :serial t
        :components
                ((:file "generic")
                 (:file "adler-32")
                 (:file "crc-32")))
       (:module "codec"
        :serial t
        :components
                ((:file "package")      ; TODO
                 (:file "generic")
                 (:file "constant")
                 (:file "type")
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
                 (:file "public")))
       (:module "digest"
        :serial t
        :components
                ((:file "macro")
                 (:file "reader")
                 (:file "common")
                 (:file "util")
                 (:file "generic")
                 (:file "public")
                 (:file "stream")
                 (:file "sha-2")))))
     (:module "tool"
      :components ((:file "test"))))))
  :in-order-to ((asdf:test-op (asdf:test-op #:epsilon/tests))))

(asdf:defsystem #:epsilon/tests
  :depends-on (#:epsilon)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :epsilon-tests :run-all-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")        ; TODO
               (:file "checksum")
               (:file "encode")
               (:file "sha-2")))

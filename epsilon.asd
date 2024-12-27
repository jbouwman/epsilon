(in-package :asdf)

(asdf:defsystem "epsilon"
  :version "0.1.0"
  :description "A utility library for SBCL"
  :depends-on (:sb-posix
               :sb-rotate-byte
               :sb-cltl2
               :sb-bsd-sockets
               :sb-rotate-byte)
  :components
  ((:module "src"
    :components
    ((:module "lib"
      :components ((:file "symbol")
                   (:file "binding")
                   (:file "map")
                   (:file "array")
                   (:file "condition")
                   (:file "string")
                   (:file "format")
                   (:file "function")
                   (:file "list")
                   (:file "json")
                   (:file "collect")
                   (:file "control")
                   (:module "type"
                    :components ((:file "package")
                                 (:file "macro-utils")
                                 (:file "vectors")
                                 (:file "streams")))
                   (:file "vector")
                   (:file "uuid")
                   (:file "xsubseq")
                   (:file "sequence")
                   (:module "char"
                    :components ((:file "package")
                                 (:module "encoding"
                                          :components ((:file "encoding")
                                                       (:file "ascii")
                                                       (:file "cp437")
                                                       (:file "iso-8859")
                                                       (:file "unicode")))
                                 (:file "external-format")
                                 (:file "string")))
                   (:file "uri")
                   (:file "stream")
                   (:file "yaml")
                   (:file "buffer")
                   (:module "digest"
                    :components ((:file "reader")
                                 (:file "common")
                                 (:file "generic")
                                 (:file "stream")
                                 (:file "sha-2")
                                 (:file "public")))
                   (:module "checksum"
                    :components ((:file "generic")
                                 (:file "adler-32")
                                 (:file "crc-32")))
                   (:module "codec"
                    :components ((:file "package")
                                 (:file "generic")
                                 (:file "constant")
                                 (:file "type")
                                 (:file "condition")
                                 (:file "hex")
                                 (:file "base64")
                                 (:file "gzip")
                                 (:file "zlib")
                                 (:file "decompress")
                                 (:file "inflate")
                                 (:file "bzip")
                                 (:file "stream")
                                 (:file "bitstream")
                                 (:file "huffman")
                                 (:file "compress")
                                 (:file "public")))))
     (:file "io")
     (:module "sys"
      :components ((:file "env")
                   (:module "ffi"
                    :components ((:file "package")
                                 (:file "sys-utils")
                                 (:file "sbcl")
                                 (:file "utils")
                                 (:file "early-types")
                                 (:file "foreign-vars")
                                 (:file "libraries")
                                 (:file "types")
                                 (:file "enum")
                                 (:file "strings")
                                 (:file "structures")
                                 (:file "functions")))
                   (:file "fs")
                   (:file "gc")
                   (:module "sync"
                    :components ((:file "error")
                                 (:file "atomic")
                                 (:file "lock")
                                 (:file "semaphore")
                                 (:file "thread")
                                 (:file "variable")
                                 (:file "timeout")))))
     (:module "lib/regex"
      :components ((:file "package")
                   (:file "specials")
                   (:file "util")
                   (:file "errors")
                   (:file "charset")
                   (:file "charmap")
                   (:file "chartest")
                   (:file "lexer")
                   (:file "parser")
                   (:file "regex-class")
                   (:file "regex-class-util")
                   (:file "convert")
                   (:file "optimize")
                   (:file "closures")
                   (:file "repetition-closures")
                   (:file "scanner")
                   (:file "public")))
     (:module "lib/archive"
      :components ((:file "package")
                   (:file "toolkit")
                   (:file "parser")
                   (:file "io")
                   (:file "tables")
                   (:file "compression")
                   (:file "encryption")
                   (:file "pkware-encryption")
                   (:file "structures")
                   (:file "zippy")
                   (:file "decode")
                   (:file "encode")))
     (:module "net"
      :components ((:file "socket")
                   (:file "tls")
                   (:file "http")))
     (:module "tool"
      :components ((:file "build")
                   (:file "test")))
     (:file "epsilon"))))
  :in-order-to ((test-op (test-op "epsilon/tests")))
  :perform (load-op :after (o c)
                    (provide :epsilon)))

(asdf:defsystem "epsilon/tests"
  :depends-on ("epsilon")
  :components
  ((:module "tests"
    :components
    ((:module "lib"
              :components ((:file "archive-tests")
                           (:file "checksum-tests")
                           (:file "codec-tests")
                           (:file "digest-tests")
                           (:file "map-tests")
                           (:file "regex-tests")
                           (:file "uri-tests")
                           (:file "yaml-tests")
                           (:file "stream-tests")))
     (:module "net"
              :components ((:file "http-tests"))))))
  :perform (test-op (o c)
                   (symbol-call :epsilon.tool.test ':run-tests)))


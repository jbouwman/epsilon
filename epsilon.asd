(asdf:defsystem #:epsilon
  :serial t
  :depends-on (:sb-bsd-sockets
               :sb-cltl2
               :sb-posix
               :sb-rotate-byte
               :file-attributes
               :pathname-utils
               :filesystem-utils
               :nibbles
               :3bz)
  :components
  ((:module "lib"
    :pathname "src/lib"
    :serial t
    :components ((:file "binding")
                 (:file "array")
                 (:file "symbol")
                 (:file "condition")
                 (:file "string")
                 (:file "function")
                 (:file "collect")
                 (:file "type")
                 (:file "list")
                 (:file "sequence")
                 (:file "io")
                 (:file "hash")
                 (:file "vector")
                 (:file "control")
                 (:file "xsubseq")
                 (:module "char"
                  :serial t
                  :components ((:file "package")
                               (:module "encoding"
                                :serial t
                                :components ((:file "encoding")
                                             (:file "ascii")
                                             (:file "iso-8859")
                                             (:file "unicode")))
                               (:file "external-format")
                               (:file "string")))
                 (:module "seq"
                  :serial t
                  :components ((:file "package")
                               (:file "vector")
                               (:file "list")
                               (:file "extended-sequence")
                               (:file "public")))
                 (:module "regex"
                  :serial t
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
                 (:file "stream")
                 (:file "buffer")
                 (:module "checksum"
                  :serial t
                  :components ((:file "generic")
                               (:file "adler-32")
                               (:file "crc-32")))
                 (:module "codec"
                  :serial t
                  :components ((:file "package")
                               (:file "generic")
                               (:file "constant")
                               (:file "type")
                               (:file "condition")
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
                               (:file "public")))
                 (:module "digest"
                  :serial t
                  :components ((:file "macro")
                               (:file "reader")
                               (:file "common")
                               (:file "util")
                               (:file "generic")
                               (:file "public")
                               (:file "stream")
                               (:file "sha-2")))
                 (:module "archive"
                  :serial t
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
                               (:file "encode")))))
                 
   (:module "src/sys"
    :serial t
    :components ((:file "env")
                 (:module "ffi"
                  :serial t
                  :components ((:file "package")
                               (:file "sys-utils")
                               (:file "sbcl")
                               (:file "utils")
                               (:file "libraries")
                               (:file "early-types")
                               (:file "types")
                               (:file "enum")
                               (:file "strings")
                               (:file "structures")
                               (:file "functions")
                               (:file "foreign-vars")))
                 (:file "gc")
                 (:module "sync"
                  :serial t
                  :components ((:file "error")
                               (:file "atomic")
                               (:file "lock")
                               (:file "semaphore")
                               (:file "thread")
                               (:file "variable")
                               (:file "timeout")))))
   (:module "src/net"
    :serial t
    :components ((:file "socket")
                 (:file "url")
                 (:module "tls"
                  :serial t
                  :components ((:file "config")
                               (:file "package")
                               (:file "reload")
                               (:file "ffi")
                               (:file "bio")
                               (:file "conditions")
                               (:file "ssl-funcall")
                               (:file "init")
                               (:file "ffi-buffer")
                               (:file "streams")
                               (:file "x509")
                               (:file "random")
                               (:file "context")
                               (:file "verify-hostname")))
                 (:module "http"
                  :serial t
                  :components ((:file "chunked-stream")
                               (:file "parser")
                               (:file "encoding")
                               (:file "connection-cache")
                               (:file "decoding-stream")
                               (:file "keep-alive-stream")
                               (:file "util")
                               (:file "body")
                               (:file "error")
                               (:file "backend/socket")
                               (:file "public")))))
   (:module "src/tool"
    :components ((:file "test"))))
   
  :in-order-to ((asdf:test-op (asdf:test-op #:epsilon/tests))))

(asdf:defsystem #:epsilon/tests
  :depends-on (#:epsilon)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :tool.test :run-all-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "lib/archive-tests")
               (:file "lib/checksum-tests")
               (:file "lib/codec-tests")
               (:file "lib/digest-tests")
               (:file "lib/regex-tests")
               (:file "lib/stream-tests")
               (:file "net/http-tests")))

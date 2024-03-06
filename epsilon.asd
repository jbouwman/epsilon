(asdf:defsystem #:epsilon
  :serial t
  :depends-on (:sb-bsd-sockets
               :sb-cltl2
               :sb-posix
               :sb-rotate-byte)
  :components
  ((:module "src/lib"
    :serial t
    :components ((:file "binding")
                 (:file "array")
                 (:file "symbol")
                 (:file "condition")
                 (:file "string")
                 (:file "function")
                 (:file "collect")
                 (:file "type")
                 (:file "type/types")
                 (:file "type/macro-utils")
                 (:file "type/vectors")
                 (:file "type/streams")
                 (:file "type/sbcl-opt/fndb")
                 (:file "type/sbcl-opt/nib-tran")
                 (:file "type/sbcl-opt/x86-64-vm" :if-feature :x86-64)
                 (:file "list")
                 (:file "sequence")
                 (:file "hash")
                 (:file "vector")
                 (:file "control")
                 (:file "xsubseq")))
   (:module "src/lib/char"
    :serial t
    :components ((:file "package")      ; TODO dump package files
                 (:module "encoding"
                  :serial t
                  :components ((:file "encoding")
                               (:file "ascii")
                               (:file "cp437")
                               (:file "iso-8859")
                               (:file "unicode")))
                 (:file "external-format")
                 (:file "string")))
   (:file "src/lib/stream")
   (:module "src/lib/seq"
    :serial t
    :components ((:file "package")
                 (:file "vector")
                 (:file "list")
                 (:file "extended-sequence")
                 (:file "public")))
   (:module "src/sys"
    :serial t
    :components ((:file "env")
                 (:file "path")
                 (:file "filesystem")
                 (:file "directory")
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
                 (:file "file")
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
   (:module "src/lib/regex"
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
   (:file "src/lib/buffer")
   (:module "src/lib/checksum"
    :serial t
    :components ((:file "generic")
                 (:file "adler-32")
                 (:file "crc-32")))
   (:module "src/lib/codec"
    :serial t
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
                 (:file "public")))
   (:module "src/lib/digest"
    :serial t
    :components ((:file "macro")
                 (:file "reader")
                 (:file "common")
                 (:file "generic")
                 (:file "public")
                 (:file "stream")
                 (:file "sha-2")))
   (:module "src/lib/archive"
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
                 (:file "encode")))
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
                         (uiop:symbol-call :TOOL.TEST :RUN-ALL-TESTS))
  :pathname "tests/"
  :serial t
  :components ((:file "lib/archive-tests")
               (:file "lib/checksum-tests")
               (:file "lib/codec-tests")
               (:file "lib/digest-tests")
               (:file "lib/regex-tests")
               (:file "lib/stream-tests")
               (:file "net/http-tests")))

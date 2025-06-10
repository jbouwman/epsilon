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
                   (:file "syntax")
                   (:file "process")
                   (:file "map")
                   (:file "array")
                   (:file "condition")
                   (:file "function")
                   (:file "sequence")
                   (:file "string")
                   (:file "list")
                   (:file "json")
                   (:file "collect")
                   (:file "control")
                   (:file "type")
                   (:file "vector")
                   (:file "uuid")
                   (:file "char")
                   (:module "encoding"
                    :components ((:file "cp437")
                                 (:file "unicode")))
                   (:file "struct")
                   (:file "uri")
                   (:file "stream")
                   (:file "reader")
                   (:file "writer")
                   (:file "yaml")
                   (:file "time")
                   (:file "base64")
                   (:module "digest"
                    :components ((:file "reader")
                                 (:file "common")
                                 (:file "generic")
                                 (:file "sha-2")
                                 (:file "public")))
                   (:module "checksum"
                    :components ((:file "generic")
                                 (:file "adler-32")
                                 (:file "crc-32")))
                   (:file "codec")
                   (:file "hex")
                   (:file "msgpack")))
     (:file "io")
     (:module "sys"
      :components ((:file "env")
                   (:file "pkg")
                   (:module "ffi"
                    :components ((:file "package")
                                 (:file "libraries")))
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
     (:file "lib/regex")
     (:file "lib/archive")
     (:module "net"
      :components ((:file "core")
                   (:file "tls")
                   (:file "http")
                   (:file "http-server")))
     (:module "tool"
      :components ((:file "format")
                   (:file "build")
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
                           (:file "char-tests")
                           (:file "checksum-tests")
                           (:file "codec-tests")
                           (:file "digest-tests")
                           (:file "map-tests")
                           (:file "msgpack-tests")
                           (:file "regex-tests")
                           (:file "uri-tests")
                           (:file "yaml-tests")
                           (:file "stream-tests")
                           (:file "sequence-tests")))
     (:module "net"
              :components ((:file "http-tests")
                           (:file "http-server-tests")))
     (:module "tool"
              :components ((:file "format-tests"))))))
  :perform (test-op (o c)
                   (symbol-call :epsilon.tool.test ':run-tests)))


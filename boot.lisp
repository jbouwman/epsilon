(in-package :cl-user)

(require :sb-bsd-sockets)
(require :sb-rotate-byte)
(require :sb-posix)

(defvar *boot-order*
  '((:module "src"
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
                   (:file "c-parser")
                   (:file "msgpack")))
     (:module "sys"
      :components ((:file "env")
                   (:file "pkg")
                   (:file "lib")
                   (:file "fs")
                   (:file "gc")
                   (:file "error")
                   (:file "atomic")
                   (:file "lock")
                   (:file "semaphore")
                   (:file "thread")
                   (:file "variable")
                   (:file "timeout")))
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
                   (:file "test")))))))

(defun boot (modules &optional basedir)
  (dolist (module modules)
    (destructuring-bind (&key file module components) module
      (cond (file
             (load (compile-file (if basedir
                                     (concatenate 'string basedir "/" file ".lisp")
                                     file))))
            (module
             (let ((basedir (if basedir
                                (concatenate 'string basedir "/" module)
                                module)))
               (boot components basedir)))))))

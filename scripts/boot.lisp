(in-package :cl-user)

(require :sb-rotate-byte)
(require :sb-posix)

(defvar *epsilon-modules*
  '((:module "src"
     :components
    ((:module "lib"
      :components ((:file "symbol")
                   (:file "syntax")
                   (:file "process")
                   (:file "charset")
                   (:file "map")
                   (:file "lexer")
                   (:file "array")
                   (:file "condition")
                   (:file "function")
                   (:file "sequence")
                   (:file "parser")
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
                   (:file "clang")
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
                   (:file "test")
                   (:file "dev")))))))

(defun load-modules (modules &optional basedir)
  (dolist (module modules)
    (destructuring-bind (&key file module components) module
      (cond (file
             (let ((filepath (if basedir
                                 (concatenate 'string basedir "/" file ".lisp")
                                 (concatenate 'string file ".lisp"))))
               ;; Print progress dot
;               (format t ".")
;               (force-output)
               ;; Suppress warnings and errors during compilation and loading
               (handler-bind ((warning #'muffle-warning)
                              (error (lambda (c) 
                                       (declare (ignore c))
                                       (invoke-restart 'continue))))
                 (let ((*standard-output* (make-broadcast-stream))
                       (*error-output* (make-broadcast-stream)))
                   (load (compile-file filepath))))))
            (module
             (let ((basedir (if basedir
                                (concatenate 'string basedir "/" module)
                                module)))
               (load-modules components basedir)))))))

(defun load-epsilon ()
  "Load the Epsilon library"
  ;(format t "Loading Epsilon...")
  (force-output)
  (load-modules *epsilon-modules*)
  ;(format t " done.~%")
  (force-output))

(load-epsilon)

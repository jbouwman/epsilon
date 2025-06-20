(in-package :cl-user)

(require :sb-rotate-byte)
(require :sb-posix)

(defvar *files*
  '("src/lib/symbol"
    "src/lib/syntax"
    "src/lib/process"
    "src/lib/charset"
    "src/lib/map"
    "src/lib/lexer"
    "src/lib/array"
    "src/lib/condition"
    "src/lib/function"
    "src/lib/sequence"
    "src/lib/parser"
    "src/lib/string"
    "src/lib/list"
    "src/lib/json"
    "src/lib/collect"
    "src/lib/control"
    "src/lib/type"
    "src/lib/vector"
    "src/lib/uuid"
    "src/lib/char"
    "src/lib/encoding/cp437"
    "src/lib/encoding/unicode"
    "src/lib/struct"
    "src/lib/uri"
    "src/lib/stream"
    "src/lib/reader"
    "src/lib/writer"
    "src/lib/yaml"
    "src/lib/time"
    "src/lib/base64"
    "src/lib/digest/reader"
    "src/lib/digest/common"
    "src/lib/digest/generic"
    "src/lib/digest/sha-2"
    "src/lib/digest/public"
    "src/lib/checksum/generic"
    "src/lib/checksum/adler-32"
    "src/lib/checksum/crc-32"
    "src/lib/codec"
    "src/lib/hex"
    "src/lib/clang"
    "src/lib/msgpack"
    "src/sys/env"
    "src/sys/pkg"
    "src/sys/lib"
    "src/sys/fs"
    "src/sys/gc"
    "src/sys/error"
    "src/sys/atomic"
    "src/sys/lock"
    "src/sys/semaphore"
    "src/sys/thread"
    "src/sys/variable"
    "src/sys/timeout"
    "src/lib/regex"
    "src/lib/archive"
    "src/net/core"
    "src/net/tls"
    "src/net/http"
    "src/net/http-server"
    "src/tool/format"
    "src/tool/build"
    "src/tool/test"
    "src/tool/dev"))

(defun concatenate-fasls (fasl-files output-file)
  "Concatenate compiled FASL files into a single boot file"
  (with-open-file (output output-file
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (dolist (fasl fasl-files)
      (when (probe-file fasl)
        (with-open-file (input fasl
                               :direction :input
                               :element-type '(unsigned-byte 8))
          (loop for byte = (read-byte input nil)
                while byte
                do (write-byte byte output)))))))

(defun load-and-build-boot (files)
  (let ((fasls '()))
    (ensure-directories-exist "target/")
    (dolist (file files)
      (let ((source (concatenate 'string file ".lisp"))
            (output (concatenate 'string file ".fasl")))
        (handler-bind ((warning #'muffle-warning)
                       (error (lambda (c) 
                                (declare (ignore c))
                                (invoke-restart 'continue))))
          (let ((*standard-output* (make-broadcast-stream))
                (*error-output* (make-broadcast-stream)))
            (compile-file source :output-file output)
            (load output)
            (push output fasls)))))
    (concatenate-fasls (reverse fasls) "target/boot.fasl")))

(defun load-epsilon ()
  "Load the Epsilon library"
  (let ((boot-fasl "target/boot.fasl"))
    (unless (probe-file boot-fasl)
      (load-and-build-boot *files*))
    (load boot-fasl)))

(load-epsilon)

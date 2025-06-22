(in-package :cl-user)

(require :sb-rotate-byte)
(require :sb-posix)

(defvar *files*)

(setf *files*
  '("lib/symbol"
    "lib/syntax"
    "lib/process"
    "lib/charset"
    "lib/map"
    "lib/set"
    "lib/lexer"
    "lib/array"
    "lib/condition"
    "lib/function"
    "lib/sequence"
    "lib/parser"
    "lib/string"
    "lib/list"
    "lib/json"
    "lib/collect"
    "lib/control"
    "lib/type"
    "lib/vector"
    "lib/uuid"
    "lib/char"
    "lib/encoding/cp437"
    "lib/encoding/unicode"
    "lib/struct"
    "lib/uri"
    "lib/stream"
    "lib/reader"
    "lib/writer"
    "lib/yaml"
    "lib/time"
    "lib/base64"
    "lib/digest/reader"
    "lib/digest/common"
    "lib/digest/generic"
    "lib/digest/sha-2"
    "lib/digest/public"
    "lib/checksum/generic"
    "lib/checksum/adler-32"
    "lib/checksum/crc-32"
    "lib/codec"
    "lib/hex"
    "sys/env"
    "sys/pkg"
    "sys/lib"
    "sys/fs"
    "sys/gc"
    "sys/error"
    "sys/atomic"
    "sys/lock"
    "sys/semaphore"
    "sys/thread"
    "sys/variable"
    "sys/timeout"
    "lib/regex"
    "lib/xml"
    "tool/build"
    "tool/test/suite"
    "tool/test/report"
    "tool/test"
    "tool/dev"))

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
    (dolist (dir (list "target"
                       "target/fasl"
                       "target/fasl/lib"
                       "target/fasl/lib/checksum"
                       "target/fasl/lib/digest"
                       "target/fasl/lib/encoding"
                       "target/fasl/sys"
                       "target/fasl/tool"
                       "target/fasl/tool/test"))
      (ignore-errors
       (sb-posix:mkdir dir #o755)))
    (dolist (file files)
      (let ((source (concatenate 'string "src/" file ".lisp"))
            (output (concatenate 'string "target/fasl/" file ".fasl")))
        (compile-file source :output-file output)
        (load output)
        (push output fasls)))
    (concatenate-fasls (reverse fasls) "target/boot.fasl")))

(defun load-epsilon ()
  "Load the Epsilon library"
  (let ((boot-fasl "target/boot.fasl"))
    (unless (probe-file boot-fasl)
      (load-and-build-boot *files*))
    (load boot-fasl)))

(load-epsilon)

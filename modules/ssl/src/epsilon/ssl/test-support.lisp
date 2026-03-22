;;;; Shared test utilities for epsilon.ssl
;;;;
;;;; Canonical implementations of byte-conversion helpers used throughout
;;;; SSL test files. Eliminates ~30 duplicated definitions (IMPL-244).

(defpackage epsilon.ssl.test-support
  (:use :cl)
  (:export
   #:hex-to-bytes
   #:string-to-bytes
   #:bytes-to-hex))

(in-package :epsilon.ssl.test-support)

(defun hex-to-bytes (hex-string)
  "Convert a hex string to a byte array."
  (let* ((len (/ (length hex-string) 2))
         (bytes (make-array len :element-type '(unsigned-byte 8))))
    (loop for i from 0 below len
          do (setf (aref bytes i)
                   (parse-integer hex-string :start (* i 2) :end (* (1+ i) 2)
                                             :radix 16)))
    bytes))

(defun string-to-bytes (str)
  "Convert an ASCII string to a byte array."
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code str))

(defun bytes-to-hex (bytes)
  "Convert a byte array to a lowercase hex string."
  (with-output-to-string (s)
    (loop for byte across bytes do (format s "~(~2,'0x~)" byte))))

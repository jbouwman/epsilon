;;;; Shared Utility Functions for Crypto Module
;;;;
;;;; This file provides utility functions used internally by multiple
;;;; crypto sub-packages. Not part of the public API.

(defpackage :epsilon.crypto.utils
  (:use :cl)
  (:local-nicknames
   (#:ffi #:epsilon.crypto.ffi))
  (:import-from :epsilon.crypto.ffi
                #:crypto-error)
  (:export #:crypto-random-bytes
           #:crypto-random-integer))

(in-package :epsilon.crypto.utils)

(defun crypto-random-bytes (n)
  "Generate cryptographically secure random bytes.
   
   Internal utility function for generating random data.
   Used by AEAD, KDF, and other crypto sub-packages.
   
   Parameters:
     n (integer): Number of random bytes to generate
   
   Returns:
     Byte vector of length n containing random data"
  (declare (type (integer 1 *) n))
  (let ((buffer (make-array n :element-type '(unsigned-byte 8))))
    (sb-sys:with-pinned-objects (buffer)
      (let ((result (ffi:%rand-bytes (sb-sys:vector-sap buffer) n)))
        (when (zerop result)
          (error 'crypto-error :code (ffi:%err-get-error)
                 :message "Failed to generate random bytes"))))
    buffer))

;; crypto-random-integer defined in package.lisp
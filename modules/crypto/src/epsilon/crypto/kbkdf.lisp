;;;; KBKDF -- Key-Based KDF in counter mode (NIST SP 800-108 §5.1)
;;;;
;;;; Derives keying material from a key-derivation key K_IN using a PRF
;;;; (HMAC). The counter-mode construction is:
;;;;
;;;;   For i = 1..n:    K(i) = PRF(K_IN, [i]_r || FixedInputData)
;;;;   K_OUT          = leftmost L bytes of K(1) || K(2) || ... || K(n)
;;;;
;;;; where r is the counter width in bits (32 in this implementation;
;;;; SP 800-108 only requires r small enough that n fits without
;;;; overflow). FixedInputData is conceptually `Label || 0x00 || Context
;;;; || [L]_32', but in practice many higher-level protocols supply
;;;; their own pre-built FixedInputData blob (the NIST CAVS vector
;;;; format is one such case), so this module accepts the blob
;;;; directly. A high-level helper (`kbkdf-counter-hmac-sha256-with-
;;;; label-context') builds the SP 800-108 default form for callers
;;;; that want it.

(defpackage epsilon.crypto.kbkdf
  (:use :cl)
  (:import (epsilon.crypto.hmac hmac))
  (:export
   #:kbkdf-counter-hmac-sha256
   #:kbkdf-counter-hmac-sha384
   #:kbkdf-counter-hmac-sha512
   #:kbkdf-counter-hmac-sha256-with-label-context))

(in-package :epsilon.crypto.kbkdf)

(defun %u32-be (x)
  "Encode a 32-bit unsigned integer as 4 big-endian bytes."
  (let ((b (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref b 0) (logand #xFF (ash x -24)))
    (setf (aref b 1) (logand #xFF (ash x -16)))
    (setf (aref b 2) (logand #xFF (ash x -8)))
    (setf (aref b 3) (logand #xFF x))
    b))

(defun %concat (a b)
  (let ((out (make-array (+ (length a) (length b))
                         :element-type '(unsigned-byte 8))))
    (replace out a :start1 0)
    (replace out b :start1 (length a))
    out))

(defun %kbkdf-counter (prf key fixed-input output-len prf-output-bytes)
  "Generic SP 800-108 counter-mode driver. PRF is a function of two
   byte vectors (key, data) returning PRF-OUTPUT-BYTES bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) key fixed-input)
           (type (integer 1) output-len prf-output-bytes))
  (let* ((iterations (ceiling output-len prf-output-bytes))
         (out (make-array output-len :element-type '(unsigned-byte 8))))
    (when (>= iterations (ash 1 32))
      (error "KBKDF: requested output exceeds 32-bit counter range"))
    (loop with offset = 0
          for i from 1 to iterations
          for block = (funcall prf key (%concat (%u32-be i) fixed-input))
          for take = (min prf-output-bytes (- output-len offset))
          do (replace out block :start1 offset :end2 take)
             (incf offset take))
    out))

(defun kbkdf-counter-hmac-sha256 (key fixed-input output-len)
  "KBKDF in counter mode with HMAC-SHA-256 as the PRF. KEY is the
   key-derivation key K_IN; FIXED-INPUT is the SP 800-108 fixed input
   data block; OUTPUT-LEN is the number of bytes to derive."
  (declare (type (simple-array (unsigned-byte 8) (*)) key fixed-input))
  (%kbkdf-counter (lambda (k m) (hmac:hmac-sha256 k m))
                  key fixed-input output-len 32))

(defun kbkdf-counter-hmac-sha384 (key fixed-input output-len)
  "KBKDF in counter mode with HMAC-SHA-384 as the PRF."
  (declare (type (simple-array (unsigned-byte 8) (*)) key fixed-input))
  (%kbkdf-counter (lambda (k m) (hmac:hmac-sha384 k m))
                  key fixed-input output-len 48))

(defun kbkdf-counter-hmac-sha512 (key fixed-input output-len)
  "KBKDF in counter mode with HMAC-SHA-512 as the PRF."
  (declare (type (simple-array (unsigned-byte 8) (*)) key fixed-input))
  (%kbkdf-counter (lambda (k m) (hmac:hmac-sha512 k m))
                  key fixed-input output-len 64))

(defun kbkdf-counter-hmac-sha256-with-label-context
    (key label context output-len)
  "Build the SP 800-108 default fixed-input form
     FixedInput = Label || 0x00 || Context || [L]_32
   and invoke KBKDF/HMAC-SHA-256. [L]_32 is OUTPUT-LEN expressed in
   bits as a 32-bit big-endian integer."
  (declare (type (simple-array (unsigned-byte 8) (*)) key label context))
  (let* ((sep (make-array 1 :element-type '(unsigned-byte 8)
                          :initial-element 0))
         (l-bits (* 8 output-len))
         (fixed-input
           (let* ((parts (list label sep context (%u32-be l-bits)))
                  (total (reduce #'+ parts :key #'length))
                  (out (make-array total :element-type '(unsigned-byte 8))))
             (loop with off = 0
                   for p in parts
                   do (replace out p :start1 off)
                      (incf off (length p)))
             out)))
    (kbkdf-counter-hmac-sha256 key fixed-input output-len)))

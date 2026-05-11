;;;; ML-KEM byte encoding and compression (FIPS 203 §4.2.1 and §4.2.1.1)
;;;;
;;;; Four closely-related functions:
;;;;
;;;;   ByteEncode_d(F)    -- pack 256 integers, each strictly less than
;;;;                         2^d, into a byte vector of length 32·d.
;;;;   ByteDecode_d(B)    -- inverse of ByteEncode_d.
;;;;   Compress_d(x)      -- quantise x in Z_q down to a d-bit value in
;;;;                         Z_{2^d} by rounding (q / 2^d) * x.
;;;;   Decompress_d(y)    -- reverse the quantisation approximately.
;;;;
;;;; d ranges over 1..12 in ML-KEM. ML-KEM-768 specifically uses:
;;;;   d = 12  for uncompressed polynomials (ek, dk raw coefficients)
;;;;   d = 1   for message bits (32-byte plaintext <-> polynomial)
;;;;   d = 4   for `v` (the short ciphertext component)
;;;;   d = 10  for `u` (the long ciphertext component, k entries of 32·10 bytes)
;;;;
;;;; The compression rounding is crucial: it is the only lossy step in
;;;; ML-KEM and its error is what the LWE-based decryption has to
;;;; tolerate. Getting the rounding direction wrong (floor vs round to
;;;; nearest) is a classic source of interop bugs, so each helper here
;;;; is tested against its inverse.

(defpackage epsilon.crypto.ml-kem-codec
  (:use :cl)
  (:import (epsilon.crypto.ml-kem-ntt ml))
  (:export
   #:byte-encode
   #:byte-decode
   #:compress
   #:decompress
   #:compress-poly
   #:decompress-poly))

(in-package :epsilon.crypto.ml-kem-codec)

;;; ---------------------------------------------------------------------------
;;; ByteEncode_d / ByteDecode_d  (FIPS 203 Algorithms 5 and 6)
;;; ---------------------------------------------------------------------------
;;;
;;; The encoding packs 256 d-bit values LSB-first into a byte stream of
;;; length 32·d. For d=12, each pair of coefficients consumes 3 bytes
;;; (24 bits). For d=1, 8 coefficients per byte. Generic packing via
;;; a bit buffer handles all values uniformly.

(defun byte-encode (f d)
  "FIPS 203 Algorithm 5: ByteEncode_d.
   F is a 256-element polynomial whose coefficients fit in d bits
   (i.e. each is in [0, 2^d)). Returns a byte vector of length 32·d."
  (declare (type (simple-array (unsigned-byte 16) (256)) f))
  (assert (<= 1 d 12) () "byte-encode: d must be 1..12 (got ~D)" d)
  (let* ((out-len (* 32 d))
         (out (make-array out-len :element-type '(unsigned-byte 8)
                                  :initial-element 0))
         (buf 0)
         (bits 0)
         (pos 0))
    (declare (type fixnum buf bits pos))
    (loop for i from 0 below ml:+n+ do
      (let ((coeff (aref f i)))
        (assert (< coeff (ash 1 d)) ()
                "byte-encode: coefficient ~D at index ~D exceeds 2^~D"
                coeff i d)
        (setf buf (logior buf (ash coeff bits)))
        (incf bits d)
        (loop while (>= bits 8) do
          (setf (aref out pos) (logand buf #xff))
          (incf pos)
          (setf buf (ash buf -8))
          (decf bits 8))))
    out))

(defun byte-decode (bytes d)
  "FIPS 203 Algorithm 6: ByteDecode_d.
   Inverse of `byte-encode`: reads 32·d bytes and returns a polynomial
   whose 256 coefficients are the d-bit values unpacked LSB-first.

   For d=12, coefficients in the range [q, 2^12) are reduced mod q to
   accommodate implementations that treat the upper range as invalid.
   Note that byte-encode / byte-decode are therefore strict inverses
   only when the input polynomial is already reduced mod q."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (assert (<= 1 d 12) () "byte-decode: d must be 1..12 (got ~D)" d)
  (assert (= (length bytes) (* 32 d)) ()
          "byte-decode: expected ~D bytes (32*~D), got ~D"
          (* 32 d) d (length bytes))
  (let* ((out (ml:make-poly))
         (mask (1- (ash 1 d)))
         (buf 0)
         (bits 0)
         (pos 0))
    (declare (type fixnum buf bits pos))
    (loop for i from 0 below ml:+n+ do
      (loop while (< bits d) do
        (setf buf (logior buf (ash (aref bytes pos) bits)))
        (incf pos)
        (incf bits 8))
      (let ((value (logand buf mask)))
        ;; For d=12 the stored coefficients may wrap around a full
        ;; [0, 2^12) range; bring them into the canonical [0, q) range.
        (when (= d 12)
          (setf value (mod value ml:+q+)))
        (setf (aref out i) value))
      (setf buf (ash buf (- d)))
      (decf bits d))
    out))

;;; ---------------------------------------------------------------------------
;;; Compress_d / Decompress_d  (FIPS 203 §4.2.1.1)
;;; ---------------------------------------------------------------------------
;;;
;;; Compress_d(x)   = round((2^d / q) · x) mod 2^d
;;; Decompress_d(y) = round((q / 2^d) · y)
;;;
;;; Both are computed with integer arithmetic (no floating point):
;;;   Compress_d(x)   = floor((x · 2^d + q/2) / q) mod 2^d
;;;   Decompress_d(y) = floor((y · q + 2^(d-1)) / 2^d)
;;;
;;; These are per-coefficient; `compress-poly` / `decompress-poly`
;;; apply them to every coefficient of a polynomial.

(defun compress (x d)
  "FIPS 203 §4.2.1.1: Compress_d on a single value x in [0, q)."
  (declare (type (integer 0 #.(1- ml:+q+)) x)
           (type (integer 1 12) d))
  (let* ((two-d (ash 1 d)))
    (mod (floor (+ (* x two-d) (floor ml:+q+ 2)) ml:+q+) two-d)))

(defun decompress (y d)
  "FIPS 203 §4.2.1.1: Decompress_d on a single value y in [0, 2^d)."
  (declare (type (integer 1 12) d))
  (let ((two-d (ash 1 d)))
    (assert (< y two-d) () "decompress: y (~D) must be < 2^d (~D)" y two-d)
    (floor (+ (* y ml:+q+) (ash 1 (1- d))) two-d)))

(defun compress-poly (f d)
  "Apply Compress_d to every coefficient of polynomial F. Returns a
   new polynomial whose coefficients are in [0, 2^d)."
  (declare (type (simple-array (unsigned-byte 16) (256)) f))
  (let ((out (ml:make-poly)))
    (loop for i from 0 below ml:+n+
          do (setf (aref out i) (compress (aref f i) d)))
    out))

(defun decompress-poly (f d)
  "Apply Decompress_d to every coefficient of polynomial F. F is
   assumed to have coefficients in [0, 2^d)."
  (declare (type (simple-array (unsigned-byte 16) (256)) f))
  (let ((out (ml:make-poly)))
    (loop for i from 0 below ml:+n+
          do (setf (aref out i) (decompress (aref f i) d)))
    out))

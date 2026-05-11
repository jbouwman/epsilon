;;;; Tests for ML-KEM byte encoding and compression (FIPS 203 §4.2.1)

(defpackage epsilon.crypto.ml-kem-codec-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.crypto.ml-kem-ntt ml)
            (epsilon.crypto.ml-kem-codec codec)))

(in-package :epsilon.crypto.ml-kem-codec-tests)

(defun pattern-poly (mask)
  "Deterministic polynomial whose coefficients are masked indices.
   The result has coefficients in [0, mask+1)."
  (let ((p (ml:make-poly)))
    (loop for i from 0 below ml:+n+
          do (setf (aref p i) (logand i mask)))
    p))

;;; ---- ByteEncode / ByteDecode ----

(deftest test-byte-encode-output-length
  "For d in 1..12, ByteEncode_d(f) always produces 32*d bytes."
  (loop for d from 1 to 12 do
    (let* ((p (pattern-poly (1- (ash 1 d))))
           (encoded (codec:byte-encode p d)))
      (assert-= (length encoded) (* 32 d)))))

(deftest test-byte-encode-decode-roundtrip-d1
  "d=1 round trip (1 bit per coefficient, used for the 32-byte message encoding)."
  (let* ((p (ml:make-poly)))
    (loop for i from 0 below ml:+n+ do (setf (aref p i) (logand i 1)))
    (let ((back (codec:byte-decode (codec:byte-encode p 1) 1)))
      (assert-true (equalp back p)))))

(deftest test-byte-encode-decode-roundtrip-d4
  "d=4 round trip (4 bits per coefficient, used for the `v`
   ciphertext component in ML-KEM-768)."
  (let* ((p (pattern-poly 15)))  ; all coefficients in [0, 16)
    (let ((back (codec:byte-decode (codec:byte-encode p 4) 4)))
      (assert-true (equalp back p)))))

(deftest test-byte-encode-decode-roundtrip-d10
  "d=10 round trip (used for the `u` ciphertext component)."
  (let* ((p (pattern-poly 1023)))
    (let ((back (codec:byte-decode (codec:byte-encode p 10) 10)))
      (assert-true (equalp back p)))))

(deftest test-byte-encode-decode-roundtrip-d12-canonical
  "d=12 round trip on a polynomial whose coefficients are already in
   [0, q). The byte-decode path reduces mod q, so round-trip is exact
   for any canonical polynomial."
  (let* ((p (ml:make-poly)))
    (loop for i from 0 below ml:+n+
          do (setf (aref p i) (mod (* i 13) ml:+q+)))
    (let ((back (codec:byte-decode (codec:byte-encode p 12) 12)))
      (assert-true (equalp back p)))))

(deftest test-byte-encode-rejects-out-of-range
  "byte-encode refuses a polynomial whose coefficients don't fit in d bits."
  (let ((p (ml:make-poly)))
    (setf (aref p 0) 16)
    (assert-condition (error) (codec:byte-encode p 4))))

(deftest test-byte-encode-d1-bit-ordering
  "d=1: the first coefficient lands in bit 0 of byte 0, the eighth in
   bit 7 of byte 0, the ninth in bit 0 of byte 1, etc."
  (let ((p (ml:make-poly)))
    (setf (aref p 0) 1)
    (setf (aref p 7) 1)
    (setf (aref p 8) 1)
    (let ((bytes (codec:byte-encode p 1)))
      (assert-= (aref bytes 0) #b10000001)  ; bits 0 and 7 set
      (assert-= (aref bytes 1) #b00000001)))) ; bit 0 set (was coefficient 8)

;;; ---- Compress / Decompress ----

(deftest test-compress-decompress-endpoints
  "Compress_d(0) = 0 and Decompress_d(0) = 0 for every d (the trivial
   zero-to-zero map at both ends of the quantisation ladder)."
  (loop for d from 1 to 12 do
    (assert-= (codec:compress 0 d) 0)
    (assert-= (codec:decompress 0 d) 0)))

(deftest test-compress-d1-message-bit-recovery
  "d=1 is the codec used to transport a single plaintext bit per
   coefficient. The transmitted bit b becomes the coefficient
   Decompress_1(b) = round(q·b/2), and Compress_1 of any value in
   the 'nearer to Decompress_1(b) than to Decompress_1(1-b)' half
   must recover b.

   The split point is at q/4 ≈ 832: compress(0..832, 1) = 0 and
   compress(833..2496, 1) = 1, then compress(2497..q-1, 1) = 2 mod 2 = 0.
   So the message-bit recovery chain
     x ∈ {0, round(q/2)} → compress → decompress → x
   is what ML-KEM's decryption actually relies on."
  ;; The two 'clean' message-bit ciphertext values:
  (assert-= (codec:compress 0 1) 0)
  (assert-= (codec:decompress 0 1) 0)
  ;; Decompress_1(1) = round(q/2) = 1665.
  (assert-= (codec:decompress 1 1) 1665)
  ;; Compress of that value recovers 1.
  (assert-= (codec:compress 1665 1) 1)
  ;; And the full recovery of the message bit round trip:
  (assert-= (codec:compress (codec:decompress 0 1) 1) 0)
  (assert-= (codec:compress (codec:decompress 1 1) 1) 1))

(deftest test-compress-d1-split-points
  "The split between compress=0 and compress=1 in d=1 falls at x ≈ q/4.
   Specifically, compress(832, 1) = 0 and compress(833, 1) = 1."
  (assert-= (codec:compress 832 1) 0)
  (assert-= (codec:compress 833 1) 1))

(deftest test-compress-decompress-approximate-identity
  "For every value x in [0, q) and every d in {4, 10} (the compression
   widths ML-KEM-768 actually uses for the ciphertext components),
   Decompress_d(Compress_d(x)) is within ⌈q/2^(d+1)⌉ of x modulo q.

   This is FIPS 203 Theorem 4.5 and is the formal statement of the
   quantisation-error bound that ML-KEM's correctness analysis relies
   on -- it bounds how far the lattice noise can push a plaintext
   coefficient before the message bit becomes unrecoverable."
  (flet ((toroidal-distance (a b)
           (let ((d (abs (- a b))))
             (min d (- ml:+q+ d)))))
    (dolist (d '(4 10))
      (let ((bound (ceiling ml:+q+ (ash 1 (1+ d)))))
        (dotimes (x ml:+q+)
          (let ((y (codec:decompress (codec:compress x d) d)))
            (assert-true (<= (toroidal-distance x y) bound))))))))

(deftest test-compress-poly-decompress-poly-shape
  "compress-poly / decompress-poly operate pointwise and preserve the
   256-element polynomial shape."
  (let* ((p (ml:make-poly)))
    (loop for i from 0 below ml:+n+
          do (setf (aref p i) (mod (* i 11) ml:+q+)))
    (let ((c (codec:compress-poly p 4)))
      (assert-= (length c) ml:+n+)
      (loop for i from 0 below ml:+n+ do (assert-true (< (aref c i) 16)))
      (let ((d (codec:decompress-poly c 4)))
        (assert-= (length d) ml:+n+)
        (loop for i from 0 below ml:+n+
              do (assert-true (< (aref d i) ml:+q+)))))))

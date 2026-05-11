;;;; ML-KEM sampling primitives (FIPS 203 §4.2.2)
;;;;
;;;; Two distinct sampling procedures are needed by ML-KEM:
;;;;
;;;;   SampleNTT (Algorithm 7)  -- rejection sampling from a SHAKE128
;;;;     stream to produce a polynomial in the NTT domain whose 256
;;;;     coefficients are uniformly distributed in Z_q.
;;;;
;;;;   SamplePolyCBD_η (Algorithm 8) -- deterministically convert a
;;;;     byte array (typically 64·η bytes drawn from a SHAKE256 PRF)
;;;;     into a polynomial whose 256 coefficients follow a centred
;;;;     binomial distribution on {-η, ..., η}. Used for the "small"
;;;;     secret and noise polynomials that give the module LWE its
;;;;     hardness.
;;;;
;;;; The sampled polynomial produced by SampleNTT is already in the
;;;; NTT domain and can be fed directly to `ml-kem-ntt:ntt-multiply`.
;;;; The polynomial produced by SamplePolyCBD is in the standard
;;;; (time) domain and must be NTT'd before being used in arithmetic
;;;; with matrix entries.

(defpackage epsilon.crypto.ml-kem-sample
  (:use :cl)
  (:import (epsilon.crypto.ml-kem-ntt ml)
            (epsilon.crypto.sha3 sha3))
  (:export
   #:sample-ntt
   #:sample-poly-cbd))

(in-package :epsilon.crypto.ml-kem-sample)

;;; ---------------------------------------------------------------------------
;;; SampleNTT (rejection sampling from SHAKE128)
;;; ---------------------------------------------------------------------------
;;;
;;; The algorithm reads 3 bytes at a time from the XOF and splits them
;;; into two 12-bit integers. Each 12-bit integer that is strictly less
;;; than q is written to the next free slot of the output polynomial;
;;; values >= q are discarded. The loop continues until all 256
;;; coefficients have been written.
;;;
;;; The 3-bytes-to-two-12-bit-values split is exactly:
;;;     d1 = B[0]         | ((B[1] & 0x0F) << 8)
;;;     d2 = (B[1] >> 4)  | (B[2]         << 4)

(defun sample-ntt (seed)
  "FIPS 203 Algorithm 7: SampleNTT.

   SEED is a 34-byte value (32-byte rho followed by two index bytes i, j).
   Returns a polynomial (256 u16 coefficients in Z_q) already in the
   NTT domain.

   The polynomial is uniform in (Z_q)^256. About 13/32 of candidate
   12-bit values are rejected on average (because 2^12 - q = 767 out
   of 4096 values are >= q)."
  (declare (type (simple-array (unsigned-byte 8) (*)) seed))
  (let* ((state (sha3:make-shake128-state))
         (out (ml:make-poly))
         (j 0))
    (sha3:shake128-update state seed)
    (loop while (< j ml:+n+) do
      (let ((block (sha3:shake128-squeeze state 3)))
        (let ((d1 (logior (aref block 0)
                          (ash (logand (aref block 1) #x0F) 8)))
              (d2 (logior (ash (aref block 1) -4)
                          (ash (aref block 2) 4))))
          (when (and (< d1 ml:+q+) (< j ml:+n+))
            (setf (aref out j) d1)
            (incf j))
          (when (and (< d2 ml:+q+) (< j ml:+n+))
            (setf (aref out j) d2)
            (incf j)))))
    out))

;;; ---------------------------------------------------------------------------
;;; SamplePolyCBD_η (centred binomial distribution)
;;; ---------------------------------------------------------------------------
;;;
;;; Input: byte array B of length 64·η.
;;; Output: polynomial in R_q with each coefficient independently drawn
;;;         from the distribution on {-η, ..., η} where
;;;            Pr[X = 0] = C(2η, η) / 2^(2η)
;;;            Pr[X = k] = C(2η, η+k) / 2^(2η) for k != 0
;;;
;;; Concretely, for each of the 256 coefficients:
;;;   1. Read 2η bits from the byte array (sequentially, LSB-first).
;;;   2. Split them into two halves of η bits each: `a` and `b`.
;;;   3. The coefficient is popcount(a) - popcount(b), reduced mod q.
;;;
;;; For η=2 (the only value ML-KEM-768 actually uses), we can read a
;;; single byte (8 bits) per two consecutive coefficients, which is
;;; both simpler and matches the reference implementation's loop
;;; structure most directly.

(defun %read-bit (bytes bit-index)
  "Read the bit at position BIT-INDEX from BYTES, with bits packed
   LSB-first within each byte (bit 0 of byte 0 comes first, bit 7 of
   byte 0 comes eighth, then bit 0 of byte 1, ...)."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type fixnum bit-index))
  (logand (ash (aref bytes (ash bit-index -3)) (- (logand bit-index 7))) 1))

(defun sample-poly-cbd (bytes eta)
  "FIPS 203 Algorithm 8: SamplePolyCBD_η.

   BYTES is a byte vector of length 64·ETA.
   ETA is 2 or 3 (ML-KEM-768 uses η=2 for both the secret and the
   error polynomials, so only 64·2 = 128-byte inputs are actually
   consumed in practice; η=3 support is here for completeness because
   other parameter sets would use it).

   Returns a 256-coefficient polynomial whose coefficients follow the
   centred binomial distribution on {-η, ..., η}, reduced mod q (so
   negative values become q-η .. q-1)."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (assert (= (length bytes) (* 64 eta)) ()
          "sample-poly-cbd: expected ~D bytes (64 * η=~D), got ~D"
          (* 64 eta) eta (length bytes))
  (assert (member eta '(2 3)) ()
          "sample-poly-cbd: only η=2 and η=3 are supported (got ~D)" eta)
  (let ((out (ml:make-poly)))
    (loop for i from 0 below ml:+n+ do
      (let ((a 0)
            (b 0))
        ;; Read η bits for a, then η bits for b, starting at bit
        ;; offset i * 2 * η in the stream.
        (loop for k from 0 below eta
              do (incf a (%read-bit bytes (+ (* 2 eta i) k))))
        (loop for k from 0 below eta
              do (incf b (%read-bit bytes (+ (* 2 eta i) eta k))))
        ;; Coefficient is (a - b) mod q, with negative values folded
        ;; back into [0, q).
        (setf (aref out i)
              (if (>= a b)
                  (- a b)
                  (- ml:+q+ (- b a))))))
    out))

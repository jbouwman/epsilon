;;;; ML-KEM polynomial ring and NTT (FIPS 203 §4.3)
;;;;
;;;; Ring: R_q = Z_q[X] / (X^n + 1), with q = 3329, n = 256.
;;;;
;;;; Since 256 divides q-1 = 3328 (3328 = 256 * 13), Z_q contains a
;;;; primitive 256-th root of unity ζ = 17. The polynomial X^256 + 1
;;;; therefore splits into 128 quadratic factors (X^2 - ζ^(2·br7(i)+1))
;;;; over Z_q, so the NTT is a 7-layer (not 8-layer) butterfly and the
;;;; result is 128 degree-1 polynomials rather than 256 scalars.
;;;;
;;;; This file implements:
;;;;   - Z_q scalar arithmetic
;;;;   - Polynomial add / sub / copy / zero
;;;;   - Forward NTT (Algorithm 9)
;;;;   - Inverse NTT (Algorithm 10) with the final 128^-1 = 3303 scaling
;;;;   - Base-case multiplication in the NTT domain (Algorithm 11)
;;;;   - Schoolbook multiplication (slow reference, used by tests to
;;;;     validate the NTT path)
;;;;
;;;; Sampling, byte encoding/decoding, compression, K-PKE, and the
;;;; ML-KEM wrapper itself are scoped to follow-up sessions.

(defpackage epsilon.crypto.ml-kem-ntt
  (:use :cl)
  (:export
   ;; Parameters
   #:+q+ #:+n+
   ;; Polynomial primitives
   #:poly
   #:make-poly
   #:poly-copy
   #:poly-add
   #:poly-sub
   ;; NTT
   #:ntt
   #:ntt-inverse
   #:ntt-multiply
   ;; Reference / testing
   #:schoolbook-multiply
   ;; Constants tables (exposed for tests)
   #:*ntt-zetas*
   #:*gammas*))

(in-package :epsilon.crypto.ml-kem-ntt)

;;; ---------------------------------------------------------------------------
;;; Parameters
;;; ---------------------------------------------------------------------------

(defconstant +q+ 3329
  "The ML-KEM modulus. q is prime and q - 1 = 3328 = 256 * 13.")

(defconstant +n+ 256
  "Degree of the ring R_q = Z_q[X] / (X^n + 1). Also the number of
   coefficients in each polynomial.")

(defconstant +zeta-base+ 17
  "A primitive 256-th root of unity in Z_q. FIPS 203 §4.3 pins ζ = 17.")

(defconstant +n-inv+ 3303
  "128^-1 mod q = -26 mod 3329 = 3303. Applied after the inverse NTT to
   normalise the output (the NTT has 128 butterfly pairs per layer, not 256).")

(deftype poly () '(simple-array (unsigned-byte 16) (256)))

(defun make-poly (&optional (initial-element 0))
  "Allocate a zero polynomial (256 coefficients in Z_q, stored as u16)."
  (make-array +n+ :element-type '(unsigned-byte 16)
                  :initial-element initial-element))

(defun poly-copy (p)
  (let ((out (make-poly)))
    (replace out p)
    out))

(defun poly-add (a b)
  "Pointwise addition in Z_q."
  (let ((out (make-poly)))
    (loop for i from 0 below +n+
          do (setf (aref out i) (mod (+ (aref a i) (aref b i)) +q+)))
    out))

(defun poly-sub (a b)
  "Pointwise subtraction in Z_q. Handles negative intermediate values
   by adding q before the mod."
  (let ((out (make-poly)))
    (loop for i from 0 below +n+
          do (setf (aref out i) (mod (+ (aref a i) (- +q+ (aref b i))) +q+)))
    out))

;;; ---------------------------------------------------------------------------
;;; Zeta table computation
;;; ---------------------------------------------------------------------------
;;;
;;; FIPS 203 uses two separate tables of powers of ζ:
;;;
;;;   ntt-zetas[k] = ζ^BitRev7(k) mod q      for k in 0..127
;;;   gammas[i]    = ζ^(2·BitRev7(i)+1) mod q for i in 0..127
;;;
;;; ntt-zetas drives the butterfly multiplications of the forward and
;;; inverse NTT. gammas is used by the base-case multiplier because
;;; X^256 + 1 factors into 128 quadratics X^2 - γ_i over Z_q, and
;;; multiplying two degree-1 polynomials modulo one of these quadratics
;;; requires the corresponding γ.
;;;
;;; The tables could be hardcoded straight from FIPS 203 Appendix A,
;;; but computing them at load time is clearer and auditable -- the
;;; tests then cross-check a handful of specific entries against
;;; published values.

(defun %mod-pow (base exponent modulus)
  (let ((result 1)
        (b (mod base modulus))
        (e exponent))
    (loop while (plusp e) do
      (when (oddp e)
        (setf result (mod (* result b) modulus)))
      (setf b (mod (* b b) modulus))
      (setf e (ash e -1)))
    result))

(defun %bit-reverse-7 (k)
  "Reverse the low 7 bits of K. FIPS 203 calls this BitRev_7."
  (let ((r 0))
    (loop repeat 7 do
      (setf r (logior (ash r 1) (logand k 1)))
      (setf k (ash k -1)))
    r))

(defparameter *ntt-zetas*
  (let ((arr (make-array 128 :element-type '(unsigned-byte 16))))
    (dotimes (k 128)
      (setf (aref arr k) (%mod-pow +zeta-base+ (%bit-reverse-7 k) +q+)))
    arr)
  "FIPS 203 Algorithm 9/10: ZETAS[k] = ζ^BitRev7(k) mod q for k in 0..127.")

(defparameter *gammas*
  (let ((arr (make-array 128 :element-type '(unsigned-byte 16))))
    (dotimes (i 128)
      (setf (aref arr i) (%mod-pow +zeta-base+ (1+ (* 2 (%bit-reverse-7 i))) +q+)))
    arr)
  "FIPS 203 Algorithm 11: γ_i = ζ^(2·BitRev7(i) + 1) mod q for i in 0..127.
   Each γ_i identifies one of the 128 quadratic factors of X^256 + 1.")

;;; ---------------------------------------------------------------------------
;;; Forward NTT (Algorithm 9)
;;; ---------------------------------------------------------------------------

(defun ntt (f)
  "Forward NTT: R_q -> T_q. Input and output are 256-coefficient
   polynomials. The output is interpreted as 128 degree-1 polynomials
   rather than 256 scalars (because the ring R_q only splits into
   quadratics over Z_q). Non-destructive: returns a fresh vector."
  (declare (type poly f))
  (let ((f-hat (poly-copy f))
        (k 1))
    (declare (type (integer 0 128) k))
    (dolist (len '(128 64 32 16 8 4 2))
      (declare (type (integer 2 128) len))
      (loop for start from 0 below +n+ by (* 2 len)
            do (let ((zeta (aref *ntt-zetas* k)))
                 (incf k)
                 (loop for j from start below (+ start len)
                       do (let* ((t-val (mod (* zeta (aref f-hat (+ j len))) +q+))
                                 (fj (aref f-hat j)))
                            (setf (aref f-hat (+ j len))
                                  (mod (+ fj (- +q+ t-val)) +q+))
                            (setf (aref f-hat j)
                                  (mod (+ fj t-val) +q+)))))))
    f-hat))

;;; ---------------------------------------------------------------------------
;;; Inverse NTT (Algorithm 10)
;;; ---------------------------------------------------------------------------

(defun ntt-inverse (f-hat)
  "Inverse NTT: T_q -> R_q. Multiplies by 128^-1 = 3303 mod q at the
   end. Non-destructive: returns a fresh vector."
  (declare (type poly f-hat))
  (let ((f (poly-copy f-hat))
        (k 127))
    (declare (type (integer 0 127) k))
    (dolist (len '(2 4 8 16 32 64 128))
      (declare (type (integer 2 128) len))
      (loop for start from 0 below +n+ by (* 2 len)
            do (let ((zeta (aref *ntt-zetas* k)))
                 (decf k)
                 (loop for j from start below (+ start len)
                       do (let* ((t-val (aref f j))
                                 (fjl (aref f (+ j len))))
                            (setf (aref f j) (mod (+ t-val fjl) +q+))
                            (setf (aref f (+ j len))
                                  (mod (* zeta (mod (+ fjl (- +q+ t-val)) +q+))
                                       +q+)))))))
    (loop for j from 0 below +n+
          do (setf (aref f j) (mod (* (aref f j) +n-inv+) +q+)))
    f))

;;; ---------------------------------------------------------------------------
;;; Base-case multiplication in the NTT domain (Algorithm 11)
;;; ---------------------------------------------------------------------------

(defun %base-case-multiply (a0 a1 b0 b1 gamma)
  "Multiply two degree-1 polynomials (a0 + a1 X) and (b0 + b1 X)
   modulo (X^2 - γ). Returns (values c0 c1) such that
     (a0 + a1 X)(b0 + b1 X) = c0 + c1 X   mod (X^2 - γ)."
  (declare (type (unsigned-byte 16) a0 a1 b0 b1 gamma))
  (values (mod (+ (* a0 b0) (mod (* (mod (* a1 b1) +q+) gamma) +q+)) +q+)
          (mod (+ (* a0 b1) (* a1 b0)) +q+)))

(defun ntt-multiply (f-hat g-hat)
  "Multiply two polynomials in NTT form. The output is the NTT form
   of the product of the underlying polynomials in R_q.

   Because R_q splits into 128 quadratics over Z_q, the NTT-domain
   representation is 128 pairs of coefficients and the product is
   computed as 128 independent degree-1 polynomial multiplications
   modulo the corresponding quadratic factor. This is NOT pointwise
   multiplication."
  (declare (type poly f-hat g-hat))
  (let ((h (make-poly)))
    (loop for i from 0 below 128 do
      (let ((gamma (aref *gammas* i)))
        (multiple-value-bind (c0 c1)
            (%base-case-multiply (aref f-hat (* 2 i))
                                 (aref f-hat (1+ (* 2 i)))
                                 (aref g-hat (* 2 i))
                                 (aref g-hat (1+ (* 2 i)))
                                 gamma)
          (setf (aref h (* 2 i)) c0)
          (setf (aref h (1+ (* 2 i))) c1))))
    h))

;;; ---------------------------------------------------------------------------
;;; Schoolbook multiplication (slow reference)
;;; ---------------------------------------------------------------------------
;;;
;;; Computes a · b in R_q = Z_q[X] / (X^256 + 1) directly by accumulating
;;; the full 256×256 convolution and folding the upper half back through
;;; the X^n + 1 ≡ 0 relation (so coefficient k + 256 becomes -k). Used
;;; only by tests as the ground truth against which ntt-multiply is
;;; compared; O(n^2) and not called from the hot path.

(defun schoolbook-multiply (a b)
  (declare (type poly a b))
  (let ((h (make-poly)))
    (loop for i from 0 below +n+ do
      (loop for j from 0 below +n+ do
        (let ((product (mod (* (aref a i) (aref b j)) +q+))
              (k (+ i j)))
          (cond
            ((< k +n+)
             (setf (aref h k) (mod (+ (aref h k) product) +q+)))
            (t
             ;; X^(k) with k >= 256: fold back via X^256 = -1
             (let ((kk (- k +n+)))
               (setf (aref h kk)
                     (mod (+ (aref h kk) (- +q+ product)) +q+))))))))
    h))

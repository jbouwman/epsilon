;;;; Tests for ML-KEM polynomial / NTT layer (FIPS 203 §4.3)

(defpackage epsilon.crypto.ml-kem-ntt-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.crypto.ml-kem-ntt ml)))

(in-package :epsilon.crypto.ml-kem-ntt-tests)

(defun random-poly (&optional (seed 0))
  "Deterministic pseudo-random polynomial for reproducible tests. Uses
   a cheap multiplicative PRNG -- the values don't need to be
   cryptographically random, just reasonably well-distributed across Z_q."
  (let ((p (ml:make-poly))
        (state (+ seed 12345)))
    (loop for i from 0 below ml:+n+
          do (setf state (mod (+ (* state 1103515245) 12345) (expt 2 32)))
             (setf (aref p i) (mod (ash state -16) ml:+q+)))
    p))

;;; ---- ZETAS sanity ----
;;;
;;; FIPS 203 defines ZETAS[k] = ζ^BitRev7(k) mod q with ζ = 17.
;;; A handful of entries are easy to check against the widely-cited
;;; reference values and serve as spot checks on the table generator.

(deftest test-ntt-zetas-basic
  "ZETAS[0] = 17^0 = 1, ZETAS[1] = 17^BitRev7(1) = 17^64 = 1729."
  (assert-= (aref ml:*ntt-zetas* 0) 1)
  (assert-= (aref ml:*ntt-zetas* 1) 1729))

(deftest test-ntt-zetas-entry-64
  "BitRev7(64) = BitRev7(0b1000000) = 0b0000001 = 1, so ZETAS[64] = ζ^1 = 17.
   This is a particularly satisfying self-check: the base ζ reappears
   at index 64 of the transformed table."
  (assert-= (aref ml:*ntt-zetas* 64) 17))

(deftest test-ntt-zetas-size
  "There are exactly 128 non-trivial ZETAS entries."
  (assert-= (length ml:*ntt-zetas*) 128)
  (assert-= (length ml:*gammas*) 128))

(deftest test-gammas-entry-0
  "γ_0 = ζ^(2·BitRev7(0)+1) = ζ^1 = 17."
  (assert-= (aref ml:*gammas* 0) 17))

(deftest test-zeta-order-256
  "17^256 ≡ 1 (mod q) and 17^128 ≢ 1 (mod q), confirming that 17 is a
   primitive 256-th root of unity in Z_q."
  (let ((q ml:+q+))
    ;; Compute 17^256 mod q via repeated squaring
    (let ((p 1))
      (dotimes (_ 256) (setf p (mod (* p 17) q)))
      (assert-= p 1))
    ;; And 17^128 mod q must not be 1
    (let ((p 1))
      (dotimes (_ 128) (setf p (mod (* p 17) q)))
      (assert-not (= p 1)))))

;;; ---- NTT round trip ----

(deftest test-ntt-roundtrip-zero
  "NTT(0) = 0; iNTT(NTT(0)) = 0. A degenerate but useful sanity check."
  (let* ((zero (ml:make-poly))
         (zero-hat (ml:ntt zero))
         (back (ml:ntt-inverse zero-hat)))
    (assert-true (equalp zero-hat zero))
    (assert-true (equalp back zero))))

(deftest test-ntt-roundtrip-unit
  "NTT then inverse-NTT of the polynomial f(X) = 1 returns f. Also
   a satisfying special case because 1 sits at the low end of the
   domain and the iNTT normalisation by 128^-1 is easy to verify
   mentally."
  (let* ((p (ml:make-poly)))
    (setf (aref p 0) 1)
    (let ((back (ml:ntt-inverse (ml:ntt p))))
      (assert-true (equalp back p)))))

(deftest test-ntt-roundtrip-random
  "For a handful of pseudo-random polynomials, iNTT(NTT(f)) = f."
  (dotimes (seed 5)
    (let* ((p (random-poly seed))
           (back (ml:ntt-inverse (ml:ntt p))))
      (assert-true (equalp back p)))))

;;; ---- NTT-domain multiplication vs schoolbook ----

(deftest test-ntt-multiply-matches-schoolbook-zero
  "Multiplying by zero in either domain gives zero."
  (let* ((zero (ml:make-poly))
         (f (random-poly 1)))
    (assert-true (equalp (ml:schoolbook-multiply f zero) zero))
    (assert-true (equalp (ml:ntt-inverse (ml:ntt-multiply (ml:ntt f) (ml:ntt zero)))
                         zero))))

(deftest test-ntt-multiply-matches-schoolbook-unit
  "Multiplying by the constant polynomial 1 returns the original."
  (let* ((one (ml:make-poly))
         (f (random-poly 2)))
    (setf (aref one 0) 1)
    (let ((ntt-result (ml:ntt-inverse (ml:ntt-multiply (ml:ntt f) (ml:ntt one)))))
      (assert-true (equalp ntt-result f))
      (assert-true (equalp (ml:schoolbook-multiply f one) f)))))

(deftest test-ntt-multiply-matches-schoolbook-random
  "For random polynomials f and g, the product computed via the NTT
   path matches the schoolbook convolution (modulo X^256 + 1).

   This is the load-bearing correctness check: if the ZETAS table,
   gammas table, forward NTT, inverse NTT, or base-case multiplier
   have any bug, this test fails for almost every random input."
  (dotimes (seed 3)
    (let* ((f (random-poly seed))
           (g (random-poly (+ seed 100)))
           (ref (ml:schoolbook-multiply f g))
           (fast (ml:ntt-inverse (ml:ntt-multiply (ml:ntt f) (ml:ntt g)))))
      (assert-true (equalp fast ref)))))

(deftest test-ntt-multiply-x-by-itself
  "X · X = X^2 — tests that low-degree polynomials with sparse support
   are handled correctly."
  (let* ((x (ml:make-poly))
         (x2 (ml:make-poly)))
    (setf (aref x 1) 1)
    (setf (aref x2 2) 1)
    (let ((fast (ml:ntt-inverse (ml:ntt-multiply (ml:ntt x) (ml:ntt x)))))
      (assert-true (equalp fast x2))
      (assert-true (equalp (ml:schoolbook-multiply x x) x2)))))

(deftest test-ntt-multiply-x-to-255-by-x
  "X^255 · X = X^256 = -1  (because the ring is Z_q[X]/(X^256 + 1))."
  (let* ((x (ml:make-poly))
         (x255 (ml:make-poly))
         (neg-one (ml:make-poly)))
    (setf (aref x 1) 1)
    (setf (aref x255 255) 1)
    (setf (aref neg-one 0) (- ml:+q+ 1))
    (let* ((ref (ml:schoolbook-multiply x255 x))
           (fast (ml:ntt-inverse (ml:ntt-multiply (ml:ntt x255) (ml:ntt x)))))
      (assert-true (equalp ref neg-one))
      (assert-true (equalp fast neg-one)))))

;;; ---- poly-add / poly-sub ----

(deftest test-poly-add-identity
  "Adding zero is the identity."
  (let* ((f (random-poly 7))
         (zero (ml:make-poly)))
    (assert-true (equalp (ml:poly-add f zero) f))))

(deftest test-poly-sub-self
  "f - f = 0."
  (let ((f (random-poly 8)))
    (assert-true (equalp (ml:poly-sub f f) (ml:make-poly)))))

(deftest test-poly-add-distributes-over-ntt
  "NTT is linear: NTT(f + g) = NTT(f) + NTT(g) pointwise."
  (let* ((f (random-poly 10))
         (g (random-poly 11))
         (lhs (ml:ntt (ml:poly-add f g)))
         (rhs (ml:poly-add (ml:ntt f) (ml:ntt g))))
    (assert-true (equalp lhs rhs))))

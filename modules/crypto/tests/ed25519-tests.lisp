;;;; Tests for Ed25519 point arithmetic

(defpackage epsilon.crypto.ed25519-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import
   (epsilon.crypto.ed25519 ed)))

(in-package :epsilon.crypto.ed25519-tests)

;;; Neutral element
(deftest test-ed-neutral
  "Neutral element is on curve"
  (assert-true (ed:ed-on-curve-p (ed:ed-neutral))))

;;; Base point
(deftest test-ed-base-point-on-curve
  "Base point is on curve"
  (assert-true (ed:ed-on-curve-p (ed:ed-base-point))))

(deftest test-ed-base-point-encoding
  "Base point encoding matches RFC 8032"
  ;; RFC 8032 Section 5.1: B has y = 4/5 mod p
  ;; The encoding is the y-coordinate:
  ;; y = 4/5 mod p = 4 * inv(5) mod p
  (let* ((bp (ed:ed-base-point))
         (encoded (ed:ed-point-encode bp)))
    (assert-= (length encoded) 32)
    ;; Known encoding of the base point from RFC 8032:
    (assert-equal (bytes-to-hex encoded)
                  "5866666666666666666666666666666666666666666666666666666666666666")))

;;; Point addition identity
(deftest test-ed-add-neutral
  "P + O = P"
  (let ((bp (ed:ed-base-point))
        (o (ed:ed-neutral)))
    (assert-true (ed:ed-point-equal (ed:ed-point-add bp o) bp))))

(deftest test-ed-add-neutral-commutative
  "O + P = P"
  (let ((bp (ed:ed-base-point))
        (o (ed:ed-neutral)))
    (assert-true (ed:ed-point-equal (ed:ed-point-add o bp) bp))))

;;; Point negation
(deftest test-ed-negate
  "P + (-P) = O"
  (let* ((bp (ed:ed-base-point))
         (neg-bp (ed:ed-point-negate bp))
         (sum (ed:ed-point-add bp neg-bp)))
    (assert-true (ed:ed-point-equal sum (ed:ed-neutral)))))

;;; Doubling
(deftest test-ed-double-on-curve
  "2*B is on curve"
  (let ((two-b (ed:ed-point-double (ed:ed-base-point))))
    (assert-true (ed:ed-on-curve-p two-b))))

(deftest test-ed-double-equals-add
  "2*B via doubling = B + B via addition"
  (let* ((bp (ed:ed-base-point))
         (via-double (ed:ed-point-double bp))
         (via-add (ed:ed-point-add bp bp)))
    (assert-true (ed:ed-point-equal via-double via-add))))

;;; Scalar multiplication
(deftest test-ed-scalar-mul-1
  "1 * B = B"
  (let ((bp (ed:ed-base-point)))
    (assert-true (ed:ed-point-equal (ed:ed-scalar-mul 1 bp) bp))))

(deftest test-ed-scalar-mul-2
  "2 * B = B + B"
  (let* ((bp (ed:ed-base-point))
         (two-b-scalar (ed:ed-scalar-mul 2 bp))
         (two-b-add (ed:ed-point-add bp bp)))
    (assert-true (ed:ed-point-equal two-b-scalar two-b-add))))

(deftest test-ed-scalar-mul-order
  "L * B = O (group order)"
  (let ((result (ed:ed-scalar-mul ed:+l+ (ed:ed-base-point))))
    (assert-true (ed:ed-point-equal result (ed:ed-neutral)))))

;;; Encode/decode round-trip
(deftest test-ed-encode-decode-base
  "Encode/decode round-trip for base point"
  (let* ((bp (ed:ed-base-point))
         (encoded (ed:ed-point-encode bp))
         (decoded (ed:ed-point-decode encoded)))
    (assert-true decoded)
    (assert-true (ed:ed-point-equal bp decoded))))

(deftest test-ed-encode-decode-neutral
  "Encode/decode round-trip for neutral element"
  (let* ((o (ed:ed-neutral))
         (encoded (ed:ed-point-encode o))
         (decoded (ed:ed-point-decode encoded)))
    (assert-true decoded)
    (assert-true (ed:ed-point-equal o decoded))))

(deftest test-ed-encode-decode-2b
  "Encode/decode round-trip for 2*B"
  (let* ((two-b (ed:ed-scalar-mul 2 (ed:ed-base-point)))
         (encoded (ed:ed-point-encode two-b))
         (decoded (ed:ed-point-decode encoded)))
    (assert-true decoded)
    (assert-true (ed:ed-point-equal two-b decoded))))

;;; Known small multiples of B
(deftest test-ed-scalar-mul-small
  "Small scalar multiples produce on-curve points"
  (let ((bp (ed:ed-base-point)))
    (loop for i from 1 to 10
          for pt = (ed:ed-scalar-mul i bp)
          do (assert-true (ed:ed-on-curve-p pt)))))

;;; ---------------------------------------------------------------------------
;;; Constant-time scalar multiplication (ed-scalar-mul-ct)
;;; ---------------------------------------------------------------------------

(deftest test-ed-scalar-mul-ct-trivial
  "ed-scalar-mul-ct on small scalars matches ed-scalar-mul."
  (let ((bp (ed:ed-base-point)))
    (loop for k from 0 to 12
          do (assert-true
              (ed:ed-point-equal (ed:ed-scalar-mul k bp)
                                 (ed:ed-scalar-mul-ct k bp))))))

(deftest test-ed-scalar-mul-ct-medium
  "Round-trip equivalence on medium-magnitude scalars."
  (let ((bp (ed:ed-base-point)))
    (dolist (k '(123456789
                  4242424242
                  #xdeadbeefdeadbeef
                  #xcafebabef00d))
      (assert-true
       (ed:ed-point-equal (ed:ed-scalar-mul k bp)
                          (ed:ed-scalar-mul-ct k bp))))))

(deftest test-ed-scalar-mul-ct-near-order
  "CT scalar mul handles scalars approaching the group order ℓ."
  (let ((bp (ed:ed-base-point))
        (cases (list (1- ed:+l+) (- ed:+l+ 100))))
    (dolist (k cases)
      (assert-true
       (ed:ed-point-equal (ed:ed-scalar-mul k bp)
                          (ed:ed-scalar-mul-ct k bp))))))

(deftest test-ed-scalar-mul-ct-bits-parameter
  "BITS parameter controls iteration count: passing fewer bits than
   the scalar's bit length truncates the high bits, matching the
   non-CT result for the truncated scalar."
  (let ((bp (ed:ed-base-point))
        (k #b1011010))                        ; 7 bits, value 90
    ;; Full bits: matches ed-scalar-mul of full k.
    (assert-true (ed:ed-point-equal
                  (ed:ed-scalar-mul k bp)
                  (ed:ed-scalar-mul-ct k bp :bits 8)))
    ;; bits=4: only the low 4 bits (0b1010 = 10) are processed.
    (assert-true (ed:ed-point-equal
                  (ed:ed-scalar-mul (ldb (byte 4 0) k) bp)
                  (ed:ed-scalar-mul-ct k bp :bits 4)))))

(deftest test-ed-scalar-mul-ct-zero
  "0 * P returns the neutral point."
  (let ((p (ed:ed-scalar-mul 7 (ed:ed-base-point))))
    (assert-true (ed:ed-point-equal (ed:ed-neutral)
                                    (ed:ed-scalar-mul-ct 0 p)))))

;;;; Tests for P-256 elliptic curve arithmetic

(defpackage epsilon.ssl.ec-p256-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:ec #:epsilon.ssl.ec-p256))
  (:enter t))

(in-package :epsilon.ssl.ec-p256-tests)

;;; Neutral element
(deftest test-p256-neutral
  "Neutral element is on curve"
  (assert-true (ec:p256-on-curve-p (ec:p256-neutral))))

;;; Generator point
(deftest test-p256-base-point-on-curve
  "Generator point G is on curve"
  (assert-true (ec:p256-on-curve-p (ec:p256-base-point))))

;;; Addition identity
(deftest test-p256-add-neutral
  "G + O = G"
  (let ((g (ec:p256-base-point))
        (o (ec:p256-neutral)))
    (assert-true (ec:p256-point-equal (ec:p256-point-add g o) g))))

(deftest test-p256-add-neutral-commutative
  "O + G = G"
  (let ((g (ec:p256-base-point))
        (o (ec:p256-neutral)))
    (assert-true (ec:p256-point-equal (ec:p256-point-add o g) g))))

;;; Negation
(deftest test-p256-negate
  "G + (-G) = O"
  (let* ((g (ec:p256-base-point))
         (neg-g (ec:p256-point-negate g))
         (sum (ec:p256-point-add g neg-g)))
    (assert-true (ec:p256-point-equal sum (ec:p256-neutral)))))

;;; Doubling
(deftest test-p256-double-on-curve
  "2*G is on curve"
  (let ((two-g (ec:p256-point-double (ec:p256-base-point))))
    (assert-true (ec:p256-on-curve-p two-g))))

(deftest test-p256-double-equals-add
  "2*G via doubling = G + G via addition"
  (let* ((g (ec:p256-base-point))
         (via-double (ec:p256-point-double g))
         (via-add (ec:p256-point-add g g)))
    (assert-true (ec:p256-point-equal via-double via-add))))

;;; Scalar multiplication
(deftest test-p256-scalar-mul-1
  "1 * G = G"
  (let ((g (ec:p256-base-point)))
    (assert-true (ec:p256-point-equal (ec:p256-scalar-mul 1 g) g))))

(deftest test-p256-scalar-mul-2
  "2 * G = G + G"
  (let* ((g (ec:p256-base-point))
         (two-g-scalar (ec:p256-scalar-mul 2 g))
         (two-g-add (ec:p256-point-add g g)))
    (assert-true (ec:p256-point-equal two-g-scalar two-g-add))))

(deftest test-p256-scalar-mul-order
  "n * G = O (group order)"
  (let ((result (ec:p256-scalar-mul ec:+n+ (ec:p256-base-point))))
    (assert-true (ec:p256-point-equal result (ec:p256-neutral)))))

;;; Known 2*G coordinates (from NIST)
(deftest test-p256-2g-coordinates
  "2*G matches known coordinates"
  (let* ((g (ec:p256-base-point))
         (two-g (ec:p256-scalar-mul 2 g))
         ;; Encode to uncompressed and extract x, y
         (encoded (ec:p256-point-encode-uncompressed two-g))
         (x 0)
         (y 0))
    (loop for i from 1 to 32
          do (setf x (logior (ash x 8) (aref encoded i))))
    (loop for i from 33 to 64
          do (setf y (logior (ash y 8) (aref encoded i))))
    ;; Known 2*G for P-256
    (assert-= x #x7cf27b188d034f7e8a52380304b51ac3c08969e277f21b35a60b48fc47669978)
    (assert-= y #x07775510db8ed040293d9ac69f7430dbba7dade63ce982299e04b79d227873d1)))

;;; Encoding/decoding
(deftest test-p256-encode-decode-roundtrip
  "Uncompressed encode/decode round-trip"
  (let* ((g (ec:p256-base-point))
         (encoded (ec:p256-point-encode-uncompressed g))
         (decoded (ec:p256-point-decode encoded)))
    (assert-true decoded)
    (assert-= (length encoded) 65)
    (assert-= (aref encoded 0) #x04)
    (assert-true (ec:p256-point-equal g decoded))))

;;; Small multiples on curve
(deftest test-p256-small-multiples-on-curve
  "Small scalar multiples are on curve"
  (let ((g (ec:p256-base-point)))
    (loop for i from 1 to 5
          for pt = (ec:p256-scalar-mul i g)
          do (assert-true (ec:p256-on-curve-p pt)))))

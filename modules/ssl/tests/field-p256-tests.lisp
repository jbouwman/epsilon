;;;; Tests for GF(P-256) field arithmetic

(defpackage epsilon.ssl.field-p256-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:p256 #:epsilon.ssl.field-p256))
  (:enter t))

(in-package :epsilon.ssl.field-p256-tests)

(defconstant +p+ p256:+p+)

;;; Basic operations
(deftest test-p256-mod
  "P-256 modular reduction"
  (assert-= (p256:p256-mod 0) 0)
  (assert-= (p256:p256-mod 42) 42)
  (assert-= (p256:p256-mod +p+) 0)
  (assert-= (p256:p256-mod (1+ +p+)) 1))

(deftest test-p256-add
  "P-256 addition"
  (assert-= (p256:p256-add 100 200) 300)
  (assert-= (p256:p256-add (- +p+ 10) 20) 10))

(deftest test-p256-sub
  "P-256 subtraction"
  (assert-= (p256:p256-sub 300 200) 100)
  (assert-= (p256:p256-sub 10 20) (- +p+ 10)))

(deftest test-p256-neg
  "P-256 negation"
  (assert-= (p256:p256-neg 42) (- +p+ 42))
  (assert-= (p256:p256-neg 0) 0))

(deftest test-p256-mul
  "P-256 multiplication"
  (assert-= (p256:p256-mul 7 11) 77)
  (assert-= (p256:p256-mul (- +p+ 1) 2) (mod (* (- +p+ 1) 2) +p+)))

(deftest test-p256-sqr
  "P-256 squaring"
  (assert-= (p256:p256-sqr 100) 10000)
  ;; (p-1)^2 mod p = 1
  (assert-= (p256:p256-sqr (1- +p+)) 1))

;;; Inversion
(deftest test-p256-inv
  "P-256 inversion: a * a^(-1) = 1"
  (let* ((a 42)
         (a-inv (p256:p256-inv a)))
    (assert-= (p256:p256-mul a a-inv) 1)))

(deftest test-p256-inv-large
  "P-256 inversion with large value"
  (let* ((a (- +p+ 2))
         (a-inv (p256:p256-inv a)))
    (assert-= (p256:p256-mul a a-inv) 1)))

;;; Byte encoding
(deftest test-p256-bytes-roundtrip
  "P-256 bytes round-trip"
  (let* ((val 123456789)
         (bytes (p256:p256-to-bytes val))
         (recovered (p256:p256-from-bytes bytes)))
    (assert-= recovered val)))

(deftest test-p256-bytes-zero
  "P-256 zero bytes"
  (let ((bytes (p256:p256-to-bytes 0)))
    (assert-= (length bytes) 32)
    (assert-true (every #'zerop bytes))))

(deftest test-p256-bytes-big-endian
  "P-256 bytes are big-endian"
  (let ((bytes (p256:p256-to-bytes 1)))
    ;; Byte 31 should be 1 (big-endian: MSB first)
    (assert-= (aref bytes 31) 1)
    (assert-= (aref bytes 0) 0)))

;;; Square root
(deftest test-p256-sqrt
  "P-256 square root"
  (let ((a (p256:p256-sqr 42)))
    (multiple-value-bind (root exists) (p256:p256-sqrt a)
      (assert-true exists)
      (assert-= (p256:p256-sqr root) a))))

(deftest test-p256-sqrt-non-residue
  "P-256 square root of non-residue"
  ;; 3 is a non-residue mod P-256
  (let ((a 3))
    (multiple-value-bind (root exists) (p256:p256-sqrt a)
      (declare (ignore root))
      (assert-not exists))))

;;; Field properties
(deftest test-p256-additive-identity
  "Zero is additive identity"
  (assert-= (p256:p256-add 42 0) 42))

(deftest test-p256-multiplicative-identity
  "One is multiplicative identity"
  (assert-= (p256:p256-mul 42 1) 42))

(deftest test-p256-distributive
  "Multiplication distributes over addition"
  (let ((a 7) (b 11) (c 13))
    (assert-= (p256:p256-mul a (p256:p256-add b c))
              (p256:p256-add (p256:p256-mul a b) (p256:p256-mul a c)))))

;;; Curve order
(deftest test-p256-curve-order
  "P-256 curve order is exported"
  (assert-true (plusp p256:+n+))
  (assert-true (< p256:+n+ +p+)))

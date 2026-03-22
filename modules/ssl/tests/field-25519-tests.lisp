;;;; Tests for GF(2^255-19) field arithmetic

(defpackage epsilon.ssl.field-25519-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:fe #:epsilon.ssl.field-25519))
  (:enter t))

(in-package :epsilon.ssl.field-25519-tests)

(defconstant +p+ fe:+p+)

;;; Basic round-trip
(deftest test-fe-roundtrip-zero
  "Field element zero round-trip"
  (assert-= (fe:fe-to-integer (fe:fe-zero)) 0))

(deftest test-fe-roundtrip-one
  "Field element one round-trip"
  (assert-= (fe:fe-to-integer (fe:fe-one)) 1))

(deftest test-fe-roundtrip-integer
  "Field element integer round-trip"
  (loop for n in '(0 1 2 42 1000000 123456789012345678901234567890)
        do (assert-= (fe:fe-to-integer (fe:fe-from-integer n)) (mod n +p+))))

(deftest test-fe-roundtrip-p-minus-1
  "Field element p-1 round-trip"
  (assert-= (fe:fe-to-integer (fe:fe-from-integer (1- +p+))) (1- +p+)))

;;; Addition
(deftest test-fe-add
  "Field element addition"
  (let ((a (fe:fe-from-integer 100))
        (b (fe:fe-from-integer 200)))
    (assert-= (fe:fe-to-integer (fe:fe-add a b)) 300)))

(deftest test-fe-add-wrap
  "Field element addition with wrap"
  (let ((a (fe:fe-from-integer (- +p+ 10)))
        (b (fe:fe-from-integer 20)))
    (assert-= (fe:fe-to-integer (fe:fe-add a b)) 10)))

;;; Subtraction
(deftest test-fe-sub
  "Field element subtraction"
  (let ((a (fe:fe-from-integer 300))
        (b (fe:fe-from-integer 200)))
    (assert-= (fe:fe-to-integer (fe:fe-sub a b)) 100)))

(deftest test-fe-sub-wrap
  "Field element subtraction with wrap"
  (let ((a (fe:fe-from-integer 10))
        (b (fe:fe-from-integer 20)))
    (assert-= (fe:fe-to-integer (fe:fe-sub a b)) (- +p+ 10))))

;;; Negation
(deftest test-fe-neg
  "Field element negation"
  (let ((a (fe:fe-from-integer 42)))
    (assert-= (fe:fe-to-integer (fe:fe-neg a)) (- +p+ 42))))

(deftest test-fe-neg-zero
  "Field element negation of zero"
  (assert-= (fe:fe-to-integer (fe:fe-neg (fe:fe-zero))) 0))

;;; Multiplication
(deftest test-fe-mul
  "Field element multiplication"
  (let ((a (fe:fe-from-integer 7))
        (b (fe:fe-from-integer 11)))
    (assert-= (fe:fe-to-integer (fe:fe-mul a b)) 77)))

(deftest test-fe-mul-large
  "Field element multiplication with large values"
  (let* ((a-int (- +p+ 1))
         (b-int 2)
         (a (fe:fe-from-integer a-int))
         (b (fe:fe-from-integer b-int)))
    (assert-= (fe:fe-to-integer (fe:fe-mul a b))
              (mod (* a-int b-int) +p+))))

(deftest test-fe-mul-self
  "Field element a*a = a^2"
  (let ((a (fe:fe-from-integer 123456789)))
    (assert-= (fe:fe-to-integer (fe:fe-mul a a))
              (fe:fe-to-integer (fe:fe-sqr a)))))

;;; Squaring
(deftest test-fe-sqr
  "Field element squaring"
  (let ((a (fe:fe-from-integer 100)))
    (assert-= (fe:fe-to-integer (fe:fe-sqr a)) 10000)))

(deftest test-fe-sqr-large
  "Field element squaring with large values"
  (let* ((a-int (- +p+ 1))
         (a (fe:fe-from-integer a-int)))
    ;; (p-1)^2 mod p = 1
    (assert-= (fe:fe-to-integer (fe:fe-sqr a)) 1)))

;;; Inversion
(deftest test-fe-inv
  "Field element inversion"
  (let* ((a-int 42)
         (a (fe:fe-from-integer a-int))
         (a-inv (fe:fe-inv a)))
    ;; a * a^(-1) = 1
    (assert-= (fe:fe-to-integer (fe:fe-mul a a-inv)) 1)))

(deftest test-fe-inv-large
  "Field element inversion with large value"
  (let* ((a-int (- +p+ 2))
         (a (fe:fe-from-integer a-int))
         (a-inv (fe:fe-inv a)))
    (assert-= (fe:fe-to-integer (fe:fe-mul a a-inv)) 1)))

;;; Byte encoding
(deftest test-fe-bytes-roundtrip
  "Field element bytes round-trip"
  (let ((a (fe:fe-from-integer 123456789)))
    (assert-= (fe:fe-to-integer (fe:fe-from-bytes (fe:fe-to-bytes a)))
              123456789)))

(deftest test-fe-bytes-zero
  "Field element zero bytes"
  (let ((bytes (fe:fe-to-bytes (fe:fe-zero))))
    (assert-= (length bytes) 32)
    (assert-true (every #'zerop bytes))))

;;; Field properties
(deftest test-fe-additive-identity
  "Zero is additive identity"
  (let ((a (fe:fe-from-integer 42)))
    (assert-true (fe:fe-equal (fe:fe-add a (fe:fe-zero)) a))))

(deftest test-fe-multiplicative-identity
  "One is multiplicative identity"
  (let ((a (fe:fe-from-integer 42)))
    (assert-true (fe:fe-equal (fe:fe-mul a (fe:fe-one)) a))))

(deftest test-fe-distributive
  "Multiplication distributes over addition"
  (let ((a (fe:fe-from-integer 7))
        (b (fe:fe-from-integer 11))
        (c (fe:fe-from-integer 13)))
    ;; a*(b+c) = a*b + a*c
    (let ((lhs (fe:fe-mul a (fe:fe-add b c)))
          (rhs (fe:fe-add (fe:fe-mul a b) (fe:fe-mul a c))))
      (assert-true (fe:fe-equal lhs rhs)))))

;;; Square root
(deftest test-fe-sqrt
  "Field element square root"
  (let* ((a (fe:fe-from-integer 4)))
    (multiple-value-bind (root exists) (fe:fe-sqrt a)
      (assert-true exists)
      (assert-= (fe:fe-to-integer (fe:fe-sqr root)) 4))))

(deftest test-fe-sqrt-non-residue
  "Field element square root of non-residue"
  ;; 2 is a non-residue mod p (Euler criterion: 2^((p-1)/2) != 1)
  (let ((a (fe:fe-from-integer 2)))
    (multiple-value-bind (root exists) (fe:fe-sqrt a)
      (declare (ignore root))
      (assert-not exists))))

;;;; Tests for modular arithmetic

(defpackage epsilon.crypto.modular-tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.crypto.modular mod-arith)))

(in-package :epsilon.crypto.modular-tests)

;;; Basic mod arithmetic
(deftest test-mod-add
  "mod-add"
  (assert-= (mod-arith:mod-add 7 5 11) 1)
  (assert-= (mod-arith:mod-add 0 0 11) 0)
  (assert-= (mod-arith:mod-add 10 10 11) 9))

(deftest test-mod-sub
  "mod-sub"
  (assert-= (mod-arith:mod-sub 3 5 11) 9)
  (assert-= (mod-arith:mod-sub 0 1 11) 10)
  (assert-= (mod-arith:mod-sub 5 5 11) 0))

(deftest test-mod-neg
  "mod-neg"
  (assert-= (mod-arith:mod-neg 3 11) 8)
  (assert-= (mod-arith:mod-neg 0 11) 0))

(deftest test-mod-mul
  "mod-mul"
  (assert-= (mod-arith:mod-mul 3 4 11) 1)
  (assert-= (mod-arith:mod-mul 5 7 11) 2))

(deftest test-mod-sqr
  "mod-sqr"
  (assert-= (mod-arith:mod-sqr 5 11) 3)
  (assert-= (mod-arith:mod-sqr 0 11) 0))

;;; Extended GCD
(deftest test-extended-gcd
  "extended-gcd"
  (multiple-value-bind (g x y) (mod-arith:extended-gcd 35 15)
    (assert-= g 5)
    (assert-= (+ (* 35 x) (* 15 y)) 5)))

(deftest test-extended-gcd-coprime
  "extended-gcd coprime"
  (multiple-value-bind (g x y) (mod-arith:extended-gcd 7 11)
    (assert-= g 1)
    (assert-= (+ (* 7 x) (* 11 y)) 1)))

;;; Modular inverse
(deftest test-mod-inv
  "mod-inv"
  (assert-= (mod-arith:mod-inv 3 11) 4)   ; 3*4 = 12 = 1 mod 11
  (assert-= (mod-arith:mod-inv 7 11) 8)   ; 7*8 = 56 = 1 mod 11
  ;; Verify: a * a^(-1) = 1 mod p
  (let ((p 104729))
    (loop for a in '(1 2 3 100 50000 104728)
          do (assert-= (mod-arith:mod-mul a (mod-arith:mod-inv a p) p) 1))))

;;; Modular exponentiation
(deftest test-mod-expt
  "mod-expt"
  (assert-= (mod-arith:mod-expt 2 10 1000) 24)
  (assert-= (mod-arith:mod-expt 3 0 11) 1)
  (assert-= (mod-arith:mod-expt 3 1 11) 3)
  (assert-= (mod-arith:mod-expt 2 10 1024) 0))

;;; Fermat's little theorem: a^(p-1) = 1 mod p for prime p
(deftest test-fermat-little-theorem
  "Fermat's little theorem"
  (let ((p 104729))
    (loop for a in '(2 3 5 7 11 100 50000)
          do (assert-= (mod-arith:mod-expt a (1- p) p) 1))))

;;; Large numbers (RSA-like)
(deftest test-mod-expt-large
  "mod-expt with large numbers"
  (let* ((p #xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA237327FFFFFFFFFFFFFFFF)
         (base 65537)
         (exp (1- p))
         (result (mod-arith:mod-expt base exp p)))
    (assert-= result 1)))

;;; mod-div
(deftest test-mod-div
  "mod-div"
  (assert-= (mod-arith:mod-div 6 3 11) 2)
  (assert-= (mod-arith:mod-div 1 7 11) 8))

;;; ---------------------------------------------------------------------------
;;; Montgomery multiplication
;;; ---------------------------------------------------------------------------

(deftest test-mont-context
  "Montgomery context construction"
  (let ((ctx (mod-arith:make-mont-context 11)))
    (assert-= (mod-arith:mont-context-n ctx) 11)
    (assert-= (mod-arith:mont-context-r-bits ctx) 4) ; integer-length(11) = 4
    (assert-true (> (mod-arith:mont-context-r-bits ctx) 0))))

(deftest test-mont-roundtrip
  "Convert to/from Montgomery form"
  (let ((ctx (mod-arith:make-mont-context 11)))
    (loop for a in '(0 1 2 5 10)
          do (assert-= (mod-arith:from-montgomery ctx (mod-arith:to-montgomery ctx a)) a))))

(deftest test-mont-mul-small
  "Montgomery multiplication with small modulus"
  (let ((ctx (mod-arith:make-mont-context 11)))
    ;; 3 * 4 mod 11 = 1
    (let ((a-m (mod-arith:to-montgomery ctx 3))
          (b-m (mod-arith:to-montgomery ctx 4)))
      (assert-= (mod-arith:from-montgomery ctx (mod-arith:mont-mul ctx a-m b-m)) 1))
    ;; 5 * 7 mod 11 = 2
    (let ((a-m (mod-arith:to-montgomery ctx 5))
          (b-m (mod-arith:to-montgomery ctx 7)))
      (assert-= (mod-arith:from-montgomery ctx (mod-arith:mont-mul ctx a-m b-m)) 2))))

(deftest test-mont-sqr-small
  "Montgomery squaring"
  (let ((ctx (mod-arith:make-mont-context 11)))
    ;; 5^2 mod 11 = 3
    (let ((a-m (mod-arith:to-montgomery ctx 5)))
      (assert-= (mod-arith:from-montgomery ctx (mod-arith:mont-sqr ctx a-m)) 3))))

(deftest test-mont-expt-small
  "Montgomery exponentiation with small values"
  (assert-= (mod-arith:mont-expt 2 10 1000) 24)
  (assert-= (mod-arith:mont-expt 3 0 11) 1)
  (assert-= (mod-arith:mont-expt 3 1 11) 3)
  (assert-= (mod-arith:mont-expt 2 10 1024) 0))

(deftest test-mont-expt-vs-mod-expt
  "Montgomery exponentiation matches naive mod-expt"
  (let ((p 104729))
    ;; Fermat's little theorem: a^(p-1) = 1 mod p
    (loop for a in '(2 3 5 7 11 100 50000)
          do (assert-= (mod-arith:mont-expt a (1- p) p) 1)
             (assert-= (mod-arith:mont-expt a (1- p) p)
                        (mod-arith:mod-expt a (1- p) p)))))

(deftest test-mont-expt-large
  "Montgomery exponentiation with RSA-sized numbers"
  ;; 2048-bit safe prime
  (let* ((p #xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA237327FFFFFFFFFFFFFFFF)
         (base 65537))
    ;; Fermat's little theorem
    (assert-= (mod-arith:mont-expt base (1- p) p) 1)
    ;; Must match naive implementation
    (assert-= (mod-arith:mont-expt base (1- p) p)
              (mod-arith:mod-expt base (1- p) p))))

(deftest test-mont-mul-medium-primes
  "Montgomery multiplication with medium-sized primes"
  ;; 128-bit prime
  (let* ((p (+ (ash 1 127) 39)) ;; well-known 128-bit prime candidate
         (ctx (mod-arith:make-mont-context p)))
    (loop for (a b) in '((12345 67890) (1 1) (0 999))
          for expected = (mod (* a b) p)
          do (let ((am (mod-arith:to-montgomery ctx a))
                   (bm (mod-arith:to-montgomery ctx b)))
               (assert-= (mod-arith:from-montgomery ctx (mod-arith:mont-mul ctx am bm))
                          expected)))))

;;; ---------------------------------------------------------------------------
;;; Constant-time modular exponentiation (mod-expt-ct)
;;; ---------------------------------------------------------------------------

(deftest test-mod-expt-ct-trivial-cases
  "mod-expt-ct agrees with mod-expt on simple inputs."
  (let ((m 23))
    ;; base^0 = 1
    (assert-= 1 (mod-arith:mod-expt-ct 5 0 m))
    ;; base^1 = base
    (assert-= 5 (mod-arith:mod-expt-ct 5 1 m))
    ;; small values
    (assert-= (mod-arith:mod-expt 7 12 m)
              (mod-arith:mod-expt-ct 7 12 m))))

(deftest test-mod-expt-ct-matches-mod-expt-fermat
  "Fermat's little theorem: a^(p-1) mod p = 1 for prime p, gcd(a,p)=1."
  (let ((p 65537)) ; well-known Fermat prime
    (dolist (a '(2 3 5 7 11 31337))
      (assert-= 1 (mod-arith:mod-expt-ct a (1- p) p)))))

(deftest test-mod-expt-ct-large-modulus
  "Exercise mod-expt-ct on a 256-bit prime."
  ;; secp256k1 base-field prime: 2^256 - 2^32 - 977
  (let* ((p (- (expt 2 256) (expt 2 32) 977))
         (a 12345678901234567890)
         (e (- p 2)))
    (assert-= (mod-arith:mod-expt a e p)
              (mod-arith:mod-expt-ct a e p))))

(deftest test-mod-expt-ct-iteration-count-is-fixed
  "The :BITS argument controls iteration count: passing a smaller
   value than the bit length of EXPONENT produces a different
   (incorrect) result, demonstrating that the loop ran fewer
   iterations rather than terminating early on the secret bits."
  (let ((p 23))
    ;; 5^7 mod 23 = 17. With BITS=3 (correct, since 7 fits in 3 bits): 17.
    (assert-= 17 (mod-arith:mod-expt-ct 5 7 p :bits 3))
    ;; With BITS=2 the most significant bit of 7 is dropped: we
    ;; should get 5^(7 mod 4) mod 23 = 5^3 mod 23 = 125 mod 23 = 10.
    (assert-= 10 (mod-arith:mod-expt-ct 5 7 p :bits 2))
    ;; With BITS=1: 5^(7 mod 2) = 5^1 = 5.
    (assert-= 5 (mod-arith:mod-expt-ct 5 7 p :bits 1))))

(deftest test-mod-expt-ct-rejects-even-modulus
  "Even moduli are rejected (Montgomery / odd-prime context only)."
  (assert-condition (error)
    (mod-arith:mod-expt-ct 3 5 8)))

(deftest test-mod-expt-ct-rsa-style-roundtrip
  "RSA-style round trip: with c = m^e mod n, m^d mod n recovers m."
  (let* ((p 12347)  ; small prime
         (q 12373)  ; small prime
         (n (* p q))
         (phi (* (1- p) (1- q)))
         (e 65537)
         (d (mod-arith:mod-inv e phi)))
    (loop for m in '(2 1234 5678 999999)
          for c = (mod-arith:mod-expt m e n)
          do (assert-= m (mod-arith:mod-expt-ct c d n)))))

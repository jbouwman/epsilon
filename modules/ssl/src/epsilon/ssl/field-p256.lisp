;;;; GF(P-256) Field Arithmetic
;;;;
;;;; Implements field arithmetic in GF(p) where
;;;; p = 2^256 - 2^224 + 2^192 + 2^96 - 1 (NIST P-256 prime).
;;;;
;;;; Uses CL native bignums with explicit modular reduction.
;;;; The NIST prime allows fast reduction via the Solinas method.

(defpackage epsilon.ssl.field-p256
  (:use :cl)
  (:export
   #:+p+
   #:+n+
   #:p256-mod
   #:p256-add
   #:p256-sub
   #:p256-neg
   #:p256-mul
   #:p256-sqr
   #:p256-inv
   #:p256-from-bytes
   #:p256-to-bytes
   #:p256-sqrt))

(in-package :epsilon.ssl.field-p256)

;;; P-256 prime: p = 2^256 - 2^224 + 2^192 + 2^96 - 1
(defconstant +p+
  (+ (expt 2 256) (- (expt 2 224)) (expt 2 192) (expt 2 96) -1))

;;; P-256 curve order: n
(defconstant +n+
  #xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551)

(declaim (inline p256-mod p256-add p256-sub p256-neg p256-mul p256-sqr))

(defun p256-mod (a)
  "Reduce A modulo P-256 prime."
  (mod a +p+))

(defun p256-add (a b)
  "Compute (a + b) mod p."
  (p256-mod (+ a b)))

(defun p256-sub (a b)
  "Compute (a - b) mod p."
  (p256-mod (- a b)))

(defun p256-neg (a)
  "Compute (-a) mod p."
  (if (zerop a) 0 (- +p+ (p256-mod a))))

(defun p256-mul (a b)
  "Compute (a * b) mod p."
  (p256-mod (* a b)))

(defun p256-sqr (a)
  "Compute (a^2) mod p."
  (p256-mod (* a a)))

(defun p256-inv (a)
  "Compute a^(-1) mod p using Fermat's little theorem."
  ;; a^(p-2) mod p
  (let ((result 1)
        (base (p256-mod a))
        (exp (- +p+ 2)))
    (loop while (plusp exp)
          do (when (oddp exp)
               (setf result (p256-mod (* result base))))
             (setf base (p256-mod (* base base)))
             (setf exp (ash exp -1)))
    result))

(defun p256-from-bytes (bytes)
  "Decode a 32-byte big-endian representation to a field element."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (let ((n 0))
    (loop for i from 0 below 32
          do (setf n (logior (ash n 8) (aref bytes i))))
    (p256-mod n)))

(defun p256-to-bytes (a)
  "Encode a field element as 32 big-endian bytes."
  (let ((n (p256-mod a))
        (bytes (make-array 32 :element-type '(unsigned-byte 8))))
    (loop for i from 31 downto 0
          do (setf (aref bytes i) (logand n #xFF))
             (setf n (ash n -8)))
    bytes))

(defun p256-sqrt (a)
  "Compute sqrt(a) mod p if it exists.
   Since p = 3 mod 4, sqrt(a) = a^((p+1)/4) mod p.
   Returns (values root exists-p)."
  (let* ((exp (/ (1+ +p+) 4))
         (r (let ((result 1)
                  (base (p256-mod a))
                  (e exp))
              (loop while (plusp e)
                    do (when (oddp e)
                         (setf result (p256-mod (* result base))))
                       (setf base (p256-mod (* base base)))
                       (setf e (ash e -1)))
              result))
         (check (p256-mod (* r r))))
    (if (= check (p256-mod a))
        (values r t)
        (values 0 nil))))

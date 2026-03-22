;;;; GF(2^255-19) simple field element arithmetic
;;;;
;;;; Shared by curve25519.lisp and ed25519.lisp for scalar mod-p operations.
;;;; The limbed representation in field-25519.lisp is a separate concern.

(defpackage epsilon.ssl.fe-25519
  (:use :cl)
  (:export
   #:+p-25519+
   #:fe-add #:fe-sub #:fe-mul #:fe-sqr #:fe-inv))

(in-package :epsilon.ssl.fe-25519)

(defconstant +p-25519+ (- (expt 2 255) 19))

(declaim (inline fe-add fe-sub fe-mul fe-sqr))

(defun fe-add (a b)
  (mod (+ a b) +p-25519+))

(defun fe-sub (a b)
  (mod (- a b) +p-25519+))

(defun fe-mul (a b)
  (mod (* a b) +p-25519+))

(defun fe-sqr (a)
  (mod (* a a) +p-25519+))

(defun fe-inv (a)
  "Compute a^(-1) mod p via Fermat's little theorem."
  (let ((result 1)
        (base (mod a +p-25519+))
        (exp (- +p-25519+ 2)))
    (loop while (plusp exp)
          do (when (oddp exp)
               (setf result (mod (* result base) +p-25519+)))
             (setf base (mod (* base base) +p-25519+))
             (setf exp (ash exp -1)))
    result))

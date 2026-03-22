;;;; P-256 (secp256r1) Elliptic Curve Arithmetic
;;;;
;;;; Implements point arithmetic on the NIST P-256 curve:
;;;;   y^2 = x^3 - 3x + b  over GF(p)
;;;; where p = 2^256 - 2^224 + 2^192 + 2^96 - 1.
;;;;
;;;; Uses Jacobian coordinates (X : Y : Z) where x = X/Z^2, y = Y/Z^3.
;;;; This avoids field inversions during point operations.

(defpackage epsilon.ssl.ec-p256
  (:use :cl)
  (:local-nicknames
   (#:field #:epsilon.ssl.field-p256))
  (:export
   ;; Point operations
   #:p256-point
   #:p256-point-x #:p256-point-y #:p256-point-z
   #:p256-neutral
   #:p256-base-point
   #:p256-point-add
   #:p256-point-double
   #:p256-scalar-mul
   #:p256-point-equal
   #:p256-on-curve-p
   #:p256-point-negate
   ;; Encoding/decoding
   #:p256-point-encode-uncompressed
   #:p256-point-decode
   ;; Constants
   #:+p+ #:+n+ #:+b+ #:+gx+ #:+gy+))

(in-package :epsilon.ssl.ec-p256)

;;; P-256 curve parameters
(defconstant +p+ field:+p+)
(defconstant +n+ field:+n+)

;;; Curve equation: y^2 = x^3 + ax + b, with a = -3
(defconstant +a+ (mod -3 +p+))

(defconstant +b+
  #x5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b)

;;; Generator point coordinates
(defconstant +gx+
  #x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296)

(defconstant +gy+
  #x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5)

;;; ---------------------------------------------------------------------------
;;; Field arithmetic (inline)
;;; ---------------------------------------------------------------------------

(declaim (inline fe-add fe-sub fe-mul fe-sqr fe-neg fe-dbl))

(defun fe-add (a b) (mod (+ a b) +p+))
(defun fe-sub (a b) (mod (- a b) +p+))
(defun fe-mul (a b) (mod (* a b) +p+))
(defun fe-sqr (a) (mod (* a a) +p+))
(defun fe-neg (a) (if (zerop a) 0 (- +p+ (mod a +p+))))
(defun fe-dbl (a) (mod (+ a a) +p+))

(defun fe-inv (a)
  (let ((result 1) (base (mod a +p+)) (exp (- +p+ 2)))
    (loop while (plusp exp)
          do (when (oddp exp)
               (setf result (mod (* result base) +p+)))
             (setf base (mod (* base base) +p+))
             (setf exp (ash exp -1)))
    result))

;;; ---------------------------------------------------------------------------
;;; Jacobian point representation (X : Y : Z) where x=X/Z^2, y=Y/Z^3
;;; ---------------------------------------------------------------------------

(defstruct (p256-point (:constructor %make-p256-point (x y z)))
  (x 0 :type integer)
  (y 0 :type integer)
  (z 0 :type integer))

(defun make-p256-point (x y z)
  (%make-p256-point x y z))

(defun p256-neutral ()
  "Return the point at infinity: (0 : 1 : 0) in Jacobian coordinates."
  (make-p256-point 0 1 0))

(defun p256-affine-point (x y)
  "Create a point from affine coordinates."
  (make-p256-point (mod x +p+) (mod y +p+) 1))

(defun p256-base-point ()
  "Return the P-256 generator point G."
  (p256-affine-point +gx+ +gy+))

(defun p256-point-is-neutral (p)
  "Test if P is the point at infinity."
  (zerop (p256-point-z p)))

;;; ---------------------------------------------------------------------------
;;; Point arithmetic in Jacobian coordinates
;;; Formulas from "Guide to Elliptic Curve Cryptography" (Hankerson et al.)
;;; ---------------------------------------------------------------------------

(defun p256-point-double (p1)
  "Double a point in Jacobian coordinates.
   Uses the formula for a=-3 (P-256 specific optimization)."
  (when (p256-point-is-neutral p1)
    (return-from p256-point-double (p256-neutral)))
  (let* ((x1 (p256-point-x p1))
         (y1 (p256-point-y p1))
         (z1 (p256-point-z p1))
         ;; For a = -3: M = 3*(X1-Z1^2)*(X1+Z1^2)
         (z1-sq (fe-sqr z1))
         (m (fe-mul 3 (fe-mul (fe-sub x1 z1-sq) (fe-add x1 z1-sq))))
         ;; S = 4*X1*Y1^2
         (y1-sq (fe-sqr y1))
         (s (fe-mul 4 (fe-mul x1 y1-sq)))
         ;; X3 = M^2 - 2*S
         (x3 (fe-sub (fe-sqr m) (fe-dbl s)))
         ;; Y3 = M*(S - X3) - 8*Y1^4
         (y3 (fe-sub (fe-mul m (fe-sub s x3))
                      (fe-mul 8 (fe-sqr y1-sq))))
         ;; Z3 = 2*Y1*Z1
         (z3 (fe-dbl (fe-mul y1 z1))))
    (make-p256-point x3 y3 z3)))

(defun p256-point-add (p1 p2)
  "Add two points in Jacobian coordinates."
  (when (p256-point-is-neutral p1)
    (return-from p256-point-add p2))
  (when (p256-point-is-neutral p2)
    (return-from p256-point-add p1))
  (let* ((x1 (p256-point-x p1)) (y1 (p256-point-y p1)) (z1 (p256-point-z p1))
         (x2 (p256-point-x p2)) (y2 (p256-point-y p2)) (z2 (p256-point-z p2))
         ;; U1 = X1*Z2^2, U2 = X2*Z1^2
         (z1-sq (fe-sqr z1))
         (z2-sq (fe-sqr z2))
         (u1 (fe-mul x1 z2-sq))
         (u2 (fe-mul x2 z1-sq))
         ;; S1 = Y1*Z2^3, S2 = Y2*Z1^3
         (s1 (fe-mul y1 (fe-mul z2 z2-sq)))
         (s2 (fe-mul y2 (fe-mul z1 z1-sq)))
         ;; H = U2 - U1
         (h (fe-sub u2 u1))
         ;; R = S2 - S1
         (r (fe-sub s2 s1)))
    ;; Check for special cases
    (when (and (zerop h) (zerop r))
      ;; P1 = P2, use doubling
      (return-from p256-point-add (p256-point-double p1)))
    (when (and (zerop h) (not (zerop r)))
      ;; P1 = -P2, result is infinity
      (return-from p256-point-add (p256-neutral)))
    (let* (;; H^2, H^3
           (h-sq (fe-sqr h))
           (h-cu (fe-mul h h-sq))
           ;; X3 = R^2 - H^3 - 2*U1*H^2
           (x3 (fe-sub (fe-sub (fe-sqr r) h-cu) (fe-dbl (fe-mul u1 h-sq))))
           ;; Y3 = R*(U1*H^2 - X3) - S1*H^3
           (y3 (fe-sub (fe-mul r (fe-sub (fe-mul u1 h-sq) x3))
                        (fe-mul s1 h-cu)))
           ;; Z3 = H*Z1*Z2
           (z3 (fe-mul h (fe-mul z1 z2))))
      (make-p256-point x3 y3 z3))))

(defun p256-point-negate (p1)
  "Negate a point: (X:Y:Z) -> (X:-Y:Z)."
  (make-p256-point (p256-point-x p1)
                   (fe-neg (p256-point-y p1))
                   (p256-point-z p1)))

(defun p256-scalar-mul (n point)
  "Compute n * point using double-and-add."
  (let ((result (p256-neutral))
        (q point)
        (k (mod n +n+)))
    (loop while (plusp k)
          do (when (oddp k)
               (setf result (p256-point-add result q)))
             (setf q (p256-point-double q))
             (setf k (ash k -1)))
    result))

(defun p256-point-equal (p1 p2)
  "Test if two Jacobian points are equal.
   (X1:Y1:Z1) = (X2:Y2:Z2) iff X1*Z2^2=X2*Z1^2 and Y1*Z2^3=Y2*Z1^3."
  (cond
    ((and (p256-point-is-neutral p1) (p256-point-is-neutral p2)) t)
    ((or (p256-point-is-neutral p1) (p256-point-is-neutral p2)) nil)
    (t (let ((z1-sq (fe-sqr (p256-point-z p1)))
             (z2-sq (fe-sqr (p256-point-z p2))))
         (and (= (fe-mul (p256-point-x p1) z2-sq)
                 (fe-mul (p256-point-x p2) z1-sq))
              (= (fe-mul (p256-point-y p1) (fe-mul (p256-point-z p2) z2-sq))
                 (fe-mul (p256-point-y p2) (fe-mul (p256-point-z p1) z1-sq))))))))

(defun p256-on-curve-p (point)
  "Check if a point lies on P-256: y^2 = x^3 - 3x + b.
   Works with Jacobian coordinates."
  (when (p256-point-is-neutral point)
    (return-from p256-on-curve-p t))
  (let* ((x (p256-point-x point))
         (y (p256-point-y point))
         (z (p256-point-z point))
         (z2 (fe-sqr z))
         (z4 (fe-sqr z2))
         (z6 (fe-mul z2 z4))
         ;; LHS = Y^2
         (lhs (fe-sqr y))
         ;; RHS = X^3 + a*X*Z^4 + b*Z^6
         (rhs (fe-add (fe-add (fe-mul x (fe-sqr x))
                               (fe-mul +a+ (fe-mul x z4)))
                       (fe-mul +b+ z6))))
    (= lhs rhs)))

;;; ---------------------------------------------------------------------------
;;; Point encoding/decoding
;;; ---------------------------------------------------------------------------

(defun p256-point-to-affine (point)
  "Convert Jacobian point to affine (x, y). Returns (values x y)."
  (when (p256-point-is-neutral point)
    (return-from p256-point-to-affine (values 0 0)))
  (let* ((z-inv (fe-inv (p256-point-z point)))
         (z-inv-sq (fe-sqr z-inv))
         (x (fe-mul (p256-point-x point) z-inv-sq))
         (y (fe-mul (p256-point-y point) (fe-mul z-inv z-inv-sq))))
    (values x y)))

(defun p256-point-encode-uncompressed (point)
  "Encode a point as uncompressed SEC1 format: 04 || x || y (65 bytes)."
  (multiple-value-bind (x y) (p256-point-to-affine point)
    (let ((bytes (make-array 65 :element-type '(unsigned-byte 8))))
      (setf (aref bytes 0) #x04)
      ;; x in big-endian (bytes 1-32)
      (let ((xn x))
        (loop for i from 32 downto 1
              do (setf (aref bytes i) (logand xn #xFF))
                 (setf xn (ash xn -8))))
      ;; y in big-endian (bytes 33-64)
      (let ((yn y))
        (loop for i from 64 downto 33
              do (setf (aref bytes i) (logand yn #xFF))
                 (setf yn (ash yn -8))))
      bytes)))

(defun p256-point-decode (bytes)
  "Decode an SEC1 encoded point. Supports uncompressed (04) format.
   Returns the point, or NIL if invalid."
  (cond
    ;; Uncompressed: 04 || x || y
    ((and (= (length bytes) 65) (= (aref bytes 0) #x04))
     (let ((x 0) (y 0))
       (loop for i from 1 to 32
             do (setf x (logior (ash x 8) (aref bytes i))))
       (loop for i from 33 to 64
             do (setf y (logior (ash y 8) (aref bytes i))))
       (let ((pt (p256-affine-point x y)))
         (if (p256-on-curve-p pt) pt nil))))
    (t nil)))

;;;; Ed25519 Twisted Edwards Curve Arithmetic
;;;;
;;;; Implements point arithmetic on the twisted Edwards curve:
;;;;   -x^2 + y^2 = 1 + d*x^2*y^2
;;;; where d = -121665/121666 mod p, p = 2^255 - 19.
;;;;
;;;; Uses extended coordinates (X : Y : Z : T) where x = X/Z, y = Y/Z, T = XY/Z.
;;;; This avoids field inversions during point operations.
;;;;
;;;; References:
;;;; - RFC 8032: Edwards-Curve Digital Signature Algorithm (EdDSA)
;;;; - Hisil, Wong, Carter, Dawson: "Twisted Edwards Curves Revisited" (2008)

(defpackage epsilon.ssl.ed25519
  (:use :cl)
  (:import-from #:epsilon.ssl.fe-25519
   #:+p-25519+ #:fe-add #:fe-sub #:fe-mul #:fe-sqr #:fe-inv)
  (:export
   ;; Point operations
   #:ed-point
   #:ed-point-x #:ed-point-y #:ed-point-z #:ed-point-t
   #:ed-neutral
   #:ed-base-point
   #:ed-point-add
   #:ed-point-double
   #:ed-scalar-mul
   #:ed-point-equal
   #:ed-on-curve-p
   #:ed-point-negate
   ;; Encoding/decoding
   #:ed-point-encode
   #:ed-point-decode
   ;; Constants
   #:+p+ #:+l+ #:+d+))

(in-package :epsilon.ssl.ed25519)

;;; The prime p = 2^255 - 19
(defconstant +p+ +p-25519+)

;;; The group order L (a prime)
(defconstant +l+ (+ (expt 2 252) 27742317777372353535851937790883648493))

;;; Curve parameter d = -121665/121666 mod p
;;; Computed as: -121665 * (121666^(p-2)) mod p
(defconstant +d+
  (let ((p +p+))
    (mod (* -121665
            (let ((result 1) (base (mod 121666 p)) (exp (- p 2)))
              (loop while (plusp exp)
                    do (when (oddp exp)
                         (setf result (mod (* result base) p)))
                       (setf base (mod (* base base) p))
                       (setf exp (ash exp -1)))
              result))
         p)))

;;; 2*d (precomputed for point addition)
(defconstant +2d+ (mod (* 2 +d+) +p+))

;;; sqrt(-1) mod p = 2^((p-1)/4) mod p
(defconstant +sqrt-neg1+
  (let ((p +p+))
    (let ((result 1) (base 2) (exp (/ (1- p) 4)))
      (loop while (plusp exp)
            do (when (oddp exp)
                 (setf result (mod (* result base) p)))
               (setf base (mod (* base base) p))
               (setf exp (ash exp -1)))
      result)))

;;; ---------------------------------------------------------------------------
;;; Field arithmetic (fe-add/sub/mul/sqr/inv imported from fe-25519)
;;; ---------------------------------------------------------------------------

(declaim (inline fe-neg))

(defun fe-neg (a) (if (zerop a) 0 (- +p+ (mod a +p+))))

(defun fe-pow (base exp)
  "Compute base^exp mod p."
  (let ((result 1) (b (mod base +p+)))
    (loop while (plusp exp)
          do (when (oddp exp)
               (setf result (mod (* result b) +p+)))
             (setf b (mod (* b b) +p+))
             (setf exp (ash exp -1)))
    result))

;;; ---------------------------------------------------------------------------
;;; Extended point representation (X : Y : Z : T) where T = XY/Z
;;; ---------------------------------------------------------------------------

(defstruct (ed-point (:constructor %make-ed-point (x y z tt)))
  (x 0 :type integer)
  (y 0 :type integer)
  (z 0 :type integer)
  (tt 0 :type integer))  ; 'tt' to avoid CL symbol T

(defun make-ed-point (x y z tt)
  (%make-ed-point x y z tt))

(defun ed-neutral ()
  "Return the neutral element (identity point): (0 : 1 : 1 : 0)."
  (make-ed-point 0 1 1 0))

(defun ed-affine-point (x y)
  "Create a point from affine coordinates (x, y)."
  (make-ed-point (mod x +p+) (mod y +p+) 1 (fe-mul x y)))

;;; Base point (from RFC 8032 Section 5.1)
;;; y = 4/5 mod p, x is the positive square root
(defun compute-base-point ()
  (let* ((y (fe-mul 4 (fe-inv 5)))
         (y2 (fe-sqr y))
         ;; x^2 = (y^2 - 1) / (d*y^2 + 1)
         (x2 (fe-mul (fe-sub y2 1) (fe-inv (fe-add (fe-mul +d+ y2) 1))))
         ;; x = x2^((p+3)/8)
         (x (fe-pow x2 (/ (+ +p+ 3) 8))))
    ;; Check: if x^2 != x2, multiply by sqrt(-1)
    (unless (= (fe-sqr x) x2)
      (setf x (fe-mul x +sqrt-neg1+)))
    ;; Ensure x is even (positive in RFC 8032 convention: x mod 2 = 0 for the base point)
    (when (oddp x)
      (setf x (fe-neg x)))
    (ed-affine-point x y)))

(defvar *base-point* nil)

(defun ed-base-point ()
  "Return the Ed25519 base point B."
  (or *base-point*
      (setf *base-point* (compute-base-point))))

;;; ---------------------------------------------------------------------------
;;; Point arithmetic using unified addition formulas
;;; ---------------------------------------------------------------------------

(defun ed-point-add (p1 p2)
  "Add two points using the extended coordinates addition formula.
   Uses the unified formula from Hisil et al. (2008)."
  (let* ((x1 (ed-point-x p1)) (y1 (ed-point-y p1))
         (z1 (ed-point-z p1)) (t1 (ed-point-tt p1))
         (x2 (ed-point-x p2)) (y2 (ed-point-y p2))
         (z2 (ed-point-z p2)) (t2 (ed-point-tt p2))
         ;; A = (Y1-X1)*(Y2-X2)
         (a (fe-mul (fe-sub y1 x1) (fe-sub y2 x2)))
         ;; B = (Y1+X1)*(Y2+X2)
         (b (fe-mul (fe-add y1 x1) (fe-add y2 x2)))
         ;; C = T1*2*d*T2
         (c (fe-mul (fe-mul t1 +2d+) t2))
         ;; D = Z1*2*Z2
         (dd (fe-mul (fe-mul z1 2) z2))
         ;; E = B-A, F = D-C, G = D+C, H = B+A
         (e (fe-sub b a))
         (f (fe-sub dd c))
         (g (fe-add dd c))
         (h (fe-add b a)))
    ;; X3 = E*F, Y3 = G*H, T3 = E*H, Z3 = F*G
    (make-ed-point (fe-mul e f) (fe-mul g h) (fe-mul f g) (fe-mul e h))))

(defun ed-point-double (p1)
  "Double a point using the dedicated doubling formula.
   More efficient than generic addition."
  (let* ((x1 (ed-point-x p1)) (y1 (ed-point-y p1)) (z1 (ed-point-z p1))
         ;; A = X1^2
         (a (fe-sqr x1))
         ;; B = Y1^2
         (b (fe-sqr y1))
         ;; C = 2*Z1^2
         (c (fe-mul 2 (fe-sqr z1)))
         ;; D = -A  (since a = -1 for Ed25519)
         (dd (fe-neg a))
         ;; E = (X1+Y1)^2 - A - B
         (e (fe-sub (fe-sub (fe-sqr (fe-add x1 y1)) a) b))
         ;; G = D + B
         (g (fe-add dd b))
         ;; F = G - C
         (f (fe-sub g c))
         ;; H = D - B
         (h (fe-sub dd b)))
    ;; X3 = E*F, Y3 = G*H, T3 = E*H, Z3 = F*G
    (make-ed-point (fe-mul e f) (fe-mul g h) (fe-mul f g) (fe-mul e h))))

(defun ed-point-negate (p1)
  "Negate a point: -(X:Y:Z:T) = (-X:Y:Z:-T)."
  (make-ed-point (fe-neg (ed-point-x p1))
                 (ed-point-y p1)
                 (ed-point-z p1)
                 (fe-neg (ed-point-tt p1))))

(defun ed-scalar-mul (n point)
  "Compute n * point using double-and-add."
  (let ((result (ed-neutral))
        (q point)
        (k (mod n +l+)))
    (loop while (plusp k)
          do (when (oddp k)
               (setf result (ed-point-add result q)))
             (setf q (ed-point-double q))
             (setf k (ash k -1)))
    result))

(defun ed-point-equal (p1 p2)
  "Test if two points are equal in projective coordinates.
   (X1:Y1:Z1) = (X2:Y2:Z2) iff X1*Z2 = X2*Z1 and Y1*Z2 = Y2*Z1."
  (and (= (fe-mul (ed-point-x p1) (ed-point-z p2))
          (fe-mul (ed-point-x p2) (ed-point-z p1)))
       (= (fe-mul (ed-point-y p1) (ed-point-z p2))
          (fe-mul (ed-point-y p2) (ed-point-z p1)))))

(defun ed-on-curve-p (point)
  "Check if a point lies on the Ed25519 curve: -x^2 + y^2 = 1 + d*x^2*y^2.
   Works with projective coordinates."
  (let* ((x (ed-point-x point)) (y (ed-point-y point)) (z (ed-point-z point))
         (z2 (fe-sqr z))
         (x2 (fe-sqr x))
         (y2 (fe-sqr y))
         ;; LHS = -X^2*Z^2 + Y^2*Z^2 = Z^2*(Y^2 - X^2) ... no,
         ;; -X^2 + Y^2 = 1 + d*X^2*Y^2 in affine; in projective:
         ;; -X^2*Z^2 + Y^2*Z^2 = Z^4 + d*X^2*Y^2 ... wait, let me redo.
         ;; Affine: -x^2 + y^2 = 1 + d*x^2*y^2  with x=X/Z, y=Y/Z
         ;; -(X/Z)^2 + (Y/Z)^2 = 1 + d*(X/Z)^2*(Y/Z)^2
         ;; Multiply both sides by Z^4:
         ;; -X^2*Z^2 + Y^2*Z^2 = Z^4 + d*X^2*Y^2
         (lhs (fe-add (fe-neg (fe-mul x2 z2)) (fe-mul y2 z2)))
         (rhs (fe-add (fe-sqr z2) (fe-mul +d+ (fe-mul x2 y2)))))
    (= lhs rhs)))

;;; ---------------------------------------------------------------------------
;;; Point encoding/decoding (RFC 8032 Section 5.1.2)
;;; ---------------------------------------------------------------------------

(defun ed-point-encode (point)
  "Encode a point as 32 bytes per RFC 8032 Section 5.1.2.
   The encoding is the y-coordinate in little-endian with the high bit
   set to the low bit of x."
  (let* ((z-inv (fe-inv (ed-point-z point)))
         (x (fe-mul (ed-point-x point) z-inv))
         (y (fe-mul (ed-point-y point) z-inv))
         (bytes (make-array 32 :element-type '(unsigned-byte 8)))
         (yn y))
    ;; Encode y in little-endian
    (loop for i from 0 below 32
          do (setf (aref bytes i) (logand yn #xFF))
             (setf yn (ash yn -8)))
    ;; Set the high bit of the last byte to the low bit of x
    (when (oddp x)
      (setf (aref bytes 31) (logior (aref bytes 31) #x80)))
    bytes))

(defun ed-point-decode (bytes)
  "Decode a 32-byte encoding to a point per RFC 8032 Section 5.1.3.
   Returns the point, or NIL if decoding fails."
  (declare (type (simple-array (unsigned-byte 8) (32)) bytes))
  ;; Extract the sign bit (high bit of last byte)
  (let* ((x-sign (ash (aref bytes 31) -7))
         ;; Decode y (little-endian, clear the sign bit)
         (y (let ((n 0))
              (loop for i from 31 downto 0
                    do (setf n (logior (ash n 8) (aref bytes i))))
              (logand n (1- (expt 2 255))))))
    ;; Check y < p
    (when (>= y +p+)
      (return-from ed-point-decode nil))
    ;; Recover x from y: x^2 = (y^2 - 1) / (d*y^2 + 1)
    (let* ((y2 (fe-sqr y))
           (u (fe-sub y2 1))           ; u = y^2 - 1
           (v (fe-add (fe-mul +d+ y2) 1)) ; v = d*y^2 + 1
           (v-inv (fe-inv v))
           (x2 (fe-mul u v-inv))        ; x^2 = u/v
           ;; Compute candidate x = x2^((p+3)/8)
           (x (fe-pow x2 (/ (+ +p+ 3) 8))))
      ;; Verify x^2 = x2
      (cond
        ((= (fe-sqr x) x2)
         ;; Good, x is correct
         nil)
        ((= (fe-sqr x) (fe-neg x2))
         ;; x needs to be multiplied by sqrt(-1)
         (setf x (fe-mul x +sqrt-neg1+)))
        (t
         ;; No valid x exists
         (return-from ed-point-decode nil)))
      ;; Adjust sign
      (when (/= (logand x 1) x-sign)
        (if (zerop x)
            (return-from ed-point-decode nil) ; x=0 but sign bit set
            (setf x (fe-neg x))))
      (ed-affine-point x y))))

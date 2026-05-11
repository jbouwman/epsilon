;;;; P-521 (secp521r1) Elliptic Curve Arithmetic
;;;;
;;;; FIPS 186-4 / SEC 2 §3.7 curve. Equation:
;;;;   y^2 = x^3 - 3x + b   over GF(p)
;;;;   p = 2^521 - 1   (a Mersenne prime)
;;;;
;;;; Notable: 521 bits is not a multiple of 8, so encoded coordinates
;;;; are 66 bytes wide and the leading byte holds only one significant
;;;; bit.

(defpackage epsilon.crypto.ec-p521
  (:use :cl)
  (:import
   (epsilon.crypto.ct ct))
  (:import-from :epsilon.crypto.primitives #:int-to-be-bytes)
  (:export
   ;; Point operations
   #:p521-point
   #:p521-point-x #:p521-point-y #:p521-point-z
   #:p521-neutral
   #:p521-base-point
   #:p521-point-add
   #:p521-point-double
   #:p521-scalar-mul
   #:p521-scalar-mul-ct
   #:p521-point-equal
   #:p521-on-curve-p
   #:p521-point-negate
   #:p521-point-to-affine
   #:p521-point-is-neutral
   ;; Encoding/decoding
   #:p521-point-encode-uncompressed
   #:p521-point-decode
   ;; Constants
   #:+p+ #:+n+ #:+b+ #:+gx+ #:+gy+
   #:+coordinate-bytes+))

(in-package :epsilon.crypto.ec-p521)

;;; FIPS 186-4 D.1.2.5 P-521 parameters

(defconstant +coordinate-bytes+ 66)   ; ceiling(521/8)

(defconstant +p+ (1- (expt 2 521)))   ; 2^521 - 1

(defconstant +n+
  #x01fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa51868783bf2f966b7fcc0148f709a5d03bb5c9b8899c47aebb6fb71e91386409)

(defconstant +a+ (mod -3 +p+))

(defconstant +b+
  #x051953eb9618e1c9a1f929a21a0b68540eea2da725b99b315f3b8b489918ef109e156193951ec7e937b1652c0bd3bb1bf073573df883d2c34f1ef451fd46b503f00)

(defconstant +gx+
  #x00c6858e06b70404e9cd9e3ecb662395b4429c648139053fb521f828af606b4d3dbaa14b5e77efe75928fe1dc127a2ffa8de3348b3c1856a429bf97e7e31c2e5bd66)

(defconstant +gy+
  #x011839296a789a3bc0045c8a5fb42c7d1bd998f54449579b446817afbd17273e662c97ee72995ef42640c550b9013fad0761353c7086a272c24088be94769fd16650)

;;; ---------------------------------------------------------------------------
;;; Field arithmetic
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
;;; Jacobian point representation
;;; ---------------------------------------------------------------------------

(defstruct (p521-point (:constructor %make-p521-point (x y z)))
  (x 0 :type integer)
  (y 0 :type integer)
  (z 0 :type integer))

(defun make-p521-point (x y z)
  (%make-p521-point x y z))

(defun p521-neutral () (make-p521-point 0 1 0))

(defun p521-affine-point (x y)
  (make-p521-point (mod x +p+) (mod y +p+) 1))

(defun p521-base-point ()
  (p521-affine-point +gx+ +gy+))

(defun p521-point-is-neutral (p) (zerop (p521-point-z p)))

;;; ---------------------------------------------------------------------------
;;; Point arithmetic in Jacobian coordinates (a = -3)
;;; ---------------------------------------------------------------------------

(defun p521-point-double (p1)
  (when (p521-point-is-neutral p1)
    (return-from p521-point-double (p521-neutral)))
  (let* ((x1 (p521-point-x p1))
         (y1 (p521-point-y p1))
         (z1 (p521-point-z p1))
         (z1-sq (fe-sqr z1))
         (m (fe-mul 3 (fe-mul (fe-sub x1 z1-sq) (fe-add x1 z1-sq))))
         (y1-sq (fe-sqr y1))
         (s (fe-mul 4 (fe-mul x1 y1-sq)))
         (x3 (fe-sub (fe-sqr m) (fe-dbl s)))
         (y3 (fe-sub (fe-mul m (fe-sub s x3))
                     (fe-mul 8 (fe-sqr y1-sq))))
         (z3 (fe-dbl (fe-mul y1 z1))))
    (make-p521-point x3 y3 z3)))

(defun p521-point-add (p1 p2)
  (when (p521-point-is-neutral p1) (return-from p521-point-add p2))
  (when (p521-point-is-neutral p2) (return-from p521-point-add p1))
  (let* ((x1 (p521-point-x p1)) (y1 (p521-point-y p1)) (z1 (p521-point-z p1))
         (x2 (p521-point-x p2)) (y2 (p521-point-y p2)) (z2 (p521-point-z p2))
         (z1-sq (fe-sqr z1))
         (z2-sq (fe-sqr z2))
         (u1 (fe-mul x1 z2-sq))
         (u2 (fe-mul x2 z1-sq))
         (s1 (fe-mul y1 (fe-mul z2 z2-sq)))
         (s2 (fe-mul y2 (fe-mul z1 z1-sq)))
         (h (fe-sub u2 u1))
         (r (fe-sub s2 s1)))
    (when (and (zerop h) (zerop r))
      (return-from p521-point-add (p521-point-double p1)))
    (when (and (zerop h) (not (zerop r)))
      (return-from p521-point-add (p521-neutral)))
    (let* ((h-sq (fe-sqr h))
           (h-cu (fe-mul h h-sq))
           (x3 (fe-sub (fe-sub (fe-sqr r) h-cu) (fe-dbl (fe-mul u1 h-sq))))
           (y3 (fe-sub (fe-mul r (fe-sub (fe-mul u1 h-sq) x3))
                       (fe-mul s1 h-cu)))
           (z3 (fe-mul h (fe-mul z1 z2))))
      (make-p521-point x3 y3 z3))))

(defun p521-point-negate (p1)
  (make-p521-point (p521-point-x p1)
                   (fe-neg (p521-point-y p1))
                   (p521-point-z p1)))

(defun p521-scalar-mul (n point)
  "Right-to-left double-and-add. Not constant-time; use only with
   public scalars (verification). Signing must use
   `p521-scalar-mul-ct'."
  (let ((result (p521-neutral))
        (q point)
        (k (mod n +n+)))
    (loop while (plusp k)
          do (when (oddp k)
               (setf result (p521-point-add result q)))
             (setf q (p521-point-double q))
             (setf k (ash k -1)))
    result))

;;; ---------------------------------------------------------------------------
;;; Constant-time scalar multiplication (RCB 2015 Algorithms 4 + 6)
;;; Identical structure to the P-256 / P-384 ports; the only changes are
;;; the curve constant b (already shadowed inside this package), the
;;; field arithmetic primitives (also local), and the bit count (521).
;;; ---------------------------------------------------------------------------

(defun %p521-projective-add (px py pz qx qy qz)
  "RCB Algorithm 4 over standard projective coordinates."
  (let* ((b +b+)
         (t0 (fe-mul px qx))
         (t1 (fe-mul py qy))
         (t2 (fe-mul pz qz))
         (t3 (fe-add px py))
         (t4 (fe-add qx qy))
         (t3 (fe-mul t3 t4))
         (t4 (fe-add t0 t1))
         (t3 (fe-sub t3 t4))
         (t4 (fe-add py pz))
         (x3 (fe-add qy qz))
         (t4 (fe-mul t4 x3))
         (x3 (fe-add t1 t2))
         (t4 (fe-sub t4 x3))
         (x3 (fe-add px pz))
         (y3 (fe-add qx qz))
         (x3 (fe-mul x3 y3))
         (y3 (fe-add t0 t2))
         (y3 (fe-sub x3 y3))
         (z3 (fe-mul b t2))
         (x3 (fe-sub y3 z3))
         (z3 (fe-add x3 x3))
         (x3 (fe-add x3 z3))
         (z3 (fe-sub t1 x3))
         (x3 (fe-add t1 x3))
         (y3 (fe-mul b y3))
         (t1 (fe-add t2 t2))
         (t2 (fe-add t1 t2))
         (y3 (fe-sub y3 t2))
         (y3 (fe-sub y3 t0))
         (t1 (fe-add y3 y3))
         (y3 (fe-add t1 y3))
         (t1 (fe-add t0 t0))
         (t0 (fe-add t1 t0))
         (t0 (fe-sub t0 t2))
         (t1 (fe-mul t4 y3))
         (t2 (fe-mul t0 y3))
         (y3 (fe-mul x3 z3))
         (y3 (fe-add y3 t2))
         (x3 (fe-mul t3 x3))
         (x3 (fe-sub x3 t1))
         (z3 (fe-mul t4 z3))
         (t1 (fe-mul t3 t0))
         (z3 (fe-add z3 t1)))
    (values x3 y3 z3)))

(defun %p521-projective-double (px py pz)
  "RCB Algorithm 6 over standard projective coordinates."
  (let* ((b +b+)
         (t0 (fe-mul px px))
         (t1 (fe-mul py py))
         (t2 (fe-mul pz pz))
         (t3 (fe-mul px py))
         (t3 (fe-add t3 t3))
         (z3 (fe-mul px pz))
         (z3 (fe-add z3 z3))
         (y3 (fe-mul b t2))
         (y3 (fe-sub y3 z3))
         (x3 (fe-add y3 y3))
         (y3 (fe-add x3 y3))
         (x3 (fe-sub t1 y3))
         (y3 (fe-add t1 y3))
         (y3 (fe-mul x3 y3))
         (x3 (fe-mul x3 t3))
         (t3 (fe-add t2 t2))
         (t2 (fe-add t2 t3))
         (z3 (fe-mul b z3))
         (z3 (fe-sub z3 t2))
         (z3 (fe-sub z3 t0))
         (t3 (fe-add z3 z3))
         (z3 (fe-add z3 t3))
         (t3 (fe-add t0 t0))
         (t0 (fe-add t3 t0))
         (t0 (fe-sub t0 t2))
         (t0 (fe-mul t0 z3))
         (y3 (fe-add y3 t0))
         (t0 (fe-mul py pz))
         (t0 (fe-add t0 t0))
         (z3 (fe-mul t0 z3))
         (x3 (fe-sub x3 z3))
         (z3 (fe-mul t0 t1))
         (z3 (fe-add z3 z3))
         (z3 (fe-add z3 z3)))
    (values x3 y3 z3)))

(defun p521-scalar-mul-ct (n point &key (bits 521))
  "Constant-time scalar multiplication on P-521. Same shape as
   P-256 / P-384; the iteration count defaults to 521 to cover any
   scalar reducible mod n."
  (let ((k (mod n +n+)))
    (let ((px 0) (py 1) (pz 0))
      (multiple-value-bind (qx qy qz)
          (if (= 1 (p521-point-z point))
              (values (p521-point-x point) (p521-point-y point) 1)
              (let* ((z-inv (fe-inv (p521-point-z point)))
                     (z-inv-sq (fe-sqr z-inv))
                     (z-inv-cu (fe-mul z-inv-sq z-inv)))
                (values (fe-mul (p521-point-x point) z-inv-sq)
                        (fe-mul (p521-point-y point) z-inv-cu)
                        1)))
        (loop for i from (1- bits) downto 0
              do (multiple-value-bind (dx dy dz)
                     (%p521-projective-double px py pz)
                   (multiple-value-bind (ax ay az)
                       (%p521-projective-add dx dy dz qx qy qz)
                     (let ((bit (ldb (byte 1 i) k)))
                       (multiple-value-setq (px py pz)
                         (values (ct:ct-select-int bit ax dx)
                                 (ct:ct-select-int bit ay dy)
                                 (ct:ct-select-int bit az dz))))))))
      (if (zerop pz)
          (p521-neutral)
          (let* ((z-inv (fe-inv pz))
                 (x (fe-mul px z-inv))
                 (y (fe-mul py z-inv)))
            (make-p521-point x y 1))))))

(defun p521-point-equal (p1 p2)
  (cond
    ((and (p521-point-is-neutral p1) (p521-point-is-neutral p2)) t)
    ((or (p521-point-is-neutral p1) (p521-point-is-neutral p2)) nil)
    (t (let ((z1-sq (fe-sqr (p521-point-z p1)))
             (z2-sq (fe-sqr (p521-point-z p2))))
         (and (= (fe-mul (p521-point-x p1) z2-sq)
                 (fe-mul (p521-point-x p2) z1-sq))
              (= (fe-mul (p521-point-y p1) (fe-mul (p521-point-z p2) z2-sq))
                 (fe-mul (p521-point-y p2) (fe-mul (p521-point-z p1) z1-sq))))))))

(defun p521-on-curve-p (point)
  (when (p521-point-is-neutral point)
    (return-from p521-on-curve-p t))
  (let* ((x (p521-point-x point))
         (y (p521-point-y point))
         (z (p521-point-z point))
         (z2 (fe-sqr z))
         (z4 (fe-sqr z2))
         (z6 (fe-mul z2 z4))
         (lhs (fe-sqr y))
         (rhs (fe-add (fe-add (fe-mul x (fe-sqr x))
                              (fe-mul +a+ (fe-mul x z4)))
                      (fe-mul +b+ z6))))
    (= lhs rhs)))

;;; ---------------------------------------------------------------------------
;;; Encoding / decoding (uncompressed SEC1)
;;; ---------------------------------------------------------------------------

(defun p521-point-to-affine (point)
  (when (p521-point-is-neutral point)
    (return-from p521-point-to-affine (values 0 0)))
  (let* ((z-inv (fe-inv (p521-point-z point)))
         (z-inv-sq (fe-sqr z-inv))
         (x (fe-mul (p521-point-x point) z-inv-sq))
         (y (fe-mul (p521-point-y point) (fe-mul z-inv z-inv-sq))))
    (values x y)))

(defun p521-point-encode-uncompressed (point)
  "SEC1 uncompressed point: 04 || x || y. 1 + 66 + 66 = 133 bytes."
  (multiple-value-bind (x y) (p521-point-to-affine point)
    (let ((bytes (make-array (1+ (* 2 +coordinate-bytes+))
                             :element-type '(unsigned-byte 8))))
      (setf (aref bytes 0) #x04)
      (replace bytes (int-to-be-bytes x +coordinate-bytes+) :start1 1)
      (replace bytes (int-to-be-bytes y +coordinate-bytes+)
               :start1 (1+ +coordinate-bytes+))
      bytes)))

(defun p521-point-decode (bytes)
  (cond
    ((and (= (length bytes) (1+ (* 2 +coordinate-bytes+)))
          (= (aref bytes 0) #x04))
     (let ((x 0) (y 0))
       (loop for i from 1 to +coordinate-bytes+
             do (setf x (logior (ash x 8) (aref bytes i))))
       (loop for i from (1+ +coordinate-bytes+)
               to (* 2 +coordinate-bytes+)
             do (setf y (logior (ash y 8) (aref bytes i))))
       (let ((pt (p521-affine-point x y)))
         (if (p521-on-curve-p pt) pt nil))))
    (t nil)))

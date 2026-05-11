;;;; P-384 (secp384r1) Elliptic Curve Arithmetic
;;;;
;;;; FIPS 186-4 / SEC 2 §3.6 curve. Equation:
;;;;   y^2 = x^3 - 3x + b   over GF(p)
;;;;   p = 2^384 - 2^128 - 2^96 + 2^32 - 1
;;;;
;;;; Layout mirrors `epsilon.crypto.ec-p256': Jacobian coordinates, generic
;;;; modular arithmetic via Common Lisp bignums (no Solinas-specific
;;;; reduction yet -- correctness first; the same pure-Lisp pattern we
;;;; use for P-256 is fast enough for ECDSA / ECDH at the rates a TLS
;;;; client needs).

(defpackage epsilon.crypto.ec-p384
  (:use :cl)
  (:import
   (epsilon.crypto.ct ct))
  (:import-from :epsilon.crypto.primitives #:int-to-be-bytes)
  (:export
   ;; Point operations
   #:p384-point
   #:p384-point-x #:p384-point-y #:p384-point-z
   #:p384-neutral
   #:p384-base-point
   #:p384-point-add
   #:p384-point-double
   #:p384-scalar-mul
   #:p384-scalar-mul-ct
   #:p384-point-equal
   #:p384-on-curve-p
   #:p384-point-negate
   #:p384-point-to-affine
   #:p384-point-is-neutral
   ;; Encoding/decoding
   #:p384-point-encode-uncompressed
   #:p384-point-decode
   ;; Constants
   #:+p+ #:+n+ #:+b+ #:+gx+ #:+gy+
   #:+coordinate-bytes+))

(in-package :epsilon.crypto.ec-p384)

;;; FIPS 186-4 D.1.2.4 P-384 parameters

(defconstant +coordinate-bytes+ 48)

(defconstant +p+
  ;; 2^384 - 2^128 - 2^96 + 2^32 - 1
  (+ (expt 2 384) (- (expt 2 128)) (- (expt 2 96)) (expt 2 32) -1))

(defconstant +n+
  #xffffffffffffffffffffffffffffffffffffffffffffffffc7634d81f4372ddf581a0db248b0a77aecec196accc52973)

(defconstant +a+ (mod -3 +p+))

(defconstant +b+
  #xb3312fa7e23ee7e4988e056be3f82d19181d9c6efe8141120314088f5013875ac656398d8a2ed19d2a85c8edd3ec2aef)

(defconstant +gx+
  #xaa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a385502f25dbf55296c3a545e3872760ab7)

(defconstant +gy+
  #x3617de4a96262c6f5d9e98bf9292dc29f8f41dbd289a147ce9da3113b5f0b8c00a60b1ce1d7e819d7a431d7c90ea0e5f)

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
  "Field inversion via Fermat's little theorem: a^(p-2) mod p."
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

(defstruct (p384-point (:constructor %make-p384-point (x y z)))
  (x 0 :type integer)
  (y 0 :type integer)
  (z 0 :type integer))

(defun make-p384-point (x y z)
  (%make-p384-point x y z))

(defun p384-neutral ()
  "Return the point at infinity: (0 : 1 : 0) in Jacobian coordinates."
  (make-p384-point 0 1 0))

(defun p384-affine-point (x y)
  "Create a point from affine coordinates."
  (make-p384-point (mod x +p+) (mod y +p+) 1))

(defun p384-base-point ()
  "Return the P-384 generator point G."
  (p384-affine-point +gx+ +gy+))

(defun p384-point-is-neutral (p)
  "Test if P is the point at infinity."
  (zerop (p384-point-z p)))

;;; ---------------------------------------------------------------------------
;;; Point arithmetic in Jacobian coordinates (a = -3 specialisation)
;;; ---------------------------------------------------------------------------

(defun p384-point-double (p1)
  "Double a point in Jacobian coordinates. Uses the a=-3 formula."
  (when (p384-point-is-neutral p1)
    (return-from p384-point-double (p384-neutral)))
  (let* ((x1 (p384-point-x p1))
         (y1 (p384-point-y p1))
         (z1 (p384-point-z p1))
         (z1-sq (fe-sqr z1))
         ;; M = 3*(X1 - Z1^2)*(X1 + Z1^2)  (the a = -3 trick)
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
    (make-p384-point x3 y3 z3)))

(defun p384-point-add (p1 p2)
  "Add two points in Jacobian coordinates."
  (when (p384-point-is-neutral p1) (return-from p384-point-add p2))
  (when (p384-point-is-neutral p2) (return-from p384-point-add p1))
  (let* ((x1 (p384-point-x p1)) (y1 (p384-point-y p1)) (z1 (p384-point-z p1))
         (x2 (p384-point-x p2)) (y2 (p384-point-y p2)) (z2 (p384-point-z p2))
         (z1-sq (fe-sqr z1))
         (z2-sq (fe-sqr z2))
         (u1 (fe-mul x1 z2-sq))
         (u2 (fe-mul x2 z1-sq))
         (s1 (fe-mul y1 (fe-mul z2 z2-sq)))
         (s2 (fe-mul y2 (fe-mul z1 z1-sq)))
         (h (fe-sub u2 u1))
         (r (fe-sub s2 s1)))
    (when (and (zerop h) (zerop r))
      (return-from p384-point-add (p384-point-double p1)))
    (when (and (zerop h) (not (zerop r)))
      (return-from p384-point-add (p384-neutral)))
    (let* ((h-sq (fe-sqr h))
           (h-cu (fe-mul h h-sq))
           (x3 (fe-sub (fe-sub (fe-sqr r) h-cu) (fe-dbl (fe-mul u1 h-sq))))
           (y3 (fe-sub (fe-mul r (fe-sub (fe-mul u1 h-sq) x3))
                       (fe-mul s1 h-cu)))
           (z3 (fe-mul h (fe-mul z1 z2))))
      (make-p384-point x3 y3 z3))))

(defun p384-point-negate (p1)
  "Negate a point: (X:Y:Z) -> (X:-Y:Z)."
  (make-p384-point (p384-point-x p1)
                   (fe-neg (p384-point-y p1))
                   (p384-point-z p1)))

(defun p384-scalar-mul (n point)
  "Compute n * point using right-to-left double-and-add.

   Not constant-time. Suitable for verification only; signing must
   use `p384-scalar-mul-ct'."
  (let ((result (p384-neutral))
        (q point)
        (k (mod n +n+)))
    (loop while (plusp k)
          do (when (oddp k)
               (setf result (p384-point-add result q)))
             (setf q (p384-point-double q))
             (setf k (ash k -1)))
    result))

;;; ---------------------------------------------------------------------------
;;; Constant-time scalar multiplication (Renes-Costello-Batina 2015)
;;;
;;; Same shape as the P-256 implementation: standard projective
;;; coordinates, RCB Algorithm 4 for general addition, Algorithm 6
;;; for doubling, both unified for short Weierstrass with a = -3.
;;; Loop count is fixed at 384 iterations (= bit length of n) and
;;; the per-bit candidate select is branchless.
;;; ---------------------------------------------------------------------------

(defun %p384-projective-add (px py pz qx qy qz)
  "RCB 2015 Algorithm 4 (complete addition for a = -3, projective)."
  (let* ((b +b+)
         (t0 (fe-mul px qx))                      ; 1
         (t1 (fe-mul py qy))                      ; 2
         (t2 (fe-mul pz qz))                      ; 3
         (t3 (fe-add px py))                      ; 4
         (t4 (fe-add qx qy))                      ; 5
         (t3 (fe-mul t3 t4))                      ; 6
         (t4 (fe-add t0 t1))                      ; 7
         (t3 (fe-sub t3 t4))                      ; 8
         (t4 (fe-add py pz))                      ; 9
         (x3 (fe-add qy qz))                      ; 10
         (t4 (fe-mul t4 x3))                      ; 11
         (x3 (fe-add t1 t2))                      ; 12
         (t4 (fe-sub t4 x3))                      ; 13
         (x3 (fe-add px pz))                      ; 14
         (y3 (fe-add qx qz))                      ; 15
         (x3 (fe-mul x3 y3))                      ; 16
         (y3 (fe-add t0 t2))                      ; 17
         (y3 (fe-sub x3 y3))                      ; 18
         (z3 (fe-mul b t2))                       ; 19
         (x3 (fe-sub y3 z3))                      ; 20
         (z3 (fe-add x3 x3))                      ; 21
         (x3 (fe-add x3 z3))                      ; 22
         (z3 (fe-sub t1 x3))                      ; 23
         (x3 (fe-add t1 x3))                      ; 24
         (y3 (fe-mul b y3))                       ; 25
         (t1 (fe-add t2 t2))                      ; 26
         (t2 (fe-add t1 t2))                      ; 27
         (y3 (fe-sub y3 t2))                      ; 28
         (y3 (fe-sub y3 t0))                      ; 29
         (t1 (fe-add y3 y3))                      ; 30
         (y3 (fe-add t1 y3))                      ; 31
         (t1 (fe-add t0 t0))                      ; 32
         (t0 (fe-add t1 t0))                      ; 33
         (t0 (fe-sub t0 t2))                      ; 34
         (t1 (fe-mul t4 y3))                      ; 35
         (t2 (fe-mul t0 y3))                      ; 36
         (y3 (fe-mul x3 z3))                      ; 37
         (y3 (fe-add y3 t2))                      ; 38
         (x3 (fe-mul t3 x3))                      ; 39
         (x3 (fe-sub x3 t1))                      ; 40
         (z3 (fe-mul t4 z3))                      ; 41
         (t1 (fe-mul t3 t0))                      ; 42
         (z3 (fe-add z3 t1)))                     ; 43
    (values x3 y3 z3)))

(defun %p384-projective-double (px py pz)
  "RCB 2015 Algorithm 6 (doubling for a = -3, projective)."
  (let* ((b +b+)
         (t0 (fe-mul px px))                      ; 1
         (t1 (fe-mul py py))                      ; 2
         (t2 (fe-mul pz pz))                      ; 3
         (t3 (fe-mul px py))                      ; 4
         (t3 (fe-add t3 t3))                      ; 5
         (z3 (fe-mul px pz))                      ; 6
         (z3 (fe-add z3 z3))                      ; 7
         (y3 (fe-mul b t2))                       ; 8
         (y3 (fe-sub y3 z3))                      ; 9
         (x3 (fe-add y3 y3))                      ; 10
         (y3 (fe-add x3 y3))                      ; 11
         (x3 (fe-sub t1 y3))                      ; 12
         (y3 (fe-add t1 y3))                      ; 13
         (y3 (fe-mul x3 y3))                      ; 14
         (x3 (fe-mul x3 t3))                      ; 15
         (t3 (fe-add t2 t2))                      ; 16
         (t2 (fe-add t2 t3))                      ; 17
         (z3 (fe-mul b z3))                       ; 18
         (z3 (fe-sub z3 t2))                      ; 19
         (z3 (fe-sub z3 t0))                      ; 20
         (t3 (fe-add z3 z3))                      ; 21
         (z3 (fe-add z3 t3))                      ; 22
         (t3 (fe-add t0 t0))                      ; 23
         (t0 (fe-add t3 t0))                      ; 24
         (t0 (fe-sub t0 t2))                      ; 25
         (t0 (fe-mul t0 z3))                      ; 26
         (y3 (fe-add y3 t0))                      ; 27
         (t0 (fe-mul py pz))                      ; 28
         (t0 (fe-add t0 t0))                      ; 29
         (z3 (fe-mul t0 z3))                      ; 30
         (x3 (fe-sub x3 z3))                      ; 31
         (z3 (fe-mul t0 t1))                      ; 32
         (z3 (fe-add z3 z3))                      ; 33
         (z3 (fe-add z3 z3)))                     ; 34
    (values x3 y3 z3)))

(defun p384-scalar-mul-ct (n point &key (bits 384))
  "Constant-time scalar multiplication on P-384.

   Same structure as `epsilon.crypto.ec-p256:p256-scalar-mul-ct': a
   fixed BITS-iteration loop, RCB unified addition and doubling
   over standard projective coordinates, branchless coordinate-by-
   coordinate select between the doubled-only and doubled-then-
   added candidate per bit. Use this in ECDSA P-384 signing and
   key derivation; verification can stay on the faster non-CT
   `p384-scalar-mul'."
  (let ((k (mod n +n+)))
    (let ((px 0) (py 1) (pz 0))                   ; running result = neutral
      (multiple-value-bind (qx qy qz)
          (if (= 1 (p384-point-z point))
              (values (p384-point-x point) (p384-point-y point) 1)
              (let* ((z-inv (fe-inv (p384-point-z point)))
                     (z-inv-sq (fe-sqr z-inv))
                     (z-inv-cu (fe-mul z-inv-sq z-inv)))
                (values (fe-mul (p384-point-x point) z-inv-sq)
                        (fe-mul (p384-point-y point) z-inv-cu)
                        1)))
        (loop for i from (1- bits) downto 0
              do (multiple-value-bind (dx dy dz)
                     (%p384-projective-double px py pz)
                   (multiple-value-bind (ax ay az)
                       (%p384-projective-add dx dy dz qx qy qz)
                     (let ((bit (ldb (byte 1 i) k)))
                       (multiple-value-setq (px py pz)
                         (values (ct:ct-select-int bit ax dx)
                                 (ct:ct-select-int bit ay dy)
                                 (ct:ct-select-int bit az dz))))))))
      (if (zerop pz)
          (p384-neutral)
          (let* ((z-inv (fe-inv pz))
                 (x (fe-mul px z-inv))
                 (y (fe-mul py z-inv)))
            (make-p384-point x y 1))))))

(defun p384-point-equal (p1 p2)
  "Compare two Jacobian points for equality."
  (cond
    ((and (p384-point-is-neutral p1) (p384-point-is-neutral p2)) t)
    ((or (p384-point-is-neutral p1) (p384-point-is-neutral p2)) nil)
    (t (let ((z1-sq (fe-sqr (p384-point-z p1)))
             (z2-sq (fe-sqr (p384-point-z p2))))
         (and (= (fe-mul (p384-point-x p1) z2-sq)
                 (fe-mul (p384-point-x p2) z1-sq))
              (= (fe-mul (p384-point-y p1) (fe-mul (p384-point-z p2) z2-sq))
                 (fe-mul (p384-point-y p2) (fe-mul (p384-point-z p1) z1-sq))))))))

(defun p384-on-curve-p (point)
  "Check that POINT lies on y^2 = x^3 - 3x + b."
  (when (p384-point-is-neutral point)
    (return-from p384-on-curve-p t))
  (let* ((x (p384-point-x point))
         (y (p384-point-y point))
         (z (p384-point-z point))
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

(defun p384-point-to-affine (point)
  "Convert Jacobian point to affine (x, y). Returns (values x y).
   Returns (values 0 0) for the point at infinity."
  (when (p384-point-is-neutral point)
    (return-from p384-point-to-affine (values 0 0)))
  (let* ((z-inv (fe-inv (p384-point-z point)))
         (z-inv-sq (fe-sqr z-inv))
         (x (fe-mul (p384-point-x point) z-inv-sq))
         (y (fe-mul (p384-point-y point) (fe-mul z-inv z-inv-sq))))
    (values x y)))

(defun p384-point-encode-uncompressed (point)
  "SEC1 uncompressed point: 04 || x || y. 1 + 48 + 48 = 97 bytes."
  (multiple-value-bind (x y) (p384-point-to-affine point)
    (let ((bytes (make-array (1+ (* 2 +coordinate-bytes+))
                             :element-type '(unsigned-byte 8))))
      (setf (aref bytes 0) #x04)
      (replace bytes (int-to-be-bytes x +coordinate-bytes+) :start1 1)
      (replace bytes (int-to-be-bytes y +coordinate-bytes+)
               :start1 (1+ +coordinate-bytes+))
      bytes)))

(defun p384-point-decode (bytes)
  "Decode a SEC1 uncompressed point. Returns the point or NIL if invalid."
  (cond
    ((and (= (length bytes) (1+ (* 2 +coordinate-bytes+)))
          (= (aref bytes 0) #x04))
     (let ((x 0) (y 0))
       (loop for i from 1 to +coordinate-bytes+
             do (setf x (logior (ash x 8) (aref bytes i))))
       (loop for i from (1+ +coordinate-bytes+)
               to (* 2 +coordinate-bytes+)
             do (setf y (logior (ash y 8) (aref bytes i))))
       (let ((pt (p384-affine-point x y)))
         (if (p384-on-curve-p pt) pt nil))))
    (t nil)))

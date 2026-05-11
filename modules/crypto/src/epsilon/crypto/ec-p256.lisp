;;;; P-256 (secp256r1) Elliptic Curve Arithmetic
;;;;
;;;; Implements point arithmetic on the NIST P-256 curve:
;;;;   y^2 = x^3 - 3x + b  over GF(p)
;;;; where p = 2^256 - 2^224 + 2^192 + 2^96 - 1.
;;;;
;;;; Uses Jacobian coordinates (X : Y : Z) where x = X/Z^2, y = Y/Z^3.
;;;; This avoids field inversions during point operations.

(defpackage epsilon.crypto.ec-p256
  (:use :cl)
  (:import
   (epsilon.crypto.field-p256 field)
   (epsilon.crypto.ct ct))
  (:export
   ;; Point operations
   #:p256-point
   #:p256-point-x #:p256-point-y #:p256-point-z
   #:p256-neutral
   #:p256-base-point
   #:p256-point-add
   #:p256-point-double
   #:p256-scalar-mul
   #:p256-scalar-mul-ct
   #:p256-point-equal
   #:p256-on-curve-p
   #:p256-point-negate
   ;; Encoding/decoding
   #:p256-point-encode-uncompressed
   #:p256-point-decode
   ;; Constants
   #:+p+ #:+n+ #:+b+ #:+gx+ #:+gy+))

(in-package :epsilon.crypto.ec-p256)

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
  "Compute n * point using right-to-left double-and-add.

   This routine is **not** constant-time. The number of iterations
   leaks the bit length of N, the conditional add leaks each
   individual bit, and the underlying `p256-point-add` has special
   cases that branch on the input. Suitable only for verification
   (where N is the public scalar). Signing paths must use
   `p256-scalar-mul-ct`."
  (let ((result (p256-neutral))
        (q point)
        (k (mod n +n+)))
    (loop while (plusp k)
          do (when (oddp k)
               (setf result (p256-point-add result q)))
             (setf q (p256-point-double q))
             (setf k (ash k -1)))
    result))

;;; ---------------------------------------------------------------------------
;;; Constant-time scalar multiplication (Renes-Costello-Batina 2015)
;;;
;;; For CT we cannot use the Jacobian-coordinate addition formula in
;;; `p256-point-add' because it has source-level branches on the
;;; neutral element and on the self-add case. Instead we work in
;;; standard projective coordinates (X:Y:Z) where x = X/Z, y = Y/Z,
;;; using the unified-formula complete addition from "Complete
;;; addition formulas for prime order elliptic curves" (Renes,
;;; Costello, Batina, 2015), Algorithm 4 for general addition and
;;; Algorithm 6 for doubling on the a = -3 short Weierstrass curve.
;;; The formulas return the correct point for every input pair --
;;; including the neutral, self-add, and inverse-pair cases -- with
;;; no conditional branches at all.
;;;
;;; The conversion from / to Jacobian is identity for points where
;;; Z = 1 (the typical case: the base point and any fresh affine
;;; point). For arbitrary Jacobian (X, Y, Z) the projective form
;;; is (X*Z, Y, Z^3); the inverse conversion goes through affine.
;;; ---------------------------------------------------------------------------

(defun %p256-projective-add (px py pz qx qy qz)
  "RCB 2015 Algorithm 4 (complete addition for a = -3, projective).
   Inputs PX,PY,PZ and QX,QY,QZ are field elements representing
   P = (PX:PY:PZ) and Q = (QX:QY:QZ). Returns (values RX RY RZ)
   for R = P + Q. No branches, defined for every input pair.

   This is a literal step-by-step translation of the 43 lines in the
   paper; each `let*` binding corresponds to one assignment in the
   algorithm and shadows the previous symbol of the same name."
  (let* ((b +b+)
         ;; 1.  t0 = X1*X2
         (t0 (fe-mul px qx))
         ;; 2.  t1 = Y1*Y2
         (t1 (fe-mul py qy))
         ;; 3.  t2 = Z1*Z2
         (t2 (fe-mul pz qz))
         ;; 4.  t3 = X1 + Y1
         (t3 (fe-add px py))
         ;; 5.  t4 = X2 + Y2
         (t4 (fe-add qx qy))
         ;; 6.  t3 = t3 * t4
         (t3 (fe-mul t3 t4))
         ;; 7.  t4 = t0 + t1
         (t4 (fe-add t0 t1))
         ;; 8.  t3 = t3 - t4
         (t3 (fe-sub t3 t4))
         ;; 9.  t4 = Y1 + Z1
         (t4 (fe-add py pz))
         ;; 10. X3 = Y2 + Z2
         (x3 (fe-add qy qz))
         ;; 11. t4 = t4 * X3
         (t4 (fe-mul t4 x3))
         ;; 12. X3 = t1 + t2
         (x3 (fe-add t1 t2))
         ;; 13. t4 = t4 - X3
         (t4 (fe-sub t4 x3))
         ;; 14. X3 = X1 + Z1
         (x3 (fe-add px pz))
         ;; 15. Y3 = X2 + Z2
         (y3 (fe-add qx qz))
         ;; 16. X3 = X3 * Y3
         (x3 (fe-mul x3 y3))
         ;; 17. Y3 = t0 + t2
         (y3 (fe-add t0 t2))
         ;; 18. Y3 = X3 - Y3
         (y3 (fe-sub x3 y3))
         ;; 19. Z3 = b * t2
         (z3 (fe-mul b t2))
         ;; 20. X3 = Y3 - Z3
         (x3 (fe-sub y3 z3))
         ;; 21. Z3 = X3 + X3
         (z3 (fe-add x3 x3))
         ;; 22. X3 = X3 + Z3
         (x3 (fe-add x3 z3))
         ;; 23. Z3 = t1 - X3
         (z3 (fe-sub t1 x3))
         ;; 24. X3 = t1 + X3
         (x3 (fe-add t1 x3))
         ;; 25. Y3 = b * Y3
         (y3 (fe-mul b y3))
         ;; 26. t1 = t2 + t2
         (t1 (fe-add t2 t2))
         ;; 27. t2 = t1 + t2
         (t2 (fe-add t1 t2))
         ;; 28. Y3 = Y3 - t2
         (y3 (fe-sub y3 t2))
         ;; 29. Y3 = Y3 - t0
         (y3 (fe-sub y3 t0))
         ;; 30. t1 = Y3 + Y3
         (t1 (fe-add y3 y3))
         ;; 31. Y3 = t1 + Y3
         (y3 (fe-add t1 y3))
         ;; 32. t1 = t0 + t0
         (t1 (fe-add t0 t0))
         ;; 33. t0 = t1 + t0
         (t0 (fe-add t1 t0))
         ;; 34. t0 = t0 - t2
         (t0 (fe-sub t0 t2))
         ;; 35. t1 = t4 * Y3
         (t1 (fe-mul t4 y3))
         ;; 36. t2 = t0 * Y3
         (t2 (fe-mul t0 y3))
         ;; 37. Y3 = X3 * Z3
         (y3 (fe-mul x3 z3))
         ;; 38. Y3 = Y3 + t2
         (y3 (fe-add y3 t2))
         ;; 39. X3 = t3 * X3
         (x3 (fe-mul t3 x3))
         ;; 40. X3 = X3 - t1
         (x3 (fe-sub x3 t1))
         ;; 41. Z3 = t4 * Z3
         (z3 (fe-mul t4 z3))
         ;; 42. t1 = t3 * t0
         (t1 (fe-mul t3 t0))
         ;; 43. Z3 = Z3 + t1
         (z3 (fe-add z3 t1)))
    (values x3 y3 z3)))

(defun %p256-projective-double (px py pz)
  "RCB 2015 Algorithm 6 (doubling for a = -3, projective).
   Literal step-by-step translation of the 34-line algorithm."
  (let* ((b +b+)
         ;; 1.  t0 = X · X
         (t0 (fe-mul px px))
         ;; 2.  t1 = Y · Y
         (t1 (fe-mul py py))
         ;; 3.  t2 = Z · Z
         (t2 (fe-mul pz pz))
         ;; 4.  t3 = X · Y
         (t3 (fe-mul px py))
         ;; 5.  t3 = t3 + t3
         (t3 (fe-add t3 t3))
         ;; 6.  Z3 = X · Z
         (z3 (fe-mul px pz))
         ;; 7.  Z3 = Z3 + Z3
         (z3 (fe-add z3 z3))
         ;; 8.  Y3 = b · t2
         (y3 (fe-mul b t2))
         ;; 9.  Y3 = Y3 - Z3
         (y3 (fe-sub y3 z3))
         ;; 10. X3 = Y3 + Y3
         (x3 (fe-add y3 y3))
         ;; 11. Y3 = X3 + Y3
         (y3 (fe-add x3 y3))
         ;; 12. X3 = t1 - Y3
         (x3 (fe-sub t1 y3))
         ;; 13. Y3 = t1 + Y3
         (y3 (fe-add t1 y3))
         ;; 14. Y3 = X3 · Y3
         (y3 (fe-mul x3 y3))
         ;; 15. X3 = X3 · t3
         (x3 (fe-mul x3 t3))
         ;; 16. t3 = t2 + t2
         (t3 (fe-add t2 t2))
         ;; 17. t2 = t2 + t3
         (t2 (fe-add t2 t3))
         ;; 18. Z3 = b · Z3
         (z3 (fe-mul b z3))
         ;; 19. Z3 = Z3 - t2
         (z3 (fe-sub z3 t2))
         ;; 20. Z3 = Z3 - t0
         (z3 (fe-sub z3 t0))
         ;; 21. t3 = Z3 + Z3
         (t3 (fe-add z3 z3))
         ;; 22. Z3 = Z3 + t3
         (z3 (fe-add z3 t3))
         ;; 23. t3 = t0 + t0
         (t3 (fe-add t0 t0))
         ;; 24. t0 = t3 + t0
         (t0 (fe-add t3 t0))
         ;; 25. t0 = t0 - t2
         (t0 (fe-sub t0 t2))
         ;; 26. t0 = t0 · Z3
         (t0 (fe-mul t0 z3))
         ;; 27. Y3 = Y3 + t0
         (y3 (fe-add y3 t0))
         ;; 28. t0 = Y · Z
         (t0 (fe-mul py pz))
         ;; 29. t0 = t0 + t0
         (t0 (fe-add t0 t0))
         ;; 30. Z3 = t0 · Z3
         (z3 (fe-mul t0 z3))
         ;; 31. X3 = X3 - Z3
         (x3 (fe-sub x3 z3))
         ;; 32. Z3 = t0 · t1
         (z3 (fe-mul t0 t1))
         ;; 33. Z3 = Z3 + Z3
         (z3 (fe-add z3 z3))
         ;; 34. Z3 = Z3 + Z3
         (z3 (fe-add z3 z3)))
    (values x3 y3 z3)))

(defun %ct-select-projective (bit if-1-x if-1-y if-1-z if-0-x if-0-y if-0-z)
  "Branchless field-element select across the three projective
   coordinates. BIT must be 0 or 1."
  (declare (type (integer 0 1) bit))
  (values (ct:ct-select-int bit if-1-x if-0-x)
          (ct:ct-select-int bit if-1-y if-0-y)
          (ct:ct-select-int bit if-1-z if-0-z)))

(defun %p256-projective-to-jacobian (px py pz)
  "Convert projective (PX:PY:PZ) to a Jacobian point with Z=1.
   Goes through affine via field inversion. Caller must guarantee
   PZ != 0 (i.e. the result is not the point at infinity)."
  (let* ((z-inv (fe-inv pz))
         (x (fe-mul px z-inv))
         (y (fe-mul py z-inv)))
    (make-p256-point x y 1)))

(defun p256-scalar-mul-ct (n point &key (bits 256))
  "Constant-time scalar multiplication on P-256.

   Performs BITS iterations regardless of the value of N. Each
   iteration doubles the running result, computes the candidate
   sum (running + POINT) using the unified RCB Algorithm 4 (which
   is well-defined for every input pair, including the neutral),
   and selects between the doubled-only and doubled-then-added
   results coordinate-by-coordinate via branchless mask-and-XOR.

   BITS defaults to 256, comfortably above ⌈log₂ N+ 1⌉ = 256, so
   the iteration count never reveals a smaller scalar.

   The input POINT is taken in Jacobian form for compatibility
   with the rest of `epsilon.crypto.ec-p256`. If its Z is not 1 it
   is converted to a Z=1 projective representation by going
   through affine; for the typical case (base point, encoded
   public key) Z is already 1 and the conversion is identity.

   Use this routine in ECDSA signing (per-signature secret nonce
   k -> k*G, secret-scalar -> public point) where N is secret. The
   non-CT `p256-scalar-mul` is faster and is appropriate for
   verification paths where the scalars are public."
  (let ((k (mod n +n+)))
    ;; Normalise input point to projective with Z=1 by going through affine.
    (let ((px 0) (py 1) (pz 0))               ; running result = neutral
      ;; Convert input to Z=1 form (affine).
      (multiple-value-bind (qx qy qz)
          (if (= 1 (p256-point-z point))
              (values (p256-point-x point) (p256-point-y point) 1)
              (let* ((z-inv (fe-inv (p256-point-z point)))
                     (z-inv-sq (fe-sqr z-inv))
                     (z-inv-cu (fe-mul z-inv-sq z-inv)))
                (values (fe-mul (p256-point-x point) z-inv-sq)
                        (fe-mul (p256-point-y point) z-inv-cu)
                        1)))
        ;; Main loop, MSB to LSB.
        (loop for i from (1- bits) downto 0
              do (multiple-value-bind (dx dy dz)
                     (%p256-projective-double px py pz)
                   (multiple-value-bind (ax ay az)
                       (%p256-projective-add dx dy dz qx qy qz)
                     (let ((bit (ldb (byte 1 i) k)))
                       (multiple-value-setq (px py pz)
                         (%ct-select-projective bit ax ay az dx dy dz)))))))
      (if (zerop pz)
          (p256-neutral)
          (%p256-projective-to-jacobian px py pz)))))

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

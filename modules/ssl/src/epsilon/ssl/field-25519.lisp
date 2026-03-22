;;;; GF(2^255 - 19) Field Arithmetic for Curve25519
;;;;
;;;; Implements field arithmetic in GF(p) where p = 2^255 - 19.
;;;; Uses radix-2^51 representation with 5 limbs stored in a
;;;; (simple-array integer (5)). Each limb holds approximately
;;;; 51 bits, with overflow room for lazy reduction.
;;;;
;;;; This representation allows multiplication without carry
;;;; propagation during the multiply-accumulate phase. Carries
;;;; are resolved during reduction.

(defpackage epsilon.ssl.field-25519
  (:use :cl)
  (:export
   #:+p+
   #:fe
   #:fe-zero
   #:fe-one
   #:fe-from-integer
   #:fe-to-integer
   #:fe-from-bytes
   #:fe-to-bytes
   #:fe-add
   #:fe-sub
   #:fe-neg
   #:fe-mul
   #:fe-sqr
   #:fe-inv
   #:fe-pow
   #:fe-copy
   #:fe-equal
   #:fe-zero-p
   #:fe-select
   #:fe-neg-p
   #:fe-abs
   #:fe-sqrt))

(in-package :epsilon.ssl.field-25519)

;;; The prime p = 2^255 - 19
(defconstant +p+ (- (expt 2 255) 19))

;;; Limb mask for 51-bit limbs
(defconstant +limb-mask+ (1- (expt 2 51)))

;;; d = -121665/121666 mod p (Edwards curve constant)
;;; a = -1 (twisted Edwards)

;;; ---------------------------------------------------------------------------
;;; Field element representation
;;; ---------------------------------------------------------------------------

(deftype fe () '(simple-array integer (5)))

(defun fe-zero ()
  "Return the zero field element."
  (make-array 5 :element-type 'integer :initial-element 0))

(defun fe-one ()
  "Return the multiplicative identity."
  (let ((r (make-array 5 :element-type 'integer :initial-element 0)))
    (setf (aref r 0) 1)
    r))

(defun fe-copy (a)
  "Return a copy of field element A."
  (copy-seq a))

(defun fe-from-integer (n)
  "Convert an integer N (0 <= N < p) to a field element.
   Splits N into 5 radix-2^51 limbs."
  (let ((r (make-array 5 :element-type 'integer :initial-element 0))
        (x (mod n +p+)))
    (loop for i from 0 below 5
          do (setf (aref r i) (logand x +limb-mask+))
             (setf x (ash x -51)))
    r))

(defun fe-to-integer (a)
  "Convert a field element to an integer (fully reduced mod p)."
  (let ((r (fe-reduce-full a)))
    (+ (aref r 0)
       (ash (aref r 1) 51)
       (ash (aref r 2) 102)
       (ash (aref r 3) 153)
       (ash (aref r 4) 204))))

(defun fe-from-bytes (bytes)
  "Decode a 32-byte little-endian representation to a field element.
   Clears the high bit (bit 255) per RFC 7748."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (let ((n 0))
    (loop for i from 31 downto 0
          do (setf n (logior (ash n 8) (aref bytes i))))
    ;; Clear bit 255
    (setf n (logand n (1- (expt 2 255))))
    (fe-from-integer (mod n +p+))))

(defun fe-to-bytes (a)
  "Encode a field element as a 32-byte little-endian array."
  (let ((n (fe-to-integer a))
        (bytes (make-array 32 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 32
          do (setf (aref bytes i) (logand n #xFF))
             (setf n (ash n -8)))
    bytes))

;;; ---------------------------------------------------------------------------
;;; Carry and reduction
;;; ---------------------------------------------------------------------------

(defun fe-carry (h)
  "Propagate carries through limbs. Returns a new field element with
   each limb fitting in ~51 bits."
  (let ((r (make-array 5 :element-type 'integer :initial-element 0)))
    ;; Copy
    (loop for i from 0 below 5 do (setf (aref r i) (aref h i)))
    ;; Propagate carries 0->1->2->3->4->0
    (loop for i from 0 below 4
          for carry = (ash (aref r i) -51)
          do (setf (aref r i) (logand (aref r i) +limb-mask+))
             (incf (aref r (1+ i)) carry))
    ;; Final carry from limb 4 wraps around with factor 19
    ;; because 2^255 = 19 (mod p)
    (let ((carry (ash (aref r 4) -51)))
      (setf (aref r 4) (logand (aref r 4) +limb-mask+))
      (incf (aref r 0) (* 19 carry)))
    r))

(defun fe-reduce-full (a)
  "Fully reduce a field element to canonical form [0, p)."
  ;; First do carry propagation
  (let ((h (fe-carry a)))
    ;; May need one more carry propagation if limb 0 overflowed
    (setf h (fe-carry h))
    ;; Now compute h - p and select h if h >= p, else h
    ;; p = 2^255 - 19, in limb form:
    ;; limb 0 of p = 2^51 - 19 = 2251799813685229
    ;; limbs 1-4 of p = 2^51 - 1 = 2251799813685247
    (let ((t0 (- (aref h 0) (- (expt 2 51) 19)))
          (t1 (- (aref h 1) +limb-mask+))
          (t2 (- (aref h 2) +limb-mask+))
          (t3 (- (aref h 3) +limb-mask+))
          (t4 (- (aref h 4) +limb-mask+)))
      ;; Propagate borrows
      (when (minusp t0) (incf t0 (expt 2 51)) (decf t1))
      (when (minusp t1) (incf t1 (expt 2 51)) (decf t2))
      (when (minusp t2) (incf t2 (expt 2 51)) (decf t3))
      (when (minusp t3) (incf t3 (expt 2 51)) (decf t4))
      ;; If t4 >= 0, then h >= p, use h-p; otherwise use h
      (if (not (minusp t4))
          (let ((r (make-array 5 :element-type 'integer :initial-element 0)))
            (setf (aref r 0) t0)
            (setf (aref r 1) t1)
            (setf (aref r 2) t2)
            (setf (aref r 3) t3)
            (setf (aref r 4) t4)
            r)
          h))))

;;; ---------------------------------------------------------------------------
;;; Field arithmetic
;;; ---------------------------------------------------------------------------

(defun fe-add (a b)
  "Compute a + b in GF(p). Returns an unreduced field element."
  (let ((r (make-array 5 :element-type 'integer :initial-element 0)))
    (loop for i from 0 below 5
          do (setf (aref r i) (+ (aref a i) (aref b i))))
    r))

(defun fe-sub (a b)
  "Compute a - b in GF(p). Adds 2p first to avoid negative limbs."
  (let ((r (make-array 5 :element-type 'integer :initial-element 0)))
    ;; Add 2*p to avoid negatives:
    ;; p = [2^51-19, 2^51-1, 2^51-1, 2^51-1, 2^51-1] in 5x51-bit limbs
    ;; 2*p limb 0 = 2*(2^51 - 19) = 4503599627370458
    ;; 2*p limbs 1-4 = 2*(2^51 - 1) = 4503599627370494
    (setf (aref r 0) (- (+ (aref a 0) 4503599627370458) (aref b 0)))
    (setf (aref r 1) (- (+ (aref a 1) 4503599627370494) (aref b 1)))
    (setf (aref r 2) (- (+ (aref a 2) 4503599627370494) (aref b 2)))
    (setf (aref r 3) (- (+ (aref a 3) 4503599627370494) (aref b 3)))
    (setf (aref r 4) (- (+ (aref a 4) 4503599627370494) (aref b 4)))
    (fe-carry r)))

(defun fe-neg (a)
  "Compute -a in GF(p)."
  (fe-sub (fe-zero) a))

(defun fe-mul (a b)
  "Compute a * b in GF(p).
   Uses schoolbook multiplication with 5x5 -> 9 product terms,
   then reduces modulo p = 2^255 - 19."
  ;; Product: each limb pair (a[i] * b[j]) contributes to product limb (i+j).
  ;; Since 2^255 = 19 mod p, limbs at position >= 5 wrap around with factor 19.
  ;; Pre-multiply b-limbs at wrapped positions by 19 for efficiency.
  (let* ((a0 (aref a 0)) (a1 (aref a 1)) (a2 (aref a 2)) (a3 (aref a 3)) (a4 (aref a 4))
         (b0 (aref b 0)) (b1 (aref b 1)) (b2 (aref b 2)) (b3 (aref b 3)) (b4 (aref b 4))
         (b1-19 (* 19 b1)) (b2-19 (* 19 b2)) (b3-19 (* 19 b3)) (b4-19 (* 19 b4))
         ;; h[0] = a0*b0 + 19*(a1*b4 + a2*b3 + a3*b2 + a4*b1)
         (h0 (+ (* a0 b0) (* a1 b4-19) (* a2 b3-19) (* a3 b2-19) (* a4 b1-19)))
         ;; h[1] = a0*b1 + a1*b0 + 19*(a2*b4 + a3*b3 + a4*b2)
         (h1 (+ (* a0 b1) (* a1 b0) (* a2 b4-19) (* a3 b3-19) (* a4 b2-19)))
         ;; h[2] = a0*b2 + a1*b1 + a2*b0 + 19*(a3*b4 + a4*b3)
         (h2 (+ (* a0 b2) (* a1 b1) (* a2 b0) (* a3 b4-19) (* a4 b3-19)))
         ;; h[3] = a0*b3 + a1*b2 + a2*b1 + a3*b0 + 19*(a4*b4)
         (h3 (+ (* a0 b3) (* a1 b2) (* a2 b1) (* a3 b0) (* a4 b4-19)))
         ;; h[4] = a0*b4 + a1*b3 + a2*b2 + a3*b1 + a4*b0
         (h4 (+ (* a0 b4) (* a1 b3) (* a2 b2) (* a3 b1) (* a4 b0)))
         (h (make-array 5 :element-type 'integer)))
    (setf (aref h 0) h0)
    (setf (aref h 1) h1)
    (setf (aref h 2) h2)
    (setf (aref h 3) h3)
    (setf (aref h 4) h4)
    (fe-carry h)))

(defun fe-sqr (a)
  "Compute a^2 in GF(p). Optimized squaring using symmetry."
  (let* ((a0 (aref a 0)) (a1 (aref a 1)) (a2 (aref a 2)) (a3 (aref a 3)) (a4 (aref a 4))
         ;; Doubled cross terms
         (a0-2 (* 2 a0)) (a1-2 (* 2 a1))
         ;; Pre-multiplied by 19
         (a1-38 (* 38 a1)) (a2-38 (* 38 a2)) (a3-38 (* 38 a3)) (a4-19 (* 19 a4))
         ;; h[0] = a0^2 + 38*(a1*a4 + a2*a3)
         (h0 (+ (* a0 a0) (* a1-38 a4) (* a2-38 a3)))
         ;; h[1] = 2*a0*a1 + 38*a2*a4 + 19*a3^2
         (h1 (+ (* a0-2 a1) (* a2-38 a4) (* 19 a3 a3)))
         ;; h[2] = 2*a0*a2 + a1^2 + 38*a3*a4
         (h2 (+ (* a0-2 a2) (* a1 a1) (* a3-38 a4)))
         ;; h[3] = 2*a0*a3 + 2*a1*a2 + 19*a4^2
         (h3 (+ (* a0-2 a3) (* a1-2 a2) (* a4-19 a4)))
         ;; h[4] = 2*a0*a4 + 2*a1*a3 + a2^2
         (h4 (+ (* a0-2 a4) (* a1-2 a3) (* a2 a2)))
         (h (make-array 5 :element-type 'integer)))
    (setf (aref h 0) h0)
    (setf (aref h 1) h1)
    (setf (aref h 2) h2)
    (setf (aref h 3) h3)
    (setf (aref h 4) h4)
    (fe-carry h)))

(defun fe-pow (a exp)
  "Compute a^exp in GF(p) using square-and-multiply on integers.
   Converts to integer, exponentiates, converts back for efficiency."
  (let ((result 1)
        (base (mod (fe-to-integer a) +p+)))
    (loop while (plusp exp)
          do (when (oddp exp)
               (setf result (mod (* result base) +p+)))
             (setf base (mod (* base base) +p+))
             (setf exp (ash exp -1)))
    (fe-from-integer result)))

(defun fe-inv (a)
  "Compute a^(-1) in GF(p) using Fermat's little theorem: a^(p-2) mod p."
  (fe-pow a (- +p+ 2)))

(defun fe-equal (a b)
  "Test if field elements A and B are equal (after full reduction)."
  (= (fe-to-integer a) (fe-to-integer b)))

(defun fe-zero-p (a)
  "Test if field element A is zero."
  (zerop (fe-to-integer a)))

(defun fe-select (flag a b)
  "If FLAG is 1, return A; if FLAG is 0, return B.
   FLAG must be 0 or 1."
  (if (= flag 1) (fe-copy a) (fe-copy b)))

(defun fe-neg-p (a)
  "Test if the canonical representation of A is odd (i.e., 'negative').
   Used in point compression."
  (oddp (fe-to-integer a)))

(defun fe-abs (a)
  "Return the 'absolute value': if fe-neg-p, return -a, else a."
  (if (fe-neg-p a) (fe-neg a) (fe-copy a)))

(defun fe-sqrt (a)
  "Compute sqrt(a) in GF(p) if it exists, or nil.
   Since p = 5 mod 8, sqrt(a) = a^((p+3)/8) if a^((p-1)/2) = 1,
   or sqrt(a) = a^((p+3)/8) * sqrt(-1) where sqrt(-1) = 2^((p-1)/4).
   Returns (values root exists-p)."
  ;; For p = 2^255-19, p mod 8 = 5
  ;; Candidate root: r = a^((p+3)/8)
  ;; If r^2 = a, return r
  ;; If r^2 = -a, return r * 2^((p-1)/4)
  ;; Otherwise, a is not a square
  (let* ((exp-p3-8 (/ (+ +p+ 3) 8))
         (r (fe-pow a exp-p3-8))
         (check (fe-sqr r)))
    (cond
      ((fe-equal check a) (values r t))
      ((fe-equal check (fe-neg a))
       ;; Multiply by sqrt(-1) = 2^((p-1)/4)
       (let* ((sqrt-neg1 (fe-pow (fe-from-integer 2) (/ (1- +p+) 4)))
              (r2 (fe-mul r sqrt-neg1)))
         (values r2 t)))
      (t (values (fe-zero) nil)))))

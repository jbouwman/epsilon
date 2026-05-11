;;;; Modular Arithmetic
;;;;
;;;; General modular arithmetic operations using CL's native bignum support.
;;;; Provides the foundation for RSA and as a fallback for field-specific
;;;; implementations.
;;;;
;;;; Includes Montgomery multiplication for efficient modular exponentiation:
;;;; replaces expensive bignum division (in `mod`) with cheaper multiply-and-shift
;;;; via Montgomery reduction (REDC). Used by RSA private key operations.

(defpackage epsilon.crypto.modular
  (:use :cl)
  (:export
   #:mod-add
   #:mod-sub
   #:mod-neg
   #:mod-mul
   #:mod-sqr
   #:mod-inv
   #:mod-expt
   #:mod-expt-ct
   #:mod-div
   #:extended-gcd
   ;; Montgomery form
   #:mont-context
   #:make-mont-context
   #:mont-context-n
   #:mont-context-r-bits
   #:to-montgomery
   #:from-montgomery
   #:mont-mul
   #:mont-sqr
   #:mont-expt))

(in-package :epsilon.crypto.modular)

(declaim (inline mod-add mod-sub mod-neg mod-mul mod-sqr))

(defun mod-add (a b p)
  "Compute (a + b) mod p."
  (mod (+ a b) p))

(defun mod-sub (a b p)
  "Compute (a - b) mod p."
  (mod (- a b) p))

(defun mod-neg (a p)
  "Compute (-a) mod p."
  (if (zerop a) 0 (- p (mod a p))))

(defun mod-mul (a b p)
  "Compute (a * b) mod p."
  (mod (* a b) p))

(defun mod-sqr (a p)
  "Compute (a * a) mod p."
  (mod (* a a) p))

(defun extended-gcd (a b)
  "Compute the extended GCD of A and B.
   Returns (values gcd x y) such that a*x + b*y = gcd."
  (let ((old-r a) (r b)
        (old-s 1) (s 0)
        (old-t 0) (tt 1))
    (loop while (not (zerop r))
          do (let ((q (floor old-r r)))
               (psetf old-r r
                      r (- old-r (* q r)))
               (psetf old-s s
                      s (- old-s (* q s)))
               (psetf old-t tt
                      tt (- old-t (* q tt)))))
    (values old-r old-s old-t)))

(defun mod-inv (a p)
  "Compute the modular inverse of A mod P.
   Returns a^(-1) mod p. Signals an error if A is not invertible."
  (multiple-value-bind (g x) (extended-gcd (mod a p) p)
    (unless (= g 1)
      (error "~A is not invertible modulo ~A" a p))
    (mod x p)))

(defun mod-expt (base exponent modulus)
  "Compute (base ^ exponent) mod modulus.

   This implementation uses right-to-left square-and-multiply with a
   conditional multiply step that branches on the bits of EXPONENT.
   It is **not** constant-time and must not be used with secret
   exponents (RSA private d, d_p, d_q; ECDSA scalar; etc.). Use
   `mod-expt-ct` for those paths."
  (declare (type integer base exponent modulus))
  (if (minusp exponent)
      (mod-expt (mod-inv base modulus) (- exponent) modulus)
      ;; Use CL's built-in, which SBCL implements efficiently
      (let ((result 1)
            (b (mod base modulus)))
        (loop while (plusp exponent)
              do (when (oddp exponent)
                   (setf result (mod (* result b) modulus)))
                 (setf b (mod (* b b) modulus))
                 (setf exponent (ash exponent -1)))
        result)))

(defun mod-expt-ct (base exponent modulus &key bits)
  "Constant-time modular exponentiation: (BASE ^ EXPONENT) mod MODULUS.

   The arithmetic work performed is independent of the secret
   EXPONENT's bit pattern: every iteration computes both the
   square-only result and the square-and-multiply result, and
   selects between them based on the exponent bit using a branchless
   mask-and-XOR over CL bignums. The loop runs a fixed number of
   iterations governed by BITS (defaulting to the bit length of
   MODULUS), so the iteration count does not leak the bit length of
   EXPONENT.

   BITS may be passed explicitly when the secret exponent is known
   to be smaller than the modulus (e.g. RSA d_p / d_q, which are
   bounded by p-1 / q-1) to save iterations -- but only if the
   chosen BITS does not itself depend on the exponent's value.

   Caveats: this routine is *algorithmically* constant-time. The
   underlying bignum arithmetic in SBCL operates on word-sized
   chunks whose count depends on the bignum's actual magnitude
   rather than the formal modulus, so the routine is not provably
   constant-time at the byte level. For full defence-in-depth the
   caller should additionally use input blinding (see
   `rsa-decrypt`'s blinding path)."
  (declare (type integer base modulus)
           (type (integer 0) exponent))
  (assert (and (> modulus 1) (oddp modulus)) ()
          "mod-expt-ct requires odd modulus > 1, got ~A" modulus)
  (let* ((bits (or bits (integer-length modulus)))
         (result 1)
         (b (mod base modulus)))
    (loop for i from (1- bits) downto 0
          do (let* ((squared (mod (* result result) modulus))
                    (with-mul (mod (* squared b) modulus))
                    (bit-int (ldb (byte 1 i) exponent))
                    ;; Branchless select: mask = -bit-int gives all-ones
                    ;; when bit-int = 1 and zero when bit-int = 0. The
                    ;; XOR/AND/XOR sequence picks WITH-MUL when bit-int
                    ;; is 1 and SQUARED otherwise without an `if`.
                    (mask (- bit-int))
                    (delta (logxor with-mul squared)))
               (setf result (logxor squared (logand mask delta)))))
    result))

(defun mod-div (a b p)
  "Compute (a / b) mod p = a * b^(-1) mod p."
  (mod-mul a (mod-inv b p) p))

;;; ---------------------------------------------------------------------------
;;; Montgomery multiplication
;;;
;;; Montgomery form replaces the expensive `(mod (* a b) n)` -- which requires
;;; a full bignum division -- with REDC, which uses a precomputed n' and a
;;; right-shift by R. For a single multiply this is not a win, but for
;;; exponentiation (thousands of multiplies with the same modulus) the savings
;;; are substantial.
;;;
;;; Convention:
;;;   R = 2^r-bits where r-bits = (integer-length n)
;;;   n' = -n^(-1) mod R  (stored as n-prime)
;;;   Montgomery form of a: aR = (a * R) mod n
;;;   REDC(T) = T * R^(-1) mod n
;;;   mont-mul(aR, bR) = REDC(aR * bR) = abR mod n
;;; ---------------------------------------------------------------------------

(defstruct (mont-context (:constructor %make-mont-context))
  "Precomputed Montgomery multiplication context for a modulus N."
  (n 0 :type integer)           ; the modulus
  (r-bits 0 :type fixnum)       ; bit width: R = 2^r-bits
  (r 0 :type integer)           ; R = 2^r-bits
  (r-mask 0 :type integer)      ; R - 1 (for fast mod R via logand)
  (n-prime 0 :type integer)     ; -n^(-1) mod R
  (r-squared 0 :type integer))  ; R^2 mod n (for converting to Montgomery form)

(defun make-mont-context (n)
  "Create a Montgomery multiplication context for odd modulus N.
   N must be odd and > 1."
  (declare (type integer n))
  (assert (and (> n 1) (oddp n)) ()
          "Montgomery modulus must be odd and > 1, got ~A" n)
  (let* ((r-bits (integer-length n))
         (r (ash 1 r-bits))
         (r-mask (1- r))
         ;; Compute n' = -n^(-1) mod R using extended GCD
         ;; We need n' such that n * n' = -1 (mod R), i.e. n * n' + 1 = 0 (mod R)
         (n-prime (let ((inv (mod-inv (- n) r)))
                    ;; mod-inv may return a value in [0, R); ensure it's positive
                    (mod inv r)))
         ;; R^2 mod n -- used to convert integers to Montgomery form
         (r-squared (mod (* r r) n)))
    (%make-mont-context :n n :r-bits r-bits :r r :r-mask r-mask
                        :n-prime n-prime :r-squared r-squared)))

(defun redc (ctx t-val)
  "Montgomery reduction: compute T * R^(-1) mod N.
   T must be in range [0, N*R)."
  (declare (type mont-context ctx) (type integer t-val))
  (let* ((n (mont-context-n ctx))
         (r-mask (mont-context-r-mask ctx))
         (r-bits (mont-context-r-bits ctx))
         (n-prime (mont-context-n-prime ctx))
         ;; m = (T mod R) * n' mod R
         (m (logand (* (logand t-val r-mask) n-prime) r-mask))
         ;; t = (T + m*N) / R
         (result (ash (+ t-val (* m n)) (- r-bits))))
    (if (>= result n) (- result n) result)))

(defun to-montgomery (ctx a)
  "Convert integer A to Montgomery form: aR mod N."
  (declare (type mont-context ctx) (type integer a))
  (redc ctx (* (mod a (mont-context-n ctx)) (mont-context-r-squared ctx))))

(defun from-montgomery (ctx ar)
  "Convert from Montgomery form back to integer: aR * R^(-1) mod N = a."
  (declare (type mont-context ctx) (type integer ar))
  (redc ctx ar))

(defun mont-mul (ctx ar br)
  "Montgomery multiplication: given aR and bR, return abR mod N."
  (declare (type mont-context ctx) (type integer ar br))
  (redc ctx (* ar br)))

(defun mont-sqr (ctx ar)
  "Montgomery squaring: given aR, return a^2 R mod N."
  (declare (type mont-context ctx) (type integer ar))
  (redc ctx (* ar ar)))

(defun mont-expt (base exponent modulus)
  "Compute (base ^ exponent) mod modulus using Montgomery multiplication.
   Falls back to standard mod-expt for even moduli (Montgomery requires odd).
   More efficient than mod-expt for large odd moduli due to avoiding
   bignum division at each step."
  (declare (type integer base exponent modulus))
  (when (minusp exponent)
    (return-from mont-expt (mont-expt (mod-inv base modulus) (- exponent) modulus)))
  (when (zerop exponent)
    (return-from mont-expt (if (= modulus 1) 0 1)))
  ;; Montgomery requires odd modulus; fall back for even
  (when (evenp modulus)
    (return-from mont-expt (mod-expt base exponent modulus)))
  (let* ((ctx (make-mont-context modulus))
         (base-m (to-montgomery ctx (mod base modulus)))
         (result base-m)
         (exp-bits (integer-length exponent)))
    ;; Left-to-right binary method with Montgomery multiply.
    ;; Process from second-highest bit down (MSB is always 1 for positive exponent).
    (loop for i from (- exp-bits 2) downto 0
          do (setf result (mont-sqr ctx result))
             (when (logbitp i exponent)
               (setf result (mont-mul ctx result base-m))))
    (from-montgomery ctx result)))

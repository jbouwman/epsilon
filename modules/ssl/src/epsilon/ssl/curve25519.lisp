;;;; Curve25519 Montgomery Ladder (X25519)
;;;;
;;;; Implements X25519 Diffie-Hellman key exchange per RFC 7748.
;;;; Uses Montgomery ladder for constant-time scalar multiplication
;;;; on Curve25519: y^2 = x^3 + 486662*x^2 + x over GF(2^255-19).
;;;;
;;;; Internally uses CL integer arithmetic with explicit mod for
;;;; performance. The field element (limbed) representation from
;;;; field-25519 is used only for encoding/decoding.

(defpackage epsilon.ssl.curve25519
  (:use :cl)
  (:import-from #:epsilon.ssl.fe-25519
   #:+p-25519+ #:fe-add #:fe-sub #:fe-mul #:fe-sqr #:fe-inv)
  (:export
   #:x25519
   #:x25519-base
   #:+base-point-u+))

(in-package :epsilon.ssl.curve25519)

;;; The prime p = 2^255 - 19
(defconstant +p+ +p-25519+)

;;; (A+2)/4 = 121666 where A = 486662
(defconstant +a24+ 121666)

;;; Base point u-coordinate = 9
(defconstant +base-point-u+ 9)

(defun clamp-scalar (k)
  "Clamp a 32-byte scalar per RFC 7748 Section 5."
  (declare (type (simple-array (unsigned-byte 8) (32)) k))
  (let ((clamped (copy-seq k)))
    (setf (aref clamped 0) (logand (aref clamped 0) 248))
    (setf (aref clamped 31) (logior (logand (aref clamped 31) 127) 64))
    clamped))

(defun decode-u-coordinate (bytes)
  "Decode a 32-byte little-endian u-coordinate, masking bit 255."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (let ((u 0))
    (loop for i from 31 downto 0
          do (setf u (logior (ash u 8) (aref bytes i))))
    (mod (logand u (1- (expt 2 255))) +p+)))

(defun decode-scalar (bytes)
  "Decode a 32-byte little-endian scalar to an integer."
  (declare (type (simple-array (unsigned-byte 8) (32)) bytes))
  (let ((n 0))
    (loop for i from 31 downto 0
          do (setf n (logior (ash n 8) (aref bytes i))))
    n))

(defun encode-u-coordinate (n)
  "Encode an integer as 32-byte little-endian."
  (let ((bytes (make-array 32 :element-type '(unsigned-byte 8)))
        (x (mod n +p+)))
    (loop for i from 0 below 32
          do (setf (aref bytes i) (logand x #xFF))
             (setf x (ash x -8)))
    bytes))

(defun x25519 (k-bytes u-bytes)
  "Compute X25519(k, u) per RFC 7748 Section 5.
   K-BYTES is a 32-byte scalar (private key).
   U-BYTES is a 32-byte u-coordinate (public key / base point).
   Returns a 32-byte shared secret.

   Uses the Montgomery ladder: constant-time scalar multiplication."
  (declare (type (simple-array (unsigned-byte 8) (32)) k-bytes u-bytes))
  (let* ((k-clamped (clamp-scalar k-bytes))
         (k (decode-scalar k-clamped))
         (u (decode-u-coordinate u-bytes))
         ;; Montgomery ladder: (x_2:z_2) = (1:0), (x_3:z_3) = (u:1)
         (x-2 1)
         (z-2 0)
         (x-3 u)
         (z-3 1))
    ;; Process bits 254 down to 0
    (loop for bit-pos from 254 downto 0
          for bit = (logand (ash k (- bit-pos)) 1)
          do
             ;; Conditional swap
             (when (= bit 1)
               (rotatef x-2 x-3)
               (rotatef z-2 z-3))
             ;; Montgomery ladder step
             (let* ((a (fe-add x-2 z-2))
                    (b (fe-sub x-2 z-2))
                    (c (fe-add x-3 z-3))
                    (d (fe-sub x-3 z-3))
                    (da (fe-mul d a))
                    (cb (fe-mul c b))
                    (aa (fe-sqr a))
                    (bb (fe-sqr b))
                    (e (fe-sub aa bb))
                    (new-x3 (fe-sqr (fe-add da cb)))
                    (new-z3 (fe-mul u (fe-sqr (fe-sub da cb))))
                    (new-x2 (fe-mul aa bb))
                    ;; z_2 = E * (BB + a24*E)
                    ;; Note: uses BB (not AA). With a24=(A+2)/4:
                    ;; BB + a24*E = (X-Z)^2 + (A+2)XZ = X^2+AXZ+Z^2
                    (new-z2 (fe-mul e (fe-add bb (fe-mul +a24+ e)))))
               (setf x-2 new-x2
                     z-2 new-z2
                     x-3 new-x3
                     z-3 new-z3))
             ;; Conditional swap back
             (when (= bit 1)
               (rotatef x-2 x-3)
               (rotatef z-2 z-3)))
    ;; Recover affine: result = x_2 * z_2^(-1) mod p
    (encode-u-coordinate (fe-mul x-2 (fe-inv z-2)))))

(defun x25519-base (k-bytes)
  "Compute X25519(k, 9) -- scalar multiplication with the base point.
   K-BYTES is a 32-byte scalar (private key).
   Returns a 32-byte public key."
  (declare (type (simple-array (unsigned-byte 8) (32)) k-bytes))
  (let ((base (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref base 0) 9)
    (x25519 k-bytes base)))

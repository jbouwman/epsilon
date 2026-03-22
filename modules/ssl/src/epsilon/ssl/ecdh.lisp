;;;; ECDH Key Exchange
;;;;
;;;; Implements Elliptic Curve Diffie-Hellman key exchange over P-256.
;;;; X25519 ECDH is in curve25519.lisp; this module provides ECDH-P256.

(defpackage epsilon.ssl.ecdh
  (:use :cl)
  (:local-nicknames
   (#:ec #:epsilon.ssl.ec-p256)
   (#:drbg #:epsilon.ssl.drbg))
  (:export
   #:ecdh-p256-generate-keypair
   #:ecdh-p256-shared-secret
   #:ecdh-p256-public-key-from-private))

(in-package :epsilon.ssl.ecdh)

(defun ecdh-p256-public-key-from-private (private-key)
  "Derive P-256 public key (EC point) from integer private key."
  (ec:p256-scalar-mul private-key (ec:p256-base-point)))

(defun ecdh-p256-generate-keypair ()
  "Generate an ECDH-P256 keypair.
   Returns (values private-key public-key) where private-key is an integer
   in [1, n-1] and public-key is a P-256 point."
  (let ((d (1+ (drbg:random-integer (1- ec:+n+)))))
    (values d (ecdh-p256-public-key-from-private d))))

(defun ecdh-p256-shared-secret (private-key peer-public-key)
  "Compute ECDH shared secret given our PRIVATE-KEY (integer) and
   PEER-PUBLIC-KEY (P-256 point).
   Returns the x-coordinate of the shared point as a 32-byte big-endian array.
   Returns NIL if the result is the point at infinity."
  ;; Validate peer public key is on curve
  (unless (ec:p256-on-curve-p peer-public-key)
    (error "ECDH-P256: peer public key is not on curve"))
  (let ((shared-point (ec:p256-scalar-mul private-key peer-public-key)))
    ;; Check for point at infinity (small subgroup attack)
    (when (ec::p256-point-is-neutral shared-point)
      (return-from ecdh-p256-shared-secret nil))
    ;; Extract x-coordinate
    (multiple-value-bind (x y) (ec::p256-point-to-affine shared-point)
      (declare (ignore y))
      ;; Encode x as 32-byte big-endian
      (let ((bytes (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
        (loop for i from 31 downto 0
              for val = x then (ash val -8)
              do (setf (aref bytes i) (logand val #xFF)))
        bytes))))

;;;; ECDH Key Exchange (NIST P-256 / P-384 / P-521)
;;;;
;;;; X25519 ECDH lives in curve25519.lisp; this module covers the NIST
;;;; prime curves used by FIPS-compliant TLS deployments. The shared
;;;; secret is the big-endian X coordinate of the agreed point, padded
;;;; to the curve's coordinate width (32 / 48 / 66 bytes).

(defpackage epsilon.crypto.ecdh
  (:use :cl)
  (:import
   (epsilon.crypto.ec-p256 ec)
   (epsilon.crypto.ec-p384 ec384)
   (epsilon.crypto.ec-p521 ec521)
   (epsilon.crypto.drbg drbg))
  (:export
   ;; P-256
   #:ecdh-p256-generate-keypair
   #:ecdh-p256-shared-secret
   #:ecdh-p256-public-key-from-private
   ;; P-384
   #:ecdh-p384-generate-keypair
   #:ecdh-p384-shared-secret
   #:ecdh-p384-public-key-from-private
   ;; P-521
   #:ecdh-p521-generate-keypair
   #:ecdh-p521-shared-secret
   #:ecdh-p521-public-key-from-private))

(in-package :epsilon.crypto.ecdh)

(defun %x-to-be-bytes (x width)
  "Encode the affine X coordinate as WIDTH big-endian bytes."
  (let ((bytes (make-array width :element-type '(unsigned-byte 8)
                           :initial-element 0)))
    (loop for i from (1- width) downto 0
          for val = x then (ash val -8)
          do (setf (aref bytes i) (logand val #xFF)))
    bytes))

;;; ---------------------------------------------------------------------------
;;; ECDH-P256
;;; ---------------------------------------------------------------------------

(defun ecdh-p256-public-key-from-private (private-key)
  "Derive the P-256 public point from an integer private scalar."
  (ec:p256-scalar-mul private-key (ec:p256-base-point)))

(defun ecdh-p256-generate-keypair ()
  "Generate an ECDH-P256 keypair. Returns (values private-key public-point)."
  (let ((d (1+ (drbg:random-integer (1- ec:+n+)))))
    (values d (ecdh-p256-public-key-from-private d))))

(defun ecdh-p256-shared-secret (private-key peer-public-key)
  "Compute the ECDH-P256 shared secret. Returns the 32-byte big-endian
   X coordinate of d * Q, or NIL if the result is the neutral point."
  (unless (ec:p256-on-curve-p peer-public-key)
    (error "ECDH-P256: peer public key is not on curve"))
  (let ((shared-point (ec:p256-scalar-mul private-key peer-public-key)))
    (when (ec::p256-point-is-neutral shared-point)
      (return-from ecdh-p256-shared-secret nil))
    (multiple-value-bind (x y) (ec::p256-point-to-affine shared-point)
      (declare (ignore y))
      (%x-to-be-bytes x 32))))

;;; ---------------------------------------------------------------------------
;;; ECDH-P384
;;; ---------------------------------------------------------------------------

(defun ecdh-p384-public-key-from-private (private-key)
  (ec384:p384-scalar-mul private-key (ec384:p384-base-point)))

(defun ecdh-p384-generate-keypair ()
  "Generate an ECDH-P384 keypair. Returns (values private-key public-point)."
  (let ((d (1+ (drbg:random-integer (1- ec384:+n+)))))
    (values d (ecdh-p384-public-key-from-private d))))

(defun ecdh-p384-shared-secret (private-key peer-public-key)
  "Compute the ECDH-P384 shared secret. Returns the 48-byte big-endian
   X coordinate of d * Q, or NIL if the result is neutral."
  (unless (ec384:p384-on-curve-p peer-public-key)
    (error "ECDH-P384: peer public key is not on curve"))
  (let ((shared-point (ec384:p384-scalar-mul private-key peer-public-key)))
    (when (ec384:p384-point-is-neutral shared-point)
      (return-from ecdh-p384-shared-secret nil))
    (multiple-value-bind (x y) (ec384:p384-point-to-affine shared-point)
      (declare (ignore y))
      (%x-to-be-bytes x ec384:+coordinate-bytes+))))

;;; ---------------------------------------------------------------------------
;;; ECDH-P521
;;; ---------------------------------------------------------------------------

(defun ecdh-p521-public-key-from-private (private-key)
  (ec521:p521-scalar-mul private-key (ec521:p521-base-point)))

(defun ecdh-p521-generate-keypair ()
  "Generate an ECDH-P521 keypair. Returns (values private-key public-point)."
  (let ((d (1+ (drbg:random-integer (1- ec521:+n+)))))
    (values d (ecdh-p521-public-key-from-private d))))

(defun ecdh-p521-shared-secret (private-key peer-public-key)
  "Compute the ECDH-P521 shared secret. Returns the 66-byte big-endian
   X coordinate of d * Q, or NIL if the result is neutral."
  (unless (ec521:p521-on-curve-p peer-public-key)
    (error "ECDH-P521: peer public key is not on curve"))
  (let ((shared-point (ec521:p521-scalar-mul private-key peer-public-key)))
    (when (ec521:p521-point-is-neutral shared-point)
      (return-from ecdh-p521-shared-secret nil))
    (multiple-value-bind (x y) (ec521:p521-point-to-affine shared-point)
      (declare (ignore y))
      (%x-to-be-bytes x ec521:+coordinate-bytes+))))

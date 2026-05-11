;;;; X25519+ML-KEM-768 hybrid KEM
;;;;
;;;; Combines classical X25519 and post-quantum ML-KEM-768 into a
;;;; single KEM whose security degrades gracefully: as long as EITHER
;;;; component remains secure, the combined shared secret is secret.
;;;; This is the "hybrid" construction TLS 1.3 is adopting as a
;;;; transition path toward post-quantum key exchange.
;;;;
;;;; Named group: X25519MLKEM768 (IANA codepoint 0x11EC).
;;;; References:
;;;;   draft-kwiatkowski-tls-ecdhe-mlkem-02
;;;;   draft-ietf-tls-ecdhe-mlkem-00 (WG adoption of the above)
;;;;
;;;; Wire format (both shares place the ML-KEM component first):
;;;;   client KeyShareEntry = ML-KEM-768 ek (1184) || X25519 pk (32)   = 1216 bytes
;;;;   server KeyShareEntry = ML-KEM-768 ct (1088) || X25519 pk (32)   = 1120 bytes
;;;;   shared secret        = ML-KEM-768 ss (32)  || X25519 ss (32)    = 64 bytes
;;;;
;;;; This module implements the primitive operations -- `hybrid-keygen`,
;;;; `hybrid-encaps`, `hybrid-decaps` -- using the same combined byte
;;;; layout the TLS 1.3 draft specifies. The actual ClientHello /
;;;; ServerHello extension plumbing lives in the tls13 module and is a
;;;; separate followup.

(defpackage epsilon.crypto.ml-kem-hybrid
  (:use :cl)
  (:import (epsilon.crypto.ml-kem ml-kem)
            (epsilon.crypto.curve25519 x25519)
            (epsilon.crypto.drbg drbg))
  (:import-from :epsilon.crypto.primitives #:ensure-byte-vector)
  (:export
   #:+public-key-length+
   #:+private-key-length+
   #:+ciphertext-length+
   #:+shared-secret-length+
   #:+tls-codepoint+
   #:hybrid-keygen
   #:hybrid-encaps
   #:hybrid-decaps
   #:hybrid-keygen-internal
   #:hybrid-encaps-internal
   #:hybrid-error))

(in-package :epsilon.crypto.ml-kem-hybrid)

;;; ---------------------------------------------------------------------------
;;; Byte layouts
;;; ---------------------------------------------------------------------------

(defconstant +mlkem-ek-length+ 1184)
(defconstant +mlkem-dk-length+ 2400)
(defconstant +mlkem-ciphertext-length+ 1088)
(defconstant +mlkem-ss-length+ 32)

(defconstant +x25519-key-length+ 32)
(defconstant +x25519-ss-length+ 32)

(defconstant +public-key-length+  (+ +mlkem-ek-length+ +x25519-key-length+))  ; 1216
(defconstant +private-key-length+ (+ +mlkem-dk-length+ +x25519-key-length+))  ; 2432
(defconstant +ciphertext-length+  (+ +mlkem-ciphertext-length+ +x25519-key-length+)) ; 1120
(defconstant +shared-secret-length+ (+ +mlkem-ss-length+ +x25519-ss-length+)) ; 64

(defconstant +tls-codepoint+ #x11EC
  "IANA TLS SupportedGroup codepoint for X25519MLKEM768.")

(define-condition hybrid-error (error)
  ((message :initarg :message :reader hybrid-error-message))
  (:report (lambda (c s) (format s "hybrid KEM: ~A" (hybrid-error-message c)))))

(defun hybrid-error (fmt &rest args)
  (error 'hybrid-error :message (apply #'format nil fmt args)))

;;; ---------------------------------------------------------------------------
;;; Key generation
;;; ---------------------------------------------------------------------------

(defun hybrid-keygen-internal (ml-kem-d ml-kem-z x25519-priv)
  "Deterministic variant of hybrid-keygen. Takes the three seeds
   explicitly (two 32-byte values for ML-KEM.KeyGen_internal and one
   32-byte X25519 private key).

   Returns (values public-key private-key) where
     public-key  = ML-KEM-768 ek (1184) || X25519 pk (32)  = 1216 bytes
     private-key = ML-KEM-768 dk (2400) || X25519 sk (32)  = 2432 bytes"
  (let ((ml-kem-d (ensure-byte-vector ml-kem-d))
        (ml-kem-z (ensure-byte-vector ml-kem-z))
        (x25519-priv (ensure-byte-vector x25519-priv)))
    (unless (= (length x25519-priv) +x25519-key-length+)
      (hybrid-error "x25519 private key must be ~D bytes (got ~D)"
                    +x25519-key-length+ (length x25519-priv)))
    (multiple-value-bind (mlkem-ek mlkem-dk)
        (ml-kem:keygen-internal ml-kem-d ml-kem-z)
      (let* ((x25519-pub (x25519:x25519-base x25519-priv))
             (public (make-array +public-key-length+
                                  :element-type '(unsigned-byte 8)))
             (private (make-array +private-key-length+
                                   :element-type '(unsigned-byte 8))))
        (replace public mlkem-ek)
        (replace public x25519-pub :start1 +mlkem-ek-length+)
        (replace private mlkem-dk)
        (replace private x25519-priv :start1 +mlkem-dk-length+)
        (values public private)))))

(defun hybrid-keygen ()
  "Generate a fresh hybrid key pair. Returns (values public-key
   private-key) with the concatenated byte layout described on
   `hybrid-keygen-internal`."
  (hybrid-keygen-internal (drbg:random-bytes 32)
                          (drbg:random-bytes 32)
                          (drbg:random-bytes 32)))

;;; ---------------------------------------------------------------------------
;;; Encapsulation
;;; ---------------------------------------------------------------------------

(defun hybrid-encaps-internal (public-key ml-kem-m x25519-eph-priv)
  "Deterministic variant of hybrid-encaps. ML-KEM-M is the 32-byte
   seed fed to ML-KEM.Encaps_internal; X25519-EPH-PRIV is the
   encapsulator's ephemeral 32-byte X25519 private key (which is
   discarded after use).

   Returns (values shared-secret ciphertext) where
     shared-secret = ML-KEM-768 ss (32) || X25519 ss (32) = 64 bytes
     ciphertext    = ML-KEM-768 ct (1088) || X25519 eph pk (32) = 1120 bytes"
  (let ((public-key (ensure-byte-vector public-key))
        (ml-kem-m (ensure-byte-vector ml-kem-m))
        (x25519-eph-priv (ensure-byte-vector x25519-eph-priv)))
    (unless (= (length public-key) +public-key-length+)
      (hybrid-error "public key must be ~D bytes (got ~D)"
                    +public-key-length+ (length public-key)))
    (unless (= (length x25519-eph-priv) +x25519-key-length+)
      (hybrid-error "x25519 ephemeral private must be ~D bytes (got ~D)"
                    +x25519-key-length+ (length x25519-eph-priv)))
    (let* ((mlkem-ek (subseq public-key 0 +mlkem-ek-length+))
           (x25519-peer-pub (subseq public-key +mlkem-ek-length+
                                    +public-key-length+)))
      (multiple-value-bind (mlkem-ss mlkem-ct)
          (ml-kem:encaps-internal mlkem-ek ml-kem-m)
        (let* ((x25519-eph-pub (x25519:x25519-base x25519-eph-priv))
               (x25519-ss (x25519:x25519 x25519-eph-priv x25519-peer-pub))
               (shared-secret (make-array +shared-secret-length+
                                           :element-type '(unsigned-byte 8)))
               (ciphertext (make-array +ciphertext-length+
                                        :element-type '(unsigned-byte 8))))
          (replace shared-secret mlkem-ss)
          (replace shared-secret x25519-ss :start1 +mlkem-ss-length+)
          (replace ciphertext mlkem-ct)
          (replace ciphertext x25519-eph-pub :start1 +mlkem-ciphertext-length+)
          (values shared-secret ciphertext))))))

(defun hybrid-encaps (public-key)
  "Encapsulate a fresh shared secret to the holder of the private key
   corresponding to PUBLIC-KEY. Returns (values shared-secret
   ciphertext)."
  (hybrid-encaps-internal public-key
                          (drbg:random-bytes 32)
                          (drbg:random-bytes 32)))

;;; ---------------------------------------------------------------------------
;;; Decapsulation
;;; ---------------------------------------------------------------------------

(defun hybrid-decaps (private-key ciphertext)
  "Recover the 64-byte hybrid shared secret from CIPHERTEXT using
   PRIVATE-KEY (the output of hybrid-keygen).

   The ML-KEM-768 half uses implicit rejection on failure (FIPS 203
   §6.3): a tampered ML-KEM ciphertext yields a pseudo-random
   ML-KEM-side shared secret rather than signalling an error. The
   X25519 half always produces some shared secret because X25519
   never rejects (it does not have an equivalent of the FO check).

   The hybrid KEM's security argument rests on the fact that the
   final secret concatenates both halves: even if an attacker
   substitutes a bad ML-KEM ciphertext and observes the rejection
   branch, they still cannot predict the X25519 half of the output
   without the peer's X25519 private key."
  (let ((private-key (ensure-byte-vector private-key))
        (ciphertext (ensure-byte-vector ciphertext)))
    (unless (= (length private-key) +private-key-length+)
      (hybrid-error "private key must be ~D bytes (got ~D)"
                    +private-key-length+ (length private-key)))
    (unless (= (length ciphertext) +ciphertext-length+)
      (hybrid-error "ciphertext must be ~D bytes (got ~D)"
                    +ciphertext-length+ (length ciphertext)))
    (let* ((mlkem-dk (subseq private-key 0 +mlkem-dk-length+))
           (x25519-sk (subseq private-key +mlkem-dk-length+
                              +private-key-length+))
           (mlkem-ct (subseq ciphertext 0 +mlkem-ciphertext-length+))
           (x25519-peer-pub (subseq ciphertext +mlkem-ciphertext-length+
                                    +ciphertext-length+))
           (mlkem-ss (ml-kem:decaps mlkem-dk mlkem-ct))
           (x25519-ss (x25519:x25519 x25519-sk x25519-peer-pub))
           (out (make-array +shared-secret-length+
                             :element-type '(unsigned-byte 8))))
      (replace out mlkem-ss)
      (replace out x25519-ss :start1 +mlkem-ss-length+)
      out)))

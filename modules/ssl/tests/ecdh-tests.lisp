;;;; Tests for ECDH key exchange

(defpackage epsilon.ssl.ecdh-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:ecdh #:epsilon.ssl.ecdh)
   (#:ec #:epsilon.ssl.ec-p256)
   (#:x25519 #:epsilon.ssl.curve25519))
  (:enter t))

(in-package :epsilon.ssl.ecdh-tests)

;;; ECDH-P256 round-trip
(deftest test-ecdh-p256-roundtrip
  "ECDH-P256 key agreement produces matching shared secrets"
  ;; Alice and Bob with known private keys
  (let* ((d-alice #xc9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721)
         (d-bob   #x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef)
         (pub-alice (ecdh:ecdh-p256-public-key-from-private d-alice))
         (pub-bob   (ecdh:ecdh-p256-public-key-from-private d-bob))
         ;; Alice computes shared secret with Bob's public key
         (secret-alice (ecdh:ecdh-p256-shared-secret d-alice pub-bob))
         ;; Bob computes shared secret with Alice's public key
         (secret-bob (ecdh:ecdh-p256-shared-secret d-bob pub-alice)))
    ;; Both should derive the same shared secret
    (assert-= (length secret-alice) 32)
    (assert-= (length secret-bob) 32)
    (assert-equalp secret-alice secret-bob)))

;;; Public keys are on curve
(deftest test-ecdh-p256-pubkeys-on-curve
  "ECDH-P256 derived public keys are on curve"
  (let* ((d1 #xc9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721)
         (d2 #x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef)
         (pub1 (ecdh:ecdh-p256-public-key-from-private d1))
         (pub2 (ecdh:ecdh-p256-public-key-from-private d2)))
    (assert-true (ec:p256-on-curve-p pub1))
    (assert-true (ec:p256-on-curve-p pub2))))

;;; Shared secret is non-trivial
(deftest test-ecdh-p256-shared-secret-nontrivial
  "Shared secret is not all zeros"
  (let* ((d-alice #xc9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721)
         (d-bob   #x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef)
         (pub-bob (ecdh:ecdh-p256-public-key-from-private d-bob))
         (secret (ecdh:ecdh-p256-shared-secret d-alice pub-bob)))
    (assert-true (some #'plusp secret))))

;;; Different key pairs produce different shared secrets
(deftest test-ecdh-p256-different-pairs
  "Different key pairs produce different shared secrets"
  (let* ((d-alice #xc9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721)
         (d-bob   #x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef)
         (d-carol #xfedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210)
         (pub-bob   (ecdh:ecdh-p256-public-key-from-private d-bob))
         (pub-carol (ecdh:ecdh-p256-public-key-from-private d-carol))
         (secret-ab (ecdh:ecdh-p256-shared-secret d-alice pub-bob))
         (secret-ac (ecdh:ecdh-p256-shared-secret d-alice pub-carol)))
    (assert-not (equalp secret-ab secret-ac))))

;;; X25519 round-trip (verifying existing implementation)
(deftest test-x25519-ecdh-roundtrip
  "X25519 ECDH key agreement round-trip"
  (let* ((sk-alice (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
         (sk-bob   (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Set known private keys
    (setf (aref sk-alice 0) 77 (aref sk-alice 31) 64)
    (setf (aref sk-bob 0) 88 (aref sk-bob 31) 64)
    ;; Derive public keys
    (let* ((pub-alice (x25519:x25519-base sk-alice))
           (pub-bob   (x25519:x25519-base sk-bob))
           ;; Compute shared secrets
           (secret-alice (x25519:x25519 sk-alice pub-bob))
           (secret-bob   (x25519:x25519 sk-bob pub-alice)))
      (assert-= (length secret-alice) 32)
      (assert-= (length secret-bob) 32)
      (assert-equalp secret-alice secret-bob))))

;;;; Key import/export round-trip tests
;;;;
;;;; Exercises every (key-type x format) combination supported by
;;;; `key-to-pem' / `key-from-pem' / `key-from-der' so format mismatches
;;;; (e.g. PKCS#1 vs PKCS#8 RSA, SEC1 vs PKCS#8 EC) are caught at the
;;;; library level rather than only surfacing in production. See
;;;; manual/implement/325_crypto-library-completeness.md finding #12.

(defpackage epsilon.crypto.tests.key-roundtrip
  (:use :cl :epsilon.test)
  (:import (epsilon.crypto.native native)
           (epsilon.crypto.pem pem)))

(in-package :epsilon.crypto.tests.key-roundtrip)

(defun %sign-verify-equiv-p (original imported)
  "Sign with ORIGINAL, verify with IMPORTED (and vice versa). Returns T
   when both sign/verify directions agree -- i.e. the imported key still
   represents the same signing key as the original."
  (let ((msg (map '(vector (unsigned-byte 8)) #'char-code "round trip")))
    (and (native:verify-message imported msg
                                (native:sign-message original msg))
         (native:verify-message original msg
                                (native:sign-message imported msg)))))

(defun %public-from (key)
  "Return a public-only native-key derived from a private KEY. Imports
   the SPKI export of KEY back through `key-from-pem' so we round-trip
   the public path too."
  (native:key-from-pem (native:key-to-pem key)))

;;; ---------------------------------------------------------------------------
;;; Ed25519
;;; ---------------------------------------------------------------------------

(deftest test-ed25519-pem-pkcs8-roundtrip
  "Ed25519 private key round-trips through PKCS#8 PEM"
  (let* ((k (native:generate-ed25519-key))
         (pem (native:key-to-pem k :private-p t :format :pkcs8))
         (k2 (native:key-from-pem pem)))
    (assert-equal :ed25519 (native:native-key-type k2))
    (assert-true (native:native-key-private-p k2))
    ;; PEM label should be the PKCS#8 generic "PRIVATE KEY" form
    (assert-equal "PRIVATE KEY" (pem:pem-block-label (pem:pem-decode pem)))
    (assert-true (%sign-verify-equiv-p k k2))))

(deftest test-ed25519-pem-auto-defaults-to-pkcs8
  "Ed25519 :auto format uses PKCS#8 (the only supported container)"
  (let* ((k (native:generate-ed25519-key))
         (pem (native:key-to-pem k :private-p t)))
    (assert-equal "PRIVATE KEY" (pem:pem-block-label (pem:pem-decode pem)))))

(deftest test-ed25519-public-key-roundtrip
  "Ed25519 public key round-trips through SPKI PEM"
  (let* ((k (native:generate-ed25519-key))
         (k-pub (%public-from k)))
    (assert-equal :ed25519 (native:native-key-type k-pub))
    (assert-not (native:native-key-private-p k-pub))
    ;; Should still verify a signature made with the private key
    (let ((msg (map '(vector (unsigned-byte 8)) #'char-code "hi")))
      (assert-true (native:verify-message k-pub msg
                                          (native:sign-message k msg))))))

;;; ---------------------------------------------------------------------------
;;; EC P-256
;;; ---------------------------------------------------------------------------

(deftest test-ec-p256-pem-pkcs8-roundtrip
  "EC P-256 private key round-trips through PKCS#8 PEM"
  (let* ((k (native:generate-ec-p256-key))
         (pem (native:key-to-pem k :private-p t :format :pkcs8))
         (k2 (native:key-from-pem pem)))
    (assert-equal :ec-p256 (native:native-key-type k2))
    (assert-true (native:native-key-private-p k2))
    (assert-equal "PRIVATE KEY" (pem:pem-block-label (pem:pem-decode pem)))
    (assert-true (%sign-verify-equiv-p k k2))))

(deftest test-ec-p256-pem-sec1-roundtrip
  "EC P-256 private key round-trips through SEC1 PEM"
  (let* ((k (native:generate-ec-p256-key))
         (pem (native:key-to-pem k :private-p t :format :sec1))
         (k2 (native:key-from-pem pem)))
    (assert-equal :ec-p256 (native:native-key-type k2))
    (assert-true (native:native-key-private-p k2))
    (assert-equal "EC PRIVATE KEY" (pem:pem-block-label (pem:pem-decode pem)))
    (assert-true (%sign-verify-equiv-p k k2))))

(deftest test-ec-p256-pem-auto-defaults-to-pkcs8
  "EC P-256 :auto format uses PKCS#8"
  (let* ((k (native:generate-ec-p256-key))
         (pem (native:key-to-pem k :private-p t)))
    (assert-equal "PRIVATE KEY" (pem:pem-block-label (pem:pem-decode pem)))))

(deftest test-ec-p256-public-key-roundtrip
  "EC P-256 public key round-trips through SPKI PEM"
  (let* ((k (native:generate-ec-p256-key))
         (k-pub (%public-from k)))
    (assert-equal :ec-p256 (native:native-key-type k-pub))
    (assert-not (native:native-key-private-p k-pub))
    (let ((msg (map '(vector (unsigned-byte 8)) #'char-code "hi")))
      (assert-true (native:verify-message k-pub msg
                                          (native:sign-message k msg))))))

;;; ---------------------------------------------------------------------------
;;; RSA
;;;
;;; Use a small key for speed -- the goal is format coverage, not key
;;; strength. `generate-rsa-key' accepts a :bits keyword.
;;; ---------------------------------------------------------------------------

(deftest test-rsa-pem-pkcs1-roundtrip
  "RSA private key round-trips through PKCS#1 (`RSA PRIVATE KEY`) PEM"
  (let* ((k (native:generate-rsa-key :bits 2048))
         (pem (native:key-to-pem k :private-p t :format :pkcs1))
         (k2 (native:key-from-pem pem)))
    (assert-equal :rsa (native:native-key-type k2))
    (assert-true (native:native-key-private-p k2))
    (assert-equal "RSA PRIVATE KEY"
                  (pem:pem-block-label (pem:pem-decode pem)))
    (assert-true (%sign-verify-equiv-p k k2))))

(deftest test-rsa-pem-pkcs8-roundtrip
  "RSA private key round-trips through PKCS#8 (`PRIVATE KEY`) PEM"
  (let* ((k (native:generate-rsa-key :bits 2048))
         (pem (native:key-to-pem k :private-p t :format :pkcs8))
         (k2 (native:key-from-pem pem)))
    (assert-equal :rsa (native:native-key-type k2))
    (assert-true (native:native-key-private-p k2))
    (assert-equal "PRIVATE KEY" (pem:pem-block-label (pem:pem-decode pem)))
    (assert-true (%sign-verify-equiv-p k k2))))

(deftest test-rsa-pem-auto-defaults-to-pkcs1
  "RSA :auto format emits the bare PKCS#1 RSAPrivateKey label that
   OpenSSL and most tooling default to."
  (let* ((k (native:generate-rsa-key :bits 2048))
         (pem (native:key-to-pem k :private-p t)))
    (assert-equal "RSA PRIVATE KEY"
                  (pem:pem-block-label (pem:pem-decode pem)))))

(deftest test-rsa-public-key-roundtrip
  "RSA public key round-trips through SPKI PEM"
  (let* ((k (native:generate-rsa-key :bits 2048))
         (k-pub (%public-from k)))
    (assert-equal :rsa (native:native-key-type k-pub))
    (assert-not (native:native-key-private-p k-pub))
    (let ((msg (map '(vector (unsigned-byte 8)) #'char-code "hi")))
      (assert-true (native:verify-message k-pub msg
                                          (native:sign-message k msg))))))

;;; ---------------------------------------------------------------------------
;;; DER (auto-detected container)
;;;
;;; `key-from-der' has to dispatch by format on its own without a PEM
;;; label. Verify it picks the right path for every container.
;;; ---------------------------------------------------------------------------

(deftest test-der-roundtrip-rsa-pkcs1
  "key-from-der recognises bare PKCS#1 RSAPrivateKey"
  (let* ((k (native:generate-rsa-key :bits 2048))
         (pem (native:key-to-pem k :private-p t :format :pkcs1))
         (block (pem:pem-decode pem))
         (der (pem:pem-block-data block))
         (k2 (native:key-from-der der :private-p t)))
    (assert-equal :rsa (native:native-key-type k2))
    (assert-true (%sign-verify-equiv-p k k2))))

(deftest test-der-roundtrip-rsa-pkcs8
  "key-from-der recognises PKCS#8 PrivateKeyInfo for RSA"
  (let* ((k (native:generate-rsa-key :bits 2048))
         (pem (native:key-to-pem k :private-p t :format :pkcs8))
         (der (pem:pem-block-data (pem:pem-decode pem)))
         (k2 (native:key-from-der der :private-p t)))
    (assert-equal :rsa (native:native-key-type k2))
    (assert-true (%sign-verify-equiv-p k k2))))

(deftest test-der-roundtrip-ec-pkcs8
  "key-from-der recognises PKCS#8 PrivateKeyInfo for EC P-256"
  (let* ((k (native:generate-ec-p256-key))
         (pem (native:key-to-pem k :private-p t :format :pkcs8))
         (der (pem:pem-block-data (pem:pem-decode pem)))
         (k2 (native:key-from-der der :private-p t)))
    (assert-equal :ec-p256 (native:native-key-type k2))
    (assert-true (%sign-verify-equiv-p k k2))))

(deftest test-der-roundtrip-ec-sec1
  "key-from-der recognises bare SEC1 ECPrivateKey"
  (let* ((k (native:generate-ec-p256-key))
         (pem (native:key-to-pem k :private-p t :format :sec1))
         (der (pem:pem-block-data (pem:pem-decode pem)))
         (k2 (native:key-from-der der :private-p t)))
    (assert-equal :ec-p256 (native:native-key-type k2))
    (assert-true (%sign-verify-equiv-p k k2))))

(deftest test-der-roundtrip-ed25519-pkcs8
  "key-from-der recognises PKCS#8 PrivateKeyInfo for Ed25519"
  (let* ((k (native:generate-ed25519-key))
         (pem (native:key-to-pem k :private-p t :format :pkcs8))
         (der (pem:pem-block-data (pem:pem-decode pem)))
         (k2 (native:key-from-der der :private-p t)))
    (assert-equal :ed25519 (native:native-key-type k2))
    (assert-true (%sign-verify-equiv-p k k2))))

(deftest test-der-roundtrip-ed25519-raw-seed
  "key-from-der treats a 32-byte input as a raw Ed25519 seed"
  (let* ((k (native:generate-ed25519-key))
         (seed (native:native-key-material k))
         (k2 (native:key-from-der seed)))
    (assert-equal :ed25519 (native:native-key-type k2))
    (assert-true (native:native-key-private-p k2))
    (assert-true (%sign-verify-equiv-p k k2))))

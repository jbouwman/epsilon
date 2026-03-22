;;;; Algorithm Availability Tests
;;;;
;;;; Tests to verify which cryptographic algorithms are available
;;;; via the native pure-Lisp backend (no OpenSSL dependency).

(defpackage epsilon.crypto.tests.availability
  (:use :cl :epsilon.syntax :epsilon.test)
  (:require (epsilon.crypto crypto))
  (:enter t))

(deftest test-hash-algorithm-availability
  "Test that required hash algorithms produce correct-length output"
  (let ((data (crypto:random-bytes 32)))
    (assert-= (length (crypto:sha256 data)) 32)
    (assert-= (length (crypto:sha384 data)) 48)
    (assert-= (length (crypto:sha512 data)) 64)
    (assert-= (length (crypto:md5 data)) 16)
    (assert-= (length (crypto:sha1 data)) 20)))

(deftest test-random-bytes
  "Test that PRNG produces bytes of correct length"
  (let ((bytes (crypto:random-bytes 32)))
    (assert-= (length bytes) 32)
    (assert-true (typep bytes '(simple-array (unsigned-byte 8) (*))))))

(deftest test-native-key-generation
  "Test native key generation algorithms"
  ;; Ed25519
  (let ((key (crypto:generate-ed25519-key)))
    (assert-true (not (null key))))

  ;; RSA (use small key size for speed in tests)
  (let ((key (crypto:generate-rsa-key :bits 2048)))
    (assert-true (not (null key)))))

(deftest test-native-key-pem-roundtrip
  "Test PEM export and re-import for native keys"
  ;; Ed25519 round-trip
  (let* ((key (crypto:generate-ed25519-key))
         (private-pem (crypto:key-to-pem key :private-p t))
         (public-pem (crypto:key-to-pem key :private-p nil))
         (imported-priv (crypto:key-from-pem private-pem :private-p t))
         (imported-pub (crypto:key-from-pem public-pem)))
    (assert-true (search "PRIVATE KEY" private-pem))
    (assert-true (search "PUBLIC KEY" public-pem))
    ;; Sign with imported private key, verify with imported public key
    (let* ((msg (crypto:random-bytes 64))
           (sig (crypto:sign-message imported-priv msg)))
      (assert-true (crypto:verify-message imported-pub msg sig)))))

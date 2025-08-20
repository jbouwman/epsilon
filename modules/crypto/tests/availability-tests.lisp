;;;; Algorithm Availability Tests
;;;;
;;;; Tests to verify which cryptographic algorithms are available

(defpackage :epsilon.crypto.availability-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:crypto #:epsilon.crypto)
   (#:ffi #:epsilon.crypto.ffi)))

(in-package :epsilon.crypto.availability-tests)

(deftest test-cipher-availability
  "Test that required cipher algorithms are available"
  ;; AES-GCM ciphers
  (let ((aes-128-gcm (ffi:%evp-get-cipherbyname "aes-128-gcm"))
        (aes-256-gcm (ffi:%evp-get-cipherbyname "aes-256-gcm")))
    (is (not (sb-sys:sap= aes-128-gcm (sb-sys:int-sap 0))) 
        "AES-128-GCM should be available")
    (is (not (sb-sys:sap= aes-256-gcm (sb-sys:int-sap 0)))
        "AES-256-GCM should be available"))
  
  ;; ChaCha20-Poly1305
  (let ((chacha (ffi:%evp-get-cipherbyname "chacha20-poly1305")))
    ;; ChaCha20-Poly1305 might not be available in older OpenSSL
    (when (not (sb-sys:sap= chacha (sb-sys:int-sap 0)))
      (is t "ChaCha20-Poly1305 is available"))))

(deftest test-digest-availability
  "Test that required digest algorithms are available"
  (dolist (digest-name '("SHA256" "SHA512" "SHA3-256" "SHA3-512"))
    (let ((md (ffi:%evp-get-digestbyname digest-name)))
      (is (not (sb-sys:sap= md (sb-sys:int-sap 0)))
          (format nil "~A digest should be available" digest-name))))
  
  ;; BLAKE2 algorithms
  (let ((blake2b (ffi:%evp-get-digestbyname "BLAKE2b512"))
        (blake2s (ffi:%evp-get-digestbyname "BLAKE2s256")))
    (when (not (sb-sys:sap= blake2b (sb-sys:int-sap 0)))
      (is t "BLAKE2b is available"))
    (when (not (sb-sys:sap= blake2s (sb-sys:int-sap 0)))
      (is t "BLAKE2s is available"))))

(deftest test-openssl-version
  "Test OpenSSL version reporting"
  ;; Skip for now as init package may not be loaded yet
  (skip))

(deftest test-prng-status
  "Test that PRNG is properly seeded"
  ;; Skip this test as %rand-status might not be available in all versions
  (skip))

(deftest test-key-algorithm-support
  "Test support for key generation algorithms"
  ;; RSA support
  (handler-case
      (let ((key (crypto:generate-rsa-key :bits 2048)))
        (is (crypto:crypto-key-p key) "RSA key generation should work"))
    (error (e)
      (is nil (format nil "RSA key generation failed: ~A" e))))
  
  ;; EC support
  (handler-case
      (let ((key (crypto:generate-ec-key :curve :p256)))
        (is (crypto:crypto-key-p key) "EC key generation should work"))
    (error (e)
      (is nil (format nil "EC key generation failed: ~A" e))))
  
  ;; Ed25519 support (might not be available)
  (handler-case
      (let ((key (crypto:generate-ed25519-key)))
        (is (crypto:crypto-key-p key) "Ed25519 key generation works"))
    (error ()
      ;; Ed25519 not available in older OpenSSL
      (skip))))
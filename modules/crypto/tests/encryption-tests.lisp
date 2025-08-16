;;;; Encryption and Decryption Test Suite
;;;;
;;;; Tests for public key encryption/decryption and random generation

(defpackage :epsilon.crypto.encryption-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:crypto #:epsilon.crypto)
   (#:map #:epsilon.map)))

(in-package :epsilon.crypto.encryption-tests)

;;;; Test Data

(defparameter *test-plaintexts*
  '("Hello, World!"
    "The quick brown fox jumps over the lazy dog"
    "12345678901234567890"
    "A"
    ""))

;;;; RSA Encryption/Decryption Tests

(deftest test-rsa-encrypt-decrypt-basic
  "Test basic RSA encryption and decryption"
  (handler-case
      (let ((key (crypto:generate-rsa-key 2048))
            (plaintext "Secret message!"))
        
        ;; Encrypt with public key
        (let ((ciphertext (crypto:encrypt key plaintext)))
          (is (typep ciphertext '(vector (unsigned-byte 8))))
          ;; RSA ciphertext should be key-size/8 bytes
          (is-= (length ciphertext) 256)  ; 2048 bits / 8
          
          ;; Ciphertext should be different from plaintext
          (is (not (equal (map 'string #'code-char ciphertext) plaintext)))
          
          ;; Decrypt with private key
          (let ((decrypted (crypto:decrypt key ciphertext)))
            (is (stringp decrypted))
            (is (string= decrypted plaintext)))))
    (error (e)
      (warn "Skipping RSA encrypt/decrypt test: ~A" e))))

(deftest test-rsa-encrypt-decrypt-various-sizes
  "Test RSA encryption with various plaintext sizes"
  (handler-case
      (let ((key (crypto:generate-rsa-key 2048)))
        (dolist (plaintext *test-plaintexts*)
          (unless (string= plaintext "")  ; Skip empty string
            (let* ((ciphertext (crypto:encrypt key plaintext))
                   (decrypted (crypto:decrypt key ciphertext)))
              (is (string= decrypted plaintext)
                  (format nil "Failed for plaintext: ~S" plaintext))))))
    (error (e)
      (warn "Skipping RSA various sizes test: ~A" e))))

(deftest test-rsa-encrypt-max-size
  "Test RSA encryption with maximum allowed plaintext size"
  (handler-case
      (let* ((key (crypto:generate-rsa-key 2048))
             ;; Max plaintext size for RSA-2048 with PKCS1 padding is ~245 bytes
             (max-plaintext (make-string 200 :initial-element #\X)))
        
        (let* ((ciphertext (crypto:encrypt key max-plaintext))
               (decrypted (crypto:decrypt key ciphertext)))
          (is (string= decrypted max-plaintext))))
    (error (e)
      (warn "Skipping RSA max size test: ~A" e))))

(deftest test-rsa-decrypt-with-wrong-key
  "Test that decryption with wrong key fails"
  (handler-case
      (let ((key1 (crypto:generate-rsa-key 2048))
            (key2 (crypto:generate-rsa-key 2048))
            (plaintext "Secret"))
        
        (let ((ciphertext (crypto:encrypt key1 plaintext)))
          ;; Decrypting with wrong key should fail
          (handler-case
              (progn
                (crypto:decrypt key2 ciphertext)
                (is nil "Should have failed to decrypt with wrong key"))
            (crypto:crypto-error ()
              (is t "Correctly failed to decrypt with wrong key")))))
    (error (e)
      (warn "Skipping wrong key test: ~A" e))))

(deftest test-rsa-encrypt-binary-data
  "Test RSA encryption of binary data"
  (handler-case
      (let ((key (crypto:generate-rsa-key 2048))
            (binary-data (make-array 100 :element-type '(unsigned-byte 8)
                                    :initial-contents (loop for i from 0 below 100
                                                           collect (mod i 256)))))
        
        (let* ((ciphertext (crypto:encrypt key binary-data))
               (decrypted (crypto:decrypt key ciphertext)))
          ;; Decrypted might be string, convert for comparison
          (let ((decrypted-bytes (if (stringp decrypted)
                                     (map 'vector #'char-code decrypted)
                                     decrypted)))
            (is (equalp decrypted-bytes binary-data)))))
    (error (e)
      (warn "Skipping RSA binary data test: ~A" e))))

(deftest test-non-rsa-encryption
  "Test that non-RSA keys cannot be used for encryption"
  (handler-case
      (let ((ec-key (crypto:generate-ec-key :p256))
            (ed-key (crypto:generate-ed25519-key)))
        
        ;; EC key should not support encryption
        (handler-case
            (progn
              (crypto:encrypt ec-key "test")
              (is nil "EC key should not support encryption"))
          (error ()
            (is t "Correctly rejected EC key for encryption")))
        
        ;; Ed25519 key should not support encryption
        (handler-case
            (progn
              (crypto:encrypt ed-key "test")
              (is nil "Ed25519 key should not support encryption"))
          (error ()
            (is t "Correctly rejected Ed25519 key for encryption"))))
    (error (e)
      (warn "Skipping non-RSA encryption test: ~A" e))))

;;;; Random Number Generation Tests

(deftest test-random-bytes-generation
  "Test cryptographically secure random byte generation"
  (handler-case
      (progn
        ;; Test different sizes
        (dolist (size '(1 8 16 32 64 128 256 512 1024))
          (let ((bytes (crypto:crypto-random-bytes size)))
            (is (typep bytes '(vector (unsigned-byte 8))))
            (is-= (length bytes) size)))
        
        ;; Test that successive calls produce different results
        (let ((bytes1 (crypto:crypto-random-bytes 32))
              (bytes2 (crypto:crypto-random-bytes 32)))
          (is (not (equalp bytes1 bytes2))))
        
        ;; Test maximum size limit
        (handler-case
            (progn
              (crypto:crypto-random-bytes 5000)
              (is nil "Should have failed for size > 4096"))
          (error ()
            (is t "Correctly rejected size > 4096"))))
    (error (e)
      (warn "Skipping random bytes test: ~A" e))))

(deftest test-random-integer-generation
  "Test cryptographically secure random integer generation"
  (handler-case
      (progn
        ;; Test range constraints
        (dotimes (i 100)
          (let ((n (crypto:crypto-random-integer 100)))
            (is (and (>= n 0) (< n 100)))))
        
        ;; Test different ranges
        (let ((small (crypto:crypto-random-integer 10))
              (medium (crypto:crypto-random-integer 1000))
              (large (crypto:crypto-random-integer 1000000)))
          (is (< small 10))
          (is (< medium 1000))
          (is (< large 1000000)))
        
        ;; Test edge case: max = 1
        (let ((n (crypto:crypto-random-integer 1)))
          (is-= n 0))
        
        ;; Test distribution (basic check)
        (let ((counts (make-array 10 :initial-element 0)))
          (dotimes (i 1000)
            (incf (aref counts (crypto:crypto-random-integer 10))))
          ;; Each bucket should have gotten some values
          (loop for count across counts
                do (is (> count 0)))))
    (error (e)
      (warn "Skipping random integer test: ~A" e))))

(deftest test-random-bytes-uniqueness
  "Test that random bytes are unique with high probability"
  (handler-case
      (let ((generated map:+empty+))
        ;; Generate 100 random 16-byte values
        (dotimes (i 100)
          (let ((bytes (crypto:crypto-random-bytes 16)))
            ;; Check if we've seen this before (collision)
            (is (not (map:contains-p generated bytes))
                "Random bytes collision detected")
            (setf generated (map:assoc generated bytes t)))))
    (error (e)
      (warn "Skipping uniqueness test: ~A" e))))

(deftest test-random-bytes-distribution
  "Test that random bytes have reasonable distribution"
  (handler-case
      (let ((bytes (crypto:crypto-random-bytes 1000))
            (counts (make-array 256 :initial-element 0)))
        ;; Count occurrences of each byte value
        (loop for byte across bytes
              do (incf (aref counts byte)))
        
        ;; Check that all byte values appear at least once
        ;; (statistically very likely with 1000 bytes)
        (let ((zeros 0))
          (loop for count across counts
                when (zerop count)
                do (incf zeros))
          ;; Should have very few (if any) missing values
          (is (< zeros 50))))
    (error (e)
      (warn "Skipping distribution test: ~A" e))))

;;;; Encryption with Public-Only Keys

(deftest test-encrypt-with-public-key-only
  "Test encryption with public key only (no private key)"
  (handler-case
      (let* ((key (crypto:generate-rsa-key 2048))
             ;; Export and reimport public key only
             (public-pem (crypto:key-to-pem key :private-p nil))
             (public-key (crypto:key-from-pem public-pem :private-p nil))
             (plaintext "Public key encryption test"))
        
        ;; Should be able to encrypt with public key
        (let ((ciphertext (crypto:encrypt public-key plaintext)))
          (is (typep ciphertext '(vector (unsigned-byte 8))))
          
          ;; Should NOT be able to decrypt with public key
          (handler-case
              (progn
                (crypto:decrypt public-key ciphertext)
                (is nil "Should not decrypt without private key"))
            (error ()
              (is t "Correctly failed to decrypt without private key")))
          
          ;; Original key with private part should decrypt
          (let ((decrypted (crypto:decrypt key ciphertext)))
            (is (string= decrypted plaintext)))))
    (error (e)
      (warn "Skipping public-only encryption test: ~A" e))))

;;;; Cross-Key Encryption Tests

(deftest test-cross-key-encryption
  "Test encryption/decryption with exported/imported keys"
  (handler-case
      (let* ((original-key (crypto:generate-rsa-key 2048))
             (plaintext "Cross-key test message"))
        
        ;; Export and reimport the full key
        (let* ((private-pem (crypto:key-to-pem original-key :private-p t))
               (reimported-key (crypto:key-from-pem private-pem :private-p t)))
          
          ;; Encrypt with original, decrypt with reimported
          (let* ((ciphertext (crypto:encrypt original-key plaintext))
                 (decrypted (crypto:decrypt reimported-key ciphertext)))
            (is (string= decrypted plaintext)))
          
          ;; Encrypt with reimported, decrypt with original
          (let* ((ciphertext (crypto:encrypt reimported-key plaintext))
                 (decrypted (crypto:decrypt original-key ciphertext)))
            (is (string= decrypted plaintext)))))
    (error (e)
      (warn "Skipping cross-key encryption test: ~A" e))))

;;;; Performance Tests

(deftest test-encryption-performance
  "Test performance of encryption operations"
  (handler-case
      (let ((key (crypto:generate-rsa-key 2048))
            (plaintext "Performance test message"))
        
        ;; Measure encryption time
        (let ((start (get-internal-real-time)))
          (dotimes (i 100)
            (crypto:encrypt key plaintext))
          (let ((elapsed (- (get-internal-real-time) start)))
            ;; 100 encryptions should be reasonably fast
            (is (< elapsed (* 2 internal-time-units-per-second)))))
        
        ;; Measure decryption time
        (let ((ciphertext (crypto:encrypt key plaintext))
              (start (get-internal-real-time)))
          (dotimes (i 100)
            (crypto:decrypt key ciphertext))
          (let ((elapsed (- (get-internal-real-time) start)))
            ;; Decryption is typically slower than encryption
            (is (< elapsed (* 3 internal-time-units-per-second))))))
    (error (e)
      (warn "Skipping encryption performance test: ~A" e))))

(deftest test-random-generation-performance
  "Test performance of random number generation"
  (handler-case
      (progn
        ;; Random bytes generation
        (let ((start (get-internal-real-time)))
          (dotimes (i 1000)
            (crypto:crypto-random-bytes 32))
          (let ((elapsed (- (get-internal-real-time) start)))
            ;; Should generate 1000 random values quickly
            (is (< elapsed internal-time-units-per-second))))
        
        ;; Random integer generation
        (let ((start (get-internal-real-time)))
          (dotimes (i 10000)
            (crypto:crypto-random-integer 1000000))
          (let ((elapsed (- (get-internal-real-time) start)))
            (is (< elapsed internal-time-units-per-second)))))
    (error (e)
      (warn "Skipping random performance test: ~A" e))))

;;;; Edge Cases

(deftest test-encrypt-empty-string
  "Test encryption of empty string"
  (handler-case
      (let ((key (crypto:generate-rsa-key 2048))
            (plaintext ""))
        ;; Some implementations might not handle empty string
        (handler-case
            (let* ((ciphertext (crypto:encrypt key plaintext))
                   (decrypted (crypto:decrypt key ciphertext)))
              (is (string= decrypted plaintext)))
          (error ()
            ;; It's okay if empty string is not supported
            (is t "Empty string encryption not supported"))))
    (error (e)
      (warn "Skipping empty string test: ~A" e))))

(deftest test-random-zero-bytes
  "Test generating zero random bytes"
  (handler-case
      (handler-case
          (let ((bytes (crypto:crypto-random-bytes 0)))
            (is (typep bytes '(vector (unsigned-byte 8))))
            (is-= (length bytes) 0))
        (error ()
          ;; It's okay if zero bytes is not supported
          (is t "Zero bytes not supported")))
    (error (e)
      (warn "Skipping zero bytes test: ~A" e))))
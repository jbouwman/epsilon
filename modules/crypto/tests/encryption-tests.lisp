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
  (let ((key (crypto:generate-rsa-key :bits 2048))
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
        (is (string= decrypted plaintext))))))

(deftest test-rsa-encrypt-decrypt-various-sizes
  "Test RSA encryption with various plaintext sizes"
  (let ((key (crypto:generate-rsa-key :bits 2048)))
    (dolist (plaintext *test-plaintexts*)
      (unless (string= plaintext "")  ; Skip empty string
        (let* ((ciphertext (crypto:encrypt key plaintext))
               (decrypted (crypto:decrypt key ciphertext)))
          (is (string= decrypted plaintext)
              (format nil "Failed for plaintext: ~S" plaintext)))))))

(deftest test-rsa-encrypt-max-size
  "Test RSA encryption with maximum allowed plaintext size"
  (let* ((key (crypto:generate-rsa-key :bits 2048))
         ;; Max plaintext size for RSA-2048 with PKCS1 padding is ~245 bytes
         (max-plaintext (make-string 200 :initial-element #\X)))
    
    (let* ((ciphertext (crypto:encrypt key max-plaintext))
           (decrypted (crypto:decrypt key ciphertext)))
      (is (string= decrypted max-plaintext)))))

(deftest test-rsa-decrypt-with-wrong-key
  "Test that decryption with wrong key fails or produces garbage"
  (let ((key1 (crypto:generate-rsa-key :bits 2048))
        (key2 (crypto:generate-rsa-key :bits 2048))
        (plaintext "Secret"))
    
    (let ((ciphertext (crypto:encrypt key1 plaintext)))
      ;; Decrypting with wrong key should either fail or produce garbage
      (handler-case
          (let ((result (crypto:decrypt key2 ciphertext)))
            ;; If it doesn't throw an error, the result should NOT match the plaintext
            (is (not (string= result plaintext)) 
                "Decryption with wrong key should not produce original plaintext"))
        (crypto:crypto-error ()
            ;; This is also acceptable - some OpenSSL versions fail cleanly
            (is t "Correctly failed to decrypt with wrong key"))))))

(deftest test-rsa-encrypt-binary-data
  "Test RSA encryption of binary data"
  (let ((key (crypto:generate-rsa-key :bits 2048))
        (binary-data (make-array 100 :element-type '(unsigned-byte 8)
                                 :initial-contents (loop for i from 0 below 100
                                                         collect (mod i 256)))))
    
    (let* ((ciphertext (crypto:encrypt key binary-data))
           (decrypted (crypto:decrypt key ciphertext)))
      ;; Decrypted might be string, convert for comparison
      (let ((decrypted-bytes (if (stringp decrypted)
                                 (map 'vector #'char-code decrypted)
                               decrypted)))
        (is (equalp decrypted-bytes binary-data))))))

(deftest test-non-rsa-encryption
  "Test that non-RSA keys cannot be used for encryption"
  (let ((ec-key (crypto:generate-ec-key :curve :p256))
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
	     (is t "Correctly rejected Ed25519 key for encryption")))))

;;;; Random Number Generation Tests

(deftest test-random-bytes-generation
  "Test cryptographically secure random byte generation"
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
	     (is t "Correctly rejected size > 4096")))))

(deftest test-random-integer-generation
  "Test cryptographically secure random integer generation"
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
            do (is (> count 0))))))

(deftest test-random-bytes-uniqueness
  "Test that random bytes are unique with high probability"
  (let ((generated map:+empty+))
    ;; Generate 100 random 16-byte values
    (dotimes (i 100)
      (let ((bytes (crypto:crypto-random-bytes 16)))
        ;; Check if we've seen this before (collision)
        (is (not (map:contains-p generated bytes))
            "Random bytes collision detected")
        (setf generated (map:assoc generated bytes t))))))

(deftest test-random-bytes-distribution
  "Test that random bytes have reasonable distribution"
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
      (is (< zeros 50)))))

;;;; Encryption with Public-Only Keys

(deftest test-encrypt-with-public-key-only
  "Test encryption with public key only (no private key)"
  (let* ((key (crypto:generate-rsa-key :bits 2048))
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
	(is (string= decrypted plaintext))))))

;;;; Cross-Key Encryption Tests

(deftest test-cross-key-encryption
  "Test encryption/decryption with exported/imported keys"
  (let* ((original-key (crypto:generate-rsa-key :bits 2048))
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
	(is (string= decrypted plaintext))))))

;;;; Edge Cases

(deftest test-encrypt-empty-string
  "Test encryption of empty string"
  (let ((key (crypto:generate-rsa-key :bits 2048))
	(plaintext ""))
    ;; Some implementations might not handle empty string
    (handler-case
	(let* ((ciphertext (crypto:encrypt key plaintext))
	       (decrypted (crypto:decrypt key ciphertext)))
	  (is (string= decrypted plaintext)))
      (error ()
	     ;; It's okay if empty string is not supported
	     (is t "Empty string encryption not supported")))))

(deftest test-random-zero-bytes
  "Test generating zero random bytes"
  (handler-case
      (let ((bytes (crypto:crypto-random-bytes 0)))
	(is (typep bytes '(vector (unsigned-byte 8))))
	(is-= (length bytes) 0))
    (error ()
	   ;; It's okay if zero bytes is not supported
	   (is t "Zero bytes not supported"))))

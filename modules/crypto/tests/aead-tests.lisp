;;;; AEAD (Authenticated Encryption) Tests for epsilon.crypto
;;;;
;;;; Tests for AES-GCM and ChaCha20-Poly1305 AEAD ciphers

(defpackage :epsilon.crypto.aead-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:crypto #:epsilon.crypto)))

(in-package :epsilon.crypto.aead-tests)

;;;; AES-GCM Tests

(deftest test-aes-gcm-basic-128
  "Test basic AES-128-GCM encryption and decryption"
  (let* ((plaintext "Hello, World! This is a secret message.")
         (key (crypto:crypto-random-bytes 16))  ; 128-bit key
         (result (crypto:aes-gcm-encrypt plaintext key)))
    
    ;; Check that we got all required components
    (is (getf result :ciphertext))
    (is (getf result :tag))
    (is (getf result :iv))
    
    ;; Ciphertext should be same length as plaintext
    (is (= (length (sb-ext:string-to-octets plaintext :external-format :utf-8))
           (length (getf result :ciphertext))))
    
    ;; Tag should be 16 bytes
    (is (= 16 (length (getf result :tag))))
    
    ;; IV should be 12 bytes
    (is (= 12 (length (getf result :iv))))
    
    ;; Decrypt and verify
    (let ((decrypted (crypto:aes-gcm-decrypt 
                      (getf result :ciphertext)
                      key
                      (getf result :tag)
                      (getf result :iv))))
      (is (equalp (sb-ext:string-to-octets plaintext :external-format :utf-8)
                  decrypted)))))

(deftest test-aes-gcm-basic-256
  "Test basic AES-256-GCM encryption and decryption"
  (let* ((plaintext "Secret data that needs strong encryption")
         (key (crypto:crypto-random-bytes 32))  ; 256-bit key
         (result (crypto:aes-gcm-encrypt plaintext key)))
    
    ;; Decrypt and verify
    (let ((decrypted (crypto:aes-gcm-decrypt 
                      (getf result :ciphertext)
                      key
                      (getf result :tag)
                      (getf result :iv))))
      (is (equalp (sb-ext:string-to-octets plaintext :external-format :utf-8)
                  decrypted)))))

(deftest test-aes-gcm-with-aad
  "Test AES-GCM with additional authenticated data"
  (let* ((plaintext "Encrypted message")
         (aad (sb-ext:string-to-octets "metadata" :external-format :utf-8))
         (key (crypto:crypto-random-bytes 32))
         (result (crypto:aes-gcm-encrypt plaintext key :aad aad)))
    
    ;; Decrypt with correct AAD should succeed
    (let ((decrypted (crypto:aes-gcm-decrypt 
                      (getf result :ciphertext)
                      key
                      (getf result :tag)
                      (getf result :iv)
                      :aad aad)))
      (is (equalp (sb-ext:string-to-octets plaintext :external-format :utf-8)
                  decrypted)))
    
    ;; Decrypt with wrong AAD should fail
    (let ((wrong-aad (sb-ext:string-to-octets "wrong" :external-format :utf-8)))
      (is-thrown 'crypto:crypto-error
                 (crypto:aes-gcm-decrypt 
                  (getf result :ciphertext)
                  key
                  (getf result :tag)
                  (getf result :iv)
                  :aad wrong-aad)))
    
    ;; Decrypt with no AAD should also fail
    (is-thrown 'crypto:crypto-error
               (crypto:aes-gcm-decrypt 
                (getf result :ciphertext)
                key
                (getf result :tag)
                (getf result :iv)))))

(deftest test-aes-gcm-custom-iv
  "Test AES-GCM with custom IV"
  (let* ((plaintext "Test message")
         (key (crypto:crypto-random-bytes 32))
         (custom-iv (crypto:crypto-random-bytes 12))
         (result (crypto:aes-gcm-encrypt plaintext key :iv custom-iv)))
    
    ;; Should use the provided IV
    (is (equalp custom-iv (getf result :iv)))
    
    ;; Should decrypt successfully
    (let ((decrypted (crypto:aes-gcm-decrypt 
                      (getf result :ciphertext)
                      key
                      (getf result :tag)
                      custom-iv)))
      (is (equalp (sb-ext:string-to-octets plaintext :external-format :utf-8)
                  decrypted)))))

(deftest test-aes-gcm-tampering-detection
  "Test that AES-GCM detects tampering"
  (let* ((plaintext "Sensitive data")
         (key (crypto:crypto-random-bytes 32))
         (result (crypto:aes-gcm-encrypt plaintext key)))
    
    ;; Tamper with ciphertext
    (let ((tampered-ciphertext (copy-seq (getf result :ciphertext))))
      (setf (aref tampered-ciphertext 0) 
            (logxor (aref tampered-ciphertext 0) #xFF))
      (is-thrown 'crypto:crypto-error
                 (crypto:aes-gcm-decrypt 
                  tampered-ciphertext
                  key
                  (getf result :tag)
                  (getf result :iv))))
    
    ;; Tamper with tag
    (let ((tampered-tag (copy-seq (getf result :tag))))
      (setf (aref tampered-tag 0)
            (logxor (aref tampered-tag 0) #xFF))
      (is-thrown 'crypto:crypto-error
                 (crypto:aes-gcm-decrypt 
                  (getf result :ciphertext)
                  key
                  tampered-tag
                  (getf result :iv))))))

;;;; ChaCha20-Poly1305 Tests

(deftest test-chacha20-poly1305-basic
  "Test basic ChaCha20-Poly1305 encryption and decryption"
  (let* ((plaintext "ChaCha20-Poly1305 test message")
         (key (crypto:crypto-random-bytes 32))
         (result (crypto:chacha20-poly1305-encrypt plaintext key)))
    
    ;; Check components
    (is (getf result :ciphertext))
    (is (getf result :tag))
    (is (getf result :nonce))
    
    ;; Sizes
    (is (= (length (sb-ext:string-to-octets plaintext :external-format :utf-8))
           (length (getf result :ciphertext))))
    (is (= 16 (length (getf result :tag))))
    (is (= 12 (length (getf result :nonce))))
    
    ;; Decrypt
    (let ((decrypted (crypto:chacha20-poly1305-decrypt 
                      (getf result :ciphertext)
                      key
                      (getf result :tag)
                      (getf result :nonce))))
      (is (equalp (sb-ext:string-to-octets plaintext :external-format :utf-8)
                  decrypted)))))

(deftest test-chacha20-poly1305-with-aad
  "Test ChaCha20-Poly1305 with AAD"
  (let* ((plaintext "Secret message")
         (aad (sb-ext:string-to-octets "header data" :external-format :utf-8))
         (key (crypto:crypto-random-bytes 32))
         (result (crypto:chacha20-poly1305-encrypt plaintext key :aad aad)))
    
    ;; Decrypt with correct AAD
    (let ((decrypted (crypto:chacha20-poly1305-decrypt 
                      (getf result :ciphertext)
                      key
                      (getf result :tag)
                      (getf result :nonce)
                      :aad aad)))
      (is (equalp (sb-ext:string-to-octets plaintext :external-format :utf-8)
                  decrypted)))
    
    ;; Decrypt with wrong AAD should fail
    (is-thrown 'crypto:crypto-error
               (crypto:chacha20-poly1305-decrypt 
                (getf result :ciphertext)
                key
                (getf result :tag)
                (getf result :nonce)
                :aad (sb-ext:string-to-octets "wrong" :external-format :utf-8)))))

(deftest test-chacha20-poly1305-custom-nonce
  "Test ChaCha20-Poly1305 with custom nonce"
  (let* ((plaintext "Test data")
         (key (crypto:crypto-random-bytes 32))
         (custom-nonce (crypto:crypto-random-bytes 12))
         (result (crypto:chacha20-poly1305-encrypt plaintext key :nonce custom-nonce)))
    
    ;; Should use provided nonce
    (is (equalp custom-nonce (getf result :nonce)))
    
    ;; Should decrypt successfully
    (let ((decrypted (crypto:chacha20-poly1305-decrypt 
                      (getf result :ciphertext)
                      key
                      (getf result :tag)
                      custom-nonce)))
      (is (equalp (sb-ext:string-to-octets plaintext :external-format :utf-8)
                  decrypted)))))

(deftest test-chacha20-poly1305-tampering-detection
  "Test ChaCha20-Poly1305 tampering detection"
  (let* ((plaintext "Important data")
         (key (crypto:crypto-random-bytes 32))
         (result (crypto:chacha20-poly1305-encrypt plaintext key)))
    
    ;; Tamper with ciphertext
    (let ((tampered (copy-seq (getf result :ciphertext))))
      (setf (aref tampered 0) (logxor (aref tampered 0) #xFF))
      (is-thrown 'crypto:crypto-error
                 (crypto:chacha20-poly1305-decrypt 
                  tampered
                  key
                  (getf result :tag)
                  (getf result :nonce))))
    
    ;; Tamper with tag
    (let ((tampered-tag (copy-seq (getf result :tag))))
      (setf (aref tampered-tag 0) (logxor (aref tampered-tag 0) #xFF))
      (is-thrown 'crypto:crypto-error
                 (crypto:chacha20-poly1305-decrypt 
                  (getf result :ciphertext)
                  key
                  tampered-tag
                  (getf result :nonce))))))

;;;; Binary Data Tests

(deftest test-aead-binary-data
  "Test AEAD with binary data"
  (let* ((binary-data (crypto:crypto-random-bytes 1024))
         (key-aes (crypto:crypto-random-bytes 32))
         (key-chacha (crypto:crypto-random-bytes 32)))
    
    ;; Test AES-GCM with binary
    (let* ((result-aes (crypto:aes-gcm-encrypt binary-data key-aes))
           (decrypted-aes (crypto:aes-gcm-decrypt 
                           (getf result-aes :ciphertext)
                           key-aes
                           (getf result-aes :tag)
                           (getf result-aes :iv))))
      (is (equalp binary-data decrypted-aes)))
    
    ;; Test ChaCha20-Poly1305 with binary
    (let* ((result-chacha (crypto:chacha20-poly1305-encrypt binary-data key-chacha))
           (decrypted-chacha (crypto:chacha20-poly1305-decrypt 
                              (getf result-chacha :ciphertext)
                              key-chacha
                              (getf result-chacha :tag)
                              (getf result-chacha :nonce))))
      (is (equalp binary-data decrypted-chacha)))))

;;;; Edge Cases

(deftest test-aead-empty-plaintext
  "Test AEAD with empty plaintext"
  (let ((key (crypto:crypto-random-bytes 32)))
    ;; AES-GCM with empty plaintext
    (let* ((result (crypto:aes-gcm-encrypt "" key))
           (decrypted (crypto:aes-gcm-decrypt 
                       (getf result :ciphertext)
                       key
                       (getf result :tag)
                       (getf result :iv))))
      (is (= 0 (length decrypted))))
    
    ;; ChaCha20-Poly1305 with empty plaintext
    (let* ((result (crypto:chacha20-poly1305-encrypt "" key))
           (decrypted (crypto:chacha20-poly1305-decrypt 
                       (getf result :ciphertext)
                       key
                       (getf result :tag)
                       (getf result :nonce))))
      (is (= 0 (length decrypted))))))

(deftest test-aead-invalid-parameters
  "Test AEAD with invalid parameters"
  ;; Invalid key sizes
  (is-thrown 'crypto:crypto-error
             (crypto:aes-gcm-encrypt "data" (crypto:crypto-random-bytes 24)))
  
  (is-thrown 'crypto:crypto-error
             (crypto:chacha20-poly1305-encrypt "data" (crypto:crypto-random-bytes 16)))
  
  ;; Invalid IV/nonce sizes
  (is-thrown 'crypto:crypto-error
             (crypto:aes-gcm-encrypt "data" 
                                     (crypto:crypto-random-bytes 32)
                                     :iv (crypto:crypto-random-bytes 16)))
  
  (is-thrown 'crypto:crypto-error
             (crypto:chacha20-poly1305-encrypt "data"
                                               (crypto:crypto-random-bytes 32)
                                               :nonce (crypto:crypto-random-bytes 16))))

;;;; Performance Benchmarks

(deftest benchmark-aead-ciphers
  "Benchmark AEAD cipher performance"
  (let ((data (crypto:crypto-random-bytes 1024))
        (key-aes (crypto:crypto-random-bytes 32))
        (key-chacha (crypto:crypto-random-bytes 32))
        (iterations 100))
    
    (format t "~%AEAD Cipher Performance Comparison:~%")
    (format t "====================================~%")
    
    ;; Benchmark AES-256-GCM encryption
    (let ((start (get-internal-real-time)))
      (dotimes (i iterations)
        (crypto:aes-gcm-encrypt data key-aes))
      (let ((elapsed (/ (* (- (get-internal-real-time) start) 1000.0)
                       internal-time-units-per-second)))
        (format t "AES-256-GCM encrypt: ~,1Fms for ~D iterations~%" elapsed iterations)))
    
    ;; Benchmark ChaCha20-Poly1305 encryption
    (let ((start (get-internal-real-time)))
      (dotimes (i iterations)
        (crypto:chacha20-poly1305-encrypt data key-chacha))
      (let ((elapsed (/ (* (- (get-internal-real-time) start) 1000.0)
                       internal-time-units-per-second)))
        (format t "ChaCha20-Poly1305 encrypt: ~,1Fms for ~D iterations~%" elapsed iterations)))))

;;;; Test Runner

(defun run-all-tests ()
  "Run all AEAD tests"
  (format t "Running AEAD Tests~%")
  (format t "==================~%")
  
  ;; AES-GCM tests
  (format t "~%Testing AES-GCM...~%")
  (test-aes-gcm-basic-128)
  (test-aes-gcm-basic-256)
  (test-aes-gcm-with-aad)
  (test-aes-gcm-custom-iv)
  (test-aes-gcm-tampering-detection)
  
  ;; ChaCha20-Poly1305 tests
  (format t "~%Testing ChaCha20-Poly1305...~%")
  (test-chacha20-poly1305-basic)
  (test-chacha20-poly1305-with-aad)
  (test-chacha20-poly1305-custom-nonce)
  (test-chacha20-poly1305-tampering-detection)
  
  ;; Additional tests
  (format t "~%Testing edge cases...~%")
  (test-aead-binary-data)
  (test-aead-empty-plaintext)
  (test-aead-invalid-parameters)
  
  ;; Benchmarks
  (format t "~%Running benchmarks...~%")
  (benchmark-aead-ciphers)
  
  (format t "~%All AEAD tests completed!~%"))
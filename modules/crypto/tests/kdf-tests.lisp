;;;; Key Derivation Function Tests for epsilon.crypto
;;;;
;;;; Tests for PBKDF2, HKDF, Scrypt and other KDF functions

(defpackage :epsilon.crypto.kdf-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:crypto #:epsilon.crypto)))

(in-package :epsilon.crypto.kdf-tests)

;;;; PBKDF2 Tests

(deftest test-pbkdf2-basic
  "Test basic PBKDF2 key derivation"
  (let* ((password "password")
         (salt "salt1234")
         (key1 (crypto:pbkdf2 password salt :iterations 1000 :key-length 32))
         (key2 (crypto:pbkdf2 password salt :iterations 1000 :key-length 32)))
    ;; Same inputs should produce same output
    (is (equalp key1 key2))
    ;; Output should be 32 bytes
    (is (= 32 (length key1)))
    ;; Output should be different with different salt
    (let ((key3 (crypto:pbkdf2 password "different-salt" :iterations 1000 :key-length 32)))
      (is (not (equalp key1 key3))))))

(deftest test-pbkdf2-iterations
  "Test that different iterations produce different keys"
  (let* ((password "test-password")
         (salt "test-salt")
         (key1 (crypto:pbkdf2 password salt :iterations 1000))
         (key2 (crypto:pbkdf2 password salt :iterations 2000)))
    (is (not (equalp key1 key2)))
    (is (= 32 (length key1)))
    (is (= 32 (length key2)))))

(deftest test-pbkdf2-key-lengths
  "Test PBKDF2 with different key lengths"
  (let ((password "password")
        (salt "salt"))
    ;; Test various key lengths
    (dolist (length '(16 24 32 48 64 128))
      (let ((key (crypto:pbkdf2 password salt :key-length length)))
        (is (= length (length key)))))))

(deftest test-pbkdf2-different-digests
  "Test PBKDF2 with different hash algorithms"
  (let ((password "password")
        (salt "salt"))
    ;; SHA-256
    (let ((key-sha256 (crypto:pbkdf2 password salt 
                                     :digest crypto:+digest-sha256+)))
      (is (= 32 (length key-sha256))))
    ;; SHA-384
    (let ((key-sha384 (crypto:pbkdf2 password salt 
                                     :digest crypto:+digest-sha384+)))
      (is (= 32 (length key-sha384))))
    ;; SHA-512
    (let ((key-sha512 (crypto:pbkdf2 password salt 
                                     :digest crypto:+digest-sha512+)))
      (is (= 32 (length key-sha512))))))

(deftest test-pbkdf2-byte-input
  "Test PBKDF2 with byte array inputs"
  (let* ((password-bytes (make-array 16 :element-type '(unsigned-byte 8)
                                        :initial-contents '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
         (salt-bytes (make-array 16 :element-type '(unsigned-byte 8)
                                    :initial-contents '(16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)))
         (key (crypto:pbkdf2 password-bytes salt-bytes :iterations 1000)))
    (is (= 32 (length key)))
    (is (typep key '(vector (unsigned-byte 8))))))

(deftest test-pbkdf2-high-iterations
  "Test PBKDF2 with high iteration count (performance test)"
  (let* ((password "secure-password")
         (salt (crypto:crypto-random-bytes 16))
         (start-time (get-internal-real-time))
         (key (crypto:pbkdf2 password salt :iterations 100000))
         (end-time (get-internal-real-time))
         (elapsed-ms (/ (* (- end-time start-time) 1000) 
                       internal-time-units-per-second)))
    (is (= 32 (length key)))
    ;; Should take some time but not too long (< 5 seconds)
    (is (< elapsed-ms 5000))
    (format t "~%PBKDF2 100k iterations took ~Dms~%" elapsed-ms)))

;;;; HKDF Tests

(deftest test-hkdf-basic
  "Test basic HKDF key derivation"
  (let* ((ikm (crypto:crypto-random-bytes 32))  ; Input key material
         (salt (crypto:crypto-random-bytes 16))
         (info (sb-ext:string-to-octets "test-info" :external-format :utf-8))
         (key1 (crypto:hkdf ikm :salt salt :info info :length 32))
         (key2 (crypto:hkdf ikm :salt salt :info info :length 32)))
    ;; Same inputs should produce same output
    (is (equalp key1 key2))
    ;; Output should be 32 bytes
    (is (= 32 (length key1)))))

(deftest test-hkdf-different-lengths
  "Test HKDF with different output lengths"
  (let ((ikm (crypto:crypto-random-bytes 32)))
    (dolist (length '(16 32 48 64 128))
      (let ((key (crypto:hkdf ikm :length length)))
        (is (= length (length key)))))))

(deftest test-hkdf-with-info
  "Test HKDF with different info parameters"
  (let* ((ikm (crypto:crypto-random-bytes 32))
         (salt (crypto:crypto-random-bytes 16))
         (info1 (sb-ext:string-to-octets "encryption" :external-format :utf-8))
         (info2 (sb-ext:string-to-octets "authentication" :external-format :utf-8))
         (key1 (crypto:hkdf ikm :salt salt :info info1 :length 32))
         (key2 (crypto:hkdf ikm :salt salt :info info2 :length 32)))
    ;; Different info should produce different keys
    (is (not (equalp key1 key2)))
    (is (= 32 (length key1)))
    (is (= 32 (length key2)))))

(deftest test-hkdf-no-salt
  "Test HKDF without salt (uses zeros)"
  (let* ((ikm (crypto:crypto-random-bytes 32))
         (key (crypto:hkdf ikm :salt nil :length 32)))
    (is (= 32 (length key)))))

(deftest test-hkdf-expand-to-multiple-keys
  "Test HKDF to derive multiple keys from one secret"
  (let* ((shared-secret (crypto:crypto-random-bytes 32))
         (salt (crypto:crypto-random-bytes 16))
         ;; Derive 96 bytes (3 x 32-byte keys)
         (key-material (crypto:hkdf shared-secret 
                                    :salt salt
                                    :info (sb-ext:string-to-octets "app-keys")
                                    :length 96))
         (encryption-key (subseq key-material 0 32))
         (mac-key (subseq key-material 32 64))
         (iv-key (subseq key-material 64 96)))
    ;; All keys should be different
    (is (not (equalp encryption-key mac-key)))
    (is (not (equalp encryption-key iv-key)))
    (is (not (equalp mac-key iv-key)))
    ;; Each key should be 32 bytes
    (is (= 32 (length encryption-key)))
    (is (= 32 (length mac-key)))
    (is (= 32 (length iv-key)))))

;;;; Scrypt Tests

(deftest test-scrypt-basic
  "Test basic scrypt key derivation"
  (handler-case
      (let* ((password "password")
             (salt "salt1234")
             (key1 (crypto:scrypt password salt :n 16384 :r 8 :p 1))
             (key2 (crypto:scrypt password salt :n 16384 :r 8 :p 1)))
        ;; Same inputs should produce same output
        (is (equalp key1 key2))
        ;; Output should be 32 bytes
        (is (= 32 (length key1)))
        ;; Different salt should produce different key
        (let ((key3 (crypto:scrypt password "different" :n 16384 :r 8 :p 1)))
          (is (not (equalp key1 key3)))))
    ;; Scrypt might not be available in all OpenSSL versions
    (crypto:crypto-error ()
      (format t "~%Scrypt not available in this OpenSSL version - skipping test~%"))))

(deftest test-scrypt-parameters
  "Test scrypt with different cost parameters"
  (handler-case
      (let ((password "test")
            (salt "salt"))
        ;; Test with minimum parameters
        (let ((key1 (crypto:scrypt password salt :n 16384 :r 8 :p 1)))
          (is (= 32 (length key1))))
        ;; Test with higher memory cost
        (let ((key2 (crypto:scrypt password salt :n 32768 :r 8 :p 1)))
          (is (= 32 (length key2)))))
    (crypto:crypto-error ()
      (format t "~%Scrypt not available - skipping parameter test~%"))))

(deftest test-scrypt-invalid-n
  "Test that scrypt rejects non-power-of-2 N values"
  (handler-case
      (progn
        (is-thrown 'crypto:crypto-error
                   (crypto:scrypt "password" "salt" :n 16383))  ; Not power of 2
        (is-thrown 'crypto:crypto-error
                   (crypto:scrypt "password" "salt" :n 24576))) ; Not power of 2
    (crypto:crypto-error ()
      ;; If scrypt itself is not available, skip this test
      (format t "~%Scrypt not available - skipping validation test~%"))))

;;;; Random Number Generation Tests

(deftest test-crypto-random-bytes
  "Test cryptographic random byte generation"
  (let ((bytes1 (crypto:crypto-random-bytes 32))
        (bytes2 (crypto:crypto-random-bytes 32)))
    ;; Should be 32 bytes
    (is (= 32 (length bytes1)))
    (is (= 32 (length bytes2)))
    ;; Should be different (extremely unlikely to be same)
    (is (not (equalp bytes1 bytes2)))
    ;; Should be byte arrays
    (is (typep bytes1 '(vector (unsigned-byte 8))))
    (is (typep bytes2 '(vector (unsigned-byte 8))))))

(deftest test-crypto-random-various-sizes
  "Test random generation with various sizes"
  (dolist (size '(1 8 16 24 32 64 128 256 512 1024))
    (let ((bytes (crypto:crypto-random-bytes size)))
      (is (= size (length bytes)))
      (is (typep bytes '(vector (unsigned-byte 8)))))))

;;;; Performance Benchmarks

(deftest benchmark-kdf-functions
  "Benchmark various KDF functions"
  (let ((password "benchmark-password")
        (salt (crypto:crypto-random-bytes 16)))
    
    ;; Benchmark PBKDF2 with different iteration counts
    (format t "~%KDF Performance Benchmarks:~%")
    (format t "===========================~%")
    
    (dolist (iterations '(1000 10000 100000))
      (let* ((start (get-internal-real-time))
             (key (crypto:pbkdf2 password salt :iterations iterations))
             (elapsed (/ (* (- (get-internal-real-time) start) 1000.0)
                        internal-time-units-per-second)))
        (format t "PBKDF2 (~D iterations): ~,1Fms~%" iterations elapsed)))
    
    ;; Benchmark HKDF
    (let* ((ikm (crypto:crypto-random-bytes 32))
           (start (get-internal-real-time))
           (key (crypto:hkdf ikm :salt salt :length 64))
           (elapsed (/ (* (- (get-internal-real-time) start) 1000.0)
                      internal-time-units-per-second)))
      (format t "HKDF (64 bytes): ~,1Fms~%" elapsed))
    
    ;; Benchmark Scrypt if available
    (handler-case
        (let* ((start (get-internal-real-time))
               (key (crypto:scrypt password salt :n 16384 :r 8 :p 1))
               (elapsed (/ (* (- (get-internal-real-time) start) 1000.0)
                          internal-time-units-per-second)))
          (format t "Scrypt (N=16384): ~,1Fms~%" elapsed))
      (crypto:crypto-error ()
        (format t "Scrypt: Not available~%")))))

;;;; Test Runner

(defun run-all-tests ()
  "Run all KDF tests"
  (format t "Running KDF Tests~%")
  (format t "=================~%")
  
  ;; PBKDF2 tests
  (format t "~%Testing PBKDF2...~%")
  (test-pbkdf2-basic)
  (test-pbkdf2-iterations)
  (test-pbkdf2-key-lengths)
  (test-pbkdf2-different-digests)
  (test-pbkdf2-byte-input)
  (test-pbkdf2-high-iterations)
  
  ;; HKDF tests
  (format t "~%Testing HKDF...~%")
  (test-hkdf-basic)
  (test-hkdf-different-lengths)
  (test-hkdf-with-info)
  (test-hkdf-no-salt)
  (test-hkdf-expand-to-multiple-keys)
  
  ;; Scrypt tests
  (format t "~%Testing Scrypt...~%")
  (test-scrypt-basic)
  (test-scrypt-parameters)
  (test-scrypt-invalid-n)
  
  ;; Random number tests
  (format t "~%Testing Random Generation...~%")
  (test-crypto-random-bytes)
  (test-crypto-random-various-sizes)
  
  ;; Benchmarks
  (format t "~%Running Benchmarks...~%")
  (benchmark-kdf-functions)
  
  (format t "~%All KDF tests completed!~%"))
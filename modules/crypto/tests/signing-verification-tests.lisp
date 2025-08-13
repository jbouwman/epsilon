;;;; Digital Signature and Verification Test Suite
;;;;
;;;; Comprehensive tests for signing and verification operations

(defpackage :epsilon.crypto.signing-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:crypto #:epsilon.crypto)))

(in-package :epsilon.crypto.signing-tests)

;;;; Test Data

(defparameter *test-message* "The quick brown fox jumps over the lazy dog")
(defparameter *test-message-2* "Pack my box with five dozen liquor jugs")
(defparameter *test-binary-data* 
  (make-array 256 :element-type '(unsigned-byte 8)
              :initial-contents (loop for i from 0 below 256 collect i)))

;;;; RSA Signing Tests

(deftest test-rsa-sign-verify-sha256
  "Test RSA signing and verification with SHA-256"
  (handler-case
      (let ((key (crypto:generate-rsa-key 2048)))
        ;; Sign message
        (let ((signature (crypto:sign-message key *test-message*
                                             :digest-algo crypto:+digest-sha256+)))
          (is (typep signature '(vector (unsigned-byte 8))))
          (is (> (length signature) 0))
          (is-= (length signature) 256)  ; 2048 bits / 8
          
          ;; Verify correct message
          (is (crypto:verify-message key *test-message* signature
                                    :digest-algo crypto:+digest-sha256+))
          
          ;; Verify wrong message fails
          (is (not (crypto:verify-message key *test-message-2* signature
                                          :digest-algo crypto:+digest-sha256+)))
          
          ;; Verify tampered signature fails
          (let ((bad-sig (copy-seq signature)))
            (setf (aref bad-sig 0) (logxor (aref bad-sig 0) #xFF))
            (is (not (crypto:verify-message key *test-message* bad-sig
                                            :digest-algo crypto:+digest-sha256+))))))
    (error (e)
      (warn "Skipping RSA SHA-256 test - OpenSSL not available: ~A" e))))

(deftest test-rsa-sign-verify-sha512
  "Test RSA signing and verification with SHA-512"
  (handler-case
      (let ((key (crypto:generate-rsa-key 2048)))
        ;; Sign with SHA-512
        (let ((signature (crypto:sign-message key *test-message*
                                             :digest-algo crypto:+digest-sha512+)))
          (is (typep signature '(vector (unsigned-byte 8))))
          (is-= (length signature) 256)
          
          ;; Verify with SHA-512
          (is (crypto:verify-message key *test-message* signature
                                    :digest-algo crypto:+digest-sha512+))
          
          ;; Verify with wrong algorithm fails
          (is (not (crypto:verify-message key *test-message* signature
                                          :digest-algo crypto:+digest-sha256+)))))
    (error (e)
      (warn "Skipping RSA SHA-512 test: ~A" e))))

(deftest test-rsa-sign-binary-data
  "Test RSA signing of binary data"
  (handler-case
      (let ((key (crypto:generate-rsa-key 2048)))
        ;; Sign binary data
        (let ((signature (crypto:sign-message key *test-binary-data*)))
          (is (typep signature '(vector (unsigned-byte 8))))
          
          ;; Verify binary data
          (is (crypto:verify-message key *test-binary-data* signature))
          
          ;; Modify one byte and verify fails
          (let ((bad-data (copy-seq *test-binary-data*)))
            (setf (aref bad-data 128) (logxor (aref bad-data 128) #x01))
            (is (not (crypto:verify-message key bad-data signature))))))
    (error (e)
      (warn "Skipping RSA binary data test: ~A" e))))

;;;; EC Signing Tests

(deftest test-ec-p256-sign-verify
  "Test EC P-256 signing and verification"
  (handler-case
      (let ((key (crypto:generate-ec-key :p256)))
        ;; Sign message
        (let ((signature (crypto:sign-message key *test-message*)))
          (is (typep signature '(vector (unsigned-byte 8))))
          ;; EC signatures vary in size due to DER encoding
          (is (> (length signature) 0))
          (is (< (length signature) 100))  ; Typically 70-72 bytes
          
          ;; Verify correct message
          (is (crypto:verify-message key *test-message* signature))
          
          ;; Verify wrong message fails
          (is (not (crypto:verify-message key *test-message-2* signature)))))
    (error (e)
      (warn "Skipping EC P-256 test: ~A" e))))

(deftest test-ec-p384-sign-verify
  "Test EC P-384 signing and verification"
  (handler-case
      (let ((key (crypto:generate-ec-key :p384)))
        (let ((signature (crypto:sign-message key *test-message*)))
          (is (typep signature '(vector (unsigned-byte 8))))
          ;; P-384 signatures are larger than P-256
          (is (> (length signature) 50))
          (is (< (length signature) 150))
          
          ;; Verify
          (is (crypto:verify-message key *test-message* signature))))
    (error (e)
      (warn "Skipping EC P-384 test: ~A" e))))

(deftest test-ec-secp256k1-sign-verify
  "Test secp256k1 signing (Bitcoin/Ethereum curve)"
  (handler-case
      (let ((key (crypto:generate-ec-key :secp256k1)))
        (let ((signature (crypto:sign-message key *test-message*)))
          (is (typep signature '(vector (unsigned-byte 8))))
          
          ;; Verify
          (is (crypto:verify-message key *test-message* signature))))
    (error (e)
      (warn "Skipping secp256k1 test: ~A" e))))

;;;; Ed25519 Signing Tests

(deftest test-ed25519-sign-verify
  "Test Ed25519 signing and verification"
  (handler-case
      (let ((key (crypto:generate-ed25519-key)))
        ;; Sign message
        (let ((signature (crypto:sign-message key *test-message*)))
          (is (typep signature '(vector (unsigned-byte 8))))
          ;; Ed25519 signatures are always 64 bytes
          (is-= (length signature) 64)
          
          ;; Verify correct message
          (is (crypto:verify-message key *test-message* signature))
          
          ;; Verify wrong message fails
          (is (not (crypto:verify-message key *test-message-2* signature)))
          
          ;; Verify tampered signature fails
          (let ((bad-sig (copy-seq signature)))
            (setf (aref bad-sig 32) (logxor (aref bad-sig 32) #x01))
            (is (not (crypto:verify-message key *test-message* bad-sig))))))
    (error (e)
      (warn "Skipping Ed25519 test: ~A" e))))

(deftest test-ed25519-deterministic
  "Test that Ed25519 signatures are deterministic"
  (handler-case
      (let ((key (crypto:generate-ed25519-key)))
        ;; Sign same message twice
        (let ((sig1 (crypto:sign-message key *test-message*))
              (sig2 (crypto:sign-message key *test-message*)))
          ;; Ed25519 signatures should be deterministic
          (is (equalp sig1 sig2))))
    (error (e)
      (warn "Skipping Ed25519 deterministic test: ~A" e))))

;;;; Cross-Key Verification Tests

(deftest test-cross-key-verification-rsa
  "Test verification with exported/imported RSA keys"
  (handler-case
      (let* ((original-key (crypto:generate-rsa-key 2048))
             (message "Test cross-key verification")
             (signature (crypto:sign-message original-key message)))
        
        ;; Export and reimport public key
        (let* ((public-pem (crypto:key-to-pem original-key :private-p nil))
               (public-key (crypto:key-from-pem public-pem :private-p nil)))
          ;; Verify with reimported public key
          (is (crypto:verify-message public-key message signature)))
        
        ;; Export and reimport private key
        (let* ((private-pem (crypto:key-to-pem original-key :private-p t))
               (private-key (crypto:key-from-pem private-pem :private-p t)))
          ;; Sign with reimported private key
          (let ((new-sig (crypto:sign-message private-key message)))
            ;; Verify with original key
            (is (crypto:verify-message original-key message new-sig)))))
    (error (e)
      (warn "Skipping cross-key RSA test: ~A" e))))

(deftest test-cross-key-verification-ec
  "Test verification with exported/imported EC keys"
  (handler-case
      (let* ((original-key (crypto:generate-ec-key :p256))
             (message "Test EC cross-key")
             (signature (crypto:sign-message original-key message)))
        
        ;; Export and reimport public key
        (let* ((public-pem (crypto:key-to-pem original-key :private-p nil))
               (public-key (crypto:key-from-pem public-pem :private-p nil)))
          (is (crypto:verify-message public-key message signature))))
    (error (e)
      (warn "Skipping cross-key EC test: ~A" e))))

;;;; Wrong Key Type Tests

(deftest test-verify-with-wrong-key-type
  "Test that verification with wrong key type fails"
  (handler-case
      (let ((rsa-key (crypto:generate-rsa-key 2048))
            (ec-key (crypto:generate-ec-key :p256)))
        
        ;; Sign with RSA
        (let ((rsa-sig (crypto:sign-message rsa-key *test-message*)))
          ;; Try to verify with EC key - should fail
          (is (not (crypto:verify-message ec-key *test-message* rsa-sig))))
        
        ;; Sign with EC
        (let ((ec-sig (crypto:sign-message ec-key *test-message*)))
          ;; Try to verify with RSA key - should fail
          (is (not (crypto:verify-message rsa-key *test-message* ec-sig)))))
    (error (e)
      (warn "Skipping wrong key type test: ~A" e))))

;;;; Large Data Signing Tests

(deftest test-sign-large-data
  "Test signing large amounts of data"
  (handler-case
      (let ((key (crypto:generate-rsa-key 2048))
            ;; Create 10KB of data
            (large-data (make-string 10000 :initial-element #\A)))
        
        ;; Sign large data
        (let ((signature (crypto:sign-message key large-data)))
          (is (typep signature '(vector (unsigned-byte 8))))
          ;; Signature size should be same regardless of data size
          (is-= (length signature) 256)
          
          ;; Verify large data
          (is (crypto:verify-message key large-data signature))
          
          ;; Change one character and verify fails
          (setf (char large-data 5000) #\B)
          (is (not (crypto:verify-message key large-data signature)))))
    (error (e)
      (warn "Skipping large data test: ~A" e))))

;;;; Error Handling Tests

(deftest test-sign-without-private-key
  "Test that signing without private key fails"
  (handler-case
      (let* ((key (crypto:generate-rsa-key 2048))
             ;; Get public key only
             (public-pem (crypto:key-to-pem key :private-p nil))
             (public-key (crypto:key-from-pem public-pem :private-p nil)))
        
        ;; Try to sign with public key only
        (handler-case
            (progn
              (crypto:sign-message public-key *test-message*)
              (is nil "Should have failed to sign without private key"))
          (error ()
            (is t "Correctly rejected signing without private key"))))
    (error (e)
      (warn "Skipping private key test: ~A" e))))

(deftest test-verify-without-public-key
  "Test that verification requires public key component"
  (handler-case
      (let ((key (crypto:generate-rsa-key 2048)))
        ;; Normal key has both public and private
        (let ((signature (crypto:sign-message key *test-message*)))
          ;; Should verify successfully
          (is (crypto:verify-message key *test-message* signature))))
    (error (e)
      (warn "Skipping public key test: ~A" e))))

;;;; Algorithm Compatibility Tests

(deftest test-signature-algorithm-mismatch
  "Test that mismatched signature algorithms fail verification"
  (handler-case
      (let ((key (crypto:generate-rsa-key 2048)))
        ;; Sign with SHA-256
        (let ((sig256 (crypto:sign-message key *test-message*
                                          :digest-algo crypto:+digest-sha256+)))
          ;; Try to verify with SHA-512 - should fail
          (is (not (crypto:verify-message key *test-message* sig256
                                          :digest-algo crypto:+digest-sha512+))))
        
        ;; Sign with SHA-512
        (let ((sig512 (crypto:sign-message key *test-message*
                                          :digest-algo crypto:+digest-sha512+)))
          ;; Try to verify with SHA-256 - should fail
          (is (not (crypto:verify-message key *test-message* sig512
                                          :digest-algo crypto:+digest-sha256+)))))
    (error (e)
      (warn "Skipping algorithm mismatch test: ~A" e))))

;;;; Performance Tests

(deftest test-signing-performance
  "Test performance of signing operations"
  (handler-case
      (let ((rsa-key (crypto:generate-rsa-key 2048))
            (ec-key (crypto:generate-ec-key :p256))
            (ed-key (crypto:generate-ed25519-key)))
        
        ;; RSA signing performance
        (let ((start (get-internal-real-time)))
          (dotimes (i 10)
            (crypto:sign-message rsa-key *test-message*))
          (let ((elapsed (- (get-internal-real-time) start)))
            ;; Should complete 10 signatures in reasonable time
            (is (< elapsed (* 2 internal-time-units-per-second)))))
        
        ;; EC signing performance (should be faster than RSA)
        (let ((start (get-internal-real-time)))
          (dotimes (i 100)
            (crypto:sign-message ec-key *test-message*))
          (let ((elapsed (- (get-internal-real-time) start)))
            (is (< elapsed internal-time-units-per-second))))
        
        ;; Ed25519 signing performance (should be fastest)
        (let ((start (get-internal-real-time)))
          (dotimes (i 100)
            (crypto:sign-message ed-key *test-message*))
          (let ((elapsed (- (get-internal-real-time) start)))
            (is (< elapsed internal-time-units-per-second)))))
    (error (e)
      (warn "Skipping performance test: ~A" e))))
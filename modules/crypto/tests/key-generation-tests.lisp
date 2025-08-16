;;;; Key Generation Test Suite for epsilon.crypto
;;;;
;;;; Comprehensive tests for RSA, EC, and Ed25519 key generation

(defpackage :epsilon.crypto.key-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:crypto #:epsilon.crypto)))

(in-package :epsilon.crypto.key-tests)

;;;; Test Fixtures

(defun with-test-key (type &rest args)
  "Create a test key of specified type for testing"
  (case type
        (:rsa (apply #'crypto:generate-rsa-key args))
        (:ec (apply #'crypto:generate-ec-key args))
        (:ed25519 (crypto:generate-ed25519-key))
        (t (error "Unknown key type: ~A" type))))

;;;; RSA Key Generation Tests

(deftest test-rsa-2048-generation
  "Test generating 2048-bit RSA keys"
  (let ((key (with-test-key :rsa 2048)))
    (when key
      (is (crypto:crypto-key-p key))
      (is (eq (crypto:crypto-key-type key) :rsa))
      (is-= (crypto:crypto-key-bits key) 2048)
      (is (crypto:crypto-key-public-p key))
      (is (crypto:crypto-key-private-p key)))))

(deftest test-rsa-3072-generation
  "Test generating 3072-bit RSA keys"
  (let ((key (with-test-key :rsa 3072)))
    (when key
      (is (crypto:crypto-key-p key))
      (is (eq (crypto:crypto-key-type key) :rsa))
      (is-= (crypto:crypto-key-bits key) 3072)
      (is (crypto:crypto-key-public-p key))
      (is (crypto:crypto-key-private-p key)))))

(deftest test-rsa-4096-generation
  "Test generating 4096-bit RSA keys"
  (let ((key (with-test-key :rsa 4096)))
    (when key
      (is (crypto:crypto-key-p key))
      (is (eq (crypto:crypto-key-type key) :rsa))
      (is-= (crypto:crypto-key-bits key) 4096)
      (is (crypto:crypto-key-public-p key))
      (is (crypto:crypto-key-private-p key)))))

(deftest test-rsa-invalid-size
  "Test that invalid RSA key sizes are rejected"
  (handler-case
      (progn
        (crypto:generate-rsa-key 1024)
        (is nil "Should have thrown error for 1024-bit key"))
    (crypto:crypto-error ()
      (is t "Correctly rejected 1024-bit key")))
  
  (handler-case
      (progn
        (crypto:generate-rsa-key 512)
        (is nil "Should have thrown error for 512-bit key"))
    (crypto:crypto-error ()
      (is t "Correctly rejected 512-bit key"))))

;;;; EC Key Generation Tests

(deftest test-ec-p256-generation
  "Test generating P-256 elliptic curve keys"
  (let ((key (with-test-key :ec :p256)))
    (when key
      (is (crypto:crypto-key-p key))
      (is (eq (crypto:crypto-key-type key) :ec))
      (is (crypto:crypto-key-public-p key))
      (is (crypto:crypto-key-private-p key))
      ;; P-256 should have ~256 bits
      (is (<= 256 (crypto:crypto-key-bits key) 257)))))

(deftest test-ec-p384-generation
  "Test generating P-384 elliptic curve keys"
  (let ((key (with-test-key :ec :p384)))
    (when key
      (is (crypto:crypto-key-p key))
      (is (eq (crypto:crypto-key-type key) :ec))
      (is (crypto:crypto-key-public-p key))
      (is (crypto:crypto-key-private-p key))
      ;; P-384 should have ~384 bits
      (is (<= 384 (crypto:crypto-key-bits key) 385)))))

(deftest test-ec-p521-generation
  "Test generating P-521 elliptic curve keys"
  (let ((key (with-test-key :ec :p521)))
    (when key
      (is (crypto:crypto-key-p key))
      (is (eq (crypto:crypto-key-type key) :ec))
      (is (crypto:crypto-key-public-p key))
      (is (crypto:crypto-key-private-p key))
      ;; P-521 should have ~521 bits
      (is (<= 521 (crypto:crypto-key-bits key) 522)))))

(deftest test-ec-secp256k1-generation
  "Test generating secp256k1 keys (Bitcoin/Ethereum curve)"
  (let ((key (with-test-key :ec :secp256k1)))
    (when key
      (is (crypto:crypto-key-p key))
      (is (eq (crypto:crypto-key-type key) :ec))
      (is (crypto:crypto-key-public-p key))
      (is (crypto:crypto-key-private-p key))
      (is (<= 256 (crypto:crypto-key-bits key) 257)))))

(deftest test-ec-invalid-curve
  "Test that invalid EC curves are rejected"
  (handler-case
      (progn
        (crypto:generate-ec-key :invalid-curve)
        (is nil "Should have thrown error for invalid curve"))
    (error ()
      (is t "Correctly rejected invalid curve"))))

;;;; Ed25519 Key Generation Tests

(deftest test-ed25519-generation
  "Test generating Ed25519 keys"
  (let ((key (with-test-key :ed25519)))
    (when key
      (is (crypto:crypto-key-p key))
      (is (eq (crypto:crypto-key-type key) :ed25519))
      (is-= (crypto:crypto-key-bits key) 256)
      (is (crypto:crypto-key-public-p key))
      (is (crypto:crypto-key-private-p key)))))

(deftest test-ed25519-consistency
  "Test that Ed25519 keys are generated consistently"
  (let ((key1 (with-test-key :ed25519))
        (key2 (with-test-key :ed25519)))
    (when (and key1 key2)
      ;; Each key should be unique
      (is (not (eq (crypto:crypto-key-handle key1)
                   (crypto:crypto-key-handle key2))))
      ;; But have same properties
      (is (eq (crypto:crypto-key-type key1) (crypto:crypto-key-type key2)))
      (is (= (crypto:crypto-key-bits key1) (crypto:crypto-key-bits key2))))))

;;;; Key Export/Import Tests

(deftest test-rsa-pem-export-import
  "Test exporting and importing RSA keys in PEM format"
  (let ((original-key (with-test-key :rsa 2048)))
    (when original-key
      ;; Export private key
      (let ((private-pem (crypto:key-to-pem original-key :private-p t)))
        (is (stringp private-pem))
        (is (search "-----BEGIN PRIVATE KEY-----" private-pem))
        (is (search "-----END PRIVATE KEY-----" private-pem))
        
        ;; Import back and verify
        (let ((imported (crypto:key-from-pem private-pem :private-p t)))
          (is (crypto:crypto-key-p imported))
          (is (eq (crypto:crypto-key-type imported) :rsa))
          (is (crypto:crypto-key-private-p imported))
          (is-= (crypto:crypto-key-bits imported) 2048)))
      
      ;; Export public key only
      (let ((public-pem (crypto:key-to-pem original-key :private-p nil)))
        (is (stringp public-pem))
        (is (search "-----BEGIN PUBLIC KEY-----" public-pem))
        (is (search "-----END PUBLIC KEY-----" public-pem))
        
        ;; Import back and verify
        (let ((imported (crypto:key-from-pem public-pem :private-p nil)))
          (is (crypto:crypto-key-p imported))
          (is (eq (crypto:crypto-key-type imported) :rsa))
          (is (crypto:crypto-key-public-p imported))
          (is (not (crypto:crypto-key-private-p imported)))
          (is-= (crypto:crypto-key-bits imported) 2048))))))

(deftest test-ec-pem-export-import
  "Test exporting and importing EC keys in PEM format"
  (let ((original-key (with-test-key :ec :p256)))
    (when original-key
      ;; Export and reimport private key
      (let* ((private-pem (crypto:key-to-pem original-key :private-p t))
             (imported (crypto:key-from-pem private-pem :private-p t)))
        (is (crypto:crypto-key-p imported))
        (is (eq (crypto:crypto-key-type imported) :ec))
        (is (crypto:crypto-key-private-p imported)))
      
      ;; Export and reimport public key
      (let* ((public-pem (crypto:key-to-pem original-key :private-p nil))
             (imported (crypto:key-from-pem public-pem :private-p nil)))
        (is (crypto:crypto-key-p imported))
        (is (eq (crypto:crypto-key-type imported) :ec))
        (is (not (crypto:crypto-key-private-p imported)))))))

(deftest test-ed25519-pem-export-import
  "Test exporting and importing Ed25519 keys in PEM format"
  (let ((original-key (with-test-key :ed25519)))
    (when original-key
      ;; Export and reimport private key
      (let* ((private-pem (crypto:key-to-pem original-key :private-p t))
             (imported (crypto:key-from-pem private-pem :private-p t)))
        (is (crypto:crypto-key-p imported))
        (is (eq (crypto:crypto-key-type imported) :ed25519))
        (is (crypto:crypto-key-private-p imported))
        (is-= (crypto:crypto-key-bits imported) 256))
      
      ;; Export and reimport public key
      (let* ((public-pem (crypto:key-to-pem original-key :private-p nil))
             (imported (crypto:key-from-pem public-pem :private-p nil)))
        (is (crypto:crypto-key-p imported))
        (is (eq (crypto:crypto-key-type imported) :ed25519))
        (is (not (crypto:crypto-key-private-p imported)))))))

;;;; Key Properties Tests

(deftest test-key-structure-fields
  "Test that key structure fields work correctly"
  (let ((key (crypto:make-crypto-key :type :rsa
                                     :bits 2048
                                     :public-p t
                                     :private-p t
                                     :handle nil)))
    (is (crypto:crypto-key-p key))
    (is (eq (crypto:crypto-key-type key) :rsa))
    (is-= (crypto:crypto-key-bits key) 2048)
    (is (crypto:crypto-key-public-p key))
    (is (crypto:crypto-key-private-p key))))

(deftest test-key-type-detection
  "Test that key types are correctly detected"
  (let ((rsa-key (with-test-key :rsa 2048))
        (ec-key (with-test-key :ec :p256))
        (ed-key (with-test-key :ed25519)))
    
    (when rsa-key
      (is (eq (crypto:crypto-key-type rsa-key) :rsa)))
    
    (when ec-key
      (is (eq (crypto:crypto-key-type ec-key) :ec)))
    
    (when ed-key
      (is (eq (crypto:crypto-key-type ed-key) :ed25519)))))

;;;; Performance Tests

(deftest test-key-generation-performance
  "Basic performance check for key generation"
  (when (with-test-key :rsa 2048)  ; Check if OpenSSL is available
    ;; RSA 2048 should complete in reasonable time
    (let ((start-time (get-internal-real-time)))
      (crypto:generate-rsa-key 2048)
      (let ((elapsed (- (get-internal-real-time) start-time)))
        ;; Should complete in less than 5 seconds
        (is (< elapsed (* 5 internal-time-units-per-second)))))
    
    ;; Ed25519 should be very fast
    (let ((start-time (get-internal-real-time)))
      (crypto:generate-ed25519-key)
      (let ((elapsed (- (get-internal-real-time) start-time)))
        ;; Should complete in less than 1 second
        (is (< elapsed internal-time-units-per-second))))))

;;;; Error Handling Tests

(deftest test-invalid-pem-import
  "Test handling of invalid PEM data"
  (handler-case
      (progn
        (crypto:key-from-pem "invalid PEM data" :private-p t)
        (is nil "Should have thrown error for invalid PEM"))
    (crypto:crypto-error ()
      (is t "Correctly rejected invalid PEM"))
    (error ()
      (is t "Error caught for invalid PEM"))))

(deftest test-private-key-required-export
  "Test that private key export requires private key"
  (let ((key (with-test-key :rsa 2048)))
    (when key
      ;; First get a public-only key
      (let* ((public-pem (crypto:key-to-pem key :private-p nil))
             (public-key (crypto:key-from-pem public-pem :private-p nil)))
        
        ;; Try to export as private - should fail
        (handler-case
            (progn
              (crypto:key-to-pem public-key :private-p t)
              (is nil "Should have thrown error for missing private key"))
          (error ()
            (is t "Correctly rejected private export of public key")))))))

;;;; Tests for Signature Operations

(defpackage :epsilon.crypto.signatures-tests
  (:use :cl)
  (:local-nicknames
   (#:sig #:epsilon.crypto.signatures)
   (#:keys #:epsilon.crypto.keys)
   (#:keyring #:epsilon.crypto.keyring)
   (#:path #:epsilon.path)
   (#:fs #:epsilon.sys.fs)
   (#:str #:epsilon.string)))

(in-package :epsilon.crypto.signatures-tests)

(epsilon.test:deftest test-sign-verify-data ()
  "Test signing and verifying raw data"
  (let ((keypair (keys:generate-keypair))
        (data "This is test data to sign"))
    ;; Sign data
    (let ((signature (sig:sign-data data keypair)))
      (epsilon.test:is-not-null signature)
      (epsilon.test:is (stringp signature))
      
      ;; Add key to keyring for verification
      (keyring:add-trusted-key keyring:*default-keyring*
                              (keys:keypair-public-key keypair)
                              (keys:keypair-key-id keypair))
      
      ;; Verify signature
      (epsilon.test:is (sig:verify-data-signature signature 
                                                 data 
                                                 (keys:keypair-public-key keypair))))))

(epsilon.test:deftest test-sign-verify-file ()
  "Test signing and verifying files"
  (let* ((keypair (keys:generate-keypair))
         (test-file (path:make-temp-path :prefix "test_file" :suffix ".txt"))
         (sig-file nil))
    (unwind-protect
         (progn
           ;; Create test file
           (fs:write-file-string test-file "This is a test file for signing.")
           
           ;; Sign file
           (multiple-value-bind (signature-path sig-info)
               (sig:sign-file test-file keypair)
             (setf sig-file signature-path)
             (epsilon.test:is-not-null signature-path)
             (epsilon.test:is (fs:exists-p signature-path))
             (epsilon.test:is-not-null sig-info)
             (epsilon.test:is-equal (keys:keypair-key-id keypair)
                                   (sig:signature-info-key-id sig-info))
             
             ;; Add key to keyring
             (keyring:add-trusted-key keyring:*default-keyring*
                                     (keys:keypair-public-key keypair)
                                     (keys:keypair-key-id keypair))
             
             ;; Verify signature
             (epsilon.test:is (sig:verify-file-signature test-file signature-path))))
      ;; Cleanup
      (when (fs:exists-p test-file)
        (fs:delete-file test-file))
      (when (and sig-file (fs:exists-p sig-file))
        (fs:delete-file sig-file)))))

(epsilon.test:deftest test-signature-info ()
  "Test signature info structure"
  (let* ((keypair (keys:generate-keypair))
         (sig-info (sig:make-signature-info
                    :algorithm :ed25519
                    :key-id (keys:keypair-key-id keypair)
                    :signature "test-signature"
                    :hash "test-hash"
                    :timestamp (get-universal-time)
                    :version "1.0")))
    (epsilon.test:is-equal :ed25519 (sig:signature-info-algorithm sig-info))
    (epsilon.test:is-equal (keys:keypair-key-id keypair) (sig:signature-info-key-id sig-info))
    (epsilon.test:is-equal "test-signature" (sig:signature-info-signature sig-info))
    (epsilon.test:is-equal "test-hash" (sig:signature-info-hash sig-info))
    (epsilon.test:is-equal "1.0" (sig:signature-info-version sig-info))
    (epsilon.test:is (numberp (sig:signature-info-timestamp sig-info)))))

(epsilon.test:deftest test-write-read-signature-file ()
  "Test writing and reading signature files"
  (let* ((keypair (keys:generate-keypair))
         (sig-info (sig:make-signature-info
                    :algorithm :ed25519
                    :key-id (keys:keypair-key-id keypair)
                    :signature "abcdef1234567890"
                    :hash "fedcba0987654321"
                    :timestamp (get-universal-time)
                    :version "1.0"))
         (temp-file (path:make-temp-path :prefix "test_sig" :suffix ".sig")))
    (unwind-protect
         (progn
           ;; Write signature
           (sig:write-signature-file temp-file sig-info)
           (epsilon.test:is (fs:exists-p temp-file))
           
           ;; Read signature
           (let ((loaded (sig:read-signature-file temp-file)))
             (epsilon.test:is-equal (sig:signature-info-algorithm sig-info)
                                   (sig:signature-info-algorithm loaded))
             (epsilon.test:is-equal (sig:signature-info-key-id sig-info)
                                   (sig:signature-info-key-id loaded))
             (epsilon.test:is-equal (sig:signature-info-signature sig-info)
                                   (sig:signature-info-signature loaded))
             (epsilon.test:is-equal (sig:signature-info-hash sig-info)
                                   (sig:signature-info-hash loaded))
             (epsilon.test:is-equal (sig:signature-info-version sig-info)
                                   (sig:signature-info-version loaded))))
      ;; Cleanup
      (when (fs:exists-p temp-file)
        (fs:delete-file temp-file)))))

(epsilon.test:deftest test-verify-with-wrong-key ()
  "Test verification fails with wrong key"
  (let ((keypair1 (keys:generate-keypair))
        (keypair2 (keys:generate-keypair))
        (data "Test data"))
    ;; Sign with keypair1
    (let ((signature (sig:sign-data data keypair1)))
      ;; Try to verify with keypair2's public key - should fail
      (epsilon.test:is-thrown-p 'error
        (sig:verify-data-signature signature 
                                  data 
                                  (keys:keypair-public-key keypair2))))))

(epsilon.test:deftest test-verify-tampered-data ()
  "Test verification fails with tampered data"
  (let ((keypair (keys:generate-keypair))
        (original-data "Original test data")
        (tampered-data "Tampered test data"))
    ;; Sign original data
    (let ((signature (sig:sign-data original-data keypair)))
      ;; Try to verify with tampered data - should fail
      (epsilon.test:is-thrown-p 'error
        (sig:verify-data-signature signature 
                                  tampered-data 
                                  (keys:keypair-public-key keypair))))))

(epsilon.test:deftest test-package-signing-workflow ()
  "Test complete package signing workflow"
  (let* ((keypair (keys:generate-keypair))
         (package-file (path:make-temp-path :prefix "test_package" :suffix ".epk"))
         (sig-file nil))
    (unwind-protect
         (progn
           ;; Create dummy package file
           (fs:write-file-string package-file "Dummy package content for testing")
           
           ;; Sign package
           (multiple-value-bind (signature-path sig-info)
               (sig:sign-package package-file keypair)
             (setf sig-file signature-path)
             
             ;; Verify signature file was created
             (epsilon.test:is (fs:exists-p signature-path))
             (epsilon.test:is (str:ends-with-p signature-path ".sig"))
             
             ;; Add key to keyring
             (keyring:add-trusted-key keyring:*default-keyring*
                                     (keys:keypair-public-key keypair)
                                     (keys:keypair-key-id keypair))
             
             ;; Verify package
             (epsilon.test:is (sig:verify-package-signature package-file signature-path))
             
             ;; Modify package and verify it fails
             (fs:write-file-string package-file "Modified package content")
             (epsilon.test:is-thrown-p 'error
               (sig:verify-package-signature package-file signature-path))))
      ;; Cleanup
      (when (fs:exists-p package-file)
        (fs:delete-file package-file))
      (when (and sig-file (fs:exists-p sig-file))
        (fs:delete-file sig-file)))))

(epsilon.test:deftest test-missing-public-key ()
  "Test verification fails when public key not in keyring"
  (let* ((keypair (keys:generate-keypair))
         (test-file (path:make-temp-path :prefix "test_file" :suffix ".txt"))
         (sig-file nil)
         ;; Create a new keyring without the key
         (empty-keyring (keyring:create-keyring)))
    (unwind-protect
         (progn
           ;; Create and sign file
           (fs:write-file-string test-file "Test content")
           (setf sig-file (sig:sign-file test-file keypair))
           
           ;; Try to verify with empty keyring - should fail
           (epsilon.test:is-thrown-p 'error
             (sig:verify-file-signature test-file sig-file :keyring empty-keyring)))
      ;; Cleanup
      (when (fs:exists-p test-file)
        (fs:delete-file test-file))
      (when (and sig-file (fs:exists-p sig-file))
        (fs:delete-file sig-file)))))
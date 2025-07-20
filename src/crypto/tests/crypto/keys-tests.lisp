;;;; Tests for Key Management

(defpackage :epsilon.crypto.keys-tests
  (:use :cl)
  (:local-nicknames
   (#:keys #:epsilon.crypto.keys)
   (#:path #:epsilon.path)
   (#:fs #:epsilon.sys.fs)))

(in-package :epsilon.crypto.keys-tests)

(epsilon.test:deftest test-generate-keypair ()
  "Test keypair generation"
  (let ((keypair (keys:generate-keypair)))
    (epsilon.test:is-not-null (keys:keypair-private-key keypair))
    (epsilon.test:is-not-null (keys:keypair-public-key keypair))
    (epsilon.test:is-not-null (keys:keypair-key-id keypair))
    (epsilon.test:is-equal :ed25519 (keys:keypair-algorithm keypair))
    (epsilon.test:is (numberp (keys:keypair-created keypair)))))

(epsilon.test:deftest test-generate-key-id ()
  "Test key ID generation"
  (let ((id1 (keys:generate-key-id))
        (id2 (keys:generate-key-id)))
    ;; Should be 16 hex characters
    (epsilon.test:is-equal 16 (length id1))
    (epsilon.test:is-equal 16 (length id2))
    ;; Should be different
    (epsilon.test:is-not (string= id1 id2))
    ;; Should be valid hex
    (epsilon.test:is (every (lambda (c) (digit-char-p c 16)) id1))))

(epsilon.test:deftest test-save-load-keypair ()
  "Test saving and loading keypairs"
  (let* ((keypair (keys:generate-keypair))
         (temp-file (path:make-temp-path :prefix "test_keypair" :suffix ".pem")))
    (unwind-protect
         (progn
           ;; Save keypair
           (keys:save-keypair keypair temp-file :format :pem)
           (epsilon.test:is (fs:exists-p temp-file))
           
           ;; Load keypair
           (let ((loaded (keys:load-keypair temp-file)))
             (epsilon.test:is-equal (keys:keypair-key-id keypair)
                                   (keys:keypair-key-id loaded))
             (epsilon.test:is-equal (keys:keypair-algorithm keypair)
                                   (keys:keypair-algorithm loaded))
             ;; Private and public keys should match
             (epsilon.test:is (string= (keys:keypair-private-key keypair)
                                      (keys:keypair-private-key loaded)))
             (epsilon.test:is (string= (keys:keypair-public-key keypair)
                                      (keys:keypair-public-key loaded)))))
      ;; Cleanup
      (when (fs:exists-p temp-file)
        (fs:delete-file temp-file)))))

(epsilon.test:deftest test-export-import-public-key ()
  "Test public key export and import"
  (let* ((keypair (keys:generate-keypair))
         (temp-file (path:make-temp-path :prefix "test_pubkey" :suffix ".pem")))
    (unwind-protect
         (progn
           ;; Export public key
           (keys:export-public-key keypair temp-file)
           (epsilon.test:is (fs:exists-p temp-file))
           
           ;; Import public key
           (let ((imported (keys:import-public-key temp-file)))
             ;; Should match original public key
             (epsilon.test:is (search (keys:keypair-public-key keypair) imported))))
      ;; Cleanup
      (when (fs:exists-p temp-file)
        (fs:delete-file temp-file)))))

(epsilon.test:deftest test-pem-format ()
  "Test PEM format conversion"
  (let ((keypair (keys:generate-keypair)))
    ;; Convert to PEM
    (let ((pem (keys:keypair-to-pem keypair)))
      (epsilon.test:is (stringp pem))
      (epsilon.test:is (search "BEGIN EPSILON PRIVATE KEY" pem))
      (epsilon.test:is (search "END EPSILON PRIVATE KEY" pem))
      (epsilon.test:is (search "BEGIN EPSILON PUBLIC KEY" pem))
      (epsilon.test:is (search "END EPSILON PUBLIC KEY" pem))
      
      ;; Parse from PEM
      (let ((parsed (keys:keypair-from-pem pem)))
        ;; Keys should be preserved (though base64 encoding might differ)
        (epsilon.test:is (search (keys:keypair-private-key keypair)
                                (keys:keypair-private-key parsed)))
        (epsilon.test:is (search (keys:keypair-public-key keypair)
                                (keys:keypair-public-key parsed)))))))
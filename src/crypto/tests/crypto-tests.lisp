;;;; Tests for main crypto package

(defpackage :epsilon.crypto-tests
  (:use :cl)
  (:local-nicknames
   (#:crypto #:epsilon.crypto)))

(in-package :epsilon.crypto-tests)

(epsilon.test:deftest test-package-exports ()
  "Test that all expected symbols are exported"
  (let ((pkg (find-package :epsilon.crypto)))
    ;; Key management exports
    (epsilon.test:is (find-symbol "KEYPAIR" pkg))
    (epsilon.test:is (find-symbol "GENERATE-KEYPAIR" pkg))
    (epsilon.test:is (find-symbol "SAVE-KEYPAIR" pkg))
    (epsilon.test:is (find-symbol "LOAD-KEYPAIR" pkg))
    
    ;; Keyring exports
    (epsilon.test:is (find-symbol "KEYRING" pkg))
    (epsilon.test:is (find-symbol "CREATE-KEYRING" pkg))
    (epsilon.test:is (find-symbol "ADD-TRUSTED-KEY" pkg))
    (epsilon.test:is (find-symbol "REVOKE-KEY" pkg))
    
    ;; Signature exports
    (epsilon.test:is (find-symbol "SIGN-PACKAGE" pkg))
    (epsilon.test:is (find-symbol "VERIFY-PACKAGE-SIGNATURE" pkg))
    (epsilon.test:is (find-symbol "SIGNATURE-INFO" pkg))))

(epsilon.test:deftest test-end-to-end-workflow ()
  "Test complete workflow using main crypto package"
  ;; Generate keypair
  (let ((keypair (crypto:generate-keypair)))
    (epsilon.test:is-not-null keypair)
    (epsilon.test:is-not-null (crypto:keypair-key-id keypair))
    
    ;; Add to keyring
    (crypto:add-trusted-key crypto:*default-keyring*
                           (crypto:keypair-public-key keypair)
                           (crypto:keypair-key-id keypair))
    
    ;; Sign some data
    (let ((data "Test data for crypto package")
          (signature (crypto:sign-data data keypair)))
      (epsilon.test:is-not-null signature)
      
      ;; Verify signature
      (epsilon.test:is (crypto:verify-data-signature 
                       signature 
                       data 
                       (crypto:keypair-public-key keypair))))))
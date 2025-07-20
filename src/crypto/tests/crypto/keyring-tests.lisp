;;;; Tests for Keyring Management

(defpackage :epsilon.crypto.keyring-tests
  (:use :cl)
  (:local-nicknames
   (#:keyring #:epsilon.crypto.keyring)
   (#:keys #:epsilon.crypto.keys)
   (#:map #:epsilon.map)
   (#:path #:epsilon.path)
   (#:fs #:epsilon.sys.fs)))

(in-package :epsilon.crypto.keyring-tests)

(epsilon.test:deftest test-create-keyring ()
  "Test keyring creation"
  (let ((kr (keyring:create-keyring)))
    (epsilon.test:is-not-null kr)
    (epsilon.test:is (typep kr 'keyring:keyring))
    (epsilon.test:is-equal 0 (map:count (keyring:keyring-trusted-keys kr)))
    (epsilon.test:is-equal 0 (map:count (keyring:keyring-revoked-keys kr)))
    (epsilon.test:is-null (keyring:keyring-default-key kr))))

(epsilon.test:deftest test-add-trusted-key ()
  "Test adding trusted keys"
  (let ((kr (keyring:create-keyring))
        (keypair (keys:generate-keypair)))
    ;; Add trusted key
    (let ((key-info (keyring:add-trusted-key kr 
                                            (keys:keypair-public-key keypair)
                                            (keys:keypair-key-id keypair)
                                            :trust-level :full)))
      (epsilon.test:is-not-null key-info)
      (epsilon.test:is-equal 1 (map:count (keyring:keyring-trusted-keys kr)))
      
      ;; Verify key can be found
      (let ((found-key (keyring:find-public-key (keys:keypair-key-id keypair) kr)))
        (epsilon.test:is-not-null found-key)
        (epsilon.test:is (string= (keys:keypair-public-key keypair) found-key))))))

(epsilon.test:deftest test-revoke-key ()
  "Test key revocation"
  (let ((kr (keyring:create-keyring))
        (keypair (keys:generate-keypair)))
    ;; Add key first
    (keyring:add-trusted-key kr 
                            (keys:keypair-public-key keypair)
                            (keys:keypair-key-id keypair))
    
    ;; Revoke the key
    (let ((revocation-info (keyring:revoke-key kr 
                                              (keys:keypair-key-id keypair)
                                              :reason "Test revocation"
                                              :revoked-by "Test User")))
      (epsilon.test:is-not-null revocation-info)
      (epsilon.test:is (keyring:key-revoked-p kr (keys:keypair-key-id keypair)))
      
      ;; Key should no longer be findable
      (epsilon.test:is-null (keyring:find-public-key (keys:keypair-key-id keypair) kr))
      
      ;; Check revoked keys list
      (let ((revoked-keys (keyring:list-revoked-keys kr)))
        (epsilon.test:is-equal 1 (length revoked-keys))
        (epsilon.test:is (member (keys:keypair-key-id keypair) revoked-keys :test #'string=))))))

(epsilon.test:deftest test-cannot-add-revoked-key ()
  "Test that revoked keys cannot be added as trusted"
  (let ((kr (keyring:create-keyring))
        (keypair (keys:generate-keypair)))
    ;; Add and then revoke
    (keyring:add-trusted-key kr 
                            (keys:keypair-public-key keypair)
                            (keys:keypair-key-id keypair))
    (keyring:revoke-key kr (keys:keypair-key-id keypair))
    
    ;; Try to add again - should error
    (epsilon.test:is-thrown-p 'error
      (keyring:add-trusted-key kr 
                              (keys:keypair-public-key keypair)
                              (keys:keypair-key-id keypair)))))

(epsilon.test:deftest test-list-trusted-keys ()
  "Test listing trusted keys"
  (let ((kr (keyring:create-keyring))
        (kp1 (keys:generate-keypair))
        (kp2 (keys:generate-keypair))
        (kp3 (keys:generate-keypair)))
    ;; Add keys
    (keyring:add-trusted-key kr (keys:keypair-public-key kp1) (keys:keypair-key-id kp1))
    (keyring:add-trusted-key kr (keys:keypair-public-key kp2) (keys:keypair-key-id kp2))
    (keyring:add-trusted-key kr (keys:keypair-public-key kp3) (keys:keypair-key-id kp3))
    
    ;; List should have all three
    (let ((trusted (keyring:list-trusted-keys kr)))
      (epsilon.test:is-equal 3 (length trusted))
      (epsilon.test:is (member (keys:keypair-key-id kp1) trusted :test #'string=))
      (epsilon.test:is (member (keys:keypair-key-id kp2) trusted :test #'string=))
      (epsilon.test:is (member (keys:keypair-key-id kp3) trusted :test #'string=)))
    
    ;; Revoke one
    (keyring:revoke-key kr (keys:keypair-key-id kp2))
    
    ;; List should have only two
    (let ((trusted (keyring:list-trusted-keys kr)))
      (epsilon.test:is-equal 2 (length trusted))
      (epsilon.test:is (member (keys:keypair-key-id kp1) trusted :test #'string=))
      (epsilon.test:is-not (member (keys:keypair-key-id kp2) trusted :test #'string=))
      (epsilon.test:is (member (keys:keypair-key-id kp3) trusted :test #'string=)))))

(epsilon.test:deftest test-save-load-keyring ()
  "Test saving and loading keyrings"
  (let* ((kr (keyring:create-keyring))
         (kp1 (keys:generate-keypair))
         (kp2 (keys:generate-keypair))
         (temp-file (path:make-temp-path :prefix "test_keyring" :suffix ".kr")))
    (unwind-protect
         (progn
           ;; Add some keys
           (keyring:add-trusted-key kr (keys:keypair-public-key kp1) (keys:keypair-key-id kp1))
           (keyring:add-trusted-key kr (keys:keypair-public-key kp2) (keys:keypair-key-id kp2))
           
           ;; Revoke one
           (keyring:revoke-key kr (keys:keypair-key-id kp2) :reason "Test")
           
           ;; Set default key
           (setf (keyring:keyring-default-key kr) (keys:keypair-key-id kp1))
           
           ;; Save keyring
           (keyring:save-keyring kr temp-file)
           (epsilon.test:is (fs:exists-p temp-file))
           
           ;; Load keyring
           (let ((loaded (keyring:load-keyring temp-file)))
             ;; Check trusted keys
             (epsilon.test:is-equal 1 (length (keyring:list-trusted-keys loaded)))
             (epsilon.test:is (member (keys:keypair-key-id kp1) 
                                    (keyring:list-trusted-keys loaded) 
                                    :test #'string=))
             
             ;; Check revoked keys
             (epsilon.test:is (keyring:key-revoked-p loaded (keys:keypair-key-id kp2)))
             
             ;; Check default key
             (epsilon.test:is-equal (keys:keypair-key-id kp1)
                                   (keyring:keyring-default-key loaded))))
      ;; Cleanup
      (when (fs:exists-p temp-file)
        (fs:delete-file temp-file)))))

(epsilon.test:deftest test-merge-keyrings ()
  "Test merging keyrings"
  (let ((kr1 (keyring:create-keyring))
        (kr2 (keyring:create-keyring))
        (kp1 (keys:generate-keypair))
        (kp2 (keys:generate-keypair))
        (kp3 (keys:generate-keypair)))
    ;; Setup keyring 1
    (keyring:add-trusted-key kr1 (keys:keypair-public-key kp1) (keys:keypair-key-id kp1))
    (keyring:add-trusted-key kr1 (keys:keypair-public-key kp2) (keys:keypair-key-id kp2))
    (setf (keyring:keyring-default-key kr1) (keys:keypair-key-id kp1))
    
    ;; Setup keyring 2
    (keyring:add-trusted-key kr2 (keys:keypair-public-key kp3) (keys:keypair-key-id kp3))
    (keyring:revoke-key kr2 (keys:keypair-key-id kp2)) ; Revoke kp2
    (setf (keyring:keyring-default-key kr2) (keys:keypair-key-id kp3))
    
    ;; Merge
    (let ((merged (keyring:merge-keyrings kr1 kr2)))
      ;; Should have kp1 and kp3 as trusted (kp2 was revoked in kr2)
      (let ((trusted (keyring:list-trusted-keys merged)))
        (epsilon.test:is-equal 2 (length trusted))
        (epsilon.test:is (member (keys:keypair-key-id kp1) trusted :test #'string=))
        (epsilon.test:is (member (keys:keypair-key-id kp3) trusted :test #'string=)))
      
      ;; kp2 should be revoked
      (epsilon.test:is (keyring:key-revoked-p merged (keys:keypair-key-id kp2)))
      
      ;; Default key should be from kr2
      (epsilon.test:is-equal (keys:keypair-key-id kp3)
                            (keyring:keyring-default-key merged)))))

(epsilon.test:deftest test-default-keyring ()
  "Test default keyring initialization"
  (epsilon.test:is-not-null keyring:*default-keyring*)
  (epsilon.test:is (typep keyring:*default-keyring* 'keyring:keyring))
  
  ;; Test using default keyring
  (let ((keypair (keys:generate-keypair)))
    (keyring:add-trusted-key keyring:*default-keyring*
                            (keys:keypair-public-key keypair)
                            (keys:keypair-key-id keypair))
    
    ;; Should be able to find using default
    (let ((found (keyring:find-public-key (keys:keypair-key-id keypair))))
      (epsilon.test:is-not-null found)
      (epsilon.test:is (string= (keys:keypair-public-key keypair) found)))))
;;;; Certificate Generation and Management Tests
;;;;
;;;; Tests for X.509 certificate creation, signing, and verification

(defpackage :epsilon.crypto.certificate-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:crypto #:epsilon.crypto)
   (#:certs #:epsilon.crypto.certificates)
   (#:openssl3 #:epsilon.crypto.openssl3)))

(in-package :epsilon.crypto.certificate-tests)

;;;; Test Fixtures

(defparameter *test-cert-dir* nil)

(fixture certificate-test-setup ()
  (:setup
   ;; Create unique test directory  
   (setf *test-cert-dir* (format nil "/tmp/epsilon-cert-~A/" (get-universal-time)))
   (ensure-directories-exist *test-cert-dir*))
  (:teardown
   ;; Clean up test certificates
   (when (probe-file *test-cert-dir*)
     (dolist (file (directory (merge-pathnames "*" *test-cert-dir*)))
       (when (probe-file file)
         (delete-file file)))
     (epsilon.sys.fs:delete-directory *test-cert-dir*))))

;;;; Certificate Generation Tests

(deftest test-generate-self-signed-certificate
  "Test generating a self-signed certificate"
  (with-fixture (fixture certificate-test-setup)
    (multiple-value-bind (cert-pem key-pem)
        (certs:generate-self-signed-certificate "test.example.com"
                                               :key-bits 2048
                                               :days 365
                                               :country "US"
                                               :state "CA"
                                               :locality "San Francisco"
                                               :organization "Test Org")
      ;; Check that PEM strings were returned
      (is-not-null cert-pem)
      (is-not-null key-pem)
      
      ;; Check PEM format markers
      (is-not-null (search "-----BEGIN CERTIFICATE-----" cert-pem))
      (is-not-null (search "-----END CERTIFICATE-----" cert-pem))
      (is-not-null (search "-----BEGIN PRIVATE KEY-----" key-pem))
      (is-not-null (search "-----END PRIVATE KEY-----" key-pem))
      
      ;; Save and verify we can reload them
      (let ((cert-file (merge-pathnames "test-cert.pem" *test-cert-dir*))
            (key-file (merge-pathnames "test-key.pem" *test-cert-dir*)))
        (certs:save-certificate cert-pem cert-file)
        (certs:save-private-key key-pem key-file)
        
        (is (probe-file cert-file))
        (is (probe-file key-file))
        
        ;; Reload and compare
        (let ((loaded-cert (certs:load-certificate cert-file))
              (loaded-key (certs:load-private-key key-file)))
          (is-equal cert-pem loaded-cert)
          (is-equal key-pem loaded-key))))))

(deftest test-certificate-with-san
  "Test generating certificate (SAN extensions not yet implemented)"
  (with-fixture (fixture certificate-test-setup)
    (multiple-value-bind (cert-pem key-pem)
        (certs:generate-self-signed-certificate "example.com")
      (is-not-null cert-pem)
      (is-not-null key-pem)
      
      ;; Extract certificate info and verify it contains expected data
      (let ((cert-info (certs:certificate-info cert-pem)))
        (is-not-null cert-info)
        (is-not-null (search "CN=example.com" (getf cert-info :subject)))))))

(deftest test-generate-ca-certificate
  "Test generating a CA certificate"
  (with-fixture (fixture certificate-test-setup)
    (multiple-value-bind (ca-cert ca-key)
        (certs:generate-ca-certificate "Test CA"
                                      :key-bits 4096
                                      :days 3650)
      (is-not-null ca-cert)
      (is-not-null ca-key)
      
      ;; CA certificates should have longer validity and stronger keys
      (is-not-null (search "-----BEGIN CERTIFICATE-----" ca-cert))
      (is-not-null (search "-----BEGIN PRIVATE KEY-----" ca-key))
      
      ;; Check certificate info
      (let ((cert-info (certs:certificate-info ca-cert)))
        (is-not-null (search "CN=Test CA" (getf cert-info :subject)))
        (is-not-null (search "OU=Certificate Authority" (getf cert-info :subject)))))))

;;;; Certificate Signing Request Tests

(deftest test-generate-and-sign-csr
  "Test generating a CSR and signing it with a CA"
  (with-fixture (fixture certificate-test-setup)
    ;; First generate a CA
    (multiple-value-bind (ca-cert ca-key)
        (certs:generate-ca-certificate "Test CA")
      
      ;; Generate a key pair for the certificate request
      (let* ((client-key (openssl3:generate-rsa-key 2048))
             (client-key-pem (certs::evp-pkey-to-pem client-key)))
        
        ;; Generate CSR
        (let ((csr-pem (certs:generate-certificate-request 
                       "client.example.com" 
                       client-key
                       :organization "Client Org"
                       :email "client@example.com")))
          (is-not-null csr-pem)
          (is-not-null (search "-----BEGIN CERTIFICATE REQUEST-----" csr-pem))
          
          ;; Sign the CSR with CA
          (let ((signed-cert (certs:sign-certificate-request 
                             csr-pem ca-cert ca-key
                             :days 90)))
            (is-not-null signed-cert)
            (is-not-null (search "-----BEGIN CERTIFICATE-----" signed-cert))
            
            ;; Verify the signed certificate against CA
            (is-true (certs:verify-certificate-chain signed-cert ca-cert))))))))

;;;; Certificate I/O Tests

(deftest test-certificate-file-operations
  "Test saving and loading certificates and keys"
  (with-fixture (fixture certificate-test-setup)
    (multiple-value-bind (cert-pem key-pem)
        (certs:generate-self-signed-certificate "file-test.example.com")
      
      (let ((cert-file (merge-pathnames "cert.pem" *test-cert-dir*))
            (key-file (merge-pathnames "key.pem" *test-cert-dir*)))
        
        ;; Save files
        (certs:save-certificate cert-pem cert-file)
        (certs:save-private-key key-pem key-file)
        
        ;; Check files exist
        (is-not-null (probe-file cert-file))
        (is-not-null (probe-file key-file))
        
        ;; Check key file permissions (should be restrictive)
        #+unix
        (let ((key-mode (sb-posix:stat-mode (sb-posix:stat key-file))))
          (is-= (logand key-mode #o777) #o600))
        
        ;; Load and verify
        (let ((loaded-cert (certs:load-certificate cert-file))
              (loaded-key (certs:load-private-key key-file)))
          (is-equal cert-pem loaded-cert)
          (is-equal key-pem loaded-key))))))

(deftest test-make-certificate-pair
  "Test the convenience function for creating cert/key pairs"
  (with-fixture (fixture certificate-test-setup)
    (multiple-value-bind (cert-file key-file)
        (certs:make-certificate-pair "test-pair" :output-dir *test-cert-dir*)
      
      (is-not-null (probe-file cert-file))
      (is-not-null (probe-file key-file))
      
      ;; Verify filenames
      (is-not-null (search "test-pair-cert.pem" (namestring cert-file)))
      (is-not-null (search "test-pair-key.pem" (namestring key-file)))
      
      ;; Load and verify they're valid PEM
      (let ((cert (certs:load-certificate cert-file))
            (key (certs:load-private-key key-file)))
        (is-not-null (search "-----BEGIN CERTIFICATE-----" cert))
        (is-not-null (search "-----BEGIN PRIVATE KEY-----" key))))))

;;;; Certificate Verification Tests

(deftest test-certificate-verification
  "Test certificate chain verification"
  (with-fixture (fixture certificate-test-setup)
    ;; Create a CA
    (multiple-value-bind (ca-cert ca-key)
        (certs:generate-ca-certificate "Verification Test CA")
      
      ;; Create a certificate signed by the CA
      (let* ((server-key (openssl3:generate-rsa-key 2048))
             (csr (certs:generate-certificate-request "server.test" server-key))
             (server-cert (certs:sign-certificate-request csr ca-cert ca-key)))
        
        ;; Verify the certificate against the CA
        (is-true (certs:verify-certificate-chain server-cert ca-cert))
        
        ;; Create another unrelated certificate
        (multiple-value-bind (other-cert other-key)
            (certs:generate-self-signed-certificate "other.test")
          (declare (ignore other-key))
          
          ;; This should NOT verify against our CA
          (is-not (certs:verify-certificate-chain other-cert ca-cert)))))))

(deftest test-certificate-info-extraction
  "Test extracting information from certificates"
  (with-fixture (fixture certificate-test-setup)
    (multiple-value-bind (cert-pem key-pem)
        (certs:generate-self-signed-certificate "info-test.example.com"
                                               :organization "Info Test Org"
                                               :country "US"
                                               :state "NY")
      (declare (ignore key-pem))
      
      (let ((info (certs:certificate-info cert-pem)))
        (is-not-null info)
        
        ;; Check subject contains expected fields
        (let ((subject (getf info :subject)))
          (is-not-null (search "CN=info-test.example.com" subject))
          (is-not-null (search "O=Info Test Org" subject))
          (is-not-null (search "C=US" subject))
          (is-not-null (search "ST=NY" subject)))
        
        ;; For self-signed, issuer should equal subject
        (is-equal (getf info :subject) (getf info :issuer))))))

;;;; Error Handling Tests

(deftest test-invalid-key-size
  "Test that invalid key sizes are rejected"
  (with-fixture (fixture certificate-test-setup)
    (is-thrown (error)
      (certs:generate-self-signed-certificate "test.com" :key-bits 512))
    (is-thrown (error)
      (certs:generate-self-signed-certificate "test.com" :key-bits 1024))))

(deftest test-certificate-with-email
  "Test generating certificate with email address"
  (with-fixture (fixture certificate-test-setup)
    (multiple-value-bind (cert-pem key-pem)
        (certs:generate-self-signed-certificate "email.test"
                                               :email "admin@email.test")
      (declare (ignore key-pem))
      
      (let ((info (certs:certificate-info cert-pem)))
        (is-not-null (search "emailAddress=admin@email.test" (getf info :subject)))))))
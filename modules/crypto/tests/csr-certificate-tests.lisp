;;;; Certificate Signing Request (CSR) and Certificate Test Suite
;;;;
;;;; Tests for X.509 certificates and CSRs

(defpackage :epsilon.crypto.cert-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:crypto #:epsilon.crypto)
   (#:log #:epsilon.log)))

(in-package :epsilon.crypto.cert-tests)

;;;; Helper Functions

(defun with-test-certificate (&key (key-type :rsa) (key-size 2048))
  "Create a test certificate for testing"
  (let ((key (case key-type
                   (:rsa (crypto:generate-rsa-key key-size))
                   (:ec (crypto:generate-ec-key :curve :p256))
                   (:ed25519 (crypto:generate-ed25519-key)))))
    (crypto:create-certificate 
     :key key
     :subject-cn "Test Certificate"
     :issuer-cn "Test CA"
     :serial 1
     :days 365)))

;;;; CSR Creation Tests

(deftest test-create-csr-rsa
  "Test creating a CSR with RSA key"
  (let ((key (crypto:generate-rsa-key :bits 2048)))
    (let ((csr-pem (crypto:create-csr key :subject-cn "test.example.com")))
      (is (stringp csr-pem))
      (is (search "-----BEGIN CERTIFICATE REQUEST-----" csr-pem))
      (is (search "-----END CERTIFICATE REQUEST-----" csr-pem)))))

(deftest test-create-csr-ec
  "Test creating a CSR with EC key"
  (let ((key (crypto:generate-ec-key :curve :p256)))
    (let ((csr-pem (crypto:create-csr key :subject-cn "ec.example.com")))
      (is (stringp csr-pem))
      (is (search "-----BEGIN CERTIFICATE REQUEST-----" csr-pem))
      (is (search "-----END CERTIFICATE REQUEST-----" csr-pem)))))

(deftest test-create-csr-ed25519
  "Test creating a CSR with Ed25519 key"
  (let ((key (crypto:generate-ed25519-key)))
    (let ((csr-pem (crypto:create-csr key :subject-cn "ed25519.example.com")))
      (is (stringp csr-pem))
      (is (search "-----BEGIN CERTIFICATE REQUEST-----" csr-pem))
      (is (search "-----END CERTIFICATE REQUEST-----" csr-pem)))))

(deftest test-csr-without-private-key
  "Test that CSR creation requires private key"
  (let* ((key (crypto:generate-rsa-key :bits 2048))
         ;; Get public key only
         (public-pem (crypto:key-to-pem key :private-p nil))
         (public-key (crypto:key-from-pem public-pem :private-p nil)))
    
    ;; Try to create CSR with public key only
    (handler-case
        (progn
          (crypto:create-csr public-key :subject-cn "fail.example.com")
          (is nil "Should have failed to create CSR without private key"))
      (error ()
	     (is t "Correctly rejected CSR without private key")))))

;;;; Certificate Creation Tests

(deftest test-create-self-signed-certificate
  "Test creating a self-signed certificate"
  (log:info "Starting self-signed certificate test")
  (let* ((key (progn 
                (log:info "Generating RSA key")
                (crypto:generate-rsa-key :bits 2048))))
    (log:info "RSA key generated successfully: ~A" (crypto:crypto-key-p key))
    (log:info "Creating certificate with subject/issuer: Self Signed Test")
    (handler-case
        (let ((cert (crypto:create-certificate
                     :key key
                     :subject-cn "Self Signed Test"
                     :issuer-cn "Self Signed Test"
                     :serial 42
                     :days 730)))
          (log:info "Certificate created successfully: ~A" (crypto:x509-certificate-p cert))
          (is (crypto:x509-certificate-p cert))
          (log:info "Certificate subject: ~A" (crypto:x509-certificate-subject cert))
          (log:info "Certificate issuer: ~A" (crypto:x509-certificate-issuer cert))
          (is (search "Self Signed Test" (crypto:x509-certificate-subject cert)))
          (is (search "Self Signed Test" (crypto:x509-certificate-issuer cert)))
          (is (equal (crypto:x509-certificate-serial cert) "42"))
          ;; Self-signed cert should verify with its own public key
          (let ((pub-key (crypto:certificate-public-key cert)))
            (is (crypto:verify-certificate cert pub-key))))
      (error (e)
        (log:error "Certificate creation failed with error: ~A" e)
        (is nil (format nil "Certificate creation failed: ~A" e))))))

(deftest test-create-certificate-with-ec-key
  "Test creating certificate with EC key"
  (let* ((key (crypto:generate-ec-key :curve :p384))
	 (cert (crypto:create-certificate
		:key key
		:subject-cn "EC Certificate"
		:serial 100
		:days 365)))
    
    (is (crypto:x509-certificate-p cert))
    (is (search "EC Certificate" (crypto:x509-certificate-subject cert)))
    
    ;; Extract public key and verify it's EC
    (let ((pub-key (crypto:certificate-public-key cert)))
      (is (crypto:crypto-key-p pub-key))
      (is (eq (crypto:crypto-key-type pub-key) :ec)))))

(deftest test-certificate-validity-period
  "Test certificate validity period settings"
  (let* ((key (crypto:generate-rsa-key :bits 2048))
	 (cert (crypto:create-certificate
		:key key
		:subject-cn "Validity Test"
		:days 90)))
    
    (is (crypto:x509-certificate-p cert))
    ;; Not-before should be around now (0)
    (is (numberp (crypto:x509-certificate-not-before cert)))
    ;; Not-after should be around 90 days from now
    (is (numberp (crypto:x509-certificate-not-after cert)))
    (let ((validity-days (/ (- (crypto:x509-certificate-not-after cert)
			       (crypto:x509-certificate-not-before cert))
                            86400)))
      ;; Should be approximately 90 days
      (is (<= 89 validity-days 91)))))

;;;; Certificate I/O Tests

(deftest test-certificate-pem-export-import
  "Test exporting and importing certificates in PEM format"
  (let* ((key (crypto:generate-rsa-key :bits 2048))
	 (original-cert (crypto:create-certificate
			 :key key
			 :subject-cn "Export Test"
			 :serial 999)))
    
    ;; Export to PEM
    (let ((cert-pem (crypto:save-certificate original-cert)))
      (is (stringp cert-pem))
      (is (search "-----BEGIN CERTIFICATE-----" cert-pem))
      (is (search "-----END CERTIFICATE-----" cert-pem))
      
      ;; Import from PEM
      (let ((imported-cert (crypto:load-certificate cert-pem)))
	(is (crypto:x509-certificate-p imported-cert))
	(is (search "Export Test" 
		    (crypto:x509-certificate-subject imported-cert)))
	(is (equal (crypto:x509-certificate-serial imported-cert) "999"))))))

(deftest test-invalid-certificate-pem
  "Test handling of invalid certificate PEM data"
  (handler-case
      (progn
	(crypto:load-certificate "invalid certificate PEM")
	(is nil "Should have failed to load invalid PEM"))
    (crypto:crypto-error ()
			 (is t "Correctly rejected invalid certificate PEM"))))

;;;; Certificate Public Key Extraction Tests

(deftest test-extract-public-key-from-certificate
  "Test extracting public key from certificate"
  (let* ((key (crypto:generate-rsa-key :bits 2048))
	 (cert (crypto:create-certificate
		:key key
		:subject-cn "Key Extraction Test"))
	 (extracted-key (crypto:certificate-public-key cert)))
    
    (is (crypto:crypto-key-p extracted-key))
    (is (crypto:crypto-key-public-p extracted-key))
    (is (not (crypto:crypto-key-private-p extracted-key)))
    (is (eq (crypto:crypto-key-type extracted-key) :rsa))
    (is-= (crypto:crypto-key-bits extracted-key) 2048)
    
    ;; Test that extracted key can verify signatures from original key
    (let* ((message "Test message for verification")
	   (signature (crypto:sign-message key message)))
      (is (crypto:verify-message extracted-key message signature)))))

;;;; Certificate Verification Tests

(deftest test-verify-self-signed-certificate
  "Test verification of self-signed certificate"
  (let* ((key (crypto:generate-rsa-key :bits 2048))
	 (cert (crypto:create-certificate
		:key key
		:subject-cn "Self Signed"
		:issuer-cn "Self Signed")))
    
    ;; Extract public key from certificate
    (let ((pub-key (crypto:certificate-public-key cert)))
      ;; Self-signed cert should verify with its own public key
      (is (crypto:verify-certificate cert pub-key)))))

(deftest test-verify-certificate-with-wrong-key
  "Test that certificate verification fails with wrong key"
  (let* ((key1 (crypto:generate-rsa-key :bits 2048))
	 (key2 (crypto:generate-rsa-key :bits 2048))
	 (cert (crypto:create-certificate
		:key key1
		:subject-cn "Test Cert")))
    
    ;; Try to verify with wrong key
    (is (not (crypto:verify-certificate cert key2)))))

;;;; Certificate Chain Tests (Simulated)

(deftest test-certificate-chain-scenario
  "Test a simulated certificate chain scenario"
  (let* (;; Create CA key and certificate
	 (ca-key (crypto:generate-rsa-key :bits 2048))
	 (ca-cert (crypto:create-certificate
		   :key ca-key
		   :subject-cn "Test Root CA"
		   :issuer-cn "Test Root CA"
		   :serial 1
		   :days 3650))
	 
	 ;; Create server key
	 (server-key (crypto:generate-rsa-key :bits 2048))
	 ;; In real scenario, CA would sign server's CSR
	 ;; Here we simulate with self-signed cert
	 (server-cert (crypto:create-certificate
		       :key server-key
		       :subject-cn "server.example.com"
		       :issuer-cn "Test Root CA"
		       :serial 1000
		       :days 365)))
    
    ;; Verify CA cert is self-signed
    (let ((ca-pub-key (crypto:certificate-public-key ca-cert)))
      (is (crypto:verify-certificate ca-cert ca-pub-key)))
    
    ;; In real scenario, server cert would be verified by CA's public key
    ;; Here we just verify structure
    (is (crypto:x509-certificate-p server-cert))
    (is (search "server.example.com" 
		(crypto:x509-certificate-subject server-cert)))
    (is (search "Test Root CA"
		(crypto:x509-certificate-issuer server-cert)))))

;;;; Key and Certificate Pair Tests

(deftest test-load-save-key-cert-pair
  "Test loading and saving key/certificate pairs"
  (let* ((key (crypto:generate-rsa-key :bits 2048))
	 (cert (crypto:create-certificate
		:key key
		:subject-cn "Pair Test"))
	 (key-file "/tmp/test-key.pem")
	 (cert-file "/tmp/test-cert.pem"))
    
    ;; Save key and certificate
    (crypto:save-key-and-cert-pair key cert key-file cert-file)
    
    ;; Check files were created
    (is (probe-file key-file))
    (is (probe-file cert-file))
    
    ;; Load them back
    (multiple-value-bind (loaded-key loaded-cert)
			 (crypto:load-key-and-cert-pair key-file cert-file)
			 
			 (is (crypto:crypto-key-p loaded-key))
			 (is (crypto:crypto-key-private-p loaded-key))
			 (is (crypto:x509-certificate-p loaded-cert))
			 (is (search "Pair Test" 
				     (crypto:x509-certificate-subject loaded-cert))))
    
    ;; Clean up
    (delete-file key-file)
    (delete-file cert-file)))

;;;; Certificate Subject/Issuer Tests

(deftest test-certificate-distinguished-names
  "Test certificate subject and issuer distinguished names"
  (let* ((key (crypto:generate-rsa-key :bits 2048))
	 (cert (crypto:create-certificate
		:key key
		:subject-cn "Test Subject CN"
		:issuer-cn "Test Issuer CN")))
    
    ;; Check subject contains CN
    (is (search "CN=Test Subject CN" 
		(crypto:x509-certificate-subject cert)))
    
    ;; Check issuer contains CN
    (is (search "CN=Test Issuer CN"
		(crypto:x509-certificate-issuer cert)))))

;;;; Performance Tests

(deftest test-certificate-creation-performance
  "Test performance of certificate creation"
  (let ((key (crypto:generate-rsa-key :bits 2048)))
    ;; Measure certificate creation time
    (let ((start (get-internal-real-time)))
      (dotimes (i 10)
	(crypto:create-certificate
	 :key key
	 :subject-cn (format nil "Perf Test ~D" i)
	 :serial i))
      (let ((elapsed (- (get-internal-real-time) start)))
	;; Should create 10 certificates in reasonable time
	(is (< elapsed (* 2 internal-time-units-per-second)))))))

(deftest test-csr-creation-performance
  "Test performance of CSR creation"
  (let ((key (crypto:generate-rsa-key :bits 2048)))
    ;; Measure CSR creation time
    (let ((start (get-internal-real-time)))
      (dotimes (i 10)
	(crypto:create-csr key 
			   :subject-cn (format nil "csr~D.example.com" i)))
      (let ((elapsed (- (get-internal-real-time) start)))
	;; Should create 10 CSRs quickly
	(is (< elapsed internal-time-units-per-second))))))

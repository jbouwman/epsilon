;;;; Basic Tests for epsilon.cryptography module
;;;;
;;;; These tests verify the module structure and basic functionality

(defpackage :epsilon.crypto.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:crypto #:epsilon.crypto)))

(in-package :epsilon.crypto.tests)

;;;; Structure Tests

(deftest test-module-loaded
  "Test that the cryptography module loads correctly"
  (is (find-package :epsilon.crypto)))

(deftest test-exported-symbols
  "Test that key symbols are exported"
  ;; TLS symbols
  (is (fboundp 'crypto:create-tls-context))
  (is (fboundp 'crypto:tls-connect))
  (is (fboundp 'crypto:tls-accept))
  (is (fboundp 'crypto:tls-read))
  (is (fboundp 'crypto:tls-write))
  
  ;; Crypto symbols
  (is (fboundp 'crypto:generate-rsa-key))
  (is (fboundp 'crypto:generate-ec-key))
  (is (fboundp 'crypto:generate-ed25519-key))
  (is (fboundp 'crypto:sign-message))
  (is (fboundp 'crypto:verify-message))
  (is (fboundp 'crypto:encrypt))
  (is (fboundp 'crypto:decrypt))
  
  ;; Certificate symbols
  (is (fboundp 'crypto:create-certificate))
  (is (fboundp 'crypto:load-certificate))
  (is (fboundp 'crypto:save-certificate)))

(deftest test-data-structures
  "Test that data structures are properly defined"
  ;; TLS structures
  (is (fboundp 'crypto:make-tls-context))
  (is (fboundp 'crypto:tls-context-p))
  (is (fboundp 'crypto:make-tls-connection))
  (is (fboundp 'crypto:tls-connection-p))
  
  ;; Crypto structures
  (is (fboundp 'crypto:make-crypto-key))
  (is (fboundp 'crypto:crypto-key-p))
  (is (fboundp 'crypto:crypto-key-type))
  (is (fboundp 'crypto:crypto-key-bits))
  
  ;; Certificate structures
  (is (fboundp 'crypto:make-x509-certificate))
  (is (fboundp 'crypto:x509-certificate-p))
  (is (fboundp 'crypto:x509-certificate-subject)))

(deftest test-constants
  "Test that constants are defined"
  (is (numberp crypto:+tls-verify-none+))
  (is (numberp crypto:+tls-verify-peer+))
  (is (stringp crypto:+digest-sha256+))
  (is (stringp crypto:+digest-sha512+)))

(deftest test-error-conditions
  "Test that error conditions are defined"
  (is (find-class 'crypto:crypto-error))
  (is (subtypep 'crypto:crypto-error 'error)))

(deftest test-structure-creation
  "Test creating basic structures without FFI"
  ;; Create TLS context
  (let ((ctx (crypto:make-tls-context :server-p nil 
                                      :cert-file "test.crt"
                                      :key-file "test.key")))
    (is (crypto:tls-context-p ctx))
    (is (not (crypto:tls-context-server-p ctx)))
    (is (equal (crypto:tls-context-cert-file ctx) "test.crt"))
    (is (equal (crypto:tls-context-key-file ctx) "test.key")))
  
  ;; Create crypto key structure
  (let ((key (crypto:make-crypto-key :type :rsa
                                     :bits 2048
                                     :public-p t
                                     :private-p t)))
    (is (crypto:crypto-key-p key))
    (is (eq (crypto:crypto-key-type key) :rsa))
    (is (= (crypto:crypto-key-bits key) 2048))
    (is (crypto:crypto-key-public-p key))
    (is (crypto:crypto-key-private-p key)))
  
  ;; Create certificate structure
  (let ((cert (crypto:make-x509-certificate :subject "CN=Test"
                                            :issuer "CN=Test CA"
                                            :serial "1")))
    (is (crypto:x509-certificate-p cert))
    (is (equal (crypto:x509-certificate-subject cert) "CN=Test"))
    (is (equal (crypto:x509-certificate-issuer cert) "CN=Test CA"))
    (is (equal (crypto:x509-certificate-serial cert) "1"))))

(deftest test-module-integration
  "Test that the module integrates properly with epsilon"
  ;; Just verify the package exists for now
  (is (find-package :epsilon.crypto))
  (is (find-package :epsilon.foreign)))
;;;; Unit Tests for mTLS Functionality
;;;;
;;;; Tests for mutual TLS authentication components

(defpackage :epsilon.crypto.test-mtls-unit
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:mtls #:epsilon.crypto.mtls)
   (#:tls #:epsilon.crypto.tls)
   (#:certs #:epsilon.crypto.certificates)
   (#:test-certs #:epsilon.crypto.test-certs)))

(in-package :epsilon.crypto.test-mtls-unit)

;;;; Test Setup

(defvar *test-suite* nil "Generated test certificate suite")

(defun setup-test-suite ()
  "Set up test certificates for mTLS tests"
  (unless *test-suite*
    (setf *test-suite* (test-certs:generate-mtls-test-suite))))

(defun teardown-test-suite ()
  "Clean up test certificates"
  (when *test-suite*
    (test-certs:cleanup-test-certs)
    (setf *test-suite* nil)))

;;;; Context Creation Tests

(deftest test-create-mtls-server-context
  "Test creating mTLS server context"
  (setup-test-suite)
  (let ((ctx (mtls:create-mtls-server-context
              :cert-file (getf *test-suite* :server-cert)
              :key-file (getf *test-suite* :server-key)
              :ca-file (getf *test-suite* :ca-cert)
              :verify-depth 4)))
    (is-not-null ctx)
    (is-true (tls:openssl-context-p ctx))
    (is-true (tls:openssl-context-server-p ctx))
    (is-equal (getf *test-suite* :server-cert) 
             (tls:openssl-context-cert-file ctx))))

(deftest test-create-mtls-client-context
  "Test creating mTLS client context"
  (setup-test-suite)
  (let ((ctx (mtls:create-mtls-client-context
              :cert-file (getf *test-suite* :client1-cert)
              :key-file (getf *test-suite* :client1-key)
              :ca-file (getf *test-suite* :ca-cert))))
    (is-not-null ctx)
    (is-true (tls:openssl-context-p ctx))
    (is-false (tls:openssl-context-server-p ctx))
    (is-equal (getf *test-suite* :client1-cert)
             (tls:openssl-context-cert-file ctx))))

(deftest test-context-requires-cert-and-key
  "Test that mTLS contexts require both cert and key"
  (setup-test-suite)
  ;; Server context requires all three files
  (signals error
    (mtls:create-mtls-server-context
     :cert-file (getf *test-suite* :server-cert)
     :key-file nil
     :ca-file (getf *test-suite* :ca-cert)))
  
  ;; Client context requires cert and key for mTLS
  (signals error
    (mtls:create-mtls-client-context
     :cert-file (getf *test-suite* :client1-cert)
     :key-file nil)))

(deftest test-configure-mtls-context
  "Test configuring mTLS context with additional settings"
  (setup-test-suite)
  (let ((ctx (mtls:create-mtls-server-context
              :cert-file (getf *test-suite* :server-cert)
              :key-file (getf *test-suite* :server-key)
              :ca-file (getf *test-suite* :ca-cert))))
    
    ;; Test strict mode configuration
    (is-no-error
      (mtls:configure-mtls-context ctx :strict-mode t))
    
    ;; Test cipher suite configuration
    (is-no-error
      (mtls:configure-mtls-context ctx 
                                   :cipher-suites '("ECDHE-RSA-AES256-GCM-SHA384"
                                                   "ECDHE-RSA-AES128-GCM-SHA256")))
    
    ;; Test minimum TLS version
    (is-no-error
      (mtls:configure-mtls-context ctx :min-tls-version "TLSv1.2"))))

;;;; Certificate Information Tests

(deftest test-get-certificate-cn
  "Test extracting Common Name from certificate"
  ;; Create a mock certificate with known subject
  (let ((cert (tls:make-x509-certificate 
               :subject "/C=US/O=Test Org/CN=test.example.com"
               :issuer "/C=US/O=Test CA/CN=Test CA")))
    (is-equal "test.example.com" (mtls:get-certificate-cn cert))))

(deftest test-get-certificate-cn-complex
  "Test CN extraction with complex subject"
  (let ((cert (tls:make-x509-certificate
               :subject "/C=US/ST=CA/L=SF/O=Org/OU=Unit/CN=server.test.com/emailAddress=admin@test.com"
               :issuer "/CN=CA")))
    (is-equal "server.test.com" (mtls:get-certificate-cn cert))))

(deftest test-match-certificate-hostname
  "Test hostname matching against certificate"
  (let ((cert (tls:make-x509-certificate
               :subject "/CN=test.example.com"
               :issuer "/CN=CA")))
    ;; Exact match
    (is-true (mtls:match-certificate-hostname cert "test.example.com"))
    ;; No match
    (is-false (mtls:match-certificate-hostname cert "other.example.com"))
    ;; Case insensitive
    (is-true (mtls:match-certificate-hostname cert "TEST.EXAMPLE.COM"))))

;;;; Session Management Tests

(deftest test-enable-session-resumption
  "Test enabling TLS session resumption"
  (setup-test-suite)
  (let ((ctx (mtls:create-mtls-server-context
              :cert-file (getf *test-suite* :server-cert)
              :key-file (getf *test-suite* :server-key)
              :ca-file (getf *test-suite* :ca-cert))))
    
    (is-no-error
      (mtls:enable-session-resumption ctx :timeout 300 :cache-size 1024))))

(deftest test-set-session-id-context
  "Test setting session ID context"
  (setup-test-suite)
  (let ((ctx (mtls:create-mtls-server-context
              :cert-file (getf *test-suite* :server-cert)
              :key-file (getf *test-suite* :server-key)
              :ca-file (getf *test-suite* :ca-cert))))
    
    (is-no-error
      (mtls:set-session-id-context ctx "test-server-v1"))))

;;;; CA List Tests

(deftest test-set-client-ca-list
  "Test setting client CA list for server"
  (setup-test-suite)
  (let ((ctx (mtls:create-mtls-server-context
              :cert-file (getf *test-suite* :server-cert)
              :key-file (getf *test-suite* :server-key)
              :ca-file (getf *test-suite* :ca-cert))))
    
    (is-no-error
      (mtls:set-client-ca-list ctx (getf *test-suite* :ca-cert)))))

;;;; High-level Helper Tests

(deftest test-with-mtls-server-macro
  "Test with-mtls-server macro"
  (setup-test-suite)
  (is-no-error
    (mtls:with-mtls-server (server :port 8443
                                   :cert-file (getf *test-suite* :server-cert)
                                   :key-file (getf *test-suite* :server-key)
                                   :ca-file (getf *test-suite* :ca-cert))
      ;; Server context should be created
      (is-not-null server)
      (is-true (tls:openssl-context-p server)))))

(deftest test-with-mtls-client-macro
  "Test with-mtls-client macro (mock connection)"
  (setup-test-suite)
  ;; This test would normally connect to a server
  ;; For unit testing, we just verify the macro expands correctly
  (is-no-error
    (macroexpand-1 
     '(mtls:with-mtls-client (client :host "localhost"
                                     :port 8443
                                     :cert-file (getf *test-suite* :client1-cert)
                                     :key-file (getf *test-suite* :client1-key)
                                     :ca-file (getf *test-suite* :ca-cert))
       (format t "Client: ~A" client)))))

;;;; Error Handling Tests

(deftest test-invalid-certificate-file
  "Test error handling for invalid certificate files"
  (signals error
    (mtls:create-mtls-server-context
     :cert-file "/nonexistent/cert.pem"
     :key-file "/nonexistent/key.pem"
     :ca-file "/nonexistent/ca.pem")))

(deftest test-certificate-key-mismatch
  "Test error when certificate and key don't match"
  (setup-test-suite)
  ;; Try to use server cert with client key (mismatch)
  (signals error
    (tls:create-openssl-context
     :server-p t
     :cert-file (getf *test-suite* :server-cert)
     :key-file (getf *test-suite* :client1-key)  ; Wrong key
     :ca-file (getf *test-suite* :ca-cert))))

;;;; Certificate Validation Mock Tests

(deftest test-verify-client-certificate-mock
  "Test client certificate verification logic"
  ;; Create mock connection with certificate
  (let ((mock-cert (tls:make-x509-certificate
                   :subject "/CN=client1"
                   :issuer "/CN=Test CA")))
    
    ;; Test CN verification
    (let ((cn (mtls:get-certificate-cn mock-cert)))
      (is-equal "client1" cn))
    
    ;; Test hostname matching
    (is-true (mtls:match-certificate-hostname mock-cert "client1"))
    (is-false (mtls:match-certificate-hostname mock-cert "client2"))))

;;;; ALPN Configuration Tests

(deftest test-context-with-alpn
  "Test creating context with ALPN protocols"
  (setup-test-suite)
  (let ((ctx (mtls:create-mtls-server-context
              :cert-file (getf *test-suite* :server-cert)
              :key-file (getf *test-suite* :server-key)
              :ca-file (getf *test-suite* :ca-cert)
              :alpn-protocols '("h2" "http/1.1"))))
    (is-not-null ctx)))

;;;; Test Suite Cleanup

(deftest test-cleanup
  "Clean up test suite after all tests"
  (teardown-test-suite)
  (is-true t))  ; Always pass, just for cleanup
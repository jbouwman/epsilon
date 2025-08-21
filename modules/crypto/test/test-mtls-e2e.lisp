;;;; End-to-End mTLS Test Script
;;;;
;;;; This script demonstrates full mTLS functionality with self-signed certificates

(format t "~%Checking required modules...~%")

;; Check if modules are available
(unless (find-package :epsilon.crypto)
  (format t "ERROR: epsilon.crypto module not loaded!~%")
  (format t "Please run with: ./epsilon --module epsilon.crypto --module epsilon.http --eval \"(load \\\"test-mtls-e2e.lisp\\\")\"~%")
  (error "Required module epsilon.crypto not found"))

(unless (find-package :epsilon.http)
  (format t "ERROR: epsilon.http module not loaded!~%")
  (format t "Please run with: ./epsilon --module epsilon.crypto --module epsilon.http --eval \"(load \\\"test-mtls-e2e.lisp\\\")\"~%")
  (error "Required module epsilon.http not found"))

(format t "Modules loaded successfully.~%~%")

;;;; Test Configuration

(defparameter *test-dir* "/tmp/epsilon-mtls-e2e-test/")
(defparameter *test-port* 8443)
(defparameter *test-server* nil)

;;;; Helper Functions

(defun ensure-test-dir ()
  "Ensure test directory exists"
  (ensure-directories-exist *test-dir*))

(defun cleanup-test-dir ()
  "Clean up test directory"
  (when (probe-file *test-dir*)
    (dolist (file (directory (merge-pathnames "*" *test-dir*)))
      (when (probe-file file)
        (delete-file file)))
    (epsilon.sys.fs:delete-directory *test-dir*)))

;;;; Certificate Generation Tests

(defun test-certificate-generation ()
  "Test generating certificates"
  (format t "Testing certificate generation...~%")
  (ensure-test-dir)
  
  ;; Test 1: Generate self-signed certificate
  (format t "  1. Generating self-signed certificate...~%")
  (handler-case
      (multiple-value-bind (cert key)
          (epsilon.crypto.certificates:generate-self-signed-certificate
           "test-server.local"
           :organization "Test Organization"
           :country "US"
           :state "CA"
           :locality "San Francisco"
           :days 365
           :dns-names '("test-server.local" "localhost")
           :ip-addresses '("127.0.0.1" "::1"))
        (let ((cert-file (merge-pathnames "server-cert.pem" *test-dir*))
              (key-file (merge-pathnames "server-key.pem" *test-dir*)))
          (epsilon.crypto.certificates:save-certificate cert cert-file)
          (epsilon.crypto.certificates:save-private-key key key-file)
          (format t "     ✓ Self-signed certificate generated and saved~%")
          (format t "       - Certificate: ~A~%" cert-file)
          (format t "       - Private Key: ~A~%" key-file)))
    (error (e)
      (format t "     ✗ Failed to generate self-signed certificate: ~A~%" e)
      (return-from test-certificate-generation nil)))
  
  ;; Test 2: Generate CA certificate
  (format t "  2. Generating CA certificate...~%")
  (handler-case
      (multiple-value-bind (ca-cert ca-key)
          (epsilon.crypto.certificates:generate-ca-certificate
           "Test Root CA"
           :organization "Test CA Organization"
           :country "US"
           :days 3650
)
        (let ((ca-cert-file (merge-pathnames "ca-cert.pem" *test-dir*))
              (ca-key-file (merge-pathnames "ca-key.pem" *test-dir*)))
          (epsilon.crypto.certificates:save-certificate ca-cert ca-cert-file)
          (epsilon.crypto.certificates:save-private-key ca-key ca-key-file)
          (format t "     ✓ CA certificate generated and saved~%")
          (format t "       - CA Certificate: ~A~%" ca-cert-file)
          (format t "       - CA Private Key: ~A~%" ca-key-file)))
    (error (e)
      (format t "     ✗ Failed to generate CA certificate: ~A~%" e)
      (return-from test-certificate-generation nil)))
  
  ;; Test 3: Generate and sign client certificate
  (format t "  3. Generating client certificate...~%")
  (handler-case
      (let* ((client-key (epsilon.crypto.openssl3:generate-rsa-key 2048))
             (client-csr (epsilon.crypto.certificates:generate-certificate-request
                         "test-client"
                         client-key
                         :organization "Test Client"
                         :country "US"))
             (ca-cert (epsilon.crypto.certificates:load-certificate
                      (merge-pathnames "ca-cert.pem" *test-dir*)))
             (ca-key (epsilon.crypto.certificates:load-private-key
                     (merge-pathnames "ca-key.pem" *test-dir*)))
             (client-cert (epsilon.crypto.certificates:sign-certificate-request
                          client-csr ca-cert ca-key
                          :days 365)))
        (let ((client-cert-file (merge-pathnames "client-cert.pem" *test-dir*))
              (client-key-file (merge-pathnames "client-key.pem" *test-dir*)))
          (epsilon.crypto.certificates:save-certificate client-cert client-cert-file)
          (epsilon.crypto.certificates:save-private-key 
           (epsilon.crypto.certificates::private-key-to-pem client-key) client-key-file)
          (format t "     ✓ Client certificate generated and signed~%")
          (format t "       - Client Certificate: ~A~%" client-cert-file)
          (format t "       - Client Private Key: ~A~%" client-key-file)))
    (error (e)
      (format t "     ✗ Failed to generate client certificate: ~A~%" e)
      (return-from test-certificate-generation nil)))
  
  (format t "  Certificate generation tests completed successfully!~%~%")
  t)

;;;; TLS Context Tests

(defun test-tls-contexts ()
  "Test creating TLS contexts with certificates"
  (format t "Testing TLS context creation...~%")
  
  ;; Test 1: Create server TLS context
  (format t "  1. Creating server TLS context...~%")
  (handler-case
      (let ((server-ctx (epsilon.crypto:create-tls-context
                        :server-p t
                        :cert-file (namestring (merge-pathnames "server-cert.pem" *test-dir*))
                        :key-file (namestring (merge-pathnames "server-key.pem" *test-dir*))
                        :ca-file (namestring (merge-pathnames "ca-cert.pem" *test-dir*))
                        :verify-mode epsilon.crypto:+tls-verify-peer+
                        :require-client-cert t)))
        (format t "     ✓ Server TLS context created~%")
        (format t "       - Server mode: ~A~%" (epsilon.crypto:tls-context-server-p server-ctx))
        (format t "       - Verify mode: ~A~%" (epsilon.crypto:tls-context-verify-mode server-ctx)))
    (error (e)
      (format t "     ✗ Failed to create server TLS context: ~A~%" e)
      (return-from test-tls-contexts nil)))
  
  ;; Test 2: Create client TLS context
  (format t "  2. Creating client TLS context...~%")
  (handler-case
      (let ((client-ctx (epsilon.crypto:create-tls-context
                        :server-p nil
                        :cert-file (namestring (merge-pathnames "client-cert.pem" *test-dir*))
                        :key-file (namestring (merge-pathnames "client-key.pem" *test-dir*))
                        :ca-file (namestring (merge-pathnames "ca-cert.pem" *test-dir*))
                        :verify-mode epsilon.crypto:+tls-verify-peer+)))
        (format t "     ✓ Client TLS context created~%")
        (format t "       - Server mode: ~A~%" (epsilon.crypto:tls-context-server-p client-ctx))
        (format t "       - Certificate loaded: ~A~%" 
                (not (null (epsilon.crypto:tls-context-cert-file client-ctx)))))
    (error (e)
      (format t "     ✗ Failed to create client TLS context: ~A~%" e)
      (return-from test-tls-contexts nil)))
  
  (format t "  TLS context tests completed successfully!~%~%")
  t)

;;;; HTTP mTLS Tests

(defun test-http-mtls ()
  "Test HTTP with mTLS"
  (format t "Testing HTTP with mTLS...~%")
  
  ;; Test 1: Create HTTPS server with client certificate requirement
  (format t "  1. Starting HTTPS server with mTLS...~%")
  (handler-case
      (let ((handler (lambda (req)
                      (declare (ignore req))
                      (epsilon.http:make-response 
                       :status 200 
                       :body "mTLS connection successful!"))))
        (setf *test-server*
              (epsilon.http.server:start-server handler
                                               :port *test-port*
                                               :ssl-p t
                                               :cert-file (namestring (merge-pathnames "server-cert.pem" *test-dir*))
                                               :key-file (namestring (merge-pathnames "server-key.pem" *test-dir*))
                                               :ca-file (namestring (merge-pathnames "ca-cert.pem" *test-dir*))
                                               :require-client-cert t))
        (format t "     ✓ HTTPS server started on port ~A~%" *test-port*)
        (format t "       - SSL enabled: ~A~%" (epsilon.http.server::server-ssl-p *test-server*))
        (format t "       - Client cert required: ~A~%" 
                (epsilon.http.server::server-require-client-cert *test-server*)))
    (error (e)
      (format t "     ✗ Failed to start HTTPS server: ~A~%" e)
      (return-from test-http-mtls nil)))
  
  ;; Give server time to start
  (sleep 1)
  
  ;; Test 2: Make HTTPS request with client certificate
  (format t "  2. Making HTTPS request with client certificate...~%")
  (handler-case
      (let ((response (epsilon.http:request 
                      (format nil "https://localhost:~A/test" *test-port*)
                      :method "GET"
                      :cert-file (namestring (merge-pathnames "client-cert.pem" *test-dir*))
                      :key-file (namestring (merge-pathnames "client-key.pem" *test-dir*))
                      :ca-file (namestring (merge-pathnames "ca-cert.pem" *test-dir*)))))
        (format t "     ✓ HTTPS request successful~%")
        (format t "       - Status: ~A~%" (epsilon.http:response-status response))
        (format t "       - Body: ~A~%" (epsilon.http:response-body response)))
    (error (e)
      (format t "     ✗ Failed to make HTTPS request: ~A~%" e)))
  
  ;; Test 3: Test request without client certificate (should fail)
  (format t "  3. Testing request without client certificate (should fail)...~%")
  (handler-case
      (let ((response (epsilon.http:request 
                      (format nil "https://localhost:~A/test" *test-port*)
                      :method "GET"
                      :ca-file (namestring (merge-pathnames "ca-cert.pem" *test-dir*)))))
        (format t "     ✗ Request succeeded without client certificate (unexpected)~%")
        (format t "       - Status: ~A~%" (epsilon.http:response-status response)))
    (error (e)
      (format t "     ✓ Request correctly failed without client certificate~%")
      (format t "       - Error: ~A~%" e)))
  
  ;; Clean up server
  (when *test-server*
    (epsilon.http.server:stop-server *test-server*)
    (setf *test-server* nil)
    (format t "     Server stopped~%"))
  
  (format t "  HTTP mTLS tests completed!~%~%")
  t)

;;;; ALPN Tests

(defun test-alpn ()
  "Test ALPN protocol negotiation"
  (format t "Testing ALPN protocol negotiation...~%")
  
  ;; Test 1: Create ALPN protocol buffer
  (format t "  1. Creating ALPN protocol buffer...~%")
  (handler-case
      (let ((protos-buffer (epsilon.crypto.alpn:make-alpn-protos-buffer 
                           '("h2" "http/1.1" "http/1.0"))))
        (format t "     ✓ ALPN protocol buffer created~%")
        (format t "       - Protocols: h2, http/1.1, http/1.0~%"))
    (error (e)
      (format t "     ✗ Failed to create ALPN protocol buffer: ~A~%" e)
      (return-from test-alpn nil)))
  
  ;; Test 2: Start server with ALPN
  (format t "  2. Starting server with ALPN support...~%")
  (handler-case
      (let ((handler (lambda (req)
                      (declare (ignore req))
                      (epsilon.http:make-response 
                       :status 200 
                       :body "ALPN negotiation successful!"))))
        (setf *test-server*
              (epsilon.http.server:start-server handler
                                               :port *test-port*
                                               :ssl-p t
                                               :cert-file (namestring (merge-pathnames "server-cert.pem" *test-dir*))
                                               :key-file (namestring (merge-pathnames "server-key.pem" *test-dir*))
                                               :alpn-protocols '("h2" "http/1.1")))
        (format t "     ✓ Server started with ALPN support~%")
        (format t "       - Supported protocols: ~A~%" 
                (epsilon.http.server::server-alpn-protocols *test-server*)))
    (error (e)
      (format t "     ✗ Failed to start server with ALPN: ~A~%" e)
      (return-from test-alpn nil)))
  
  ;; Clean up
  (when *test-server*
    (epsilon.http.server:stop-server *test-server*)
    (setf *test-server* nil))
  
  (format t "  ALPN tests completed!~%~%")
  t)

;;;; Main Test Runner

(defun run-all-tests ()
  "Run all mTLS tests"
  (format t "~%========================================~%")
  (format t "    Epsilon mTLS End-to-End Tests~%")
  (format t "========================================~%~%")
  
  (let ((all-passed t))
    ;; Run certificate generation tests
    (unless (test-certificate-generation)
      (setf all-passed nil))
    
    ;; Run TLS context tests
    (unless (test-tls-contexts)
      (setf all-passed nil))
    
    ;; Run HTTP mTLS tests
    (unless (test-http-mtls)
      (setf all-passed nil))
    
    ;; Run ALPN tests
    (unless (test-alpn)
      (setf all-passed nil))
    
    ;; Summary
    (format t "========================================~%")
    (if all-passed
        (format t "  All tests PASSED!~%")
        (format t "  Some tests FAILED!~%"))
    (format t "========================================~%~%")
    
    ;; Clean up
    (cleanup-test-dir)
    (format t "Test directory cleaned up.~%")
    
    all-passed))

;; Run the tests
(run-all-tests)
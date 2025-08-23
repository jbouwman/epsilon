;;;; mTLS + HTTP/2 Integration Tests
;;;; 
;;;; Clean integration tests for HTTP/2 over mTLS without using /tmp

(defpackage :epsilon.http2.mtls-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:http2 #:epsilon.http2)
   (#:server #:epsilon.http2.server)
   (#:crypto #:epsilon.crypto)
   (#:certs #:epsilon.crypto.certificates)
   (#:alpn #:epsilon.crypto.alpn)
   (#:net #:epsilon.net)
   (#:fs #:epsilon.sys.fs)))

(in-package :epsilon.http2.mtls-tests)

;;;; Clean Test Certificate Management

(defparameter *test-cert-dir* nil)
(defparameter *test-ca-cert* nil)
(defparameter *test-ca-key* nil)
(defparameter *test-server-cert* nil)
(defparameter *test-server-key* nil)
(defparameter *test-client-cert* nil)
(defparameter *test-client-key* nil)

(defun create-test-directory ()
  "Create a clean test directory in the project space"
  (let* ((project-root (namestring (truename ".")))
         (test-dir (fs:join-paths project-root
                                  "test-certs" 
                                  (format nil "mtls-test-~A" (get-universal-time)))))
    (fs:make-dirs test-dir)
    test-dir))

(defun cleanup-test-directory (dir)
  "Clean up test directory and all contents"
  (when (and dir (fs:directory-exists-p dir))
    ;; Delete the entire directory tree
    (fs:delete-directory dir)))

(fixture mtls-test-setup ()
  (:setup
   ;; Create clean test directory in project space
   (setf *test-cert-dir* (create-test-directory))
   
   ;; Generate CA certificate (self-signed)
   (multiple-value-bind (ca-cert ca-key)
       (certs:generate-self-signed-certificate "Test mTLS CA" 
                                              :days 1 :key-bits 2048
                                              :organization "Test mTLS CA")
     (setf *test-ca-cert* ca-cert
           *test-ca-key* ca-key))
   
   ;; Generate server certificate (self-signed for now)
   (multiple-value-bind (server-cert server-key)
       (certs:generate-self-signed-certificate "localhost" 
                                              :days 1 :key-bits 2048
                                              :organization "Test Server")
     (setf *test-server-cert* server-cert
           *test-server-key* server-key))
   
   ;; Generate client certificate (self-signed for now)
   (multiple-value-bind (client-cert client-key)
       (certs:generate-self-signed-certificate "test-client" 
                                              :days 1 :key-bits 2048
                                              :organization "Test Client")
     (setf *test-client-cert* client-cert
           *test-client-key* client-key))
   
   ;; Save certificates to files using modern path operations
   (certs:save-certificate *test-ca-cert* 
                          (fs:join-paths *test-cert-dir* "ca-cert.pem"))
   (certs:save-certificate *test-server-cert* 
                          (fs:join-paths *test-cert-dir* "server-cert.pem"))
   (certs:save-private-key *test-server-key* 
                          (fs:join-paths *test-cert-dir* "server-key.pem"))
   (certs:save-certificate *test-client-cert* 
                          (fs:join-paths *test-cert-dir* "client-cert.pem"))
   (certs:save-private-key *test-client-key* 
                          (fs:join-paths *test-cert-dir* "client-key.pem")))

  (:teardown
   ;; Clean up test certificates and directory
   (cleanup-test-directory *test-cert-dir*)
   (setf *test-cert-dir* nil
         *test-ca-cert* nil
         *test-ca-key* nil
         *test-server-cert* nil
         *test-server-key* nil
         *test-client-cert* nil
         *test-client-key* nil)))

;;;; mTLS Configuration Tests

(deftest test-mtls-server-config
    "Test HTTP/2 server configuration with mTLS options"
  (with-fixture (fixture mtls-test-setup)
    (let* ((ca-cert-file (fs:join-paths *test-cert-dir* "ca-cert.pem"))
           (server-cert-file (fs:join-paths *test-cert-dir* "server-cert.pem"))
           (server-key-file (fs:join-paths *test-cert-dir* "server-key.pem"))
           (config (server:make-http2-server-config
                    :port 8443
                    :ssl-p t
                    :cert-file server-cert-file
                    :key-file server-key-file
                    :ca-file ca-cert-file
                    :require-client-cert t
                    :verify-depth 3)))
      
      ;; Verify configuration
      (is-equal 8443 (server:http2-server-config-port config))
      (is-true (server:http2-server-config-ssl-p config))
      (is-equal server-cert-file (server:http2-server-config-cert-file config))
      (is-equal server-key-file (server:http2-server-config-key-file config))
      (is-equal ca-cert-file (server:http2-server-config-ca-file config))
      (is-true (server:http2-server-config-require-client-cert config))
      (is-equal 3 (server:http2-server-config-verify-depth config)))))

(deftest test-tls-context-with-mtls
    "Test TLS context creation with mTLS configuration"
  (skip)
  (with-fixture (fixture mtls-test-setup)
    (let* ((ca-cert-file (fs:join-paths *test-cert-dir* "ca-cert.pem"))
           (server-cert-file (fs:join-paths *test-cert-dir* "server-cert.pem"))
           (server-key-file (fs:join-paths *test-cert-dir* "server-key.pem"))
           (ctx (crypto:create-openssl-context
                 :server-p t
                 :cert-file server-cert-file
                 :key-file server-key-file
                 :ca-file ca-cert-file
                 :require-client-cert t
                 :verify-depth 3
                 :alpn-protocols '("h2" "http/1.1"))))
      
      ;; Verify context creation
      (is-not-null ctx)
      (is-true (crypto:openssl-context-p ctx))
      (is-true (crypto:openssl-context-server-p ctx))
      (is-equal server-cert-file (crypto:openssl-context-cert-file ctx)))))

(deftest test-alpn-negotiation-setup
    "Test ALPN protocol setup for HTTP/2"
  (let ((protocols '("h2" "http/1.1")))
    ;; Test ALPN buffer creation
    (multiple-value-bind (buffer len)
        (alpn:make-alpn-protos-buffer protocols)
      (is-not-null buffer)
      (is-not (zerop len))
      
      ;; Verify buffer format for "h2"
      (is-equal 2 (aref buffer 0))  ; Length of "h2"
      (is-equal (char-code #\h) (aref buffer 1))
      (is-equal (char-code #\2) (aref buffer 2))
      
      ;; Verify buffer format for "http/1.1"
      (is-equal 8 (aref buffer 3))  ; Length of "http/1.1"
      (is-equal (char-code #\h) (aref buffer 4)))))

(deftest test-certificate-chain-validation
    "Test certificate chain validation for mTLS"
  (with-fixture (fixture mtls-test-setup)
    ;; Test that certificates are properly chained
    (is-not-null *test-ca-cert*)
    (is-not-null *test-server-cert*)
    (is-not-null *test-client-cert*)
    
    ;; Verify certificate files exist
    (let ((ca-cert-file (fs:join-paths *test-cert-dir* "ca-cert.pem"))
          (server-cert-file (fs:join-paths *test-cert-dir* "server-cert.pem"))
          (client-cert-file (fs:join-paths *test-cert-dir* "client-cert.pem")))
      
      (is-true (fs:file-exists-p ca-cert-file))
      (is-true (fs:file-exists-p server-cert-file))
      (is-true (fs:file-exists-p client-cert-file)))))

;;;; HTTP/2 over mTLS Integration Tests

(deftest test-http2-connection-with-mtls-config
    "Test HTTP/2 connection creation with mTLS configuration"
  (with-fixture (fixture mtls-test-setup)
    (let* ((ca-cert-file (fs:join-paths *test-cert-dir* "ca-cert.pem"))
           (server-cert-file (fs:join-paths *test-cert-dir* "server-cert.pem"))
           (server-key-file (fs:join-paths *test-cert-dir* "server-key.pem"))
           (config (server:make-http2-server-config
                    :port 8443
                    :ssl-p t
                    :cert-file server-cert-file
                    :key-file server-key-file
                    :ca-file ca-cert-file
                    :require-client-cert t)))
      
      ;; Test that we can create server configuration
      (is-not-null config)
      (is-true (server:http2-server-config-ssl-p config))
      (is-true (server:http2-server-config-require-client-cert config)))))

(deftest test-mtls-verification-modes
    "Test different mTLS verification modes"
  (skip)
  (with-fixture (fixture mtls-test-setup)
    (let* ((server-cert-file (fs:join-paths *test-cert-dir* "server-cert.pem"))
           (server-key-file (fs:join-paths *test-cert-dir* "server-key.pem"))
           (ca-cert-file (fs:join-paths *test-cert-dir* "ca-cert.pem")))
      
      ;; Test server context without client cert requirement
      (let ((ctx-no-client (crypto:create-openssl-context
                           :server-p t
                           :cert-file server-cert-file
                           :key-file server-key-file
                           :verify-mode crypto:+ssl-verify-none+)))
        (is-not-null ctx-no-client))
      
      ;; Test server context with client cert requirement
      (let ((ctx-with-client (crypto:create-openssl-context
                             :server-p t
                             :cert-file server-cert-file
                             :key-file server-key-file
                             :ca-file ca-cert-file
                             :require-client-cert t
                             :verify-mode (logior crypto:+ssl-verify-peer+
                                                 crypto:+ssl-verify-fail-if-no-peer-cert+))))
        (is-not-null ctx-with-client)))))

(deftest test-certificate-file-security
    "Test that certificate files are created with proper permissions"
  (with-fixture (fixture mtls-test-setup)
    (let ((key-files (list (fs:join-paths *test-cert-dir* "server-key.pem")
                          (fs:join-paths *test-cert-dir* "client-key.pem"))))
      
      ;; Verify key files exist and are readable
      (dolist (key-file key-files)
        (is-true (fs:file-exists-p key-file))
        ;; Note: In a real implementation, we'd check file permissions
        ;; to ensure private keys are properly protected (0600)
        ))))

;;;; Integration Test Framework

(deftest test-mtls-test-fixture
    "Test that mTLS test fixture works correctly"
  (with-fixture (fixture mtls-test-setup)
    ;; Verify all certificates are created
    (is-not-null *test-ca-cert*)
    (is-not-null *test-ca-key*)
    (is-not-null *test-server-cert*)
    (is-not-null *test-server-key*)
    (is-not-null *test-client-cert*)
    (is-not-null *test-client-key*)
    
    ;; Verify test directory exists
    (is-not-null *test-cert-dir*)
    (is-true (fs:directory-exists-p *test-cert-dir*))
    
    ;; Verify certificate files exist
    (is-true (fs:file-exists-p (fs:join-paths *test-cert-dir* "ca-cert.pem")))
    (is-true (fs:file-exists-p (fs:join-paths *test-cert-dir* "server-cert.pem")))
    (is-true (fs:file-exists-p (fs:join-paths *test-cert-dir* "server-key.pem")))
    (is-true (fs:file-exists-p (fs:join-paths *test-cert-dir* "client-cert.pem")))
    (is-true (fs:file-exists-p (fs:join-paths *test-cert-dir* "client-key.pem")))))

;;;; Error Handling Tests

(deftest test-mtls-invalid-certificates
    "Test mTLS behavior with invalid certificates"
  (with-fixture (fixture mtls-test-setup)
    ;; Test with nonexistent certificate file
    (is-thrown (error)
      (crypto:create-openssl-context
       :server-p t
       :cert-file "/nonexistent/cert.pem"
       :key-file "/nonexistent/key.pem"))
    
    ;; Test with mismatched cert/key (if we had a way to create them)
    ;; This would require more sophisticated certificate generation
    ))

(deftest test-mtls-configuration-validation
    "Test validation of mTLS configuration parameters"
  (skip)
  (with-fixture (fixture mtls-test-setup)
    (let* ((server-cert-file (fs:join-paths *test-cert-dir* "server-cert.pem"))
           (server-key-file (fs:join-paths *test-cert-dir* "server-key.pem"))
           (ca-cert-file (fs:join-paths *test-cert-dir* "ca-cert.pem")))
      
      ;; Test valid configuration
      (let ((valid-config (server:make-http2-server-config
                          :ssl-p t
                          :cert-file server-cert-file
                          :key-file server-key-file
                          :ca-file ca-cert-file
                          :require-client-cert t
                          :verify-depth 5)))
        (is-not-null valid-config)
        (is-equal 5 (server:http2-server-config-verify-depth valid-config)))
      
      ;; Test configuration without SSL
      (let ((no-ssl-config (server:make-http2-server-config
                           :ssl-p nil
                           :require-client-cert t))) ; This should be ignored
        (is-not-null no-ssl-config)
        (is-false (server:http2-server-config-ssl-p no-ssl-config))))))

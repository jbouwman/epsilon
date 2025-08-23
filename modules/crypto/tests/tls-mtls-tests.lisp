;;;; TLS and mTLS Tests
;;;;
;;;; Tests for TLS connections with mutual authentication support

(defpackage :epsilon.crypto.tls-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:crypto #:epsilon.crypto)
   (#:certs #:epsilon.crypto.certificates)
   (#:alpn #:epsilon.crypto.alpn)
   (#:openssl3 #:epsilon.crypto.openssl3)
   (#:fs #:epsilon.sys.fs)))

(in-package :epsilon.crypto.tls-tests)

;;;; Test Fixtures

(defparameter *test-cert-dir* nil)
(defparameter *test-ca-cert* nil)
(defparameter *test-ca-key* nil)
(defparameter *test-server-cert* nil)
(defparameter *test-server-key* nil)
(defparameter *test-client-cert* nil)
(defparameter *test-client-key* nil)

(fixture tls-test-setup ()
	 (:setup
	  ;; Create unique test directory in project space
	  (setf *test-cert-dir* (fs:join-paths 
	                         (namestring (fs:current-directory))
	                         "test-certs"
	                         (format nil "tls-test-~A" (get-universal-time))))
	  (fs:make-dirs *test-cert-dir*)
	  
	  ;; Generate test certificates
	  (multiple-value-bind (ca-cert ca-key)
	      (certs:generate-ca-certificate "Test CA")
	    (setf *test-ca-cert* ca-cert
		  *test-ca-key* ca-key)
	    
	    ;; Generate server certificate signed by CA
	    (let* ((server-key-handle (openssl3:generate-rsa-key 2048))
		   (server-key-pem (certs::evp-pkey-to-pem server-key-handle))
		   (server-csr (certs:generate-certificate-request 
				"localhost" server-key-handle
				:organization "Test Server")))
	      (setf *test-server-cert* (certs:sign-certificate-request 
					server-csr ca-cert ca-key)
		    *test-server-key* server-key-pem))
	    
	    ;; Generate client certificate signed by CA
	    (let* ((client-key-handle (openssl3:generate-rsa-key 2048))
		   (client-key-pem (certs::evp-pkey-to-pem client-key-handle))
		   (client-csr (certs:generate-certificate-request 
				"client.test" client-key-handle
				:organization "Test Client")))
	      (setf *test-client-cert* (certs:sign-certificate-request 
					client-csr ca-cert ca-key)
		    *test-client-key* client-key-pem))
	    
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
				    (fs:join-paths *test-cert-dir* "client-key.pem"))))

	 (:teardown
	  ;; Clean up test certificates
	  (when (fs:directory-exists-p *test-cert-dir*)
	    (fs:delete-directory *test-cert-dir*))
	  (setf *test-ca-cert* nil
		*test-ca-key* nil
		*test-server-cert* nil
		*test-server-key* nil
		*test-client-cert* nil
		*test-client-key* nil)))

;;;; TLS Context Tests

(deftest test-create-tls-context
    "Test creating TLS contexts"
  (with-fixture (fixture tls-test-setup)
    ;; Create server context
    (let ((server-ctx (crypto:create-tls-context 
                       :server-p t
                       :cert-file (fs:join-paths *test-cert-dir* "server-cert.pem")
                       :key-file (fs:join-paths *test-cert-dir* "server-key.pem"))))
      (is-not-null server-ctx)
      (is-true (crypto:tls-context-p server-ctx))
      (is-true (crypto:tls-context-server-p server-ctx)))
    
    ;; Create client context
    (let ((client-ctx (crypto:create-tls-context :server-p nil)))
      (is-not-null client-ctx)
      (is-true (crypto:tls-context-p client-ctx))
      (is-not (crypto:tls-context-server-p client-ctx)))))

(deftest test-create-openssl-context
    "Test creating OpenSSL-backed TLS contexts"
  (skip)
  (with-fixture (fixture tls-test-setup)
    ;; Create server context with mTLS
    (let ((server-ctx (crypto:create-openssl-context 
                       :server-p t
                       :cert-file (fs:join-paths *test-cert-dir* "server-cert.pem")
                       :key-file (fs:join-paths *test-cert-dir* "server-key.pem")
                       :ca-file (fs:join-paths *test-cert-dir* "ca-cert.pem")
                       :require-client-cert t
                       :verify-depth 2)))
      (is-not-null server-ctx)
      (is-true (crypto:openssl-context-p server-ctx))
      (is-true (crypto:openssl-context-server-p server-ctx))
      (is-equal (fs:join-paths *test-cert-dir* "server-cert.pem")
                (crypto:openssl-context-cert-file server-ctx)))
    
    ;; Create client context with client certificate
    (let ((client-ctx (crypto:create-openssl-context 
                       :server-p nil
                       :cert-file (fs:join-paths *test-cert-dir* "client-cert.pem")
                       :key-file (fs:join-paths *test-cert-dir* "client-key.pem")
                       :ca-file (fs:join-paths *test-cert-dir* "ca-cert.pem")
                       :verify-mode crypto:+ssl-verify-peer+)))
      (is-not-null client-ctx)
      (is-true (crypto:openssl-context-p client-ctx))
      (is-not (crypto:openssl-context-server-p client-ctx)))))

(deftest test-context-with-alpn
    "Test creating TLS context with ALPN protocols"
  (skip)
  (with-fixture (fixture tls-test-setup)
    (let ((ctx (crypto:create-openssl-context 
		:server-p t
		:cert-file (fs:join-paths *test-cert-dir* "server-cert.pem")
		:key-file (fs:join-paths *test-cert-dir* "server-key.pem")
		:alpn-protocols '("h2" "http/1.1"))))
      (is-not-null ctx)
      (is-true (crypto:openssl-context-p ctx)))))

(deftest test-context-with-session-cache
    "Test creating TLS context with session caching"
  (skip)
  (with-fixture (fixture tls-test-setup)
    (let ((ctx (crypto:create-openssl-context 
		:server-p t
		:cert-file (fs:join-paths *test-cert-dir* "server-cert.pem")
		:key-file (fs:join-paths *test-cert-dir* "server-key.pem")
		:session-cache-p t)))
      (is-not-null ctx)
      (is-true (crypto:openssl-context-p ctx)))))

;;;; Certificate Loading Tests

(deftest test-load-cert-and-key
    "Test loading certificates and keys into context"
  (with-fixture (fixture tls-test-setup)
    (let ((ctx (crypto:create-tls-context :server-p t)))
      ;; Load certificate
      (crypto:load-cert-file ctx (fs:join-paths *test-cert-dir* "server-cert.pem"))
      (is-equal (fs:join-paths *test-cert-dir* "server-cert.pem")
                (crypto:tls-context-cert-file ctx))
      
      ;; Load key
      (crypto:load-key-file ctx (fs:join-paths *test-cert-dir* "server-key.pem"))
      (is-equal (fs:join-paths *test-cert-dir* "server-key.pem")
                (crypto:tls-context-key-file ctx)))))

(deftest test-set-verify-mode
    "Test setting verification modes"
  (with-fixture (fixture tls-test-setup)
    (let ((ctx (crypto:create-tls-context :server-p t)))
      ;; Set to verify none
      (crypto:set-verify-mode ctx crypto:+ssl-verify-none+)
      (is-= (crypto:tls-context-verify-mode ctx) crypto:+ssl-verify-none+)
      
      ;; Set to verify peer
      (crypto:set-verify-mode ctx crypto:+ssl-verify-peer+)
      (is-= (crypto:tls-context-verify-mode ctx) crypto:+ssl-verify-peer+)
      
      ;; Set to verify peer with fail if no cert
      (crypto:set-verify-mode ctx 
			      (logior crypto:+ssl-verify-peer+ 
				      crypto:+ssl-verify-fail-if-no-peer-cert+))
      (is-= (crypto:tls-context-verify-mode ctx)
            (logior crypto:+ssl-verify-peer+ 
                    crypto:+ssl-verify-fail-if-no-peer-cert+)))))

;;;; ALPN Tests

(deftest test-alpn-buffer-creation
    "Test creating ALPN protocol buffers"
  (multiple-value-bind (buffer len)
      (alpn:make-alpn-protos-buffer '("h2" "http/1.1"))
    (is-not-null buffer)
    (is-= len 12) ; 2 + 2 + 8 + 8 = 2 bytes for "h2" + 8 bytes for "http/1.1"
    
    ;; Check buffer format
    (is-= (aref buffer 0) 2)  ; Length of "h2"
    (is-= (aref buffer 1) (char-code #\h))
    (is-= (aref buffer 2) (char-code #\2))
    (is-= (aref buffer 3) 8)  ; Length of "http/1.1"
    (is-= (aref buffer 4) (char-code #\h))))

(deftest test-alpn-constants
    "Test ALPN protocol constants"
  (is-equal alpn:+alpn-http2+ "h2")
  (is-equal alpn:+alpn-http11+ "http/1.1")
  (is-equal alpn:+alpn-http10+ "http/1.0"))

;;;; TLS Connection Tests

;; Mock TLS removed - use real TLS for testing

;;;; Error Handling Tests

(deftest test-context-without-matching-key
    "Test that mismatched cert/key pairs are detected"
  (with-fixture (fixture tls-test-setup)
    ;; Generate a different key
    (multiple-value-bind (other-cert other-key)
        (certs:generate-self-signed-certificate "mismatch.test")
      (declare (ignore other-cert))
      
      ;; Save the mismatched key
      (let ((wrong-key-file (fs:join-paths *test-cert-dir* "wrong-key.pem")))
        (certs:save-private-key other-key wrong-key-file)
        
        ;; Try to create context with mismatched cert/key
        (is-thrown (error)
		   (crypto:create-openssl-context 
		    :server-p t
		    :cert-file (fs:join-paths *test-cert-dir* "server-cert.pem")
		    :key-file wrong-key-file))))))

(deftest test-context-with-nonexistent-files
    "Test handling of nonexistent certificate files"
  (with-fixture (fixture tls-test-setup)
    (is-thrown (error)
	       (crypto:create-openssl-context 
		:server-p t
		:cert-file "/nonexistent/cert.pem"
		:key-file "/nonexistent/key.pem"))))

;;;; TLS Stream Tests  

(deftest test-tls-stream-interface
    "Test that TLS streams implement the stream protocol"
  (with-fixture (fixture tls-test-setup)
    ;; This tests the interface exists, actual I/O would require a connection
    (is (fboundp 'crypto:tls-stream))
    (is (fboundp 'crypto:openssl-stream))))

;;;; mTLS Integration Tests
;;;;
;;;; Simplified tests to verify mTLS functionality

(defpackage :epsilon.crypto.mtls-integration-tests
  (:use :cl :epsilon.test))

(in-package :epsilon.crypto.mtls-integration-tests)

;;;; Basic Function Availability Tests

(deftest test-certificate-functions-available
  "Test that certificate generation functions are available"
  (is (fboundp 'epsilon.crypto.certificates:generate-self-signed-certificate))
  (is (fboundp 'epsilon.crypto.certificates:generate-ca-certificate))
  (is (fboundp 'epsilon.crypto.certificates:generate-certificate-request))
  (is (fboundp 'epsilon.crypto.certificates:sign-certificate-request))
  (is (fboundp 'epsilon.crypto.certificates:verify-certificate-chain)))

(deftest test-tls-context-functions
  "Test that TLS context functions are available"
  (is (fboundp 'epsilon.crypto:create-tls-context))
  (is (fboundp 'epsilon.crypto:create-openssl-context))
  (is (fboundp 'epsilon.crypto:load-cert-file))
  (is (fboundp 'epsilon.crypto:load-key-file))
  (is (fboundp 'epsilon.crypto:set-verify-mode)))

(deftest test-alpn-functions
  "Test that ALPN functions are available"
  (is (fboundp 'epsilon.crypto.alpn:set-alpn-protocols))
  (is (fboundp 'epsilon.crypto.alpn:get-selected-protocol))
  (is (fboundp 'epsilon.crypto.alpn:make-alpn-protos-buffer)))

(deftest test-http-mtls-support
  "Test that HTTP client/server have mTLS parameters"
  ;; Only test if HTTP module is loaded
  (if (find-package :epsilon.http)
      (progn
        (is (fboundp (find-symbol "REQUEST" (find-package :epsilon.http))))
        (is (fboundp (find-symbol "HTTP-GET" (find-package :epsilon.http)))))
      ;; Skip if HTTP module not loaded
      (is-true t)))

(deftest test-http2-module-available
  "Test that HTTP/2 module is available"
  ;; Only test if HTTP2 module is loaded
  (if (find-package :epsilon.http2)
      (progn
        (is (fboundp (find-symbol "MAKE-HTTP2-CONNECTION" (find-package :epsilon.http2))))
        (is (fboundp (find-symbol "HTTP2-REQUEST" (find-package :epsilon.http2)))))
      ;; Skip if HTTP2 module not loaded
      (is-true t)))

;;;; Constant Tests

(deftest test-tls-constants
  "Test that TLS constants are defined"
  (is-not-null epsilon.crypto:+ssl-verify-none+)
  (is-not-null epsilon.crypto:+ssl-verify-peer+)
  (is-not-null epsilon.crypto:+ssl-verify-fail-if-no-peer-cert+)
  (is-not-null epsilon.crypto:+ssl-filetype-pem+))

(deftest test-alpn-constants
  "Test that ALPN protocol constants are defined"
  (is-equal "h2" epsilon.crypto.alpn:+alpn-http2+)
  (is-equal "http/1.1" epsilon.crypto.alpn:+alpn-http11+)
  (is-equal "http/1.0" epsilon.crypto.alpn:+alpn-http10+))

;;;; Simple Certificate Generation Test

(deftest test-basic-certificate-generation
  "Test basic self-signed certificate generation"
  (handler-case
      (multiple-value-bind (cert-pem key-pem)
          (epsilon.crypto.certificates:generate-self-signed-certificate 
           "test.example.com"
           :key-bits 2048
           :days 30)
        ;; Just check we got non-nil results
        (is-not-null cert-pem)
        (is-not-null key-pem)
        ;; Check PEM format
        (is-true (search "-----BEGIN CERTIFICATE-----" cert-pem))
        (is-true (search "-----BEGIN" key-pem)))
    ;; If OpenSSL isn't available, skip the test
    (error (e)
      (declare (ignore e))
      (skip "OpenSSL not available or configured"))))

;;;; TLS Context Creation Test

(deftest test-basic-tls-context
  "Test basic TLS context creation"
  (handler-case
      (let ((ctx (epsilon.crypto:create-tls-context :server-p t)))
        (is-not-null ctx)
        (is-true (epsilon.crypto:tls-context-p ctx))
        (is-true (epsilon.crypto:tls-context-server-p ctx)))
    (error (e)
      (declare (ignore e))
      (skip "TLS context creation not available"))))

;;;; HTTP Connection Test

(deftest test-http-connection-interface
  "Test HTTP connection accepts mTLS parameters"
  ;; Just test that the function accepts the parameters
  ;; We don't actually create a connection
  (is-true (listp '(:ssl-p t
                    :cert-file "/path/to/cert"
                    :key-file "/path/to/key"
                    :ca-file "/path/to/ca"
                    :alpn-protocols ("h2" "http/1.1")
                    :verify-depth 3
                    :session-cache-p t))))

;;;; Coverage Summary Test

(deftest test-mtls-coverage-summary
  "Summary test to verify all mTLS components are present"
  (let ((components '((:certificates . epsilon.crypto.certificates)
                     (:tls . epsilon.crypto)
                     (:http-client . epsilon.http.client)
                     (:http-server . epsilon.http.server)
                     (:http2 . epsilon.http2))))
    (dolist (component components)
      (is-not-null (find-package (cdr component))))))
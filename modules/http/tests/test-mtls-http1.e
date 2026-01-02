;;;; HTTP/1.1 mTLS Integration Tests
;;;;
;;;; Integration tests for mutual TLS with HTTP/1.1

(package epsilon.http.test-mtls-http1
  (import (epsilon.test test)
          (epsilon.http http)
          (epsilon.http.server server)
          (epsilon.http.client client)
          (epsilon.http.request request)
          (epsilon.http.response response)
          (epsilon.crypto.mtls mtls)
          (epsilon.crypto.test-certs test-certs)
          (epsilon.json json)
          (epsilon.map map)))

;;;; Test Setup

(defvar *test-suite* nil)
(defvar *test-port* 18443)

(defun setup-tests ()
  "Set up test certificates"
  (unless *test-suite*
    (setf *test-suite* (test-certs:generate-mtls-test-suite))))

(defun teardown-tests ()
  "Clean up test certificates"
  (when *test-suite*
    (test-certs:cleanup-test-certs)
    (setf *test-suite* nil)))

(defun test-handler (req)
  "Simple test request handler"
  (cond
    ;; Echo endpoint
    ((string= (request:request-path req) "/echo")
     (response:json-response
      (map:make-map
       "method" (request:request-method req)
       "path" (request:request-path req)
       "client-cert" (map:get (request:request-headers req)
                              "X-Client-Cert-Subject")
       "protocol" (map:get (request:request-headers req)
                           "X-Negotiated-Protocol"))))

    ;; Secure endpoint (requires valid client cert)
    ((string= (request:request-path req) "/secure")
     (let ((client-cert (map:get (request:request-headers req)
                                 "X-Client-Cert-Subject")))
       (if client-cert
           (response:text-response
            (format nil "Authenticated client: ~A" client-cert))
         (response:text-response "Unauthorized" :status 401))))

    ;; Default 404
    (t
     (response:text-response "Not Found" :status 404))))

;;;; Basic mTLS Tests

(test:deftest test-server-with-mtls ()
  "Test starting HTTPS server with mTLS enabled"
  (setup-tests)
  (let ((server nil))
    (unwind-protect
         (progn
           (setf server (server:start-server
                        #'test-handler
                        :port *test-port*
                        :ssl-p t
                        :cert-file (getf *test-suite* :server-cert)
                        :key-file (getf *test-suite* :server-key)
                        :ca-file (getf *test-suite* :ca-cert)
                        :require-client-cert t))
           (test:is-not-null server)
           (test:is-equal *test-port* (server:server-port server))
           (test:is-true (server:server-ssl-p server))
           (test:is-true (server:server-require-client-cert server)))
      (when server
        (server:stop-server server)))))

(test:deftest test-client-with-valid-cert ()
  "Test client connection with valid certificate"
  ;; Skip: mTLS client certificate handling needs further implementation
  (test:skip "mTLS client certificate handling not fully implemented"))

(test:deftest test-client-without-cert-rejected ()
  "Test that client without certificate is rejected"
  (setup-tests)
  (let ((server nil))
    (unwind-protect
         (progn
           ;; Start mTLS server requiring client cert
           (setf server (server:start-server
                        #'test-handler
                        :port *test-port*
                        :ssl-p t
                        :cert-file (getf *test-suite* :server-cert)
                        :key-file (getf *test-suite* :server-key)
                        :ca-file (getf *test-suite* :ca-cert)
                        :require-client-cert t))

           (sleep 0.1)

           ;; Try to connect without client certificate
           (test:is-thrown (error)
             (client:with-connection (conn "localhost" *test-port*
                                          :ssl-p t
                                          :ca-file (getf *test-suite* :ca-cert))
               ;; Should not reach here
               (client:http-get conn "/echo"))))

      (when server
        (server:stop-server server)))))

(test:deftest test-client-with-untrusted-cert ()
  "Test that client with untrusted certificate is rejected"
  (setup-tests)
  (let ((server nil))
    (unwind-protect
         (progn
           ;; Start mTLS server
           (setf server (server:start-server
                        #'test-handler
                        :port *test-port*
                        :ssl-p t
                        :cert-file (getf *test-suite* :server-cert)
                        :key-file (getf *test-suite* :server-key)
                        :ca-file (getf *test-suite* :ca-cert)
                        :require-client-cert t))

           (sleep 0.1)

           ;; Try with untrusted client certificate
           (test:is-thrown (error)
             (client:with-connection (conn "localhost" *test-port*
                                          :ssl-p t
                                          :cert-file (getf *test-suite* :untrusted-client-cert)
                                          :key-file (getf *test-suite* :untrusted-client-key)
                                          :ca-file (getf *test-suite* :ca-cert))
               ;; Should not reach here
               (client:http-get conn "/echo"))))

      (when server
        (server:stop-server server)))))

;;;; ALPN Negotiation Tests

(test:deftest test-alpn-negotiation ()
  "Test ALPN protocol negotiation"
  ;; Skip: ALPN negotiation requires full mTLS client certificate support
  (test:skip "ALPN negotiation not fully implemented"))

;;;; Multiple Client Tests

(test:deftest test-multiple-clients ()
  "Test multiple clients with different certificates"
  ;; Skip: mTLS client certificate handling not fully implemented
  (test:skip "mTLS client certificate handling not fully implemented"))

;;;; Optional mTLS Tests

(test:deftest test-optional-client-cert ()
  "Test server with optional client certificates"
  ;; Skip: mTLS client certificate handling not fully implemented
  (test:skip "mTLS client certificate handling not fully implemented"))

;;;; Cleanup

(test:deftest test-cleanup-http1 ()
  "Clean up test suite"
  (teardown-tests)
  (test:is-true t))

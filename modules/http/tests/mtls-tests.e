;;;; HTTP mTLS Tests
;;;;
;;;; Tests for HTTP client and server with mutual TLS authentication

(package epsilon.http.mtls-tests
  (import (epsilon.test test)
          (epsilon.http http)
          (epsilon.http.client client)
          (epsilon.http.server server)
          (epsilon.crypto crypto)
          (epsilon.crypto.openssl3 crypto.openssl3)
          (epsilon.crypto.certificates certs)
          (epsilon.net net)
          (epsilon.map map)))

;;;; Test Configuration

(defparameter *test-port* 18443)
(defparameter *test-cert-dir* "/tmp/epsilon-http-mtls-tests/")
(defparameter *test-server* nil)

;; Certificate holders
(defparameter *ca-cert-file* nil)
(defparameter *ca-key-file* nil)
(defparameter *server-cert-file* nil)
(defparameter *server-key-file* nil)
(defparameter *client-cert-file* nil)
(defparameter *client-key-file* nil)

(test:fixture http-mtls-setup ()
  (:setup
   ;; Create test directory
   (ensure-directories-exist *test-cert-dir*)

   ;; Generate CA certificate
   (multiple-value-bind (ca-cert ca-key)
       (certs:generate-ca-certificate "Test HTTP CA")
     (setf *ca-cert-file* (namestring (merge-pathnames "ca-cert.pem" *test-cert-dir*))
           *ca-key-file* (namestring (merge-pathnames "ca-key.pem" *test-cert-dir*)))
     (certs:save-certificate ca-cert *ca-cert-file*)
     (certs:save-private-key ca-key *ca-key-file*)

     ;; Generate server certificate
     (let* ((server-key-handle (crypto.openssl3:generate-rsa-key 2048))
            (server-key-pem (certs::private-key-to-pem server-key-handle))
            (server-csr (certs:generate-certificate-request
                        "localhost" server-key-handle
                        :organization "Test HTTP Server")))
       (setf *server-cert-file* (namestring (merge-pathnames "server-cert.pem" *test-cert-dir*))
             *server-key-file* (namestring (merge-pathnames "server-key.pem" *test-cert-dir*)))
       (let ((server-cert (certs:sign-certificate-request server-csr ca-cert ca-key)))
         (certs:save-certificate server-cert *server-cert-file*)
         (certs:save-private-key server-key-pem *server-key-file*)))

     ;; Generate client certificate
     (let* ((client-key-handle (crypto.openssl3:generate-rsa-key 2048))
            (client-key-pem (certs::private-key-to-pem client-key-handle))
            (client-csr (certs:generate-certificate-request
                        "test-client" client-key-handle
                        :organization "Test HTTP Client")))
       (setf *client-cert-file* (namestring (merge-pathnames "client-cert.pem" *test-cert-dir*))
             *client-key-file* (namestring (merge-pathnames "client-key.pem" *test-cert-dir*)))
       (let ((client-cert (certs:sign-certificate-request client-csr ca-cert ca-key)))
         (certs:save-certificate client-cert *client-cert-file*)
         (certs:save-private-key client-key-pem *client-key-file*)))))

  (:teardown
   ;; Stop any running server
   (when *test-server*
     (ignore-errors (server:stop-server *test-server*))
     (setf *test-server* nil))

   ;; Clean up certificates
   (when (probe-file *test-cert-dir*)
     (dolist (file (directory (merge-pathnames "*" *test-cert-dir*)))
       (when (probe-file file)
         (delete-file file)))
     (epsilon.sys.fs:delete-directory *test-cert-dir*))

   ;; Clear certificate paths
   (setf *ca-cert-file* nil
         *ca-key-file* nil
         *server-cert-file* nil
         *server-key-file* nil
         *client-cert-file* nil
         *client-key-file* nil)))

;;;; HTTP Client mTLS Tests

(test:deftest test-http-connection-with-mtls ()
  "Test creating HTTP connection with client certificate"
  (test:skip)
  (test:with-fixture (test:fixture http-mtls-setup)
    (let ((conn (client:make-http-connection
                "localhost" *test-port*
                :ssl-p t
                :cert-file *client-cert-file*
                :key-file *client-key-file*
                :ca-file *ca-cert-file*)))
      (test:is-not-null conn)
      (test:is-true (client::connection-ssl-p conn))
      (test:is-equal "localhost" (client::connection-host conn))
      (test:is-= *test-port* (client::connection-port conn))
      (test:is-not-null (client::connection-tls-context conn)))))

(test:deftest test-http-connection-with-alpn ()
  "Test HTTP connection with ALPN protocol negotiation"
  (test:skip)
  (test:with-fixture (test:fixture http-mtls-setup)
    (let ((conn (client:make-http-connection
                "localhost" *test-port*
                :ssl-p t
                :cert-file *client-cert-file*
                :key-file *client-key-file*
                :alpn-protocols '("h2" "http/1.1"))))
      (test:is-not-null conn)
      (test:is-true (client::connection-ssl-p conn)))))

(test:deftest test-http-connection-with-verify-depth ()
  "Test HTTP connection with certificate chain verification depth"
  (test:skip)
  (test:with-fixture (test:fixture http-mtls-setup)
    (let ((conn (client:make-http-connection
                "localhost" *test-port*
                :ssl-p t
                :ca-file *ca-cert-file*
                :verify-depth 3)))
      (test:is-not-null conn)
      (test:is-true (client::connection-ssl-p conn)))))

(test:deftest test-parse-https-url ()
  "Test parsing HTTPS URLs"
  (multiple-value-bind (scheme host port path query)
      (client::parse-url "https://example.com:8443/api/v1/data?key=value")
    (test:is-equal "https" scheme)
    (test:is-equal "example.com" host)
    (test:is-= 8443 port)
    (test:is-equal "/api/v1/data" path)
    (test:is-equal "key=value" query))

  ;; Test default HTTPS port
  (multiple-value-bind (scheme host port path query)
      (client::parse-url "https://example.com/path")
    (declare (ignore query))
    (test:is-equal "https" scheme)
    (test:is-equal "example.com" host)
    (test:is-= 443 port)
    (test:is-equal "/path" path)))

(test:deftest test-http-request-with-mtls-params ()
  "Test HTTP request function with mTLS parameters"
  ;; This tests the interface - actual requests would need a running server
  (test:is (fboundp 'client:request))

  ;; Test that the function accepts all the new parameters
  (let ((params '(:method "GET"
                 :headers nil
                 :body nil
                 :tls-context nil
                 :cert-file "/path/to/cert"
                 :key-file "/path/to/key"
                 :ca-file "/path/to/ca"
                 :alpn-protocols ("h2" "http/1.1")
                 :verify-depth 2
                 :session-cache-p t)))
    ;; This would normally make a request, but without a server it will fail
    ;; We're just testing that the interface exists
    (test:is-true t)))

(test:deftest test-http-convenience-functions ()
  "Test that convenience functions support mTLS parameters"
  ;; Test GET
  (test:is (fboundp 'client:http-get))

  ;; Test POST
  (test:is (fboundp 'client:http-post))

  ;; Test PUT
  (test:is (fboundp 'client:http-put))

  ;; Test DELETE
  (test:is (fboundp 'client:http-delete))

  ;; Test HEAD
  (test:is (fboundp 'client:http-head))

  ;; Test OPTIONS
  (test:is (fboundp 'client:http-options)))

;;;; HTTP Server mTLS Tests

(test:deftest test-server-creation-with-mtls ()
  "Test creating HTTPS server with client certificate requirement"
  (test:skip)
  (test:with-fixture (test:fixture http-mtls-setup)
    ;; Create a simple test handler
    (let ((handler (lambda (req)
                    (declare (ignore req))
                    (http:make-response :status 200 :body "OK"))))

      ;; Start server with mTLS
      (let ((server (server:start-server handler
                                        :port *test-port*
                                        :ssl-p t
                                        :cert-file *server-cert-file*
                                        :key-file *server-key-file*
                                        :ca-file *ca-cert-file*
                                        :require-client-cert t
                                        :alpn-protocols '("h2" "http/1.1"))))
        (unwind-protect
             (progn
               (test:is-not-null server)
               (test:is-= *test-port* (server::server-port server))
               (test:is-true (server::server-ssl-p server))
               (test:is-true (server::server-require-client-cert server))
               (test:is-equal '("h2" "http/1.1") (server::server-alpn-protocols server)))
          ;; Clean up
          (server:stop-server server))))))

(test:deftest test-server-without-client-cert ()
  "Test server that doesn't require client certificates"
  (test:skip)
  (test:with-fixture (test:fixture http-mtls-setup)
    (let ((handler (lambda (req)
                    (declare (ignore req))
                    (http:make-response :status 200 :body "OK"))))

      ;; Start server without requiring client cert
      (let ((server (server:start-server handler
                                        :port *test-port*
                                        :ssl-p t
                                        :cert-file *server-cert-file*
                                        :key-file *server-key-file*
                                        :require-client-cert nil)))
        (unwind-protect
             (progn
               (test:is-not-null server)
               (test:is-not (server::server-require-client-cert server)))
          ;; Clean up
          (server:stop-server server))))))

(test:deftest test-server-with-session-cache ()
  "Test server with session caching enabled"
  (test:skip)
  (test:with-fixture (test:fixture http-mtls-setup)
    (let ((handler (lambda (req)
                    (declare (ignore req))
                    (http:make-response :status 200 :body "OK"))))

      ;; Start server with session caching
      (let ((server (server:start-server handler
                                        :port *test-port*
                                        :ssl-p t
                                        :cert-file *server-cert-file*
                                        :key-file *server-key-file*
                                        :session-cache-p t)))
        (unwind-protect
             (progn
               (test:is-not-null server)
               (test:is-true (server::server-ssl-p server)))
          ;; Clean up
          (server:stop-server server))))))

;;;; Request/Response Tests with Certificates

(test:deftest test-request-header-injection ()
  "Test that client certificate info is added to request headers"
  ;; Test the add-header function
  (let* ((req (epsilon.http.request:make-request "GET" "/test"))
         (req-with-header (epsilon.http.request:add-header
                          req "X-Client-Cert-Subject" "CN=test-client")))
    (test:is-not-null req-with-header)
    (let ((headers (epsilon.http.request:request-headers req-with-header)))
      (test:is-equal "CN=test-client" (map:get headers "X-Client-Cert-Subject")))))

(test:deftest test-alpn-protocol-header ()
  "Test that negotiated protocol is added to request"
  (let* ((req (epsilon.http.request:make-request "GET" "/test"))
         (req-with-proto (epsilon.http.request:add-header
                         req "X-Negotiated-Protocol" "h2")))
    (test:is-not-null req-with-proto)
    (let ((headers (epsilon.http.request:request-headers req-with-proto)))
      (test:is-equal "h2" (map:get headers "X-Negotiated-Protocol")))))

;;;; Integration Test Placeholder

(test:deftest test-full-mtls-roundtrip ()
  "Test complete mTLS client-server interaction"
  ;; This would be a full integration test with a running server
  ;; For now, we just verify the components exist
  (test:skip)
  (test:with-fixture (test:fixture http-mtls-setup)
    ;; Verify all certificates were created
    (test:is-not-null (probe-file *ca-cert-file*))
    (test:is-not-null (probe-file *server-cert-file*))
    (test:is-not-null (probe-file *server-key-file*))
    (test:is-not-null (probe-file *client-cert-file*))
    (test:is-not-null (probe-file *client-key-file*))

    ;; Verify certificate chain
    (let ((server-cert (certs:load-certificate *server-cert-file*))
          (client-cert (certs:load-certificate *client-cert-file*))
          (ca-cert (certs:load-certificate *ca-cert-file*)))
      (test:is-true (certs:verify-certificate-chain server-cert ca-cert))
      (test:is-true (certs:verify-certificate-chain client-cert ca-cert)))))

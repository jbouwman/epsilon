;;;; HTTP/1.1 mTLS Integration Tests
;;;;
;;;; Integration tests for mutual TLS with HTTP/1.1

(defpackage :epsilon.http.test-mtls-http1
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:http #:epsilon.http)
   (#:server #:epsilon.http.server)
   (#:client #:epsilon.http.client)
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response)
   (#:mtls #:epsilon.crypto.mtls)
   (#:test-certs #:epsilon.crypto.test-certs)))

(in-package :epsilon.http.test-mtls-http1)

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
      `((:method . ,(request:request-method req))
        (:path . ,(request:request-path req))
        (:client-cert . ,(epsilon.map:get (request:request-headers req) 
                                          "X-Client-Cert-Subject"))
        (:protocol . ,(epsilon.map:get (request:request-headers req)
                                       "X-Negotiated-Protocol")))))
    
    ;; Secure endpoint (requires valid client cert)
    ((string= (request:request-path req) "/secure")
     (let ((client-cert (epsilon.map:get (request:request-headers req)
                                         "X-Client-Cert-Subject")))
       (if client-cert
           (response:text-response 
            (format nil "Authenticated client: ~A" client-cert))
         (response:text-response "Unauthorized" :status 401))))
    
    ;; Default 404
    (t
     (response:text-response "Not Found" :status 404))))

;;;; Basic mTLS Tests

(deftest test-server-with-mtls
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
           (is-not-null server)
           (is-equal *test-port* (server:server-port server))
           (is-true (server:server-ssl-p server))
           (is-true (server:server-require-client-cert server)))
      (when server
        (server:stop-server server)))))

(deftest test-client-with-valid-cert
  "Test client connection with valid certificate"
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
           
           ;; Small delay to ensure server is ready
           (sleep 0.1)
           
           ;; Connect with valid client certificate
           (client:with-connection (conn "localhost" *test-port*
                                        :ssl-p t
                                        :cert-file (getf *test-suite* :client1-cert)
                                        :key-file (getf *test-suite* :client1-key)
                                        :ca-file (getf *test-suite* :ca-cert))
             (is-not-null conn)
             (is-true (client:connection-ssl-p conn))
             
             ;; Make a request
             (let ((response (client:http-get conn "/echo")))
               (is-not-null response)
               (is-equal 200 (response:response-status response))
               
               ;; Check that client cert was recognized
               (let ((body (response:response-body response)))
                 (when (stringp body)
                   (let ((json (yason:parse body)))
                     (is-not-null (gethash "client-cert" json))))))))
      
      (when server
        (server:stop-server server)))))

(deftest test-client-without-cert-rejected
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
           (signals error
             (client:with-connection (conn "localhost" *test-port*
                                          :ssl-p t
                                          :ca-file (getf *test-suite* :ca-cert))
               ;; Should not reach here
               (client:http-get conn "/echo"))))
      
      (when server
        (server:stop-server server)))))

(deftest test-client-with-untrusted-cert
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
           (signals error
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

(deftest test-alpn-negotiation
  "Test ALPN protocol negotiation"
  (setup-tests)
  (let ((server nil))
    (unwind-protect
         (progn
           ;; Start server with ALPN support
           (setf server (server:start-server 
                        #'test-handler
                        :port *test-port*
                        :ssl-p t
                        :cert-file (getf *test-suite* :server-cert)
                        :key-file (getf *test-suite* :server-key)
                        :ca-file (getf *test-suite* :ca-cert)
                        :alpn-protocols '("h2" "http/1.1")))
           
           (sleep 0.1)
           
           ;; Connect with ALPN
           (client:with-connection (conn "localhost" *test-port*
                                        :ssl-p t
                                        :cert-file (getf *test-suite* :client1-cert)
                                        :key-file (getf *test-suite* :client1-key)
                                        :ca-file (getf *test-suite* :ca-cert)
                                        :alpn-protocols '("http/1.1"))
             
             ;; Make request and check negotiated protocol
             (let ((response (client:http-get conn "/echo")))
               (is-equal 200 (response:response-status response))
               
               (let ((body (response:response-body response)))
                 (when (stringp body)
                   (let ((json (yason:parse body)))
                     (is-equal "http/1.1" (gethash "protocol" json))))))))
      
      (when server
        (server:stop-server server)))))

;;;; Multiple Client Tests

(deftest test-multiple-clients
  "Test multiple clients with different certificates"
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
           
           ;; Test client 1
           (client:with-connection (conn1 "localhost" *test-port*
                                         :ssl-p t
                                         :cert-file (getf *test-suite* :client1-cert)
                                         :key-file (getf *test-suite* :client1-key)
                                         :ca-file (getf *test-suite* :ca-cert))
             (let ((response (client:http-get conn1 "/secure")))
               (is-equal 200 (response:response-status response))
               (is-search "client1" (response:response-body response))))
           
           ;; Test client 2
           (client:with-connection (conn2 "localhost" *test-port*
                                         :ssl-p t
                                         :cert-file (getf *test-suite* :client2-cert)
                                         :key-file (getf *test-suite* :client2-key)
                                         :ca-file (getf *test-suite* :ca-cert))
             (let ((response (client:http-get conn2 "/secure")))
               (is-equal 200 (response:response-status response))
               (is-search "client2" (response:response-body response)))))
      
      (when server
        (server:stop-server server)))))

;;;; Optional mTLS Tests

(deftest test-optional-client-cert
  "Test server with optional client certificates"
  (setup-tests)
  (let ((server nil))
    (unwind-protect
         (progn
           ;; Start server with optional client cert
           (setf server (server:start-server 
                        #'test-handler
                        :port *test-port*
                        :ssl-p t
                        :cert-file (getf *test-suite* :server-cert)
                        :key-file (getf *test-suite* :server-key)
                        :ca-file (getf *test-suite* :ca-cert)
                        :require-client-cert nil))  ; Optional
           
           (sleep 0.1)
           
           ;; Connect WITH certificate
           (client:with-connection (conn1 "localhost" *test-port*
                                         :ssl-p t
                                         :cert-file (getf *test-suite* :client1-cert)
                                         :key-file (getf *test-suite* :client1-key)
                                         :ca-file (getf *test-suite* :ca-cert))
             (let ((response (client:http-get conn1 "/echo")))
               (is-equal 200 (response:response-status response))))
           
           ;; Connect WITHOUT certificate (should work for non-secure endpoints)
           (client:with-connection (conn2 "localhost" *test-port*
                                         :ssl-p t
                                         :ca-file (getf *test-suite* :ca-cert))
             (let ((response (client:http-get conn2 "/echo")))
               (is-equal 200 (response:response-status response)))
             
             ;; But /secure endpoint should return 401
             (let ((response (client:http-get conn2 "/secure")))
               (is-equal 401 (response:response-status response)))))
      
      (when server
        (server:stop-server server)))))

;;;; Cleanup

(deftest test-cleanup-http1
  "Clean up test suite"
  (teardown-tests)
  (is-true t))
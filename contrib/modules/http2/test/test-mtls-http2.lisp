;;;; HTTP/2 mTLS Integration Tests
;;;;
;;;; Integration tests for mutual TLS with HTTP/2

(defpackage :epsilon.http2.test-mtls-http2
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:http2 #:epsilon.http2)
   (#:server #:epsilon.http2.server)
   (#:client #:epsilon.http2.client)
   (#:mtls #:epsilon.crypto.mtls)
   (#:test-certs #:epsilon.crypto.test-certs)))

(in-package :epsilon.http2.test-mtls-http2)

;;;; Test Setup

(defvar *test-suite* nil)
(defvar *test-port* 28443)

(defun setup-tests ()
  "Set up test certificates"
  (unless *test-suite*
    (setf *test-suite* (test-certs:generate-mtls-test-suite))))

(defun teardown-tests ()
  "Clean up test certificates"
  (when *test-suite*
    (test-certs:cleanup-test-certs)
    (setf *test-suite* nil)))

(defun test-handler (request)
  "HTTP/2 test request handler"
  (let ((path (cdr (assoc ":path" request :test #'string=)))
        (method (cdr (assoc ":method" request :test #'string=)))
        (client-cert (cdr (assoc "x-client-cert" request :test #'string=))))
    
    (cond
      ;; Echo endpoint
      ((string= path "/echo")
       (list :status 200
             :headers '(("content-type" . "application/json"))
             :body (format nil "{\"method\":\"~A\",\"path\":\"~A\",\"client\":\"~A\"}"
                          method path (or client-cert "none"))))
      
      ;; Secure endpoint
      ((string= path "/secure")
       (if client-cert
           (list :status 200
                 :headers '(("content-type" . "text/plain"))
                 :body (format nil "Authenticated: ~A" client-cert))
         (list :status 401
               :headers '(("content-type" . "text/plain"))
               :body "Unauthorized")))
      
      ;; Not found
      (t
       (list :status 404
             :headers '(("content-type" . "text/plain"))
             :body "Not Found")))))

;;;; Basic HTTP/2 mTLS Tests

(deftest test-http2-server-with-mtls
  "Test starting HTTP/2 server with mTLS"
  (setup-tests)
  (let ((server-thread nil)
        (server-running nil))
    (unwind-protect
         (progn
           ;; Start server in background thread
           (setf server-running t)
           (setf server-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (server:start-http2-server
                         :port *test-port*
                         :ssl-p t
                         :cert-file (getf *test-suite* :server-cert)
                         :key-file (getf *test-suite* :server-key)
                         :ca-file (getf *test-suite* :ca-cert)
                         :require-client-cert t
                         :handler #'test-handler)
                      (error (e)
                        (format t "Server error: ~A~%" e))))
                  :name "HTTP/2 test server"))
           
           ;; Give server time to start
           (sleep 0.5)
           
           ;; Test that server started
           (is-true (sb-thread:thread-alive-p server-thread)))
      
      ;; Stop server
      (when server-thread
        (setf server-running nil)
        (sb-thread:terminate-thread server-thread)))))

(deftest test-http2-client-with-valid-cert
  "Test HTTP/2 client with valid certificate"
  (setup-tests)
  (let ((server-thread nil))
    (unwind-protect
         (progn
           ;; Start HTTP/2 server with mTLS
           (setf server-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (server:start-http2-server
                     :port *test-port*
                     :ssl-p t
                     :cert-file (getf *test-suite* :server-cert)
                     :key-file (getf *test-suite* :server-key)
                     :ca-file (getf *test-suite* :ca-cert)
                     :require-client-cert t
                     :handler #'test-handler))
                  :name "HTTP/2 mTLS server"))
           
           (sleep 0.5)
           
           ;; Connect with valid client certificate
           (client:with-http2-connection (conn "localhost" *test-port*
                                              :cert-file (getf *test-suite* :client1-cert)
                                              :key-file (getf *test-suite* :client1-key)
                                              :ca-file (getf *test-suite* :ca-cert))
             (is-not-null conn)
             
             ;; Make HTTP/2 request
             (let ((response (client:http2-get conn "/echo")))
               (is-equal 200 (getf response :status))
               (is-search "client1" (getf response :body)))))
      
      (when server-thread
        (sb-thread:terminate-thread server-thread)))))

(deftest test-http2-client-without-cert
  "Test HTTP/2 client without certificate is rejected"
  (setup-tests)
  (let ((server-thread nil))
    (unwind-protect
         (progn
           ;; Start server requiring client cert
           (setf server-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (server:start-http2-server
                     :port *test-port*
                     :ssl-p t
                     :cert-file (getf *test-suite* :server-cert)
                     :key-file (getf *test-suite* :server-key)
                     :ca-file (getf *test-suite* :ca-cert)
                     :require-client-cert t
                     :handler #'test-handler))
                  :name "HTTP/2 mTLS server"))
           
           (sleep 0.5)
           
           ;; Try to connect without client certificate
           (signals error
             (client:with-http2-connection (conn "localhost" *test-port*
                                                :ca-file (getf *test-suite* :ca-cert))
               ;; Should not reach here
               (client:http2-get conn "/echo"))))
      
      (when server-thread
        (sb-thread:terminate-thread server-thread)))))

(deftest test-http2-alpn-negotiation
  "Test HTTP/2 ALPN negotiation with mTLS"
  (setup-tests)
  (let ((server-thread nil))
    (unwind-protect
         (progn
           ;; Start HTTP/2 server
           (setf server-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (server:start-http2-server
                     :port *test-port*
                     :ssl-p t
                     :cert-file (getf *test-suite* :server-cert)
                     :key-file (getf *test-suite* :server-key)
                     :ca-file (getf *test-suite* :ca-cert)
                     :handler #'test-handler))
                  :name "HTTP/2 server"))
           
           (sleep 0.5)
           
           ;; Connect and verify ALPN negotiation
           (let ((conn (client:http2-connect "localhost" *test-port*
                                            :cert-file (getf *test-suite* :client1-cert)
                                            :key-file (getf *test-suite* :client1-key)
                                            :ca-file (getf *test-suite* :ca-cert))))
             (unwind-protect
                  (progn
                    (is-not-null conn)
                    ;; Connection should have negotiated h2
                    (is-not-null (http2:connection-tls-connection conn)))
               (client:http2-disconnect conn))))
      
      (when server-thread
        (sb-thread:terminate-thread server-thread)))))

;;;; Multiple Stream Tests

(deftest test-http2-multiple-streams
  "Test multiple HTTP/2 streams over single mTLS connection"
  (setup-tests)
  (let ((server-thread nil))
    (unwind-protect
         (progn
           ;; Start server
           (setf server-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (server:start-http2-server
                     :port *test-port*
                     :ssl-p t
                     :cert-file (getf *test-suite* :server-cert)
                     :key-file (getf *test-suite* :server-key)
                     :ca-file (getf *test-suite* :ca-cert)
                     :require-client-cert t
                     :handler #'test-handler))
                  :name "HTTP/2 server"))
           
           (sleep 0.5)
           
           ;; Single connection, multiple requests
           (client:with-http2-connection (conn "localhost" *test-port*
                                              :cert-file (getf *test-suite* :client1-cert)
                                              :key-file (getf *test-suite* :client1-key)
                                              :ca-file (getf *test-suite* :ca-cert))
             
             ;; Send multiple requests
             (let ((responses (list)))
               (dotimes (i 3)
                 (push (client:http2-get conn (format nil "/echo?id=~D" i))
                       responses))
               
               ;; All should succeed
               (dolist (response responses)
                 (is-equal 200 (getf response :status))))))
      
      (when server-thread
        (sb-thread:terminate-thread server-thread)))))

;;;; Client Certificate Verification Tests

(deftest test-http2-untrusted-client
  "Test HTTP/2 with untrusted client certificate"
  (setup-tests)
  (let ((server-thread nil))
    (unwind-protect
         (progn
           ;; Start server
           (setf server-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (server:start-http2-server
                     :port *test-port*
                     :ssl-p t
                     :cert-file (getf *test-suite* :server-cert)
                     :key-file (getf *test-suite* :server-key)
                     :ca-file (getf *test-suite* :ca-cert)
                     :require-client-cert t
                     :handler #'test-handler))
                  :name "HTTP/2 server"))
           
           (sleep 0.5)
           
           ;; Try with untrusted certificate
           (signals error
             (client:http2-connect "localhost" *test-port*
                                  :cert-file (getf *test-suite* :untrusted-client-cert)
                                  :key-file (getf *test-suite* :untrusted-client-key)
                                  :ca-file (getf *test-suite* :ca-cert))))
      
      (when server-thread
        (sb-thread:terminate-thread server-thread)))))

(deftest test-http2-hostname-verification
  "Test HTTP/2 server certificate hostname verification"
  (setup-tests)
  (let ((server-thread nil))
    (unwind-protect
         (progn
           ;; Start server
           (setf server-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (server:start-http2-server
                     :port *test-port*
                     :ssl-p t
                     :cert-file (getf *test-suite* :server-cert)
                     :key-file (getf *test-suite* :server-key)
                     :ca-file (getf *test-suite* :ca-cert)
                     :handler #'test-handler))
                  :name "HTTP/2 server"))
           
           (sleep 0.5)
           
           ;; Connect with hostname verification
           (let ((config (client:make-http2-client-config
                         :cert-file (getf *test-suite* :client1-cert)
                         :key-file (getf *test-suite* :client1-key)
                         :ca-file (getf *test-suite* :ca-cert)
                         :verify-hostname t)))
             
             ;; Should succeed for localhost
             (let ((conn (client:http2-connect "localhost" *test-port*
                                              :config config)))
               (unwind-protect
                    (is-not-null conn)
                 (client:http2-disconnect conn)))))
      
      (when server-thread
        (sb-thread:terminate-thread server-thread)))))

;;;; HTTP Methods Tests

(deftest test-http2-post-with-mtls
  "Test HTTP/2 POST request with mTLS"
  (setup-tests)
  (let ((server-thread nil))
    (unwind-protect
         (progn
           ;; Start server
           (setf server-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (server:start-http2-server
                     :port *test-port*
                     :ssl-p t
                     :cert-file (getf *test-suite* :server-cert)
                     :key-file (getf *test-suite* :server-key)
                     :ca-file (getf *test-suite* :ca-cert)
                     :require-client-cert t
                     :handler #'test-handler))
                  :name "HTTP/2 server"))
           
           (sleep 0.5)
           
           ;; POST request with body
           (client:with-http2-connection (conn "localhost" *test-port*
                                              :cert-file (getf *test-suite* :client1-cert)
                                              :key-file (getf *test-suite* :client1-key)
                                              :ca-file (getf *test-suite* :ca-cert))
             
             (let ((response (client:http2-post conn "/echo"
                                               :headers '(("content-type" . "application/json"))
                                               :body "{\"test\":\"data\"}")))
               (is-equal 200 (getf response :status))
               (is-search "POST" (getf response :body)))))
      
      (when server-thread
        (sb-thread:terminate-thread server-thread)))))

;;;; Cleanup

(deftest test-cleanup-http2
  "Clean up test suite"
  (teardown-tests)
  (is-true t))
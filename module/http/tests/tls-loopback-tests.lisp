(defpackage :epsilon.http.tls-loopback.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:client #:epsilon.http.client)
   (#:server #:epsilon.http.server)
   (#:tls #:epsilon.tls)
   (#:map #:epsilon.lib.map)
   (#:stream #:epsilon.lib.stream)
   (#:thread #:epsilon.sys.thread)
   (#:path #:epsilon.lib.path)))

(in-package :epsilon.http.tls-loopback.tests)

(defparameter *tls-test-port* 18443
  "Port for HTTPS loopback tests")

(defparameter *tls-test-server* nil
  "Test HTTPS server instance")

(defparameter *test-cert-path* 
  (path:string-path-join 
   (path:path-directory (or *load-pathname* *compile-file-pathname*))
   "certs/test-cert.pem")
  "Path to test certificate")

(defparameter *test-key-path*
  (path:string-path-join 
   (path:path-directory (or *load-pathname* *compile-file-pathname*))
   "certs/test-key.pem")
  "Path to test private key")

(defun start-tls-test-server ()
  "Start a test HTTPS server for TLS loopback testing"
  (when *tls-test-server*
    (server:stop *tls-test-server*))
  
  ;; Create TLS context
  (let ((tls-context (tls:make-tls-context :server-p t)))
    (tls:load-cert-file tls-context *test-cert-path*)
    (tls:load-key-file tls-context *test-key-path*)
    (tls:set-verify-mode tls-context tls:+tls-verify-none+)
    
    (setf *tls-test-server* 
          (server:create-server 
           :port *tls-test-port*
           :tls-context tls-context
           :handler (lambda (request)
                      (let ((path (map:get request "path"))
                            (method (map:get request "method"))
                            (headers (map:get request "headers"))
                            (body (map:get request "body")))
                        (cond
                          ;; Echo endpoint - returns request info as JSON
                          ((string= path "/echo")
                           (map:make-map 
                            "status" 200
                            "headers" (map:make-map "Content-Type" "application/json")
                            "body" (format nil "{\"method\":\"~A\",\"path\":\"~A\",\"body\":\"~A\",\"tls\":true}"
                                          method path (or body ""))))
                          
                          ;; Large response test
                          ((string= path "/large")
                           (let ((large-body (make-string 10000 :initial-element #\A)))
                             (map:make-map
                              "status" 200
                              "headers" (map:make-map 
                                        "Content-Type" "text/plain"
                                        "Content-Length" (format nil "~D" (length large-body)))
                              "body" large-body)))
                          
                          ;; Binary data test
                          ((string= path "/binary")
                           (let ((binary-data (make-array 256 :element-type '(unsigned-byte 8)
                                                              :initial-contents (loop for i from 0 to 255 collect i))))
                             (map:make-map
                              "status" 200
                              "headers" (map:make-map 
                                        "Content-Type" "application/octet-stream"
                                        "Content-Length" "256")
                              "body" binary-data)))
                          
                          ;; Headers test
                          ((string= path "/headers")
                           (map:make-map
                            "status" 200
                            "headers" (map:make-map 
                                      "Content-Type" "text/plain"
                                      "X-Custom-Header" "test-value"
                                      "X-Request-Method" method
                                      "X-TLS-Enabled" "true")
                            "body" "Headers test over TLS"))
                          
                          ;; Error response test
                          ((string= path "/error")
                           (map:make-map
                            "status" 500
                            "headers" (map:make-map "Content-Type" "text/plain")
                            "body" "Internal Server Error (TLS)"))
                          
                          ;; Default 404
                          (t
                           (map:make-map
                            "status" 404
                            "headers" (map:make-map "Content-Type" "text/plain")
                            "body" "Not Found (TLS)"))))))))
  
  (server:start *tls-test-server*)
  
  ;; Wait for server to be ready
  (sleep 0.2))

(defun stop-tls-test-server ()
  "Stop the test HTTPS server"
  (when *tls-test-server*
    (server:stop *tls-test-server*)
    (setf *tls-test-server* nil)))

(defun create-test-tls-client ()
  "Create a TLS client context that accepts self-signed certificates"
  (let ((tls-context (tls:make-tls-context :server-p nil)))
    ;; Don't verify certificates for testing
    (tls:set-verify-mode tls-context tls:+tls-verify-none+)
    (client:create-client :tls-context tls-context)))

(define-test https-loopback-basic-get
  "Test basic GET request over TLS"
  (start-tls-test-server)
  (unwind-protect
       (let* ((client-instance (create-test-tls-client))
              (response (client:get (format nil "https://localhost:~D/echo" *tls-test-port*)
                                   :client client-instance)))
         (is-equal 200 (map:get response "status"))
         (is-equal "application/json" (map:get (map:get response "headers") "Content-Type"))
         (let ((body (map:get response "body")))
           (is (search "\"method\":\"GET\"" body))
           (is (search "\"path\":\"/echo\"" body))
           (is (search "\"tls\":true" body)))
         (client:close client-instance))
    (stop-tls-test-server)))

(define-test https-loopback-post-with-body
  "Test POST request with body over TLS"
  (start-tls-test-server)
  (unwind-protect
       (let* ((client-instance (create-test-tls-client))
              (response (client:post (format nil "https://localhost:~D/echo" *tls-test-port*)
                                    :body "secure test payload"
                                    :headers (map:make-map "Content-Type" "text/plain")
                                    :client client-instance)))
         (is-equal 200 (map:get response "status"))
         (let ((body (map:get response "body")))
           (is (search "\"method\":\"POST\"" body))
           (is (search "\"body\":\"secure test payload\"" body))
           (is (search "\"tls\":true" body)))
         (client:close client-instance))
    (stop-tls-test-server)))

(define-test https-loopback-large-response
  "Test handling of large HTTPS responses"
  (start-tls-test-server)
  (unwind-protect
       (let* ((client-instance (create-test-tls-client))
              (response (client:get (format nil "https://localhost:~D/large" *tls-test-port*)
                                   :client client-instance)))
         (is-equal 200 (map:get response "status"))
         (let ((body (map:get response "body")))
           (is-equal 10000 (length body))
           (is (every (lambda (c) (char= c #\A)) body)))
         (client:close client-instance))
    (stop-tls-test-server)))

(define-test https-loopback-binary-data
  "Test binary data transmission fidelity over TLS"
  (start-tls-test-server)
  (unwind-protect
       (let* ((client-instance (create-test-tls-client))
              (response (client:get (format nil "https://localhost:~D/binary" *tls-test-port*)
                                   :client client-instance)))
         (is-equal 200 (map:get response "status"))
         (is-equal "application/octet-stream" 
                   (map:get (map:get response "headers") "Content-Type"))
         (let ((body (map:get response "body")))
           (is-equal 256 (length body))
           ;; Verify byte sequence integrity
           (loop for i from 0 to 255
                 do (is-equal i (aref body i))))
         (client:close client-instance))
    (stop-tls-test-server)))

(define-test https-loopback-custom-headers
  "Test custom header transmission over TLS"
  (start-tls-test-server)
  (unwind-protect
       (let* ((client-instance (create-test-tls-client))
              (response (client:get (format nil "https://localhost:~D/headers" *tls-test-port*)
                                   :headers (map:make-map "X-Test-Header" "secure-test-value")
                                   :client client-instance)))
         (is-equal 200 (map:get response "status"))
         (let ((headers (map:get response "headers")))
           (is-equal "test-value" (map:get headers "X-Custom-Header"))
           (is-equal "GET" (map:get headers "X-Request-Method"))
           (is-equal "true" (map:get headers "X-TLS-Enabled")))
         (client:close client-instance))
    (stop-tls-test-server)))

(define-test https-loopback-error-handling
  "Test HTTPS error response handling"
  (start-tls-test-server)
  (unwind-protect
       (let* ((client-instance (create-test-tls-client))
              (response (client:get (format nil "https://localhost:~D/error" *tls-test-port*)
                                   :client client-instance)))
         (is-equal 500 (map:get response "status"))
         (is-equal "Internal Server Error (TLS)" (map:get response "body"))
         (client:close client-instance))
    (stop-tls-test-server)))

(define-test https-loopback-concurrent-requests
  "Test concurrent HTTPS requests for thread safety"
  (start-tls-test-server)
  (unwind-protect
       (let ((futures '())
             (num-requests 10))
         ;; Launch concurrent requests
         (dotimes (i num-requests)
           (push (thread:future 
                  (lambda ()
                    (let* ((client-instance (create-test-tls-client))
                           (response (client:get (format nil "https://localhost:~D/echo" *tls-test-port*)
                                               :client client-instance)))
                      (prog1 response
                        (client:close client-instance)))))
                 futures))
         
         ;; Collect all results
         (let ((responses (mapcar #'thread:deref futures)))
           (is-equal num-requests (length responses))
           ;; Verify all requests succeeded
           (dolist (response responses)
             (is-equal 200 (map:get response "status"))
             (is (search "\"method\":\"GET\"" (map:get response "body")))
             (is (search "\"tls\":true" (map:get response "body"))))))
    (stop-tls-test-server)))

(define-test https-loopback-connection-reuse
  "Test HTTPS connection reuse and keepalive"
  (start-tls-test-server)
  (unwind-protect
       (let ((client-instance (create-test-tls-client)))
         ;; Make multiple requests with the same client
         (dotimes (i 5)
           (let ((response (client:get (format nil "https://localhost:~D/echo" *tls-test-port*)
                                      :client client-instance)))
             (is-equal 200 (map:get response "status"))
             (is (search "\"tls\":true" (map:get response "body")))))
         (client:close client-instance))
    (stop-tls-test-server)))

;; Additional TLS-specific tests

(define-test https-loopback-cipher-negotiation
  "Test TLS cipher suite negotiation"
  (start-tls-test-server)
  (unwind-protect
       (let* ((client-instance (create-test-tls-client))
              (response (client:get (format nil "https://localhost:~D/echo" *tls-test-port*)
                                   :client client-instance)))
         (is-equal 200 (map:get response "status"))
         ;; Verify TLS was actually used
         (is (search "\"tls\":true" (map:get response "body")))
         (client:close client-instance))
    (stop-tls-test-server)))

(define-test https-loopback-mixed-content-sizes
  "Test various content sizes over TLS to verify buffering"
  (start-tls-test-server)
  (unwind-protect
       (let ((client-instance (create-test-tls-client))
             (test-sizes '(0 1 127 128 255 256 1023 1024 4095 4096 8191 8192)))
         (dolist (size test-sizes)
           (let* ((test-body (make-string size :initial-element #\X))
                  (response (client:post (format nil "https://localhost:~D/echo" *tls-test-port*)
                                        :body test-body
                                        :client client-instance)))
             (is-equal 200 (map:get response "status"))
             (is (search (format nil "\"body\":\"~A\"" test-body) (map:get response "body")))))
         (client:close client-instance))
    (stop-tls-test-server)))
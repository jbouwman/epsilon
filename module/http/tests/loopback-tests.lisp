(defpackage :epsilon.http.loopback.tests
  (:use :cl :epsilon.tool.test)
  (:local-nicknames
   (#:client #:epsilon.http.client)
   (#:server #:epsilon.http.server)
   (#:map #:epsilon.lib.map)
   (#:stream #:epsilon.lib.stream)
   (#:thread #:epsilon.sys.thread)))

(in-package :epsilon.http.loopback.tests)

(defparameter *test-port* 18080
  "Port for HTTP loopback tests")

(defparameter *test-server* nil
  "Test server instance")

(defun start-test-server ()
  "Start a test HTTP server for loopback testing"
  (when *test-server*
    (server:stop *test-server*))
  
  (setf *test-server* 
        (server:create-server 
         :port *test-port*
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
                          "body" (format nil "{\"method\":\"~A\",\"path\":\"~A\",\"body\":\"~A\"}"
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
                                    "X-Request-Method" method)
                          "body" "Headers test"))
                        
                        ;; Error response test
                        ((string= path "/error")
                         (map:make-map
                          "status" 500
                          "headers" (map:make-map "Content-Type" "text/plain")
                          "body" "Internal Server Error"))
                        
                        ;; Default 404
                        (t
                         (map:make-map
                          "status" 404
                          "headers" (map:make-map "Content-Type" "text/plain")
                          "body" "Not Found")))))))
  
  (server:start *test-server*)
  
  ;; Wait for server to be ready
  (sleep 0.1))

(defun stop-test-server ()
  "Stop the test HTTP server"
  (when *test-server*
    (server:stop *test-server*)
    (setf *test-server* nil)))

(define-test http-loopback-basic-get
  "Test basic GET request loopback"
  (start-test-server)
  (unwind-protect
       (let ((response (client:get (format nil "http://localhost:~D/echo" *test-port*))))
         (is-equal 200 (map:get response "status"))
         (is-equal "application/json" (map:get (map:get response "headers") "Content-Type"))
         (let ((body (map:get response "body")))
           (is (search "\"method\":\"GET\"" body))
           (is (search "\"path\":\"/echo\"" body))))
    (stop-test-server)))

(define-test http-loopback-post-with-body
  "Test POST request with body loopback"
  (start-test-server)
  (unwind-protect
       (let ((response (client:post (format nil "http://localhost:~D/echo" *test-port*)
                                   :body "test payload"
                                   :headers (map:make-map "Content-Type" "text/plain"))))
         (is-equal 200 (map:get response "status"))
         (let ((body (map:get response "body")))
           (is (search "\"method\":\"POST\"" body))
           (is (search "\"body\":\"test payload\"" body))))
    (stop-test-server)))

(define-test http-loopback-large-response
  "Test handling of large HTTP responses"
  (start-test-server)
  (unwind-protect
       (let ((response (client:get (format nil "http://localhost:~D/large" *test-port*))))
         (is-equal 200 (map:get response "status"))
         (let ((body (map:get response "body")))
           (is-equal 10000 (length body))
           (is (every (lambda (c) (char= c #\A)) body))))
    (stop-test-server)))

(define-test http-loopback-binary-data
  "Test binary data transmission fidelity"
  (start-test-server)
  (unwind-protect
       (let ((response (client:get (format nil "http://localhost:~D/binary" *test-port*))))
         (is-equal 200 (map:get response "status"))
         (is-equal "application/octet-stream" 
                   (map:get (map:get response "headers") "Content-Type"))
         (let ((body (map:get response "body")))
           (is-equal 256 (length body))
           ;; Verify byte sequence integrity
           (loop for i from 0 to 255
                 do (is-equal i (aref body i)))))
    (stop-test-server)))

(define-test http-loopback-custom-headers
  "Test custom header transmission"
  (start-test-server)
  (unwind-protect
       (let ((response (client:get (format nil "http://localhost:~D/headers" *test-port*)
                                  :headers (map:make-map "X-Test-Header" "test-value"))))
         (is-equal 200 (map:get response "status"))
         (let ((headers (map:get response "headers")))
           (is-equal "test-value" (map:get headers "X-Custom-Header"))
           (is-equal "GET" (map:get headers "X-Request-Method"))))
    (stop-test-server)))

(define-test http-loopback-error-handling
  "Test HTTP error response handling"
  (start-test-server)
  (unwind-protect
       (let ((response (client:get (format nil "http://localhost:~D/error" *test-port*))))
         (is-equal 500 (map:get response "status"))
         (is-equal "Internal Server Error" (map:get response "body")))
    (stop-test-server)))

(define-test http-loopback-concurrent-requests
  "Test concurrent HTTP requests for thread safety"
  (start-test-server)
  (unwind-protect
       (let ((futures '())
             (num-requests 10))
         ;; Launch concurrent requests
         (dotimes (i num-requests)
           (push (thread:future 
                  (lambda ()
                    (client:get (format nil "http://localhost:~D/echo" *test-port*))))
                 futures))
         
         ;; Collect all results
         (let ((responses (mapcar #'thread:deref futures)))
           (is-equal num-requests (length responses))
           ;; Verify all requests succeeded
           (dolist (response responses)
             (is-equal 200 (map:get response "status"))
             (is (search "\"method\":\"GET\"" (map:get response "body"))))))
    (stop-test-server)))

(define-test http-loopback-connection-reuse
  "Test HTTP connection reuse and keepalive"
  (start-test-server)
  (unwind-protect
       (let ((client-instance (client:create-client :keepalive t)))
         ;; Make multiple requests with the same client
         (dotimes (i 5)
           (let ((response (client:get (format nil "http://localhost:~D/echo" *test-port*)
                                      :client client-instance)))
             (is-equal 200 (map:get response "status"))))
         (client:close client-instance))
    (stop-test-server)))
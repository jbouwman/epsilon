(defpackage :epsilon.net.msgpack-http.tests
  (:use
   :cl
   :epsilon.tool.test)
  (:local-nicknames
   (:http :epsilon.net.http)
   (:server :epsilon.net.http.server)
   (:mp :epsilon.lib.msgpack)
   (:map :epsilon.lib.map)))

(in-package :epsilon.net.msgpack-http.tests)

;; Define a MessagePack content handler for the server

(defun handle-msgpack (request response)
  "Handle a MessagePack request and respond with MessagePack."
  ;; Set response content type to MessagePack
  (server:set-header response "Content-Type" "application/msgpack")
  
  ;; Decode request body (if any)
  (let* ((request-body (server:request-body request))
         (input-data (when request-body 
                      (mp:decode request-body)))
         ;; Create a simple response map that echoes the request
         ;; and adds a timestamp
         (response-data (map:assoc 
                         (map:assoc map:+empty+ 
                                   "status" "ok")
                         "echo" input-data)))
    
    ;; Encode and return the response
    (server:write-body response (mp:encode response-data))))

;; Client function to make MessagePack HTTP requests

(defun msgpack-request (url data &key (method :post))
  "Send a MessagePack request to URL with DATA and return decoded response."
  (let* ((encoded-data (mp:encode data))
         ;; Make the HTTP request with MessagePack data
         (response (http:request url
                               :method method 
                               :headers '(("Content-Type" . "application/msgpack")
                                          ("Accept" . "application/msgpack"))
                               :content encoded-data))
         ;; Decode the response body
         (decoded (mp:decode (getf response :body))))
    decoded))

;; Test for MessagePack HTTP server-client interaction
(deftest msgpack-http-echo-test ()
  "Test basic MessagePack over HTTP with an echo server."
  (let* ((server nil)
         (router (server:make-router))
         (port 12345)
         (test-data (map:assoc 
                    (map:assoc map:+empty+
                              "name" "test")
                    "value" 42)))
    
    ;; Set up a route for MessagePack requests
    (server:connect-route router "/msgpack" #'handle-msgpack :method :post)
    
    (unwind-protect
         (progn
           ;; Create and start server
           (setf server (server:make-server 
                        (lambda (req res) 
                          (server:route router req res))
                        :port port))
           (server:start server)
           
           ;; Make a request to the server
           (let ((response (msgpack-request 
                           (format nil "http://localhost:~D/msgpack" port)
                           test-data)))
             
             ;; Verify response
             (is (string= "ok" (map:lookup response "status")))
             (is (equal (map:lookup test-data "name") 
                       (map:lookup (map:lookup response "echo") "name")))
             (is (= (map:lookup test-data "value")
                   (map:lookup (map:lookup response "echo") "value")))))
      
      ;; Cleanup
      (when (and server (server:server-running-p server))
        (server:stop server)))))

;; Test to verify roundtrip encoding/decoding of complex data types
(deftest msgpack-http-complex-data-test ()
  "Test MessagePack over HTTP with more complex data structures."
  (let* ((server nil)
         (router (server:make-router))
         (port 12346)
         ;; Create a complex test structure with various data types
         (test-data (map:assoc 
                    (map:assoc 
                     (map:assoc map:+empty+
                               "string" "hello world")
                     "integer" 12345)
                    "array" #(1 2 3 4 5))
                   ))
    
    ;; Set up a route for MessagePack requests
    (server:connect-route router "/msgpack" #'handle-msgpack :method :post)
    
    (unwind-protect
         (progn
           ;; Create and start server
           (setf server (server:make-server 
                        (lambda (req res) 
                          (server:route router req res))
                        :port port))
           (server:start server)
           
           ;; Make a request to the server
           (let ((response (msgpack-request 
                           (format nil "http://localhost:~D/msgpack" port)
                           test-data)))
             
             ;; Verify response contains echoed data with all types preserved
             (is (string= "ok" (map:lookup response "status")))
             (let ((echo (map:lookup response "echo")))
               (is (string= "hello world" (map:lookup echo "string")))
               (is (= 12345 (map:lookup echo "integer")))
               (is (equalp #(1 2 3 4 5) (map:lookup echo "array"))))))
      
      ;; Cleanup
      (when (and server (server:server-running-p server))
        (server:stop server)))))

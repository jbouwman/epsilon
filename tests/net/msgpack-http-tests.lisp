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

(defun handle-msgpack (request response)
  (server:set-header response "Content-Type" "application/msgpack")
  (let* ((request-body (server:request-body request))
         (input-data (when request-body 
                      (mp:decode request-body)))
         (response-data (map:make-map "status" "ok"
                                      "echo" input-data)))
    (server:write-body response (mp:encode response-data))))

(defun msgpack-request (url data &key (method :post))
  "Send a MessagePack request to URL with DATA and return decoded response."
  (let* ((encoded-data (mp:encode data))
         (response (http:request url
                                 :method method 
                                 :headers (map:make-map "Content-Type" "application/msgpack"
                                                        "Accept" "application/msgpack")
                                 :content encoded-data))
         (decoded (mp:decode (getf response :body))))
    decoded))

(deftest msgpack-http-echo-test ()
  "Test basic MessagePack over HTTP with an echo server."
  (let* ((server nil)
         (router (server:make-router))
         (port 12345)
         (test-data (map:make-map "name" "test"
                                  "value" 42)))
    (server:connect-route router "/msgpack" #'handle-msgpack :method :post)
    (unwind-protect
         (progn
           (setf server (server:make-server 
                        (lambda (req res) 
                          (server:route router req res))
                        :port port))
           (server:start server)
           
           (let ((response (msgpack-request 
                           (format nil "http://localhost:~D/msgpack" port)
                           test-data)))
             (is (string= "ok" (map:get response "status")))
             (is (equal (map:get test-data "name") 
                       (map:get (map:get response "echo") "name")))
             (is (= (map:get test-data "value")
                   (map:get (map:get response "echo") "value")))))
      (when (and server (server:server-running-p server))
        (server:stop server)))))

(deftest msgpack-http-complex-data-test ()
  "Test MessagePack over HTTP with more complex data structures."
  (let* ((server nil)
         (router (server:make-router))
         (port 12346)
         (test-data (map:make-map "string" "hello world"
                                  "integer" 12345
                                  "array" #(1 2 3 4 5))))
    (server:connect-route router "/msgpack" #'handle-msgpack :method :post)
    (unwind-protect
         (progn
           (setf server (server:make-server 
                        (lambda (req res) 
                          (server:route router req res))
                        :port port))
           (server:start server)
           (let ((response (msgpack-request 
                           (format nil "http://localhost:~D/msgpack" port)
                           test-data)))
             (is (string= "ok" (map:get response "status")))
             (let ((echo (map:get response "echo")))
               (is (string= "hello world" (map:get echo "string")))
               (is (= 12345 (map:get echo "integer")))
               (is (equalp #(1 2 3 4 5) (map:get echo "array"))))))
      (when (and server (server:server-running-p server))
        (server:stop server)))))

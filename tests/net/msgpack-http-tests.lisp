(defpackage #:epsilon.net.msgpack-http.tests
  (:use 
   #:cl
   #:epsilon.tool.test)
  (:local-nicknames
   (#:msgpack-http #:epsilon.net.msgpack-http)
   (#:msgpack #:epsilon.lib.msgpack)
   (#:map #:epsilon.lib.map)
   (#:sync #:epsilon.sys.sync)
   (#:net #:epsilon.net)
   (#:thread #:epsilon.sys.sync.thread)))

(in-package #:epsilon.net.msgpack-http.tests)

;; Utility functions for testing

(defun find-unused-port ()
  "Find an unused TCP port to use for testing"
  ;; Start from a high port number
  (loop for port from 50000 upto 60000
        when (handler-case
                 (let ((socket (net:make-tcp-socket)))
                   (net:bind socket "127.0.0.1" port)
                   (net:close-socket socket)
                   t)
               (error () nil))
        return port))

(defun with-server-client (fn &key (address "127.0.0.1"))
  "Set up a server and client on an unused port, call fn with the client"
  (let* ((port (find-unused-port))
         (server (msgpack-http:make-server :port port :address address))
         (base-url (format nil "http://~A:~D" address port)))
    
    ;; Add the standard routes
    (msgpack-http:define-handler server "/echo" (request body)
      body)
    
    (msgpack-http:define-handler server "/message" (request body)
      (map:make-map "status" "OK"))
    
    ;; Start the server in a separate thread
    (msgpack-http:start-server server)
    
    (unwind-protect
         (let ((client (msgpack-http:make-client base-url)))
           ;; Wait a bit for the server to be ready
           (sleep 0.1)
           (funcall fn client))
      ;; Ensure the server is stopped
      (msgpack-http:stop-server server))))

;; Test the echo route
(deftest test-echo-route ()
  "Test the echo route with different MessagePack values"
  (with-server-client
      (lambda (client)
        ;; Test with a simple string
        (let* ((test-data "Hello, world!")
               (response (msgpack-http:post client "/echo" test-data)))
          (is (equal (getf response :status) 200)
              "Echo route should return 200 OK")
          (is (equal (getf response :body) test-data)
              "Echo route should return the same data it received"))
        
        ;; Test with a complex map
        (let* ((test-data (map:make-map "key1" "value1" 
                                       "key2" 42
                                       "nested" (map:make-map "inner" "value")))
               (response (msgpack-http:post client "/echo" test-data)))
          (is (equal (getf response :status) 200)
              "Echo route should return 200 OK")
          (is (map:map-equal (getf response :body) test-data)
              "Echo route should return the same map it received"))
        
        ;; Test with an array
        (let* ((test-data #(1 2 3 "four" 5.0))
               (response (msgpack-http:post client "/echo" test-data)))
          (is (equal (getf response :status) 200)
              "Echo route should return 200 OK")
          (is (equalp (getf response :body) test-data)
              "Echo route should return the same array it received")))))

;; Test the message route
(deftest test-message-route ()
  "Test the message route which should always return OK"
  (with-server-client
      (lambda (client)
        ;; Test with empty body
        (let ((response (msgpack-http:post client "/message" nil)))
          (is (equal (getf response :status) 200)
              "Message route should return 200 OK")
          (is (equal (map:get (getf response :body) "status") "OK")
              "Message route should return a map with status OK"))
        
        ;; Test with some data
        (let ((response (msgpack-http:post client "/message" "Some data")))
          (is (equal (getf response :status) 200)
              "Message route should return 200 OK")
          (is (equal (map:get (getf response :body) "status") "OK")
              "Message route should return a map with status OK regardless of input")))))

;; Test concurrent requests
(deftest test-concurrent-requests ()
  "Test multiple concurrent requests to the server"
  (with-server-client
      (lambda (client)
        (let ((threads nil)
              (results (make-array 10))
              (barrier (sync:make-barrier 10)))
          
          ;; Create 10 threads making requests in parallel
          (dotimes (i 10)
            (push (thread:make-thread
                   (lambda ()
                     (sync:wait-on-barrier barrier)
                     (let ((test-data (format nil "Thread-~D" i))
                           (response (msgpack-http:post client "/echo" (format nil "Thread-~D" i))))
                       (setf (aref results i) (equal (getf response :body) test-data))))
                   :name (format nil "Test Thread ~D" i))
                  threads))
          
          ;; Wait for all threads to complete
          (mapc #'thread:join-thread threads)
          
          ;; Check all results
          (dotimes (i 10)
            (is (aref results i)
                (format nil "Thread ~D should receive correct response" i)))))))

;; Test error conditions
(deftest test-error-conditions ()
  "Test error conditions when using the client"
  (with-server-client
      (lambda (client)
        ;; Test invalid route
        (let ((response (msgpack-http:get client "/nonexistent")))
          (is (equal (getf response :status) 404)
              "Non-existent route should return 404 Not Found"))
        
        ;; Test POST to a GET-only route (if applicable)
        ;; Or other protocol errors you want to test
        )))

;; Test server functionality
(deftest test-server-management ()
  "Test server start/stop behavior"
  (let* ((port (find-unused-port))
         (server (msgpack-http:make-server :port port)))
    
    ;; Add a test route
    (msgpack-http:define-handler server "/test" (request body)
      "test")
    
    ;; Test server start
    (msgpack-http:start-server server)
    (let ((client (msgpack-http:make-client (format nil "http://127.0.0.1:~D" port))))
      (sleep 0.1) ;; Give the server a moment to start
      
      (let ((response (msgpack-http:get client "/test")))
        (is (equal (getf response :status) 200)
            "Server should respond after starting"))
      
      ;; Test server stop
      (msgpack-http:stop-server server)
      (sleep 0.1) ;; Give the server a moment to stop
      
      (handler-case
          (progn
            (msgpack-http:get client "/test")
            (fail "Server should not respond after stopping"))
        (error (e)
          (declare (ignore e))
          (pass "Connection should fail after server is stopped"))))))

;; Run all tests
(defun run-tests ()
  (test-echo-route)
  (test-message-route)
  (test-concurrent-requests)
  (test-error-conditions)
  (test-server-management))
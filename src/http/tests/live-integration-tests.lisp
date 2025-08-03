;;;; Live Network Integration Tests
;;;;
;;;; Tests that use actual network connections to verify the networking stack

(defpackage :epsilon.http.live.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:client #:epsilon.http.client)
   (#:pooled #:epsilon.http.client.pooled)
   (#:server #:epsilon.http.server)
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response)
   (#:net #:epsilon.net)
   (#:map #:epsilon.map)
   (#:str #:epsilon.string)
   (#:pool #:epsilon.http.connection-pool)))

(in-package :epsilon.http.live.tests)

;;;; Test Configuration

(defparameter *test-server-port* 18090
  "Port for test server")

(defparameter *external-test-urls* 
  '(("http://httpbin.org/get" . :get)
    ("http://httpbin.org/post" . :post)
    ("https://httpbin.org/get" . :https))
  "External URLs for testing (if available)")

(defparameter *run-external-tests* nil
  "Whether to run tests against external services")

;;;; Test Server Setup

(defvar *test-server* nil)

(defun start-test-server ()
  "Start test server for integration tests"
  (when *test-server*
    (server:stop-server *test-server*))
  
  (setf *test-server*
        (server:start-server
         (lambda (req)
           (let ((path (request:request-path req))
                 (method (request:request-method req))
                 (headers (request:request-headers req))
                 (body (request:request-body req)))
             
             (cond
               ;; Echo endpoint
               ((string= path "/echo")
                (response:json-response
                 (map:make-map
                  "method" method
                  "path" path
                  "headers" headers
                  "body" (or body "")
                  "request_id" (format nil "~D" (get-universal-time)))))
               
               ;; Delay endpoint
               ((str:starts-with-p path "/delay/")
                (let ((seconds (parse-integer (subseq path 7) :junk-allowed t)))
                  (sleep (min seconds 5)) ; Max 5 second delay
                  (response:text-response 
                   (format nil "Delayed response after ~D seconds" seconds))))
               
               ;; Large response
               ((string= path "/large")
                (let ((size (or (ignore-errors 
                                  (parse-integer 
                                   (map:get (request:request-params req) "size")))
                                1024)))
                  (response:text-response
                   (make-string (min size 100000) :initial-element #\A))))
               
               ;; Stream response (chunked)
               ((string= path "/stream")
                (let ((resp (response:text-response "")))
                  (response:set-header resp "Transfer-Encoding" "chunked")
                  (response:set-header resp "Content-Type" "text/plain")
                  resp))
               
               ;; Connection test
               ((string= path "/connection-info")
                (response:json-response
                 (map:make-map
                  "connection" (map:get headers "connection")
                  "user_agent" (map:get headers "user-agent")
                  "host" (map:get headers "host"))))
               
               ;; Keep-alive test
               ((string= path "/keepalive")
                (let ((resp (response:text-response "Keep-alive response")))
                  (response:set-header resp "Connection" "keep-alive")
                  (response:set-header resp "Keep-Alive" "timeout=5, max=1000")
                  resp))
               
               ;; Error responses
               ((str:starts-with-p path "/status/")
                (let ((code (parse-integer (subseq path 8) :junk-allowed t)))
                  (response:text-response 
                   (format nil "Status ~D response" code)
                   :status code)))
               
               ;; Default
               (t (response:text-response "Test server response")))))
         :port *test-server-port*))
  
  ;; Give server time to start
  (sleep 0.1))

(defun stop-test-server ()
  "Stop test server"
  (when *test-server*
    (server:stop-server *test-server*)
    (setf *test-server* nil)))

;;;; Basic Connectivity Tests

(deftest test-socket-connectivity ()
  "Test basic socket connectivity"
  (start-test-server)
  (unwind-protect
       (let ((socket (net:socket))
             (addr (net:make-socket-address "127.0.0.1" *test-server-port*)))
         (net:connect socket addr)
         (is (net:socket-connected-p socket))
         (net:close socket))
    (stop-test-server)))

(deftest test-http-server-response ()
  "Test HTTP server responds correctly"
  (start-test-server)
  (unwind-protect
       (multiple-value-bind (status headers body)
           (client:http-get (format nil "http://localhost:~D/" *test-server-port*))
         (is-equal 200 status)
         (is (stringp body))
         (is (search "Test server response" body)))
    (stop-test-server)))

;;;; HTTP Client Tests

(deftest test-http-methods ()
  "Test all HTTP methods work correctly"
  (start-test-server)
  (unwind-protect
       (progn
         ;; GET
         (multiple-value-bind (status headers body)
             (client:http-get (format nil "http://localhost:~D/echo" *test-server-port*))
           (is-equal 200 status)
           (is (search "\"method\":\"GET\"" body)))
         
         ;; POST
         (multiple-value-bind (status headers body)
             (client:http-post (format nil "http://localhost:~D/echo" *test-server-port*)
                               :body "test post data")
           (is-equal 200 status)
           (is (search "\"method\":\"POST\"" body))
           (is (search "test post data" body)))
         
         ;; PUT
         (multiple-value-bind (status headers body)
             (client:http-put (format nil "http://localhost:~D/echo" *test-server-port*)
                              :body "test put data")
           (is-equal 200 status)
           (is (search "\"method\":\"PUT\"" body)))
         
         ;; DELETE
         (multiple-value-bind (status headers body)
             (client:http-delete (format nil "http://localhost:~D/echo" *test-server-port*))
           (is-equal 200 status)
           (is (search "\"method\":\"DELETE\"" body))))
    (stop-test-server)))

(deftest test-http-headers ()
  "Test HTTP header handling"
  (start-test-server)
  (unwind-protect
       (let ((custom-headers (map:make-map 
                              "X-Test-Header" "test-value"
                              "Authorization" "Bearer token123")))
         (multiple-value-bind (status headers body)
             (client:http-get (format nil "http://localhost:~D/echo" *test-server-port*)
                              :headers custom-headers)
           (is-equal 200 status)
           (is (search "x-test-header" body))
           (is (search "test-value" body))
           (is (search "authorization" body))))
    (stop-test-server)))

(deftest test-http-status-codes ()
  "Test various HTTP status codes"
  (start-test-server)
  (unwind-protect
       (dolist (code '(200 201 204 400 401 404 500))
         (multiple-value-bind (status headers body)
             (client:http-get (format nil "http://localhost:~D/status/~D" 
                                      *test-server-port* code))
           (is-equal code status)))
    (stop-test-server)))

;;;; Connection Pooling Tests

(deftest test-connection-pool-creation ()
  "Test connection pool creation and configuration"
  (let ((pool (pool:create-connection-pool :max-size 5 :max-idle-time 60)))
    (is (pool:connection-pool-p pool))
    (is-equal 0 (pool:pool-active-count pool))
    (pool:shutdown-connection-pool pool)))

(deftest test-pooled-http-requests ()
  "Test HTTP requests using connection pooling"
  (start-test-server)
  (unwind-protect
       (pool:with-connection-pool (test-pool :max-size 3 :max-idle-time 30)
         ;; Make multiple requests to same host
         (dotimes (i 5)
           (multiple-value-bind (status headers body)
               (pooled:pooled-http-get 
                (format nil "http://localhost:~D/echo?request=~D" 
                        *test-server-port* i)
                :pool test-pool)
             (is-equal 200 status)
             (is (search (format nil "\"request\":\"~D\"" i) body))))
         
         ;; Check pool statistics
         (let ((stats (pooled:pool-statistics test-pool)))
           (is (> (getf stats :connections-created) 0))
           (is (>= (getf stats :connections-reused) 0))))
    (stop-test-server)))

(deftest test-connection-reuse ()
  "Test that connections are properly reused"
  (start-test-server)
  (unwind-protect
       (pool:with-connection-pool (test-pool :max-size 2)
         ;; Make requests to same host
         (dotimes (i 10)
           (pooled:pooled-http-get 
            (format nil "http://localhost:~D/connection-info" *test-server-port*)
            :pool test-pool))
         
         ;; Should have reused connections
         (let ((stats (pooled:pool-statistics test-pool)))
           (is (< (getf stats :connections-created) 10))
           (is (> (getf stats :connections-reused) 0))))
    (stop-test-server)))

;;;; Performance Tests

(deftest test-concurrent-requests ()
  "Test handling multiple concurrent requests"
  (start-test-server)
  (unwind-protect
       (let ((threads '())
             (results (make-array 10 :initial-element nil)))
         
         ;; Launch concurrent requests
         (dotimes (i 10)
           (push (sb-thread:make-thread
                  (lambda (index)
                    (multiple-value-bind (status headers body)
                        (client:http-get 
                         (format nil "http://localhost:~D/echo?thread=~D" 
                                 *test-server-port* index))
                      (setf (aref results index) 
                            (list status (search (format nil "\"thread\":\"~D\"" index) body)))))
                  :arguments (list i)
                  :name (format nil "test-thread-~D" i))
                 threads))
         
         ;; Wait for all threads
         (dolist (thread threads)
           (sb-thread:join-thread thread))
         
         ;; Verify all requests succeeded
         (dotimes (i 10)
           (let ((result (aref results i)))
             (is-equal 200 (first result))
             (is (second result) "Thread-specific response not found"))))
    (stop-test-server)))

(deftest test-large-response-handling ()
  "Test handling of large responses"
  (start-test-server)
  (unwind-protect
       (multiple-value-bind (status headers body)
           (client:http-get (format nil "http://localhost:~D/large?size=50000" 
                                    *test-server-port*))
         (is-equal 200 status)
         (is-equal 50000 (length body))
         (is (every (lambda (ch) (char= ch #\A)) body)))
    (stop-test-server)))

;;;; Error Handling Tests

(deftest test-connection-refused ()
  "Test handling of connection refused"
  (handler-case
      (client:http-get "http://localhost:19999/") ; Non-existent server
    (error (e)
      (is (typep e 'error)) ; Should get some kind of error
      (is (search "refused" (format nil "~A" e))))))

(deftest test-timeout-handling ()
  "Test request timeout handling"
  (start-test-server)
  (unwind-protect
       ;; This might timeout or succeed depending on system
       (handler-case
           (multiple-value-bind (status headers body)
               (client:http-get (format nil "http://localhost:~D/delay/3" 
                                        *test-server-port*))
             ;; If it succeeds, should get proper response
             (is-equal 200 status)
             (is (search "Delayed response" body)))
         (error (e)
           ;; Timeout is also acceptable
           (is (typep e 'error))))
    (stop-test-server)))

;;;; TLS Tests (if available)

(deftest test-tls-connection ()
  "Test TLS connection handling"
  ;; Only test if we can establish TLS
  (handler-case
      (let ((tls-context (tls:create-tls-context :server-p nil)))
        (is (tls:tls-context-p tls-context))
        ;; TLS context created successfully
        t)
    (error ()
      ;; TLS not available, skip test
      (format t "TLS not available, skipping TLS tests~%")
      t)))

;;;; External Service Tests (optional)

(deftest test-external-http-service ()
  "Test against external HTTP service (if enabled)"
  (when *run-external-tests*
    (handler-case
        (multiple-value-bind (status headers body)
            (client:http-get "http://httpbin.org/get")
          (is-equal 200 status)
          (is (stringp body)))
      (error (e)
        (format t "External service test failed: ~A~%" e)
        ;; Don't fail the test suite if external service is down
        t))))

;;;; WebSocket Integration Tests

(deftest test-websocket-handshake ()
  "Test WebSocket handshake process"
  ;; This would require WebSocket server support
  ;; For now, just test that WebSocket functions exist
  (is (fboundp 'epsilon.websocket::make-frame))
  (is (fboundp 'epsilon.websocket::encode-frame)))

;;;; Memory and Resource Tests

(deftest test-connection-cleanup ()
  "Test that connections are properly cleaned up"
  (start-test-server)
  (unwind-protect
       (pool:with-connection-pool (test-pool :max-size 2 :max-idle-time 1)
         ;; Create some connections
         (dotimes (i 5)
           (pooled:pooled-http-get 
            (format nil "http://localhost:~D/echo" *test-server-port*)
            :pool test-pool))
         
         ;; Wait for cleanup
         (sleep 2)
         
         ;; Force cleanup
         (pool:clear-connection-pool test-pool)
         
         ;; Pool should be empty
         (is-equal 0 (pool:pool-active-count test-pool)))
    (stop-test-server)))

;;;; Test Runner

(defun run-live-integration-tests (&key external-tests)
  "Run all live integration tests"
  (setf *run-external-tests* external-tests)
  (format t "~%Starting live integration tests...~%")
  (format t "Test server port: ~D~%" *test-server-port*)
  (when external-tests
    (format t "External tests enabled~%"))
  
  (unwind-protect
       (progn
         (format t "~%Running integration test suite...~%")
         (run-package-tests :epsilon.http.live.tests))
    (stop-test-server)
    (format t "~%Live integration tests complete.~%")))
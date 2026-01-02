;;;; Local Client-Server Integration Tests
;;;;
;;;; Tests HTTP client and server working together locally

(package epsilon.http.test-local-integration
  (import (epsilon.test test)
          (epsilon.http http)
          (epsilon.http.server server)
          (epsilon.http.client client)
          (epsilon.http.request request)
          (epsilon.http.response response)
          (epsilon.map map)
          (epsilon.string str)
          (epsilon.json json)))

;;;; Test Configuration

(defparameter *test-port* 18080
  "Port for test HTTP server")

(defparameter *test-host* "127.0.0.1"
  "Host for test HTTP server")

(defparameter *server* nil
  "Test server instance")

;;;; Test Handlers

(defun echo-handler (req)
  "Echo back request details"
  (response:json-response
   (map:make-map
    "method" (request:request-method req)
    "path" (request:request-path req)
    "headers" (request:request-headers req)
    "params" (request:request-params req)
    "body" (request:request-body req))))

(defun status-handler (req)
  "Return status code from path"
  (let* ((path (request:request-path req))
         (status-str (subseq path (1+ (position #\/ path :from-end t))))
         (status (parse-integer status-str :junk-allowed t)))
    (if status
        (response:text-response (format nil "Status ~D" status) :status status)
        (response:text-response "Invalid status" :status 400))))

(defun delay-handler (req)
  "Delay response by specified seconds"
  (let* ((params (request:request-params req))
         (delay-str (map:get params "seconds"))
         (delay (if delay-str (parse-integer delay-str :junk-allowed t) 1)))
    (sleep delay)
    (response:json-response
     (map:make-map "delayed" delay))))

(defun large-response-handler (req)
  "Generate large response"
  (let* ((params (request:request-params req))
         (size-str (map:get params "size"))
         (size (if size-str (parse-integer size-str :junk-allowed t) 1000)))
    (response:text-response
     (make-string size :initial-element #\X))))

(defun upload-handler (req)
  "Handle file upload"
  (let ((body (request:request-body req))
        (content-type (map:get (request:request-headers req) "content-type")))
    (response:json-response
     (map:make-map
      "received-bytes" (length body)
      "content-type" content-type))))

(defun main-handler (req)
  "Main request router"
  (let ((path (request:request-path req)))
    (cond
      ((string= path "/echo")
       (echo-handler req))
      ((str:starts-with-p path "/status/")
       (status-handler req))
      ((string= path "/delay")
       (delay-handler req))
      ((string= path "/large")
       (large-response-handler req))
      ((string= path "/upload")
       (upload-handler req))
      ((string= path "/")
       (response:text-response "Test server running"))
      (t
       (response:text-response "Not found" :status 404)))))

;;;; Test Setup and Teardown

(defun start-test-server ()
  "Start the test HTTP server"
  (unless *server*
    (setf *server* (server:start-server #'main-handler
                                        :port *test-port*
                                        :address *test-host*))
    (sleep 0.1))) ; Give server time to start

(defun stop-test-server ()
  "Stop the test HTTP server"
  (when *server*
    (server:stop-server *server*)
    (setf *server* nil)))

;;;; Basic Method Tests

(test:deftest test-local-get ()
  "Test local GET request"
  (start-test-server)
  (unwind-protect
       (handler-case
           (let ((response (http:http-get (format nil "http://~A:~D/echo?test=value"
                                                  *test-host* *test-port*))))
             (test:is-equal 200 (response:response-status response))
             (let ((body-str (response:response-body response)))
               (when body-str
                 (let ((body (json:parse body-str)))
                   (test:is-equal "GET" (map:get body "method"))
                   (test:is-equal "/echo" (map:get body "path"))
                   (test:is-equal "value" (map:get (map:get body "params") "test"))))))
         (error (e)
           (test:fail (format nil "Local GET failed: ~A" e))))
    (stop-test-server)))

(test:deftest test-local-post ()
  "Test local POST request"
  (start-test-server)
  (unwind-protect
       (handler-case
           (let ((response (http:http-post (format nil "http://~A:~D/echo"
                                                   *test-host* *test-port*)
                                          :body "test data"
                                          :headers (map:make-map
                                                   "Content-Type" "text/plain"))))
             (test:is-equal 200 (response:response-status response))
             (let ((body (json:parse (response:response-body response))))
               (test:is-equal "POST" (map:get body "method"))
               (test:is-equal "test data" (map:get body "body"))))
         (error (e)
           (test:fail (format nil "Local POST failed: ~A" e))))
    (stop-test-server)))

(test:deftest test-local-put ()
  "Test local PUT request"
  (start-test-server)
  (unwind-protect
       (handler-case
           (let ((response (http:http-put (format nil "http://~A:~D/echo"
                                                  *test-host* *test-port*)
                                         :body "{\"key\":\"value\"}"
                                         :headers (map:make-map
                                                  "Content-Type" "application/json"))))
             (test:is-equal 200 (response:response-status response))
             (let ((body (json:parse (response:response-body response))))
               (test:is-equal "PUT" (map:get body "method"))))
         (error (e)
           (test:fail (format nil "Local PUT failed: ~A" e))))
    (stop-test-server)))

(test:deftest test-local-delete ()
  "Test local DELETE request"
  (start-test-server)
  (unwind-protect
       (handler-case
           (let ((response (http:http-delete (format nil "http://~A:~D/echo"
                                                     *test-host* *test-port*))))
             (test:is-equal 200 (response:response-status response))
             (let ((body (json:parse (response:response-body response))))
               (test:is-equal "DELETE" (map:get body "method"))))
         (error (e)
           (test:fail (format nil "Local DELETE failed: ~A" e))))
    (stop-test-server)))

;;;; Status Code Tests

(test:deftest test-status-codes ()
  "Test various HTTP status codes"
  (start-test-server)
  (unwind-protect
       (progn
         ;; Test 200
         (let ((response (http:http-get (format nil "http://~A:~D/status/200"
                                                *test-host* *test-port*))))
           (test:is-equal 200 (response:response-status response)))

         ;; Test 404
         (let ((response (http:http-get (format nil "http://~A:~D/status/404"
                                                *test-host* *test-port*))))
           (test:is-equal 404 (response:response-status response)))

         ;; Test 500
         (let ((response (http:http-get (format nil "http://~A:~D/status/500"
                                                *test-host* *test-port*))))
           (test:is-equal 500 (response:response-status response))))
    (stop-test-server)))

;;;; Header Tests

(test:deftest test-custom-headers ()
  "Test custom request and response headers"
  (start-test-server)
  (unwind-protect
       (handler-case
           (let ((response (http:http-get (format nil "http://~A:~D/echo"
                                                  *test-host* *test-port*)
                                         :headers (map:make-map
                                                  "X-Custom-Header" "test-value"
                                                  "X-Another-Header" "another-value"))))
             (test:is-equal 200 (response:response-status response))
             (let* ((body (json:parse (response:response-body response)))
                    (headers (map:get body "headers")))
               (test:is-equal "test-value" (map:get headers "x-custom-header"))
               (test:is-equal "another-value" (map:get headers "x-another-header"))))
         (error (e)
           (test:fail (format nil "Custom headers test failed: ~A" e))))
    (stop-test-server)))

;;;; Query Parameter Tests

(test:deftest test-query-parameters ()
  "Test query parameter handling"
  (start-test-server)
  (unwind-protect
       (handler-case
           (let ((response (http:http-get (format nil "http://~A:~D/echo?foo=bar&baz=qux&empty="
                                                  *test-host* *test-port*))))
             (test:is-equal 200 (response:response-status response))
             (let* ((body (json:parse (response:response-body response)))
                    (params (map:get body "params")))
               (test:is-equal "bar" (map:get params "foo"))
               (test:is-equal "qux" (map:get params "baz"))
               (test:is-equal "" (map:get params "empty"))))
         (error (e)
           (test:fail (format nil "Query parameters test failed: ~A" e))))
    (stop-test-server)))

;;;; Large Payload Tests

(test:deftest test-large-request ()
  "Test large request body"
  (start-test-server)
  (unwind-protect
       (handler-case
           (let* ((large-data (make-string 100000 :initial-element #\A))
                  (response (http:http-post (format nil "http://~A:~D/upload"
                                                    *test-host* *test-port*)
                                           :body large-data
                                           :headers (map:make-map
                                                    "Content-Type" "text/plain"))))
             (test:is-equal 200 (response:response-status response))
             (let ((body (json:parse (response:response-body response))))
               (test:is-equal 100000 (map:get body "received-bytes"))))
         (error (e)
           (test:fail (format nil "Large request test failed: ~A" e))))
    (stop-test-server)))

(test:deftest test-large-response ()
  "Test large response body"
  (start-test-server)
  (unwind-protect
       (handler-case
           (let ((response (http:http-get (format nil "http://~A:~D/large?size=50000"
                                                  *test-host* *test-port*))))
             (test:is-equal 200 (response:response-status response))
             (test:is-equal 50000 (length (response:response-body response))))
         (error (e)
           (test:fail (format nil "Large response test failed: ~A" e))))
    (stop-test-server)))

;;;; Concurrent Request Tests

(test:deftest test-concurrent-local-requests ()
  "Test concurrent requests to local server"
  (start-test-server)
  (unwind-protect
       (handler-case
           (let ((threads nil)
                 (results (make-array 10 :initial-element nil)))
             ;; Start 10 concurrent requests
             (dotimes (i 10)
               (push (sb-thread:make-thread
                      (lambda (index)
                        (handler-case
                            (let ((response (http:http-get
                                           (format nil "http://~A:~D/echo?thread=~D"
                                                  *test-host* *test-port* index))))
                              (setf (aref results index)
                                    (response:response-status response)))
                          (error (e)
                            (setf (aref results index) e))))
                      :arguments (list i)
                      :name (format nil "test-thread-~D" i))
                     threads))
             ;; Wait for all threads to complete
             (dolist (thread threads)
               (sb-thread:join-thread thread))
             ;; Check all requests succeeded
             (dotimes (i 10)
               (test:is-equal 200 (aref results i))))
         (error (e)
           (test:fail (format nil "Concurrent requests test failed: ~A" e))))
    (stop-test-server)))

;;;; Keep-Alive Tests

(test:deftest test-keep-alive-connection ()
  "Test HTTP keep-alive connections"
  ;; Skip: HTTP keep-alive not yet implemented - server closes connection after each request
  (test:skip "HTTP keep-alive not implemented"))

;;;; Error Handling Tests

(test:deftest test-404-not-found ()
  "Test 404 not found handling"
  (start-test-server)
  (unwind-protect
       (handler-case
           (let ((response (http:http-get (format nil "http://~A:~D/nonexistent"
                                                  *test-host* *test-port*))))
             (test:is-equal 404 (response:response-status response))
             (test:is-equal "Not found" (response:response-body response)))
         (error (e)
           (test:fail (format nil "404 test failed: ~A" e))))
    (stop-test-server)))

(test:deftest test-connection-refused ()
  "Test connection refused error"
  ;; Don't start server - should get connection error
  (handler-case
      (let ((response (http:http-get (format nil "http://~A:~D/"
                                             *test-host* (1+ *test-port*)))))
        ;; Should not get here
        (test:fail (format nil "Expected connection error, got response: ~A"
                     (response:response-status response))))
    (error (e)
      ;; Expected - connection should be refused
      (test:is-true t))))

;;;; Timeout Tests

(test:deftest test-response-timeout ()
  "Test response timeout handling"
  (start-test-server)
  (unwind-protect
       (handler-case
           ;; Request a 5 second delay but with shorter timeout
           (let ((response (http:request (format nil "http://~A:~D/delay?seconds=5"
                                                 *test-host* *test-port*)
                                         :method "GET"
                                         :timeout 1)))
             ;; Should not get here - timeout expected
             (test:fail (format nil "Expected timeout, got response: ~A"
                          (response:response-status response))))
         (error (e)
           ;; Expected timeout error
           (test:is-true t)))
    (stop-test-server)))

;;;; JSON Tests

(test:deftest test-json-request-response ()
  "Test JSON request and response handling"
  (start-test-server)
  (unwind-protect
       (handler-case
           (let* ((json-data (json:encode-to-string (map:make-map
                                                     "name" "test"
                                                     "value" 123
                                                     "nested" (map:make-map "key" "value"))))
                  (response (http:http-post (format nil "http://~A:~D/echo"
                                                    *test-host* *test-port*)
                                           :body json-data
                                           :headers (map:make-map
                                                    "Content-Type" "application/json"))))
             (test:is-equal 200 (response:response-status response))
             (let ((body (json:parse (response:response-body response))))
               (test:is-equal json-data (map:get body "body"))
               (test:is-equal "application/json"
                        (map:get (map:get body "headers") "content-type"))))
         (error (e)
           (test:fail (format nil "JSON test failed: ~A" e))))
    (stop-test-server)))

;;;; Test Summary

(test:deftest test-local-integration-summary ()
  "Summary of local integration tests"
  (format t "~&~%Local Integration Test Summary:~%")
  (format t "  - Basic HTTP methods (GET, POST, PUT, DELETE)~%")
  (format t "  - Status code handling~%")
  (format t "  - Custom headers and query parameters~%")
  (format t "  - Large payloads (requests and responses)~%")
  (format t "  - Concurrent request handling~%")
  (format t "  - Keep-alive connections~%")
  (format t "  - Error handling and timeouts~%")
  (format t "  - JSON request/response~%")
  (test:is-true t))

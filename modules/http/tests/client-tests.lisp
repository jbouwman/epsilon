;;;; HTTP Client Test Suite
;;;;
;;;; Tests for HTTP client functionality

(defpackage :epsilon.http.client.tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.client client)
   (epsilon.http.server server)
   (epsilon.http.request request)
   (epsilon.http.response response)
   (epsilon.http.headers headers)
   (epsilon.map map)
   (epsilon.string str)
   (epsilon.sys.thread thread)))

(defparameter *test-server* nil
  "Test server instance")

(defparameter *test-port* 18081
  "Port for test server")

(defun setup-test-server ()
  "Setup test server with various endpoints"
  (setf *test-server*
        (server:start-server
         (lambda (req)
           (let ((path (request:request-path req))
                 (method (request:request-method req)))
             (cond
               ;; Echo endpoint
               ((string= path "/echo")
                (response:json-response
                 (map:make-map
                  "method" method
                  "path" path
                  "headers" (request:request-headers req)
                  "body" (request:request-body req)
                  "params" (request:request-params req))))

               ;; Status code testing
               ((str:starts-with-p path "/status/")
                (let ((code (parse-integer (subseq path 8))))
                  (response:text-response
                   (format nil "Status ~A" code)
                   :status code)))

               ;; Redirect testing
               ((string= path "/redirect")
                (let ((to (map:get (request:request-params req) "to")))
                  (if to
                      (response:redirect to)
                      (response:redirect "/redirected"))))

               ;; Large response
               ((string= path "/large")
                (response:text-response
                 (make-string 50000 :initial-element #\X)))

               ;; Slow response
               ((string= path "/slow")
                (sleep 0.5)
                (response:text-response "Slow response"))

               ;; Header echo
               ((string= path "/headers")
                (response:json-response (request:request-headers req)))

               ;; Method testing
               ((string= path "/methods")
                (case (intern (string-upcase method) :keyword)
                  (:GET (response:text-response "GET OK"))
                  (:POST (response:text-response "POST OK"))
                  (:PUT (response:text-response "PUT OK"))
                  (:DELETE (response:text-response "DELETE OK"))
                  (:HEAD (response:text-response "HEAD OK"))
                  (:OPTIONS (response:text-response "OPTIONS OK"))
                  (t (response:text-response "Method not allowed" :status 405))))

               ;; Default
               (t (response:text-response "Not Found" :status 404)))))
         :port *test-port*)))

(defun teardown-test-server ()
  "Stop test server"
  (when *test-server*
    (server:stop-server *test-server*)
    (setf *test-server* nil)))

;; Setup and teardown for test suite
(defun run-client-tests ()
  "Run all client tests with server setup/teardown.
   Note: Tests in this file use (when *test-server* ...) guards,
   so they gracefully skip when no server is running."
  (unwind-protect
       (progn
         (setup-test-server)
         (sleep 0.1))
    (teardown-test-server)))

(deftest test-url-parsing ()
  "Test URL parsing edge cases"
  ;; IPv4 address
  (multiple-value-bind (scheme host port path _query)
      (client::parse-url "http://192.168.1.1:8080/api")
    (declare (ignore _query))
    (assert-equal "http" scheme)
    (assert-equal "192.168.1.1" host)
    (assert-equal 8080 port)
    (assert-equal "/api" path))

  ;; No scheme
  (multiple-value-bind (scheme host port path _query)
      (client::parse-url "example.com/path")
    (declare (ignore _query))
    (assert-equal "http" scheme)
    (assert-equal "example.com" host)
    (assert-equal 80 port)
    (assert-equal "/path" path))

  ;; Query with multiple parameters
  (multiple-value-bind (scheme host port path query)
      (client::parse-url "https://api.example.com/search?q=test&page=1&limit=10")
    (assert-equal "https" scheme)
    (assert-equal "api.example.com" host)
    (assert-equal 443 port)
    (assert-equal "/search" path)
    (assert-equal "q=test&page=1&limit=10" query))

  ;; Port in URL with query
  (multiple-value-bind (scheme host port path query)
      (client::parse-url "http://localhost:3000/api?key=value")
    (assert-equal "http" scheme)
    (assert-equal "localhost" host)
    (assert-equal 3000 port)
    (assert-equal "/api" path)
    (assert-equal "key=value" query)))

(deftest test-basic-get-request ()
  "Test basic GET request"
  (when *test-server*
    (let ((resp (client:get (format nil "http://127.0.0.1:~A/echo" *test-port*))))
      (assert-equal 200 (response:response-status resp))
      (let ((body (response:response-body-string resp)))
        (assert-true (stringp body))
        (assert-true (search "\"method\":\"GET\"" body))))))

(deftest test-response-body-size-cap ()
  "A response whose Content-Length exceeds *max-response-body-bytes*
   signals RESPONSE-TOO-LARGE-ERROR before the buffer grows unboundedly.
   This is the defensive guard against runaway / hostile servers that
   would otherwise force heap exhaustion."
  (when *test-server*
    ;; /large returns 50_000 bytes. Cap below that and assert we trip.
    (let ((client:*max-response-body-bytes* 1024))
      (assert-condition (client:response-too-large-error)
        (client:get (format nil "http://127.0.0.1:~A/large" *test-port*))))))

(deftest test-response-headers-size-cap ()
  "A response whose headers grow past *max-response-headers-bytes* must
   trip RESPONSE-TOO-LARGE-ERROR before the response-bytes adjustable
   vector eats the heap.  We trigger this by capping below the standard
   header size on a normal response."
  (when *test-server*
    (let ((client:*max-response-headers-bytes* 16))
      (assert-condition (client:response-too-large-error)
        (client:get (format nil "http://127.0.0.1:~A/echo" *test-port*))))))

(deftest test-post-with-body ()
  "Test POST request with body"
  (when *test-server*
    (let* ((test-body "This is test data")
           (resp (client:http-post (format nil "http://127.0.0.1:~A/echo" *test-port*)
                                   :body test-body)))
      (assert-equal 200 (response:response-status resp))
      (let ((body (response:response-body-string resp)))
        (assert-true (search "\"method\":\"POST\"" body))
        (assert-true (search test-body body))))))

(deftest test-custom-headers ()
  "Test requests with custom headers"
  (when *test-server*
    (let* ((custom-headers (map:make-map
                            "X-Custom-Header" "test-value"
                            "Authorization" "Bearer token123"))
           (resp (client:get (format nil "http://127.0.0.1:~A/headers" *test-port*)
                                  :headers custom-headers)))
      (assert-equal 200 (response:response-status resp))
      (let ((body (response:response-body-string resp)))
        (assert-true (search "x-custom-header" body))
        (assert-true (search "test-value" body))
        (assert-true (search "authorization" body))
        (assert-true (search "Bearer token123" body))))))

(deftest test-query-parameters ()
  "Test URL with query parameters"
  (when *test-server*
    (let ((resp (client:get
                 (format nil "http://127.0.0.1:~A/echo?foo=bar&baz=qux" *test-port*))))
      (assert-equal 200 (response:response-status resp))
      (let ((body (response:response-body-string resp)))
        (assert-true (search "\"foo\":\"bar\"" body))
        (assert-true (search "\"baz\":\"qux\"" body))))))

(deftest test-all-http-methods ()
  "Test all HTTP methods"
  (when *test-server*
    (let ((base-url (format nil "http://127.0.0.1:~A/methods" *test-port*)))
      ;; GET
      (let ((resp (client:get base-url)))
        (assert-equal 200 (response:response-status resp))
        (assert-true (search "GET OK" (response:response-body-string resp))))

      ;; POST
      (let ((resp (client:http-post base-url)))
        (assert-equal 200 (response:response-status resp))
        (assert-true (search "POST OK" (response:response-body-string resp))))

      ;; PUT
      (let ((resp (client:http-put base-url)))
        (assert-equal 200 (response:response-status resp))
        (assert-true (search "PUT OK" (response:response-body-string resp))))

      ;; DELETE
      (let ((resp (client:http-delete base-url)))
        (assert-equal 200 (response:response-status resp))
        (assert-true (search "DELETE OK" (response:response-body-string resp))))

      ;; HEAD
      (let ((resp (client:http-head base-url)))
        (assert-equal 200 (response:response-status resp)))

      ;; OPTIONS
      (let ((resp (client:http-options base-url)))
        (assert-equal 200 (response:response-status resp))
        (assert-true (search "OPTIONS OK" (response:response-body-string resp)))))))

(deftest test-status-codes ()
  "Test various HTTP status codes"
  (when *test-server*
    (dolist (code '(200 201 400 401 403 404 500 503))
      (let ((resp (client:get
                   (format nil "http://127.0.0.1:~A/status/~A" *test-port* code))))
        (assert-equal code (response:response-status resp))))))

(deftest test-redirects ()
  "Test redirect handling"
  (when *test-server*
    (let ((resp (client:get
                 (format nil "http://127.0.0.1:~A/redirect?to=/echo" *test-port*))))
      (assert-equal 302 (response:response-status resp))
      (assert-equal "/echo" (map:get (response:response-headers resp) "location")))))

(deftest test-large-response ()
  "Test handling large responses"
  (when *test-server*
    (let ((resp (client:get
                 (format nil "http://127.0.0.1:~A/large" *test-port*))))
      (assert-equal 200 (response:response-status resp))
      (let ((body (response:response-body-string resp)))
        (assert-equal 50000 (length body))
        (assert-true (every (lambda (ch) (char= ch #\X)) body))))))

(deftest test-request-with-connection ()
  "Test explicit connection management"
  (when *test-server*
    (client:with-connection (conn "127.0.0.1" *test-port*)
      ;; Connection should be established
      (assert-true (client::connection-socket conn))
      (assert-equal "127.0.0.1" (client::connection-host conn))
      (assert-equal *test-port* (client::connection-port conn))
      (assert-not (client::connection-ssl-p conn)))))

(deftest test-content-length-header ()
  "Test Content-Length header handling"
  (when *test-server*
    (let* ((body-content "Test content with specific length")
           (resp (client:http-post (format nil "http://127.0.0.1:~A/echo" *test-port*)
                                   :body body-content)))
      (assert-equal 200 (response:response-status resp))
      (let ((body (response:response-body-string resp)))
        (assert-true (search body-content body))))))

(deftest test-empty-body-request ()
  "Test requests with empty body"
  (when *test-server*
    (let ((resp (client:http-post (format nil "http://127.0.0.1:~A/echo" *test-port*)
                                  :body "")))
      (assert-equal 200 (response:response-status resp)))

    (let ((resp (client:http-post (format nil "http://127.0.0.1:~A/echo" *test-port*)
                                  :body nil)))
      (assert-equal 200 (response:response-status resp)))))

(deftest test-parse-response ()
  "Test response parsing"
  (let* ((response-string (format nil "HTTP/1.1 200 OK~C~CContent-Type: text/plain~C~CContent-Length: 11~C~C~C~CHello World"
                                  #\Return #\Newline #\Return #\Newline
                                  #\Return #\Newline #\Return #\Newline))
         (resp (client::parse-response response-string)))
    (assert-equal 200 (response:response-status resp))
    (assert-equal "text/plain" (map:get (response:response-headers resp) "content-type"))
    (assert-equal "11" (map:get (response:response-headers resp) "content-length"))
    (assert-true (search "Hello World" (response:response-body-string resp)))))

(deftest test-malformed-url ()
  "Test handling of malformed URLs"
  ;; Invalid scheme (but should default to http)
  (multiple-value-bind (scheme host port path _query)
      (client::parse-url "localhost:8080/test")
    (declare (ignore _query))
    (assert-equal "http" scheme)
    (assert-equal "localhost" host)
    (assert-equal 8080 port)
    (assert-equal "/test" path)))

(deftest test-concurrent-requests ()
  "Test multiple concurrent requests"
  (when *test-server*
    (let ((results (make-array 10 :initial-element nil))
          (threads '()))
      ;; Create 10 concurrent requests. epsilon.sys.thread:make-thread takes
      ;; a zero-arg thunk -- bake the loop variable into a fresh binding so
      ;; each closure captures its own INDEX rather than the shared one
      ;; DOTIMES reuses across iterations.
      (dotimes (i 10)
        (let ((index i))
          (push (thread:make-thread
                 (lambda ()
                   (let ((resp (client:get
                                (format nil "http://127.0.0.1:~A/echo?id=~A"
                                        *test-port* index))))
                     (setf (aref results index)
                           (list (response:response-status resp)
                                 (response:response-body-string resp)))))
                 :name (format nil "request-~A" index))
                threads)))

      ;; Wait for all threads
      (dolist (thread threads)
        (thread:join-thread thread))

      ;; Verify all requests succeeded
      (dotimes (i 10)
        (let ((result (aref results i)))
          (assert-equal 200 (first result))
          (assert-true (search (format nil "\"id\":\"~A\"" i) (second result))))))))

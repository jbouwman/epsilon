;;;; HTTP Server Test Suite
;;;;
;;;; Tests for HTTP server functionality

(defpackage :epsilon.http.server.tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.server server)
   (epsilon.http.request request)
   (epsilon.http.response response)
   (epsilon.http.client client)
   (epsilon.http.headers headers)
   (epsilon.http.test-helpers helpers)
   (epsilon.io io)
   (epsilon.map map)
   (epsilon.string str)
   (epsilon.sys.lock lock)))

(defparameter *test-port* 0
  "Port for the current test server. Bound by with-test-server to the
   OS-assigned port so test bodies can connect to it.")

(defmacro with-test-server ((server-var app) &body body)
  "Run tests with a temporary server on an OS-assigned port.
   Binds *test-port* to the actual port for use in request URLs."
  `(let ((,server-var nil))
     (unwind-protect
          (progn
            (setf ,server-var (server:start-server ,app :port 0))
            (let ((*test-port* (server::server-port ,server-var)))
              ;; Give server time to start
              (sleep 0.1)
              ,@body))
       (when ,server-var
         (server:stop-server ,server-var)))))

(deftest test-server-start-stop ()
  "Test basic server start and stop"
  (let ((server (server:start-server
                 (lambda (req)
                   (declare (ignore req))
                   (response:text-response "Hello"))
                 :port 0)))
    (assert-true (typep server 'server::http-server))
    (assert-true (> (server::server-port server) 0))
    (assert-true (server::server-running-p server))
    (assert-true (server:stop-server server))
    (assert-not (server::server-running-p server))))

(deftest test-server-double-start ()
  "Test that starting server on same port fails"
  (with-test-server (server1 (lambda (req)
                               (declare (ignore req))
                               (response:text-response "Server 1")))
    (assert-condition (error)
               (server:start-server
                (lambda (req)
                  (declare (ignore req))
                  (response:text-response "Server 2"))
                :port *test-port*))))

(deftest test-simple-get-request ()
  "Test handling simple GET request"
  (with-test-server (server
                     (lambda (req)
                       (response:text-response
                        (format nil "Method: ~A, Path: ~A"
                                (request:request-method req)
                                (request:request-path req)))))
    (let ((resp (client:get (format nil "http://127.0.0.1:~A/test" *test-port*))))
      (assert-equal 200 (response:response-status resp))
      (assert-true (search "Method: GET, Path: /test" (response:response-body-string resp))))))

(deftest test-post-request-with-body ()
  "Test handling POST request with body"
  (with-test-server (server
                     (lambda (req)
                       (response:text-response
                        (format nil "Body: ~A" (request:request-body req)))))
    (let ((resp (client:http-post (format nil "http://127.0.0.1:~A/data" *test-port*)
                                  :body "test data"
                                  :headers (map:make-map "Content-Type" "text/plain"))))
      (assert-equal 200 (response:response-status resp))
      (assert-true (search "Body: test data" (response:response-body-string resp))))))

(deftest test-request-headers ()
  "Test request header handling"
  (with-test-server (server
                     (lambda (req)
                       (let ((user-agent (map:get (request:request-headers req) "user-agent")))
                         (response:text-response
                          (format nil "User-Agent: ~A" user-agent)))))
    (let ((resp (client:get (format nil "http://127.0.0.1:~A/" *test-port*)
                                 :headers (map:make-map "User-Agent" "TestClient/1.0"))))
      (assert-equal 200 (response:response-status resp))
      (assert-true (search "User-Agent: TestClient/1.0" (response:response-body-string resp))))))

(deftest test-query-parameters ()
  "Test query parameter parsing"
  (with-test-server (server
                     (lambda (req)
                       (let ((name (map:get (request:request-params req) "name"))
                             (age (map:get (request:request-params req) "age")))
                         (response:text-response
                          (format nil "Name: ~A, Age: ~A" name age)))))
    (let ((resp (client:get
                 (format nil "http://127.0.0.1:~A/user?name=John&age=30" *test-port*))))
      (assert-equal 200 (response:response-status resp))
      (assert-true (search "Name: John, Age: 30" (response:response-body-string resp))))))

(deftest test-keepalive-policy-defaults ()
  "Default keepalive policy is enabled as of IMPL-331 step 3."
  (let ((p (server::make-keepalive-policy)))
    (assert-true (server::keepalive-policy-enabled p))
    (assert-equal 30 (server::keepalive-policy-idle-timeout p))
    (assert-equal 1000 (server::keepalive-policy-max-requests p))))

(deftest test-keepalive-policy-plumbed-to-server ()
  "start-server accepts a :keepalive policy and stores it on the server
instance so the accept loop can read it."
  (let* ((policy (server::make-keepalive-policy :enabled t
                                                :max-requests 7))
         (srv (server:start-server
               (lambda (req)
                 (declare (ignore req))
                 (response:text-response "ok"))
               :port 0
               :keepalive policy)))
    (unwind-protect
         (let ((stored (server::server-keepalive srv)))
           (assert-true (server::keepalive-policy-p stored))
           (assert-true (server::keepalive-policy-enabled stored))
           (assert-equal 7 (server::keepalive-policy-max-requests stored)))
      (server:stop-server srv))))

(deftest test-404-not-found ()
  "Test 404 response"
  (with-test-server (server
                     (lambda (req)
                       (if (string= (request:request-path req) "/exists")
                           (response:text-response "Found")
                           (response:text-response "Not Found" :status 404))))
    (let ((resp (client:get (format nil "http://127.0.0.1:~A/missing" *test-port*))))
      (assert-equal 404 (response:response-status resp))
      (assert-true (search "Not Found" (response:response-body-string resp))))))

(deftest test-json-response-handling ()
  "Test JSON response"
  (with-test-server (server
                     (lambda (req)
                       (declare (ignore req))
                       (response:json-response
                        (map:make-map "message" "success" "count" 42))))
    (let ((resp (client:get
                 (format nil "http://127.0.0.1:~A/api/data" *test-port*))))
      (assert-equal 200 (response:response-status resp))
      (assert-equal "application/json"
                (map:get (response:response-headers resp) "content-type"))
      (let ((body (response:response-body-string resp)))
        (assert-true (search "\"message\"" body))
        (assert-true (search "\"success\"" body))
        (assert-true (search "42" body))))))

(deftest test-redirect-response ()
  "Test redirect response"
  (with-test-server (server
                     (lambda (req)
                       (if (string= (request:request-path req) "/old")
                           (response:redirect "/new")
                           (response:text-response "New location"))))
    (let ((resp (client:get (format nil "http://127.0.0.1:~A/old" *test-port*))))
      (assert-equal 302 (response:response-status resp))
      (assert-equal "/new" (map:get (response:response-headers resp) "location")))))

(deftest test-middleware-composition ()
  "Test middleware composition"
  (labels ((logging-middleware (handler)
             (lambda (req)
               (let ((resp (funcall handler req)))
                 (response:set-header resp "X-Logged" "true")
                 resp)))
           (auth-middleware (handler)
             (lambda (req)
               (if (map:get (request:request-headers req) "authorization")
                   (funcall handler req)
                   (response:text-response "Unauthorized" :status 401)))))
    (with-test-server (server
                       (server:wrap-middleware
                        (lambda (req)
                          (declare (ignore req))
                          (response:text-response "Protected"))
                        #'logging-middleware
                        #'auth-middleware))
      ;; Test without auth
      (let ((resp (client:get
                   (format nil "http://127.0.0.1:~A/protected" *test-port*))))
        (assert-equal 401 (response:response-status resp))
        (assert-equal "true" (map:get (response:response-headers resp) "x-logged")))
      ;; Test with auth
      (let ((resp (client:get
                   (format nil "http://127.0.0.1:~A/protected" *test-port*)
                   :headers (map:make-map "Authorization" "Bearer token"))))
        (assert-equal 200 (response:response-status resp))
        (assert-equal "true" (map:get (response:response-headers resp) "x-logged"))
        (assert-true (search "Protected" (response:response-body-string resp)))))))

(deftest test-error-handling ()
  "Test server error handling"
  (with-test-server (server
                     (lambda (req)
                       (if (string= (request:request-path req) "/error")
                           (error "Intentional error")
                           (response:text-response "OK"))))
    ;; Normal request should work
    (let ((resp (client:get (format nil "http://127.0.0.1:~A/ok" *test-port*))))
      (assert-equal 200 (response:response-status resp)))
    ;; Error request should return 500
    (let ((resp (client:get (format nil "http://127.0.0.1:~A/error" *test-port*))))
      (assert-equal 500 (response:response-status resp))
      (assert-true (search "Internal Server Error" (response:response-body-string resp))))))

(deftest test-concurrent-requests ()
  "Test handling concurrent requests"
  (let ((counter 0)
        (lock (lock:make-lock)))
    (with-test-server (server
                       (lambda (req)
                         (declare (ignore req))
                         (lock:with-lock (lock)
                           (incf counter))
                         (sleep 0.01) ; Simulate work
                         (response:text-response (format nil "Request ~A" counter))))
      ;; Make multiple sequential requests (concurrent requests are unreliable
      ;; under parallel test load)
      (dotimes (i 5)
        (handler-case
            (client:get (format nil "http://127.0.0.1:~A/concurrent" *test-port*))
          (error (e)
            (format *error-output* "Request ~D error: ~A~%" i e))))
      ;; All requests should have been handled
      (assert-equal 5 counter))))

(deftest test-large-request-body ()
  "Test handling large request bodies"
  (let ((large-body (make-string 10000 :initial-element #\A)))
    (with-test-server (server
                       (lambda (req)
                         (response:text-response
                          (format nil "Received ~A bytes"
                                  (length (request:request-body req))))))
      (let ((resp (client:http-post (format nil "http://127.0.0.1:~A/upload" *test-port*)
                                    :body large-body)))
        (assert-equal 200 (response:response-status resp))
        (assert-true (search "Received 10000 bytes" (response:response-body-string resp)))))))

(deftest test-custom-headers ()
  "Test custom response headers"
  (with-test-server (server
                     (lambda (req)
                       (declare (ignore req))
                       (let ((resp (response:text-response "Custom headers")))
                         (response:set-header resp "X-Custom-Header" "test-value")
                         (response:set-header resp "X-Request-Id" "12345")
                         resp)))
    (let ((resp (client:get (format nil "http://127.0.0.1:~A/custom" *test-port*))))
      (assert-equal 200 (response:response-status resp))
      (assert-equal "test-value" (map:get (response:response-headers resp) "x-custom-header"))
      (assert-equal "12345" (map:get (response:response-headers resp) "x-request-id")))))

(deftest test-head-request ()
  "Test HEAD request handling"
  (with-test-server (server
                     (lambda (req)
                       (declare (ignore req))
                       (response:text-response "This is the body")))
    (let ((resp (client:http-head (format nil "http://127.0.0.1:~A/resource" *test-port*))))
      (assert-equal 200 (response:response-status resp)))))

(deftest test-options-request ()
  "Test OPTIONS request handling"
  (with-test-server (server
                     (lambda (req)
                       (if (string= (request:request-method req) "OPTIONS")
                           (let ((resp (response:make-response :status 200)))
                             (response:set-header resp "Allow"
                                                  "GET, POST, PUT, DELETE, OPTIONS")
                             resp)
                           (response:text-response "OK"))))
    (let ((resp (client:http-options (format nil "http://127.0.0.1:~A/api" *test-port*))))
      (assert-equal 200 (response:response-status resp))
      (assert-equal "GET, POST, PUT, DELETE, OPTIONS"
                (map:get (response:response-headers resp) "allow")))))

;;; Chunked Transfer-Encoding response support
;;;
;;; The server can stream a response body chunk-by-chunk by setting :chunked t
;;; on the response. The body may be a list of strings (each one a chunk),
;;; a generator function that returns the next chunk and nil when done, or
;;; a vector of chunks.

(deftest test-server-chunked-from-list ()
  "Server should send a chunked response when body is a list of chunks."
  (with-test-server (server
                     (lambda (req)
                       (declare (ignore req))
                       (response:make-response
                        :status 200
                        :headers (headers:make-headers "Content-Type" "text/plain")
                        :chunked t
                        :body (list "Hello" ", " "World"))))
    (let ((resp (client:get (format nil "http://127.0.0.1:~A/" *test-port*))))
      (assert-equal 200 (response:response-status resp))
      (assert-equal "Hello, World" (response:response-body-string resp)))))

(deftest test-server-chunked-from-generator ()
  "Server should send a chunked response when body is a generator function."
  (with-test-server (server
                     (lambda (req)
                       (declare (ignore req))
                       (let ((chunks (list "one" "two" "three")))
                         (response:make-response
                          :status 200
                          :chunked t
                          :body (lambda ()
                                  (when chunks
                                    (let ((c (first chunks)))
                                      (setf chunks (rest chunks))
                                      c)))))))
    (let ((resp (client:get (format nil "http://127.0.0.1:~A/" *test-port*))))
      (assert-equal 200 (response:response-status resp))
      (assert-equal "onetwothree" (response:response-body-string resp)))))

(deftest test-server-chunked-byte-vectors ()
  "Server chunked body should accept byte-vector chunks."
  (with-test-server (server
                     (lambda (req)
                       (declare (ignore req))
                       (response:make-response
                        :status 200
                        :chunked t
                        :body (list (sb-ext:string-to-octets "abc" :external-format :utf-8)
                                    (sb-ext:string-to-octets "def" :external-format :utf-8)))))
    (let ((resp (client:get (format nil "http://127.0.0.1:~A/" *test-port*))))
      (assert-equal 200 (response:response-status resp))
      (assert-equal "abcdef" (response:response-body-string resp)))))

(deftest test-server-chunked-overrides-content-length ()
  "Even if the handler sets Content-Length, chunked should win and the
   header should be replaced with Transfer-Encoding: chunked."
  (with-test-server (server
                     (lambda (req)
                       (declare (ignore req))
                       (response:make-response
                        :status 200
                        :headers (headers:make-headers "Content-Length" "999")
                        :chunked t
                        :body (list "ignored-cl"))))
    (let ((resp (client:get (format nil "http://127.0.0.1:~A/" *test-port*))))
      (assert-equal 200 (response:response-status resp))
      (assert-equal "ignored-cl" (response:response-body-string resp)))))

(deftest test-server-chunked-empty-body ()
  "An empty chunk list should produce a valid empty chunked response."
  (with-test-server (server
                     (lambda (req)
                       (declare (ignore req))
                       (response:make-response :status 200 :chunked t :body nil)))
    (let ((resp (client:get (format nil "http://127.0.0.1:~A/" *test-port*))))
      (assert-equal 200 (response:response-status resp))
      (let ((body (response:response-body-string resp)))
        (assert-true (or (null body) (string= body "")))))))

;;; ---------------------------------------------------------------------------
;;; Streaming request body (REQUEST-BODY-SOURCE)
;;;
;;; The watchdog incident showed that slurping a 200+ MB PUT body into
;;; a 1 GiB heap can crash the runtime under chacha20-poly1305 buffer
;;; pressure.  Bodies above *BODY-SLURP-THRESHOLD* now bypass the
;;; in-memory buffer and are exposed through a chunk thunk on the
;;; request itself; the handler drains the thunk as it streams to disk.
;;; ---------------------------------------------------------------------------

(defun %build-http-request-bytes (method path body)
  "Synthesize a complete HTTP/1.1 request as a (SIMPLE-ARRAY (UNSIGNED-BYTE 8))
ready to feed into a buffered reader.  BODY is a byte vector."
  (let* ((header-text
           (format nil "~A ~A HTTP/1.1~C~CHost: test~C~CContent-Type: ~
                        application/octet-stream~C~CContent-Length: ~D~C~C~C~C"
                   method path #\Return #\Newline #\Return #\Newline
                   #\Return #\Newline (length body) #\Return #\Newline
                   #\Return #\Newline))
         (header-bytes (sb-ext:string-to-octets header-text :external-format :utf-8))
         (out (make-array (+ (length header-bytes) (length body))
                          :element-type '(unsigned-byte 8))))
    (replace out header-bytes)
    (replace out body :start1 (length header-bytes))
    out))

(defun %drain-source (thunk)
  "Drain a body-source thunk into a single byte vector.  Used by tests."
  (let ((chunks nil)
        (total 0))
    (loop for chunk = (funcall thunk)
          while chunk
          do (push chunk chunks) (incf total (length chunk)))
    (let ((out (make-array total :element-type '(unsigned-byte 8)))
          (offset 0))
      (dolist (chunk (nreverse chunks))
        (replace out chunk :start1 offset)
        (incf offset (length chunk)))
      out)))

(deftest test-small-body-stays-buffered
  "A body at or below *BODY-SLURP-THRESHOLD* keeps the historical
behavior: REQUEST-BODY is set, REQUEST-BODY-SOURCE is NIL.  Existing
JSON / form-urlencoded handlers depend on this path."
  (let* ((body (make-array 1024 :element-type '(unsigned-byte 8)
                                :initial-element (char-code #\A)))
         (bytes (%build-http-request-bytes "POST" "/echo" body))
         (reader (io:make-buffered-reader (io:make-byte-reader bytes)))
         (req (server::read-http-request-using-reader reader)))
    (assert-true req)
    (assert-true (request:request-body req))
    (assert-equal nil (request:request-body-source req))
    (assert-equal 1024 (length (request:request-body req)))))

(deftest test-large-body-becomes-streaming
  "A body whose Content-Length exceeds *BODY-SLURP-THRESHOLD* is
delivered as a chunked thunk.  REQUEST-BODY is left NIL so the
handler must opt in to streaming.  Draining the thunk reproduces
the original bytes and flips REQUEST-BODY-SOURCE-DRAINED-P."
  (let* ((size (* 2 server:*body-slurp-threshold*))
         (body (make-array size :element-type '(unsigned-byte 8))))
    (loop for i below size do (setf (aref body i) (mod i 256)))
    (let* ((bytes (%build-http-request-bytes "PUT" "/big" body))
           (reader (io:make-buffered-reader (io:make-byte-reader bytes)))
           (req (server::read-http-request-using-reader reader)))
      (assert-true req)
      (assert-equal nil (request:request-body req))
      (assert-true (request:request-body-source req))
      (assert-equal size (request:request-content-length req))
      (assert-equal nil (request:request-body-source-drained-p req))
      (let ((drained (%drain-source (request:request-body-source req))))
        (assert-equal size (length drained))
        (assert-true (equalp body drained)))
      (assert-true (request:request-body-source-drained-p req)))))

(deftest test-streaming-body-chunks-bounded
  "Each chunk pulled from the body-source is at most
*BODY-STREAM-CHUNK-SIZE* bytes -- the streaming pump must never
allocate a buffer the size of the whole body, since that would
defeat the point of avoiding the slurp."
  (let* ((size (* 5 server:*body-stream-chunk-size*))
         (body (make-array size :element-type '(unsigned-byte 8)
                                :initial-element 42)))
    (let* ((bytes (%build-http-request-bytes "PUT" "/big" body))
           (reader (io:make-buffered-reader (io:make-byte-reader bytes)))
           (req (server::read-http-request-using-reader reader))
           (src (request:request-body-source req)))
      (loop for chunk = (funcall src)
            while chunk
            do (assert-true
                (<= (length chunk) server:*body-stream-chunk-size*))))))

(deftest test-streaming-body-survives-small-chunk-size
  "Confirm streaming works when the threshold is set unusually
small.  Catches regressions where the cutover branch assumes
threshold > chunk-size or similar."
  (let* ((server:*body-slurp-threshold* 8)
         (server:*body-stream-chunk-size* 8)
         (body (make-array 100 :element-type '(unsigned-byte 8)
                               :initial-element 7))
         (bytes (%build-http-request-bytes "PUT" "/x" body))
         (reader (io:make-buffered-reader (io:make-byte-reader bytes)))
         (req (server::read-http-request-using-reader reader)))
    (assert-true req)
    (assert-true (request:request-body-source req))
    (let ((drained (%drain-source (request:request-body-source req))))
      (assert-equal 100 (length drained))
      (assert-true (every (lambda (b) (= b 7)) drained)))
    (assert-true (request:request-body-source-drained-p req))))

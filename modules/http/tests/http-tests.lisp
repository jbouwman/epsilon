;;;; HTTP Tests

(defpackage epsilon.http.tests
  (:use :cl :epsilon.syntax :epsilon.test)
  (:import (epsilon.http.client client)
            (epsilon.http.server server)
            (epsilon.http.request request)
            (epsilon.http.response response)
            (epsilon.http.headers headers)
            (epsilon.map map)
            (epsilon.string str)))

(deftest test-parse-url ()
  "Test URL parsing functionality"
  (multiple-value-bind (scheme host port path query)
      (client::parse-url "http://example.com:8080/path?foo=bar")
    (assert-equal "http" scheme)
    (assert-equal "example.com" host)
    (assert-equal 8080 port)
    (assert-equal "/path" path)
    (assert-equal "foo=bar" query))

  (multiple-value-bind (scheme host port path query)
      (client::parse-url "https://example.com/")
    (assert-equal "https" scheme)
    (assert-equal "example.com" host)
    (assert-equal 443 port)
    (assert-equal "/" path)
    (assert-equal nil query))

  ;; Test additional URL formats
  (multiple-value-bind (scheme host port path query)
      (client::parse-url "https://api.example.com:8080/users/123")
    (assert-equal "https" scheme)
    (assert-equal "api.example.com" host)
    (assert-equal 8080 port)
    (assert-equal "/users/123" path)
    (assert-equal nil query))

  (multiple-value-bind (scheme host port path query)
      (client::parse-url "http://localhost:3000/")
    (assert-equal "http" scheme)
    (assert-equal "localhost" host)
    (assert-equal 3000 port)
    (assert-equal "/" path)
    (assert-equal nil query)))

(deftest test-request-creation ()
  "Test HTTP request object creation"
  (let ((req (request:make-request "GET" "/test"
                                  :headers (map:make-map "Host" "example.com")
                                  :params (map:make-map "q" "search"))))
    (assert-equal "GET" (request:request-method req))
    (assert-equal "/test" (request:request-path req))
    (assert-equal "example.com" (map:get (request:request-headers req) "Host"))
    (assert-equal "search" (map:get (request:request-params req) "q"))))

(deftest test-response-creation ()
  "Test HTTP response object creation"
  (let ((resp (response:make-response :status 200
                                     :headers (map:make-map "Content-Type" "text/html")
                                     :body "Hello World")))
    (assert-equal 200 (response:response-status resp))
    (assert-equal "text/html" (map:get (response:response-headers resp) "Content-Type"))
    (assert-equal "Hello World" (response:response-body resp))))

(deftest test-json-response ()
  "Test JSON response creation"
  (let* ((data (map:make-map "message" "hello"))
         (resp (response:json-response data :status 201)))
    (assert-equal 201 (response:response-status resp))
    (assert-equal "application/json" (map:get (response:response-headers resp) "Content-Type"))
    (assert-true (stringp (response:response-body resp)))))

(deftest test-query-string-parsing ()
  "Test query string parsing"
  (let ((params (request:parse-query-string "foo=bar&baz=qux&empty=")))
    (assert-equal "bar" (map:get params "foo"))
    (assert-equal "qux" (map:get params "baz"))
    (assert-equal "" (map:get params "empty")))

  ;; Test empty query string
  (let ((params (request:parse-query-string "")))
    (assert-equal map:+empty+ params))

  ;; Test nil query string
  (let ((params (request:parse-query-string nil)))
    (assert-equal map:+empty+ params)))

(deftest test-url-decoding ()
  "Test URL decoding"
  (assert-equal "hello world" (request::url-decode "hello+world"))
  (assert-equal "hello world" (request::url-decode "hello%20world"))
  (assert-equal "test@example.com" (request::url-decode "test%40example.com"))

  ;; Test additional URL decoding cases
  (assert-equal "simple" (request::url-decode "simple"))
  (assert-equal "100%" (request::url-decode "100%25")))

(deftest test-status-text ()
  "Test HTTP status text conversion"
  (assert-equal "OK" (response::status-text 200))
  (assert-equal "Created" (response::status-text 201))
  (assert-equal "No Content" (response::status-text 204))
  (assert-equal "Bad Request" (response::status-text 400))
  (assert-equal "Unauthorized" (response::status-text 401))
  (assert-equal "Forbidden" (response::status-text 403))
  (assert-equal "Not Found" (response::status-text 404))
  (assert-equal "Method Not Allowed" (response::status-text 405))
  (assert-equal "Internal Server Error" (response::status-text 500))
  (assert-equal "Unknown" (response::status-text 999)))

(deftest test-response-to-string ()
  "Test HTTP response string formatting"
  (let* ((resp (response:make-response :status 200
                                      :headers (map:make-map "Content-Type" "text/plain")
                                      :body "Hello"))
         (response-string (response:response-to-string resp)))
    (assert-true (search "HTTP/1.1 200 OK" response-string))
    (assert-true (search "Content-Type: text/plain" response-string))
    (assert-true (search "Content-Length: 5" response-string))
    (assert-true (search "Hello" response-string)))

  ;; Test response without body
  (let* ((resp (response:make-response :status 204))
         (response-string (response:response-to-string resp)))
    (assert-true (search "HTTP/1.1 204 No Content" response-string))
    (assert-false (search "Content-Length" response-string))))

(deftest test-html-response ()
  "Test HTML response creation"
  (let ((resp (response:html-response "<h1>Test</h1>" :status 200)))
    (assert-equal 200 (response:response-status resp))
    (assert-equal "text/html; charset=utf-8" (map:get (response:response-headers resp) "Content-Type"))
    (assert-equal "<h1>Test</h1>" (response:response-body resp))))

(deftest test-text-response ()
  "Test plain text response creation"
  (let ((resp (response:text-response "Plain text" :status 200)))
    (assert-equal 200 (response:response-status resp))
    (assert-equal "text/plain; charset=utf-8" (map:get (response:response-headers resp) "Content-Type"))
    (assert-equal "Plain text" (response:response-body resp))))

(deftest test-redirect-response ()
  "Test redirect response creation"
  (let ((resp (response:redirect "/login" :status 302)))
    (assert-equal 302 (response:response-status resp))
    (assert-equal "/login" (map:get (response:response-headers resp) "Location"))))

(deftest test-form-data-parsing ()
  "Test form data parsing"
  (let ((params (request:parse-form-data "name=john&age=30&active=true")))
    (assert-equal "john" (map:get params "name"))
    (assert-equal "30" (map:get params "age"))
    (assert-equal "true" (map:get params "active"))))

(deftest test-http-request-parsing ()
  "Test complete HTTP request parsing"
  (let* ((request-string (format nil "GET /test?q=search HTTP/1.1~C~CHost: example.com~C~CUser-Agent: test~C~C~C~C"
                                        #\Return #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline))
         (req (request:parse-http-request request-string)))
    (assert-equal "GET" (request:request-method req))
    (assert-equal "/test" (request:request-path req))
    (assert-equal "example.com" (map:get (request:request-headers req) "host"))
    (assert-equal "test" (map:get (request:request-headers req) "user-agent"))
    (assert-equal "search" (map:get (request:request-params req) "q"))))

(deftest test-response-body-accessors ()
  "Test typed body accessors for string and byte bodies"
  ;; String body
  (let ((resp (response:make-response :status 200 :body "Hello")))
    (assert-equal "Hello" (response:response-body-string resp))
    (assert-true (typep (response:response-body-bytes resp) '(vector (unsigned-byte 8))))
    (assert-equal 5 (length (response:response-body-bytes resp))))

  ;; Byte body
  (let* ((bytes (make-array 4 :element-type '(unsigned-byte 8)
                              :initial-contents '(72 101 108 108)))
         (resp (response:make-response :status 200 :body bytes)))
    (assert-true (typep (response:response-body resp) '(vector (unsigned-byte 8))))
    (assert-equal "Hell" (response:response-body-string resp))
    (assert-true (equalp bytes (response:response-body-bytes resp))))

  ;; Nil body
  (let ((resp (response:make-response :status 204)))
    (assert-true (null (response:response-body-string resp)))
    (assert-true (null (response:response-body-bytes resp))))

  ;; Binary body with all byte values round-trips through bytes accessor
  (let* ((bytes (make-array 256 :element-type '(unsigned-byte 8)
                                :initial-contents (loop for i from 0 below 256 collect i)))
         (resp (response:make-response :status 200 :body bytes)))
    (assert-true (equalp bytes (response:response-body-bytes resp)))))

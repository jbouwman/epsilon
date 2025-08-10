(defpackage :epsilon.http.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:client #:epsilon.http.client)
   (#:server #:epsilon.http.server)
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response)
   (#:map #:epsilon.map)
   (#:str #:epsilon.string)))

(in-package :epsilon.http.tests)

(deftest test-parse-url ()
  "Test URL parsing functionality"
  (multiple-value-bind (scheme host port path query)
      (client::parse-url "http://example.com:8080/path?foo=bar")
    (is-equal "http" scheme)
    (is-equal "example.com" host)
    (is-equal 8080 port)
    (is-equal "/path" path)
    (is-equal "foo=bar" query))
  
  (multiple-value-bind (scheme host port path query)
      (client::parse-url "https://example.com/")
    (is-equal "https" scheme)
    (is-equal "example.com" host)
    (is-equal 443 port)
    (is-equal "/" path)
    (is-equal nil query))
  
  ;; Test additional URL formats
  (multiple-value-bind (scheme host port path query)
      (client::parse-url "https://api.example.com:8080/users/123")
    (is-equal "https" scheme)
    (is-equal "api.example.com" host)  
    (is-equal 8080 port)
    (is-equal "/users/123" path)
    (is-equal nil query))
  
  (multiple-value-bind (scheme host port path query)
      (client::parse-url "http://localhost:3000/")
    (is-equal "http" scheme)
    (is-equal "localhost" host)
    (is-equal 3000 port)
    (is-equal "/" path)
    (is-equal nil query)))

(deftest test-request-creation ()
  "Test HTTP request object creation"
  (let ((req (request:make-request "GET" "/test"
                                  :headers (map:make-map "Host" "example.com")
                                  :params (map:make-map "q" "search"))))
    (is-equal "GET" (request:request-method req))
    (is-equal "/test" (request:request-path req))
    (is-equal "example.com" (map:get (request:request-headers req) "Host"))
    (is-equal "search" (map:get (request:request-params req) "q"))))

(deftest test-response-creation ()
  "Test HTTP response object creation"
  (let ((resp (response:make-response :status 200 
                                     :headers (map:make-map "Content-Type" "text/html")
                                     :body "Hello World")))
    (is-equal 200 (response:response-status resp))
    (is-equal "text/html" (map:get (response:response-headers resp) "Content-Type"))
    (is-equal "Hello World" (response:response-body resp))))

(deftest test-json-response ()
  "Test JSON response creation"
  (let* ((data (map:make-map "message" "hello"))
         (resp (response:json-response data :status 201)))
    (is-equal 201 (response:response-status resp))
    (is-equal "application/json" (map:get (response:response-headers resp) "Content-Type"))
    (is (stringp (response:response-body resp)))))

(deftest test-query-string-parsing ()
  "Test query string parsing"
  (let ((params (request:parse-query-string "foo=bar&baz=qux&empty=")))
    (is-equal "bar" (map:get params "foo"))
    (is-equal "qux" (map:get params "baz"))
    (is-equal "" (map:get params "empty")))
  
  ;; Test empty query string
  (let ((params (request:parse-query-string "")))
    (is-equal map:+empty+ params))
  
  ;; Test nil query string  
  (let ((params (request:parse-query-string nil)))
    (is-equal map:+empty+ params)))

(deftest test-url-decoding ()
  "Test URL decoding"
  (is-equal "hello world" (request::url-decode "hello+world"))
  (is-equal "hello world" (request::url-decode "hello%20world"))
  (is-equal "test@example.com" (request::url-decode "test%40example.com"))
  
  ;; Test additional URL decoding cases
  (is-equal "simple" (request::url-decode "simple"))
  (is-equal "100%" (request::url-decode "100%25")))

(deftest test-status-text ()
  "Test HTTP status text conversion"
  (is-equal "OK" (response::status-text 200))
  (is-equal "Created" (response::status-text 201))
  (is-equal "No Content" (response::status-text 204))
  (is-equal "Bad Request" (response::status-text 400))
  (is-equal "Unauthorized" (response::status-text 401))
  (is-equal "Forbidden" (response::status-text 403))
  (is-equal "Not Found" (response::status-text 404))
  (is-equal "Method Not Allowed" (response::status-text 405))
  (is-equal "Internal Server Error" (response::status-text 500))
  (is-equal "Unknown" (response::status-text 999)))

(deftest test-response-to-string ()
  "Test HTTP response string formatting"
  (let* ((resp (response:make-response :status 200 
                                      :headers (map:make-map "Content-Type" "text/plain")
                                      :body "Hello"))
         (response-string (response:response-to-string resp)))
    (is (search "HTTP/1.1 200 OK" response-string))
    (is (search "Content-Type: text/plain" response-string))
    (is (search "content-length: 5" response-string))
    (is (search "Hello" response-string)))
  
  ;; Test response without body
  (let* ((resp (response:make-response :status 204))
         (response-string (response:response-to-string resp)))
    (is (search "HTTP/1.1 204 No Content" response-string))
    (is-not (search "content-length" response-string))))

(deftest test-html-response ()
  "Test HTML response creation"
  (let ((resp (response:html-response "<h1>Test</h1>" :status 200)))
    (is-equal 200 (response:response-status resp))
    (is-equal "text/html; charset=utf-8" (map:get (response:response-headers resp) "Content-Type"))
    (is-equal "<h1>Test</h1>" (response:response-body resp))))

(deftest test-text-response ()
  "Test plain text response creation"
  (let ((resp (response:text-response "Plain text" :status 200)))
    (is-equal 200 (response:response-status resp))
    (is-equal "text/plain; charset=utf-8" (map:get (response:response-headers resp) "Content-Type"))
    (is-equal "Plain text" (response:response-body resp))))

(deftest test-redirect-response ()
  "Test redirect response creation"
  (let ((resp (response:redirect "/login" :status 302)))
    (is-equal 302 (response:response-status resp))
    (is-equal "/login" (map:get (response:response-headers resp) "Location"))))

(deftest test-form-data-parsing ()
  "Test form data parsing"
  (let ((params (request:parse-form-data "name=john&age=30&active=true")))
    (is-equal "john" (map:get params "name"))
    (is-equal "30" (map:get params "age"))
    (is-equal "true" (map:get params "active"))))

(deftest test-http-request-parsing ()
  "Test complete HTTP request parsing"
  (let* ((request-string (format nil "GET /test?q=search HTTP/1.1~C~CHost: example.com~C~CUser-Agent: test~C~C~C~C" 
                                        #\Return #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline))
         (req (request:parse-http-request request-string)))
    (is-equal "GET" (request:request-method req))
    (is-equal "/test" (request:request-path req))
    (is-equal "example.com" (map:get (request:request-headers req) "host"))
    (is-equal "test" (map:get (request:request-headers req) "user-agent"))
    (is-equal "search" (map:get (request:request-params req) "q"))))
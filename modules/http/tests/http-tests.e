;;;; HTTP Tests

(package epsilon.http.tests
  (import (epsilon.test test)
          (epsilon.http.client client)
          (epsilon.http.server server)
          (epsilon.http.request request)
          (epsilon.http.response response)
          (epsilon.map map)
          (epsilon.string str)))

(test:deftest test-parse-url ()
  "Test URL parsing functionality"
  (multiple-value-bind (scheme host port path query)
      (client::parse-url "http://example.com:8080/path?foo=bar")
    (test:is-equal "http" scheme)
    (test:is-equal "example.com" host)
    (test:is-equal 8080 port)
    (test:is-equal "/path" path)
    (test:is-equal "foo=bar" query))

  (multiple-value-bind (scheme host port path query)
      (client::parse-url "https://example.com/")
    (test:is-equal "https" scheme)
    (test:is-equal "example.com" host)
    (test:is-equal 443 port)
    (test:is-equal "/" path)
    (test:is-equal nil query))

  ;; Test additional URL formats
  (multiple-value-bind (scheme host port path query)
      (client::parse-url "https://api.example.com:8080/users/123")
    (test:is-equal "https" scheme)
    (test:is-equal "api.example.com" host)
    (test:is-equal 8080 port)
    (test:is-equal "/users/123" path)
    (test:is-equal nil query))

  (multiple-value-bind (scheme host port path query)
      (client::parse-url "http://localhost:3000/")
    (test:is-equal "http" scheme)
    (test:is-equal "localhost" host)
    (test:is-equal 3000 port)
    (test:is-equal "/" path)
    (test:is-equal nil query)))

(test:deftest test-request-creation ()
  "Test HTTP request object creation"
  (let ((req (request:make-request "GET" "/test"
                                  :headers (map:make-map "Host" "example.com")
                                  :params (map:make-map "q" "search"))))
    (test:is-equal "GET" (request:request-method req))
    (test:is-equal "/test" (request:request-path req))
    (test:is-equal "example.com" (map:get (request:request-headers req) "Host"))
    (test:is-equal "search" (map:get (request:request-params req) "q"))))

(test:deftest test-response-creation ()
  "Test HTTP response object creation"
  (let ((resp (response:make-response :status 200
                                     :headers (map:make-map "Content-Type" "text/html")
                                     :body "Hello World")))
    (test:is-equal 200 (response:response-status resp))
    (test:is-equal "text/html" (map:get (response:response-headers resp) "Content-Type"))
    (test:is-equal "Hello World" (response:response-body resp))))

(test:deftest test-json-response ()
  "Test JSON response creation"
  (let* ((data (map:make-map "message" "hello"))
         (resp (response:json-response data :status 201)))
    (test:is-equal 201 (response:response-status resp))
    (test:is-equal "application/json" (map:get (response:response-headers resp) "Content-Type"))
    (test:is (stringp (response:response-body resp)))))

(test:deftest test-query-string-parsing ()
  "Test query string parsing"
  (let ((params (request:parse-query-string "foo=bar&baz=qux&empty=")))
    (test:is-equal "bar" (map:get params "foo"))
    (test:is-equal "qux" (map:get params "baz"))
    (test:is-equal "" (map:get params "empty")))

  ;; Test empty query string
  (let ((params (request:parse-query-string "")))
    (test:is-equal map:+empty+ params))

  ;; Test nil query string
  (let ((params (request:parse-query-string nil)))
    (test:is-equal map:+empty+ params)))

(test:deftest test-url-decoding ()
  "Test URL decoding"
  (test:is-equal "hello world" (request::url-decode "hello+world"))
  (test:is-equal "hello world" (request::url-decode "hello%20world"))
  (test:is-equal "test@example.com" (request::url-decode "test%40example.com"))

  ;; Test additional URL decoding cases
  (test:is-equal "simple" (request::url-decode "simple"))
  (test:is-equal "100%" (request::url-decode "100%25")))

(test:deftest test-status-text ()
  "Test HTTP status text conversion"
  (test:is-equal "OK" (response::status-text 200))
  (test:is-equal "Created" (response::status-text 201))
  (test:is-equal "No Content" (response::status-text 204))
  (test:is-equal "Bad Request" (response::status-text 400))
  (test:is-equal "Unauthorized" (response::status-text 401))
  (test:is-equal "Forbidden" (response::status-text 403))
  (test:is-equal "Not Found" (response::status-text 404))
  (test:is-equal "Method Not Allowed" (response::status-text 405))
  (test:is-equal "Internal Server Error" (response::status-text 500))
  (test:is-equal "Unknown" (response::status-text 999)))

(test:deftest test-response-to-string ()
  "Test HTTP response string formatting"
  (let* ((resp (response:make-response :status 200
                                      :headers (map:make-map "Content-Type" "text/plain")
                                      :body "Hello"))
         (response-string (response:response-to-string resp)))
    (test:is (search "HTTP/1.1 200 OK" response-string))
    (test:is (search "Content-Type: text/plain" response-string))
    (test:is (search "content-length: 5" response-string))
    (test:is (search "Hello" response-string)))

  ;; Test response without body
  (let* ((resp (response:make-response :status 204))
         (response-string (response:response-to-string resp)))
    (test:is (search "HTTP/1.1 204 No Content" response-string))
    (test:is-not (search "content-length" response-string))))

(test:deftest test-html-response ()
  "Test HTML response creation"
  (let ((resp (response:html-response "<h1>Test</h1>" :status 200)))
    (test:is-equal 200 (response:response-status resp))
    (test:is-equal "text/html; charset=utf-8" (map:get (response:response-headers resp) "Content-Type"))
    (test:is-equal "<h1>Test</h1>" (response:response-body resp))))

(test:deftest test-text-response ()
  "Test plain text response creation"
  (let ((resp (response:text-response "Plain text" :status 200)))
    (test:is-equal 200 (response:response-status resp))
    (test:is-equal "text/plain; charset=utf-8" (map:get (response:response-headers resp) "Content-Type"))
    (test:is-equal "Plain text" (response:response-body resp))))

(test:deftest test-redirect-response ()
  "Test redirect response creation"
  (let ((resp (response:redirect "/login" :status 302)))
    (test:is-equal 302 (response:response-status resp))
    (test:is-equal "/login" (map:get (response:response-headers resp) "Location"))))

(test:deftest test-form-data-parsing ()
  "Test form data parsing"
  (let ((params (request:parse-form-data "name=john&age=30&active=true")))
    (test:is-equal "john" (map:get params "name"))
    (test:is-equal "30" (map:get params "age"))
    (test:is-equal "true" (map:get params "active"))))

(test:deftest test-http-request-parsing ()
  "Test complete HTTP request parsing"
  (let* ((request-string (format nil "GET /test?q=search HTTP/1.1~C~CHost: example.com~C~CUser-Agent: test~C~C~C~C"
                                        #\Return #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline))
         (req (request:parse-http-request request-string)))
    (test:is-equal "GET" (request:request-method req))
    (test:is-equal "/test" (request:request-path req))
    (test:is-equal "example.com" (map:get (request:request-headers req) "host"))
    (test:is-equal "test" (map:get (request:request-headers req) "user-agent"))
    (test:is-equal "search" (map:get (request:request-params req) "q"))))

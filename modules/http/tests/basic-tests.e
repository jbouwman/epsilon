;;;; Basic HTTP Tests
;;;;
;;;; Simple tests for HTTP functionality without server dependencies

(package epsilon.http.basic.tests
  (import (epsilon.test test)
          (epsilon.http.client client)
          (epsilon.http.request request)
          (epsilon.http.response response)
          (epsilon.map map)
          (epsilon.string str)))

(test:deftest test-url-parsing ()
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

(test:deftest test-format-request-line ()
  "Test request line formatting"
  (test:is-equal "GET /test HTTP/1.1"
            (client::format-request-line "GET" "/test" nil))
  (test:is-equal "GET /test?q=search HTTP/1.1"
            (client::format-request-line "GET" "/test" "q=search")))

(test:deftest test-extract-content-length-positive ()
  "Test content length extraction with valid values"
  (test:is-equal 100
            (client::extract-content-length (format nil "Content-Length: 100~C~C" #\Return #\Linefeed)))
  (test:is-equal 0
            (client::extract-content-length (format nil "Content-Length: 0~C~C" #\Return #\Linefeed))))

(test:deftest test-extract-content-length-negative ()
  "Test content length extraction with no Content-Length header"
  (test:is (null (client::extract-content-length (format nil "User-Agent: test-agent~C~C" #\Return #\Linefeed)))))

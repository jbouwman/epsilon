(defpackage :epsilon.http.client.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:http #:epsilon.http.client)
   (#:map #:epsilon.map)))

(in-package :epsilon.http.client.tests)

(define-test parse-url-test
  "Test URL parsing"
  (multiple-value-bind (scheme host port path query)
      (http::parse-url "http://example.com:8080/path?foo=bar")
    (is-equal "http" scheme)
    (is-equal "example.com" host)
    (is-equal 8080 port)
    (is-equal "/path" path)
    (is-equal "foo=bar" query))
  
  (multiple-value-bind (scheme host port path query)
      (http::parse-url "https://example.com/")
    (is-equal "https" scheme)
    (is-equal "example.com" host)
    (is-equal 443 port)
    (is-equal "/" path)
    (is-equal nil query)))

(define-test format-request-line-test
  "Test request line formatting"
  (is-equal "GET / HTTP/1.1"
            (http::format-request-line "GET" "/" nil))
  
  (is-equal "POST /api/users?limit=10 HTTP/1.1"
            (http::format-request-line "POST" "/api/users" "limit=10")))

(define-test parse-response-test
  "Test response parsing"
  (let ((response "HTTP/1.1 200 OK
Content-Type: text/plain
Content-Length: 13

Hello, World!"))
    (multiple-value-bind (status headers body)
        (http::parse-response response)
      (is-equal 200 status)
      (is-equal "text/plain" (map:get headers "Content-Type"))
      (is-equal "13" (map:get headers "Content-Length"))
      (is-equal "Hello, World!" body))))

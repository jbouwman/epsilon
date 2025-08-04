;;;; Basic HTTP Tests
;;;;
;;;; Simple tests for HTTP functionality without server dependencies

(defpackage :epsilon.http.basic.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:client #:epsilon.http.client)
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response)
   (#:map #:epsilon.map)
   (#:str #:epsilon.string)))

(in-package :epsilon.http.basic.tests)

(deftest test-url-parsing ()
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

(deftest test-format-request-line ()
  "Test request line formatting"
  (is-equal "GET /test HTTP/1.1" 
            (client::format-request-line "GET" "/test" nil))
  (is-equal "GET /test?q=search HTTP/1.1" 
            (client::format-request-line "GET" "/test" "q=search")))

(deftest test-extract-content-length-positive ()
  "Test content length extraction with valid values"
  (is-equal 100 
            (client::extract-content-length "Content-Length: 100\r\n"))
  (is-equal 0 
            (client::extract-content-length "Content-Length: 0\r\n")))

(deftest test-extract-content-length-negative ()
  "Test content length extraction with no Content-Length header"
  (is (null (client::extract-content-length "User-Agent: test-agent\r\n"))))
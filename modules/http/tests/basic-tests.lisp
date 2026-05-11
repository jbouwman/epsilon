;;;; Basic HTTP Tests
;;;;
;;;; Simple tests for HTTP functionality without server dependencies

(defpackage epsilon.http.basic.tests
  (:use :cl :epsilon.syntax :epsilon.test)
  (:import (epsilon.http.client client)
            (epsilon.http.request request)
            (epsilon.http.response response)
            (epsilon.http.headers headers)
            (epsilon.map map)
            (epsilon.string str)))

(deftest test-url-parsing ()
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

(deftest test-format-request-line ()
  "Test request line formatting"
  (assert-equal "GET /test HTTP/1.1"
            (client::format-request-line "GET" "/test" nil))
  (assert-equal "GET /test?q=search HTTP/1.1"
            (client::format-request-line "GET" "/test" "q=search")))

(deftest test-extract-content-length-positive ()
  "Test content length extraction with valid values"
  (assert-equal 100
            (client::extract-content-length (format nil "Content-Length: 100~C~C" #\Return #\Linefeed)))
  (assert-equal 0
            (client::extract-content-length (format nil "Content-Length: 0~C~C" #\Return #\Linefeed))))

(deftest test-extract-content-length-negative ()
  "Test content length extraction with no Content-Length header"
  (assert-true (null (client::extract-content-length (format nil "User-Agent: test-agent~C~C" #\Return #\Linefeed)))))

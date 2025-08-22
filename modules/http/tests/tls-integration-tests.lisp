;;;; TLS Integration Tests for HTTP Client
;;;;
;;;; Test-first approach to fixing TLS support in HTTP module

(defpackage :epsilon.http.tls-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:http #:epsilon.http)
   (#:client #:epsilon.http.client)
   (#:crypto #:epsilon.crypto)
   (#:net #:epsilon.net)
   (#:fixture #:epsilon.test.fixture)))

(in-package :epsilon.http.tls-tests)

;;;; Basic TLS Connection Tests

(deftest test-parse-https-url ()
  "Test parsing HTTPS URLs correctly"
  (multiple-value-bind (scheme host port path query)
      (client::parse-url "https://example.com/path?query=1")
    (is-equal scheme "https")
    (is-equal host "example.com")
    (is-= port 443)
    (is-equal path "/path")
    (is-equal query "query=1")))

(deftest test-https-default-port ()
  "Test HTTPS URLs default to port 443"
  (multiple-value-bind (scheme host port path query)
      (client::parse-url "https://api.github.com/repos")
    (is-equal scheme "https")
    (is-equal host "api.github.com")
    (is-= port 443)
    (is-equal path "/repos")
    (is-equal query nil)))

;;;; Real HTTPS Request Tests (commented out by default)

#|
(deftest test-real-https-request ()
  "Test real HTTPS request to httpbin.org"
  ;; This test requires actual network and working OpenSSL
  (let ((response (http:http-get "https://httpbin.org/get")))
    (is (not (null response)))
    (is-= (getf response :status) 200)
    (let ((body (getf response :body)))
      (is (search "\"url\":" body))
      (is (search "\"headers\":" body)))))

(deftest test-real-https-post ()
  "Test real HTTPS POST to httpbin.org"
  (let* ((body "{\"test\":\"data\"}")
         (response (http:http-post "https://httpbin.org/post" 
                                   :headers (epsilon.map:make-map 
                                             "Content-Type" "application/json")
                                   :body body)))
    (is-= (getf response :status) 200)
    (let ((response-body (getf response :body)))
      (is (search "\"data\": \"{\\\"test\\\":\\\"data\\\"}\"" response-body)))))
|#

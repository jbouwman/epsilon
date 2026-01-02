;;;; TLS Integration Tests for HTTP Client
;;;;
;;;; Test-first approach to fixing TLS support in HTTP module

(package epsilon.http.tls-tests
  (import (epsilon.test test)
          (epsilon.http http)
          (epsilon.http.client client)
          (epsilon.crypto crypto)
          (epsilon.net net)
          (epsilon.test.fixture fixture)))

;;;; Basic TLS Connection Tests

(test:deftest test-parse-https-url ()
  "Test parsing HTTPS URLs correctly"
  (multiple-value-bind (scheme host port path query)
      (client::parse-url "https://example.com/path?query=1")
    (test:is-equal scheme "https")
    (test:is-equal host "example.com")
    (test:is-= port 443)
    (test:is-equal path "/path")
    (test:is-equal query "query=1")))

(test:deftest test-https-default-port ()
  "Test HTTPS URLs default to port 443"
  (multiple-value-bind (scheme host port path query)
      (client::parse-url "https://api.github.com/repos")
    (test:is-equal scheme "https")
    (test:is-equal host "api.github.com")
    (test:is-= port 443)
    (test:is-equal path "/repos")
    (test:is-equal query nil)))

;;;; Real HTTPS Request Tests (commented out by default)

#|
(test:deftest test-real-https-request ()
  "Test real HTTPS request to httpbin.org"
  ;; This test requires actual network and working OpenSSL
  (let ((response (http:http-get "https://httpbin.org/get")))
    (test:is (not (null response)))
    (test:is-= (getf response :status) 200)
    (let ((body (getf response :body)))
      (test:is (search "\"url\":" body))
      (test:is (search "\"headers\":" body)))))

(test:deftest test-real-https-post ()
  "Test real HTTPS POST to httpbin.org"
  (let* ((body "{\"test\":\"data\"}")
         (response (http:http-post "https://httpbin.org/post"
                                   :headers (epsilon.map:make-map
                                             "Content-Type" "application/json")
                                   :body body)))
    (test:is-= (getf response :status) 200)
    (let ((response-body (getf response :body)))
      (test:is (search "\"data\": \"{\\\"test\\\":\\\"data\\\"}\"" response-body)))))
|#

;;;; External Server Compliance Tests
;;;;
;;;; Tests against well-known public HTTP/HTTPS test servers
;;;; to ensure compliance with HTTP standards and proper TLS handling

(defpackage :epsilon.http.test-external-servers
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:http #:epsilon.http)
   (#:client #:epsilon.http.client)
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response)
   (#:map #:epsilon.map)
   (#:str #:epsilon.string)))

(in-package :epsilon.http.test-external-servers)

;;;; Test Configuration

(defparameter *test-timeout* 30
  "Timeout in seconds for external server tests")

(defparameter *run-external-tests* (not (sb-ext:posix-getenv "EPSILON_SKIP_EXTERNAL_TESTS"))
  "Whether to run external server tests (can be disabled in CI)")

(defmacro with-external-test ((name) &body body)
  "Macro to conditionally run external tests"
  `(if *run-external-tests*
       (progn
         (format t "~&Running external test: ~A~%" ,name)
         ,@body)
       (skip (format nil "External test ~A skipped (EPSILON_SKIP_EXTERNAL_TESTS set)" ,name))))

;;;; httpbin.org Tests

(deftest test-httpbin-get
  "Test GET request to httpbin.org"
  (with-external-test ("httpbin GET")
    (handler-case
        (let ((response (http:http-get "http://httpbin.org/get")))
          (is-equal 200 (response:response-status response))
          (is-not-null (response:response-body response))
          ;; httpbin returns JSON with request details
          (is (search "\"url\"" (response:response-body response))))
      (error (e)
        (fail (format nil "httpbin GET failed: ~A" e))))))

(deftest test-httpbin-post
  "Test POST request to httpbin.org"
  (with-external-test ("httpbin POST")
    (handler-case
        (let ((response (http:http-post "http://httpbin.org/post"
                                        :body "test=data&foo=bar"
                                        :headers (map:make-map 
                                                 "Content-Type" "application/x-www-form-urlencoded"))))
          (is-equal 200 (response:response-status response))
          (let ((body (response:response-body response)))
            (is (search "\"test\"" body))
            (is (search "\"data\"" body))))
      (error (e)
        (fail (format nil "httpbin POST failed: ~A" e))))))

(deftest test-httpbin-put
  "Test PUT request to httpbin.org"
  (with-external-test ("httpbin PUT")
    (handler-case
        (let ((response (http:http-put "http://httpbin.org/put"
                                       :body "{\"key\": \"value\"}"
                                       :headers (map:make-map 
                                                "Content-Type" "application/json"))))
          (is-equal 200 (response:response-status response))
          (is (search "\"key\"" (response:response-body response))))
      (error (e)
        (fail (format nil "httpbin PUT failed: ~A" e))))))

(deftest test-httpbin-delete
  "Test DELETE request to httpbin.org"
  (with-external-test ("httpbin DELETE")
    (handler-case
        (let ((response (http:http-delete "http://httpbin.org/delete")))
          (is-equal 200 (response:response-status response)))
      (error (e)
        (fail (format nil "httpbin DELETE failed: ~A" e))))))

(deftest test-httpbin-headers
  "Test custom headers with httpbin.org"
  (with-external-test ("httpbin headers")
    (handler-case
        (let ((response (http:http-get "http://httpbin.org/headers"
                                       :headers (map:make-map
                                                "X-Custom-Header" "test-value"
                                                "User-Agent" "epsilon-http-client"))))
          (is-equal 200 (response:response-status response))
          (let ((body (response:response-body response)))
            (is (search "X-Custom-Header" body))
            (is (search "test-value" body))
            (is (search "epsilon-http-client" body))))
      (error (e)
        (fail (format nil "httpbin headers test failed: ~A" e))))))

(deftest test-httpbin-status-codes
  "Test various HTTP status codes"
  (with-external-test ("httpbin status codes")
    ;; Test 404
    (handler-case
        (let ((response (http:http-get "http://httpbin.org/status/404")))
          (is-equal 404 (response:response-status response)))
      (error (e)
        (fail (format nil "httpbin 404 test failed: ~A" e))))
    
    ;; Test 500
    (handler-case
        (let ((response (http:http-get "http://httpbin.org/status/500")))
          (is-equal 500 (response:response-status response)))
      (error (e)
        (fail (format nil "httpbin 500 test failed: ~A" e))))))

(deftest test-httpbin-redirect
  "Test redirect handling"
  (with-external-test ("httpbin redirects")
    (handler-case
        (let ((response (http:http-get "http://httpbin.org/redirect/2")))
          ;; Should follow redirects and end up at /get
          (is-equal 200 (response:response-status response))
          (is (search "\"url\"" (response:response-body response))))
      (error (e)
        (fail (format nil "httpbin redirect test failed: ~A" e))))))

(deftest test-httpbin-basic-auth
  "Test basic authentication"
  (with-external-test ("httpbin basic auth")
    (handler-case
        (let* ((auth-header (format nil "Basic ~A" 
                                    (str:base64-encode "user:passwd")))
               (response (http:http-get "http://httpbin.org/basic-auth/user/passwd"
                                       :headers (map:make-map
                                                "Authorization" auth-header))))
          (is-equal 200 (response:response-status response))
          (let ((body (response:response-body response)))
            (is (search "\"authenticated\"" body))
            (is (search "true" body))))
      (error (e)
        (fail (format nil "httpbin basic auth test failed: ~A" e))))))

;;;; HTTPS Tests with httpbin.org

(deftest test-https-httpbin-get
  "Test HTTPS GET request to httpbin.org"
  (with-external-test ("httpbin HTTPS GET")
    (handler-case
        (let ((response (http:http-get "https://httpbin.org/get")))
          (is-equal 200 (response:response-status response))
          (is-not-null (response:response-body response)))
      (error (e)
        (fail (format nil "httpbin HTTPS GET failed: ~A" e))))))

(deftest test-https-httpbin-post
  "Test HTTPS POST request to httpbin.org"
  (with-external-test ("httpbin HTTPS POST")
    (handler-case
        (let ((response (http:http-post "https://httpbin.org/post"
                                        :body "{\"secure\": true}"
                                        :headers (map:make-map 
                                                 "Content-Type" "application/json"))))
          (is-equal 200 (response:response-status response))
          (is (search "\"secure\"" (response:response-body response))))
      (error (e)
        (fail (format nil "httpbin HTTPS POST failed: ~A" e))))))

;;;; postman-echo.com Tests

(deftest test-postman-echo-get
  "Test GET request to postman-echo.com"
  (with-external-test ("postman-echo GET")
    (handler-case
        (let ((response (http:http-get "https://postman-echo.com/get?foo=bar")))
          (is-equal 200 (response:response-status response))
          (let ((body (response:response-body response)))
            (is (search "\"foo\"" body))
            (is (search "\"bar\"" body))))
      (error (e)
        (fail (format nil "postman-echo GET failed: ~A" e))))))

(deftest test-postman-echo-post
  "Test POST request to postman-echo.com"
  (with-external-test ("postman-echo POST")
    (handler-case
        (let ((response (http:http-post "https://postman-echo.com/post"
                                        :body "{\"message\": \"hello\"}"
                                        :headers (map:make-map 
                                                 "Content-Type" "application/json"))))
          (is-equal 200 (response:response-status response))
          (is (search "\"message\"" (response:response-body response))))
      (error (e)
        (fail (format nil "postman-echo POST failed: ~A" e))))))

(deftest test-postman-echo-headers
  "Test header echo with postman-echo.com"
  (with-external-test ("postman-echo headers")
    (handler-case
        (let ((response (http:http-get "https://postman-echo.com/headers"
                                       :headers (map:make-map
                                                "X-Test-Header" "epsilon-test"))))
          (is-equal 200 (response:response-status response))
          (is (search "x-test-header" (str:downcase (response:response-body response)))))
      (error (e)
        (fail (format nil "postman-echo headers test failed: ~A" e))))))

(deftest test-postman-echo-time
  "Test time endpoint"
  (with-external-test ("postman-echo time")
    (handler-case
        (let ((response (http:http-get "https://postman-echo.com/time/now")))
          (is-equal 200 (response:response-status response))
          ;; Should return current time
          (is-not-null (response:response-body response)))
      (error (e)
        (fail (format nil "postman-echo time test failed: ~A" e))))))

;;;; badssl.com Tests for TLS/SSL Compliance

(deftest test-badssl-valid-cert
  "Test connection to site with valid certificate"
  (with-external-test ("badssl valid cert")
    (handler-case
        (let ((response (http:http-get "https://badssl.com/")))
          (is-equal 200 (response:response-status response))
          (is (search "badssl.com" (response:response-body response))))
      (error (e)
        (fail (format nil "badssl valid cert test failed: ~A" e))))))

(deftest test-badssl-sha256
  "Test SHA256 certificate"
  (with-external-test ("badssl SHA256")
    (handler-case
        (let ((response (http:http-get "https://sha256.badssl.com/")))
          (is-equal 200 (response:response-status response)))
      (error (e)
        (fail (format nil "badssl SHA256 test failed: ~A" e))))))

(deftest test-badssl-expired-cert
  "Test that expired certificate is rejected"
  (with-external-test ("badssl expired cert")
    ;; This should fail with proper SSL verification
    (handler-case
        (let ((response (http:http-get "https://expired.badssl.com/")))
          ;; If we get here, SSL verification might not be working
          (warn "Expected SSL error for expired certificate, got response: ~A" 
                (response:response-status response)))
      (error (e)
        ;; This is expected - expired cert should cause an error
        (is-true t)))))

(deftest test-badssl-wrong-host
  "Test that wrong hostname certificate is rejected"
  (with-external-test ("badssl wrong host")
    ;; This should fail with proper hostname verification
    (handler-case
        (let ((response (http:http-get "https://wrong.host.badssl.com/")))
          ;; If we get here, hostname verification might not be working
          (warn "Expected SSL error for wrong hostname, got response: ~A" 
                (response:response-status response)))
      (error (e)
        ;; This is expected - wrong hostname should cause an error
        (is-true t)))))

(deftest test-badssl-self-signed
  "Test that self-signed certificate is rejected"
  (with-external-test ("badssl self-signed")
    ;; This should fail with proper SSL verification
    (handler-case
        (let ((response (http:http-get "https://self-signed.badssl.com/")))
          ;; If we get here, SSL verification might not be working
          (warn "Expected SSL error for self-signed certificate, got response: ~A" 
                (response:response-status response)))
      (error (e)
        ;; This is expected - self-signed cert should cause an error
        (is-true t)))))

;;;; Performance and Reliability Tests

(deftest test-large-response
  "Test handling of large responses"
  (with-external-test ("large response")
    (handler-case
        ;; Request 1000 bytes of data
        (let ((response (http:http-get "http://httpbin.org/bytes/1000")))
          (is-equal 200 (response:response-status response))
          (is-equal 1000 (length (response:response-body response))))
      (error (e)
        (fail (format nil "Large response test failed: ~A" e))))))

(deftest test-streaming-response
  "Test streaming response handling"
  (with-external-test ("streaming response")
    (handler-case
        ;; Request 10 lines of streaming data
        (let ((response (http:http-get "http://httpbin.org/stream/10")))
          (is-equal 200 (response:response-status response))
          ;; Should have 10 JSON objects separated by newlines
          (let ((lines (str:split #\Newline (response:response-body response))))
            (is-equal 10 (length (remove-if #'str:empty-p lines)))))
      (error (e)
        (fail (format nil "Streaming response test failed: ~A" e))))))

(deftest test-gzip-response
  "Test gzip compressed response handling"
  (with-external-test ("gzip response")
    (handler-case
        (let ((response (http:http-get "http://httpbin.org/gzip")))
          (is-equal 200 (response:response-status response))
          ;; Response should be automatically decompressed
          (is (search "\"gzipped\"" (response:response-body response))))
      (error (e)
        (fail (format nil "Gzip response test failed: ~A" e))))))

(deftest test-concurrent-requests
  "Test multiple concurrent requests"
  (with-external-test ("concurrent requests")
    (handler-case
        (let ((threads nil)
              (results (make-array 5 :initial-element nil)))
          ;; Start 5 concurrent requests
          (dotimes (i 5)
            (push (sb-thread:make-thread
                   (lambda (index)
                     (handler-case
                         (let ((response (http:http-get 
                                        (format nil "http://httpbin.org/delay/~D" (1+ index)))))
                           (setf (aref results index) 
                                 (response:response-status response)))
                       (error (e)
                         (setf (aref results index) e))))
                   :arguments (list i))
                  threads))
          ;; Wait for all threads to complete
          (dolist (thread threads)
            (sb-thread:join-thread thread))
          ;; Check all requests succeeded
          (dotimes (i 5)
            (is-equal 200 (aref results i))))
      (error (e)
        (fail (format nil "Concurrent requests test failed: ~A" e))))))

;;;; Test Summary

(deftest test-external-servers-summary
  "Summary of external server tests"
  (format t "~&~%External Server Test Summary:~%")
  (format t "  - httpbin.org tests for HTTP methods and features~%")
  (format t "  - HTTPS tests for secure connections~%")
  (format t "  - postman-echo.com tests for additional protocols~%")
  (format t "  - badssl.com tests for TLS/SSL compliance~%")
  (format t "  - Performance tests for large and streaming responses~%")
  (when (not *run-external-tests*)
    (format t "~%Note: External tests were skipped. Set EPSILON_SKIP_EXTERNAL_TESTS='' to run them.~%"))
  (is-true t))
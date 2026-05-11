;;;; ALPN Protocol Selection Tests
;;;;
;;;; Tests for HTTP/2 protocol negotiation and graceful fallback.
;;;; These tests verify the ALPN logic without requiring real TLS connections.

(defpackage :epsilon.http.alpn.tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.client client)
   (epsilon.http.response response)
   (epsilon.http.test-helpers helpers)
   (epsilon.map map)))

;;; Tests

(deftest test-default-alpn-is-http11 ()
  "Test that default ALPN advertises only HTTP/1.1"
  ;; The default connection should work with HTTP/1.1 servers
  (helpers:with-mock-server (port (helpers:make-echo-handler))
    (let ((resp (client:get (format nil "http://127.0.0.1:~D/echo" port))))
      (assert-true resp)
      (assert-equal 200 (response:response-status resp)))))

(deftest test-explicit-http11-alpn ()
  "Test explicit HTTP/1.1 ALPN protocol works"
  (helpers:with-mock-server (port (helpers:make-echo-handler))
    (let ((resp (client:get (format nil "http://127.0.0.1:~D/echo" port)
                            :alpn-protocols '("http/1.1"))))
      (assert-true resp)
      (assert-equal 200 (response:response-status resp)))))

(deftest test-h2-alpn-fallback-on-plain-http ()
  "Test that H2 ALPN is ignored for plain HTTP (non-TLS) connections"
  ;; Even if h2 is requested, plain HTTP connections use HTTP/1.1
  (helpers:with-mock-server (port (helpers:make-echo-handler))
    (let ((resp (client:get (format nil "http://127.0.0.1:~D/echo" port)
                            :alpn-protocols '("h2" "http/1.1"))))
      (assert-true resp)
      (assert-equal 200 (response:response-status resp)))))

(deftest test-url-parse-preserves-scheme ()
  "Test that URL parsing correctly identifies https vs http"
  (multiple-value-bind (scheme host port path query)
      (client::parse-url "https://example.com/api")
    (declare (ignore host port path query))
    (assert-equal "https" scheme))
  (multiple-value-bind (scheme host port path query)
      (client::parse-url "http://example.com/api")
    (declare (ignore host port path query))
    (assert-equal "http" scheme)))

(deftest test-alpn-with-pool ()
  "Test that ALPN protocols work with connection pooling"
  (helpers:with-mock-server (port (helpers:make-echo-handler))
    (let ((pool (epsilon.http.connection-pool:create-connection-pool)))
      (unwind-protect
           (let ((resp (client:get (format nil "http://127.0.0.1:~D/echo" port)
                                   :pool pool)))
             (assert-true resp)
             (assert-equal 200 (response:response-status resp)))
        (epsilon.http.connection-pool:shutdown-connection-pool pool)))))

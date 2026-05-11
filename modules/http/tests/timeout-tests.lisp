;;;; HTTP Client Timeout Tests
;;;;
;;;; Tests for request timeout support in the HTTP client.
;;;; Uses in-process servers with delayed responses.

(defpackage :epsilon.http.timeout.tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.client client)
   (epsilon.http.response response)
   (epsilon.http.test-helpers helpers)
   (epsilon.map map)))

;;; Tests

(deftest test-timeout-parameter-accepted ()
  "Test that :timeout parameter is accepted without error"
  (helpers:with-mock-server (port (helpers:make-static-handler "ok"))
    (let ((resp (client:request
                 (format nil "http://127.0.0.1:~D/" port)
                 :timeout 5)))
      (assert-true resp)
      (assert-equal 200 (response:response-status resp)))))

(deftest test-timeout-on-slow-response ()
  "Test that a slow server triggers a timeout error"
  (helpers:with-mock-server (port (helpers:make-delayed-handler 5 "slow"))
    (assert-condition (error)
      (client:request (format nil "http://127.0.0.1:~D/" port) :timeout 1))))

(deftest test-timeout-fast-response-succeeds ()
  "Test that a fast response within timeout succeeds"
  (helpers:with-mock-server (port (helpers:make-delayed-handler 0.1 "fast"))
    (let ((resp (client:request
                 (format nil "http://127.0.0.1:~D/" port)
                 :timeout 5)))
      (assert-true resp)
      (assert-equal 200 (response:response-status resp))
      (assert-equal "fast" (response:response-body-string resp)))))

(deftest test-timeout-nil-means-no-timeout ()
  "Test that :timeout nil means no timeout (default behavior)"
  (helpers:with-mock-server (port (helpers:make-static-handler "ok"))
    (let ((resp (client:request
                 (format nil "http://127.0.0.1:~D/" port)
                 :timeout nil)))
      (assert-true resp)
      (assert-equal 200 (response:response-status resp)))))

(deftest test-timeout-with-get-convenience ()
  "Test timeout works with the get convenience function"
  (helpers:with-mock-server (port (helpers:make-static-handler "ok"))
    (let ((resp (client:get (format nil "http://127.0.0.1:~D/" port) :timeout 5)))
      (assert-true resp)
      (assert-equal 200 (response:response-status resp)))))

(deftest test-timeout-with-post-convenience ()
  "Test timeout works with the http-post convenience function"
  (helpers:with-mock-server (port (helpers:make-echo-handler))
    (let ((resp (client:http-post (format nil "http://127.0.0.1:~D/echo" port)
                                  :body "test" :timeout 5)))
      (assert-true resp)
      (assert-equal 200 (response:response-status resp)))))

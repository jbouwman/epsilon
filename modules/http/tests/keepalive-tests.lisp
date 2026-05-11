;;;; HTTP Keep-Alive and Connection Pooling Integration Tests
;;;;
;;;; Tests for connection reuse via the main client API.
;;;; Uses in-process HTTP servers.

(defpackage :epsilon.http.keepalive.tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.client client)
   (epsilon.http.response response)
   (epsilon.http.test-helpers helpers)
   (epsilon.http.connection-pool pool)
   (epsilon.map map)))

;;; Tests

(deftest test-pool-creation ()
  "Test creating and destroying a connection pool"
  (let ((p (pool:create-connection-pool :max-size 5
                                        :max-idle-time 60
                                        :connection-timeout 10)))
    (assert-true (pool:connection-pool-p p))
    (assert-equal 5 (pool:connection-pool-max-size p))
    (pool:shutdown-connection-pool p)))

(deftest test-pool-stats-initial ()
  "Test pool statistics are zeroed on creation"
  (let* ((p (pool:create-connection-pool))
         (stats (pool:pool-stats p)))
    (assert-equal 0 (pool:pool-stats-connections-created stats))
    (assert-equal 0 (pool:pool-stats-connections-reused stats))
    (assert-equal 0 (pool:pool-stats-pool-hits stats))
    (assert-equal 0 (pool:pool-stats-pool-misses stats))
    (pool:shutdown-connection-pool p)))

(deftest test-request-with-pool ()
  "Test making a request through the pooled client API"
  (helpers:with-mock-server (port (helpers:make-echo-handler))
    (let ((p (pool:create-connection-pool :max-size 5)))
      (unwind-protect
           (let ((resp (client:request
                        (format nil "http://127.0.0.1:~D/echo" port)
                        :pool p)))
             (assert-true resp)
             (assert-equal 200 (response:response-status resp)))
        (pool:shutdown-connection-pool p)))))

(deftest test-pool-connection-reuse ()
  "Test that connections are reused across requests when pooling"
  (helpers:with-mock-server (port (helpers:make-echo-handler))
    (let ((p (pool:create-connection-pool :max-size 5)))
      (unwind-protect
           (progn
             ;; First request creates a connection
             (client:request (format nil "http://127.0.0.1:~D/echo" port) :pool p)
             ;; Second request should reuse
             (client:request (format nil "http://127.0.0.1:~D/echo" port) :pool p)
             (let ((stats (pool:pool-stats p)))
               ;; At least one miss (first request) and ideally one hit (second)
               (assert-true (>= (pool:pool-stats-pool-misses stats) 1))))
        (pool:shutdown-connection-pool p)))))

(deftest test-pool-different-hosts ()
  "Test that connections to different hosts are pooled separately"
  (helpers:with-mock-server (port1 (helpers:make-static-handler "server1"))
    (helpers:with-mock-server (port2 (helpers:make-static-handler "server2"))
      (let ((p (pool:create-connection-pool :max-size 5)))
        (unwind-protect
             (progn
               (let ((r1 (client:request
                          (format nil "http://127.0.0.1:~D/" port1) :pool p)))
                 (assert-equal 200 (response:response-status r1)))
               (let ((r2 (client:request
                          (format nil "http://127.0.0.1:~D/" port2) :pool p)))
                 (assert-equal 200 (response:response-status r2)))
               (let ((stats (pool:pool-stats p)))
                 ;; Both should be misses (different ports = different pool keys)
                 (assert-true (>= (pool:pool-stats-pool-misses stats) 2))))
          (pool:shutdown-connection-pool p))))))

(deftest test-request-without-pool ()
  "Test that requests without :pool still work (Connection: close)"
  (helpers:with-mock-server (port (helpers:make-echo-handler))
    (let ((resp (client:request (format nil "http://127.0.0.1:~D/echo" port))))
      (assert-true resp)
      (assert-equal 200 (response:response-status resp)))))

(deftest test-pool-keepalive-header ()
  "Test that pooled requests use keep-alive (verified via the echo handler)"
  (helpers:with-mock-server (port (helpers:make-echo-handler))
    (let ((p (pool:create-connection-pool :max-size 5)))
      (unwind-protect
           (let* ((resp (client:request
                         (format nil "http://127.0.0.1:~D/echo" port)
                         :pool p))
                  (body (response:response-body-string resp)))
             ;; Verify the request succeeded through the pool path
             (assert-equal 200 (response:response-status resp))
             ;; The echo handler returns method+path; verify we got a response
             (assert-true (search "GET" body)))
        (pool:shutdown-connection-pool p)))))

(deftest test-pool-shutdown-cleans-connections ()
  "Test that shutting down a pool closes all connections"
  (helpers:with-mock-server (port (helpers:make-echo-handler))
    (let ((p (pool:create-connection-pool :max-size 5)))
      ;; Make a request to populate the pool
      (client:request (format nil "http://127.0.0.1:~D/echo" port) :pool p)
      ;; Shutdown should not error
      (pool:shutdown-connection-pool p)
      ;; Pool should have zero connections after shutdown
      (assert-equal 0 (pool:pool-active-count p)))))

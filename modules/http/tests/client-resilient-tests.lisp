;;;; Resilient HTTP Client Tests
;;;;
;;;; Tests for the composition of pool + retry + circuit breaker exposed
;;;; by epsilon.http.client.resilient. Each test verifies one aspect of
;;;; the composition: connection reuse via pool, retry on transient failure,
;;;; and circuit-breaker tripping after repeated failures.

(defpackage :epsilon.http.client.resilient.tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.client.resilient resilient)
   (epsilon.http.client client)
   (epsilon.http.connection-pool pool)
   (epsilon.http.retry retry)
   (epsilon.http.errors errors)
   (epsilon.http.response response)
   (epsilon.http.test-helpers helpers)))

(in-package :epsilon.http.client.resilient.tests)

(deftest test-resilient-get-success ()
  "A simple successful GET should return a 200 response."
  (helpers:with-mock-server (port (helpers:make-static-handler "ok"))
    (let ((cfg (resilient:make-resilient-config
                :pool (pool:create-connection-pool)
                :retry-policy nil
                :circuit-breaker nil)))
      (let ((resp (resilient:resilient-get
                   (format nil "http://127.0.0.1:~D/" port)
                   :config cfg)))
        (assert-true resp)
        (assert-equal 200 (response:response-status resp))))))

(deftest test-resilient-routes-through-supplied-pool ()
  "When a pool is configured the request should go through it (verifying
   that the resilient layer hands the pool to client:request and that the
   pool integration doesn't break the path).

   We can't test cross-request connection reuse here because the in-process
   test server closes connections at the end of every handle-client call;
   true keep-alive is exercised in the keep-alive test suite."
  (helpers:with-mock-server (port (helpers:make-static-handler "ok"))
    (let* ((p (pool:create-connection-pool))
           (cfg (resilient:make-resilient-config
                 :pool p
                 :retry-policy nil
                 :circuit-breaker nil)))
      (unwind-protect
           (let ((r (resilient:resilient-get
                     (format nil "http://127.0.0.1:~D/" port)
                     :config cfg)))
             (assert-equal 200 (response:response-status r))
             ;; The pool should have recorded one miss (connection created)
             (let ((stats (pool:pool-stats p)))
               (assert-true (>= (pool:pool-stats-pool-misses stats) 1))))
        (pool:shutdown-connection-pool p)))))

(deftest test-resilient-signals-on-server-error ()
  "A 500 response should be signaled as http-server-error."
  (helpers:with-mock-server (port (lambda (req)
                                    (declare (ignore req))
                                    (response:make-response :status 500
                                                            :body "boom")))
    (let ((cfg (resilient:make-resilient-config
                :pool (pool:create-connection-pool)
                :retry-policy nil
                :circuit-breaker nil)))
      (assert-condition (errors:http-server-error)
        (resilient:resilient-get
         (format nil "http://127.0.0.1:~D/" port)
         :config cfg)))))

(deftest test-resilient-retry-recovers ()
  "Configured with a small retry policy, the resilient client should
   recover when the server returns 503 once and then 200."
  (let ((calls 0))
    (helpers:with-mock-server (port (lambda (req)
                                      (declare (ignore req))
                                      (incf calls)
                                      (if (= calls 1)
                                          (response:make-response :status 503
                                                                  :body "down")
                                          (response:text-response "ok"))))
      (let* ((policy (retry:make-retry-policy
                      :max-attempts 3
                      :initial-delay 0.01
                      :max-delay 0.05
                      :multiplier 1.0
                      :jitter 0.01
                      :backoff-strategy #'retry::fixed-delay))
             (cfg (resilient:make-resilient-config
                   :pool (pool:create-connection-pool)
                   :retry-policy policy
                   :circuit-breaker nil)))
        (let ((resp (resilient:resilient-get
                     (format nil "http://127.0.0.1:~D/" port)
                     :config cfg)))
          (assert-equal 200 (response:response-status resp))
          (assert-equal 2 calls))))))

(deftest test-resilient-non-retryable-4xx-fails-immediately ()
  "404 is not retryable, so resilient-get should signal once without
   retrying."
  (let ((calls 0))
    (helpers:with-mock-server (port (lambda (req)
                                      (declare (ignore req))
                                      (incf calls)
                                      (response:make-response :status 404
                                                              :body "missing")))
      (let* ((policy (retry:make-retry-policy
                      :max-attempts 5
                      :initial-delay 0.01
                      :backoff-strategy #'retry::fixed-delay))
             (cfg (resilient:make-resilient-config
                   :pool (pool:create-connection-pool)
                   :retry-policy policy
                   :circuit-breaker nil)))
        (assert-condition (errors:http-client-error)
          (resilient:resilient-get
           (format nil "http://127.0.0.1:~D/" port)
           :config cfg))
        (assert-equal 1 calls)))))

(deftest test-status-code-error-p ()
  "status-code-error-p should classify 200/3xx as non-error and 4xx/5xx as error."
  (assert-false (resilient:status-code-error-p 200))
  (assert-false (resilient:status-code-error-p 301))
  (assert-true  (resilient:status-code-error-p 400))
  (assert-true  (resilient:status-code-error-p 404))
  (assert-true  (resilient:status-code-error-p 500))
  (assert-true  (resilient:status-code-error-p 503)))

(deftest test-make-resilient-config-defaults ()
  "make-resilient-config should leave fields nil if not supplied."
  (let ((cfg (resilient:make-resilient-config)))
    (assert-nil (resilient:resilient-config-pool cfg))
    (assert-nil (resilient:resilient-config-retry-policy cfg))
    (assert-nil (resilient:resilient-config-circuit-breaker cfg))))

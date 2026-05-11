;;;; Connection Pool Tests
;;;;
;;;; Tests for the connection-pool module's lifecycle behavior.
;;;; The HTTP cleanup thread should start on first pool creation,
;;;; not at module load time, so processes that load the module without
;;;; ever creating a pool (LSP, build tools, REPLs) don't pay the cost
;;;; of a background thread.

(defpackage :epsilon.http.connection-pool.tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.connection-pool pool)))

(in-package :epsilon.http.connection-pool.tests)

(deftest test-create-pool-starts-cleanup-thread ()
  "Creating the first pool should start the cleanup thread."
  ;; Reset state for the test
  (let ((pool::*cleanup-thread* nil))
    (assert-nil pool::*cleanup-thread*)
    (let ((p (pool:create-connection-pool)))
      (declare (ignore p))
      (assert-not-null pool::*cleanup-thread*)
      ;; Cleanup
      (pool::stop-cleanup-thread))))

(deftest test-create-pool-reuses-cleanup-thread ()
  "Creating additional pools should reuse the existing cleanup thread."
  (let ((pool::*cleanup-thread* nil))
    (let ((p1 (pool:create-connection-pool)))
      (declare (ignore p1))
      (let ((thread-1 pool::*cleanup-thread*))
        (let ((p2 (pool:create-connection-pool)))
          (declare (ignore p2))
          (assert-eq thread-1 pool::*cleanup-thread*))
        (pool::stop-cleanup-thread)))))

(deftest test-pool-creation-after-shutdown-restarts-thread ()
  "After shutting down, creating a new pool should start a new cleanup thread."
  (let ((pool::*cleanup-thread* nil))
    (let ((p1 (pool:create-connection-pool)))
      (declare (ignore p1)))
    (let ((first-thread pool::*cleanup-thread*))
      (pool::stop-cleanup-thread)
      (assert-nil pool::*cleanup-thread*)
      (let ((p2 (pool:create-connection-pool)))
        (declare (ignore p2))
        (assert-not-null pool::*cleanup-thread*)
        ;; Should be a fresh thread, not the prior one
        (assert-not (eq first-thread pool::*cleanup-thread*))
        (pool::stop-cleanup-thread)))))

;;; pool-metrics

(deftest test-pool-metrics-empty-pool ()
  "A freshly-created pool should report zero counters and the configured
   max size."
  (let* ((p (pool:create-connection-pool :max-size 7))
         (m (pool:pool-metrics p)))
    (unwind-protect
         (progn
           (assert-equal 0 (cdr (assoc "epsilon_http_pool_connections_created" m
                                       :test #'string=)))
           (assert-equal 0 (cdr (assoc "epsilon_http_pool_connections_reused" m
                                       :test #'string=)))
           (assert-equal 0 (cdr (assoc "epsilon_http_pool_connections_closed" m
                                       :test #'string=)))
           (assert-equal 0 (cdr (assoc "epsilon_http_pool_pool_hits" m
                                       :test #'string=)))
           (assert-equal 0 (cdr (assoc "epsilon_http_pool_pool_misses" m
                                       :test #'string=)))
           (assert-equal 0 (cdr (assoc "epsilon_http_pool_active_connections" m
                                       :test #'string=)))
           (assert-equal 7 (cdr (assoc "epsilon_http_pool_max_size" m
                                       :test #'string=))))
      (pool:shutdown-connection-pool p))))

(deftest test-pool-metrics-respects-prefix ()
  "Custom :prefix should rename all metrics."
  (let ((p (pool:create-connection-pool :max-size 3)))
    (unwind-protect
         (let ((m (pool:pool-metrics p :prefix "custom")))
           (assert-true (assoc "custom_max_size" m :test #'string=))
           (assert-equal 3 (cdr (assoc "custom_max_size" m :test #'string=)))
           (assert-nil (assoc "epsilon_http_pool_max_size" m :test #'string=)))
      (pool:shutdown-connection-pool p))))

(deftest test-format-prometheus-metrics ()
  "format-prometheus-metrics should render metrics as one-line-per-metric."
  (let* ((metrics '(("foo_a" . 1) ("foo_b" . 42)))
         (text (with-output-to-string (s)
                 (pool:format-prometheus-metrics metrics s))))
    (assert-true (search "foo_a 1" text))
    (assert-true (search "foo_b 42" text))))

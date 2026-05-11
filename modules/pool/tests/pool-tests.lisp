;;;; Tests for epsilon.pool

(defpackage epsilon.pool-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:import (epsilon.pool pool)
            (epsilon.string str)))

;;;; Test Resource Factory

(defvar *test-resource-counter* 0)

(defun make-test-resource ()
  "Factory for test resources"
  (incf *test-resource-counter*)
  (format nil "resource-~D" *test-resource-counter*))

(defun destroy-test-resource (resource)
  "Destroyer for test resources"
  (declare (ignore resource))
  ;; In real use, this would cleanup the resource
  t)

(defun validate-test-resource (resource)
  "Validator for test resources"
  (and (stringp resource)
       (str:starts-with-p "resource-" resource)))

;;;; Basic Pool Tests

(deftest test-pool-creation ()
  "Test basic pool creation and configuration"
  (let ((pool (pool:create-pool :factory #'make-test-resource
                             :destroyer #'destroy-test-resource
                             :validator #'validate-test-resource
                             :max-size 5
                             :min-size 2)))
    (assert-true (pool:pool-p pool))
    (assert-= (pool:pool-capacity pool) 5)))

(deftest test-pool-acquire-release ()
  "Test basic acquire and release operations"
  (let ((pool (pool:create-pool :factory #'make-test-resource
                             :destroyer #'destroy-test-resource
                             :max-size 3))
        (*test-resource-counter* 0))

    ;; Acquire first resource
    (let ((resource1 (pool:acquire pool)))
      (assert-true (not (null resource1)))
      (assert-true (string= resource1 "resource-1"))

      ;; Acquire second resource
      (let ((resource2 (pool:acquire pool)))
        (assert-true (not (null resource2)))
        (assert-true (string= resource2 "resource-2"))
        (assert-true (not (string= resource1 resource2)))

        ;; Release resources
        (pool:release pool resource1)
        (pool:release pool resource2))

      ;; Reacquire should reuse existing resource
      (let ((resource3 (pool:acquire pool)))
        (assert-true (or (string= resource3 "resource-1")
                (string= resource3 "resource-2")))
        (pool:release pool resource3)))

    (pool:destroy-pool pool)))

(deftest test-pool-with-resource-macro ()
  "Test with-resource macro for automatic cleanup"
  (let ((pool (pool:create-pool :factory #'make-test-resource
                             :destroyer #'destroy-test-resource))
        (*test-resource-counter* 0)
        (acquired-resource nil))

    (pool:with-resource (resource pool)
      (setf acquired-resource resource)
      (assert-true (not (null resource)))
      (assert-true (string= resource "resource-1")))

    ;; Resource should be returned to pool
    (let ((reused-resource (pool:acquire pool)))
      (assert-true (string= reused-resource acquired-resource))
      (pool:release pool reused-resource))

    (pool:destroy-pool pool)))

(deftest test-pool-max-size-limit ()
  "Test pool respects max size limit"
  (let ((pool (pool:create-pool :factory #'make-test-resource
                             :max-size 2))
        (*test-resource-counter* 0))

    ;; Acquire max resources
    (let ((resource1 (pool:acquire pool))
          (resource2 (pool:acquire pool)))

      ;; Third acquire should timeout quickly
      (assert-condition (error)
        (pool:acquire pool :timeout 1))

      ;; Release one resource
      (pool:release pool resource1)

      ;; Now should be able to acquire again
      (let ((resource3 (pool:acquire pool)))
        (assert-true (not (null resource3)))
        (pool:release pool resource2)
        (pool:release pool resource3)))

    (pool:destroy-pool pool)))

(deftest test-pool-try-acquire ()
  "Test non-blocking try-acquire"
  (let ((pool (pool:create-pool :factory #'make-test-resource
                             :max-size 1))
        (*test-resource-counter* 0))

    ;; Should succeed when pool empty
    (let ((resource1 (pool:try-acquire pool)))
      (assert-true (not (null resource1)))

      ;; Should fail when pool full
      (let ((resource2 (pool:try-acquire pool)))
        (assert-true (null resource2)))

      ;; Release and try again
      (pool:release pool resource1)
      (let ((resource3 (pool:try-acquire pool)))
        (assert-true (not (null resource3)))
        (pool:release pool resource3)))

    (pool:destroy-pool pool)))

;;;; Pool Statistics Tests

(deftest test-pool-statistics ()
  "Test pool statistics tracking"
  (let ((pool (pool:create-pool :factory #'make-test-resource
                             :destroyer #'destroy-test-resource))
        (*test-resource-counter* 0))

    (let ((stats-before (pool:pool-stats pool)))
      (assert-= (pool:pool-stats-created stats-before) 0)
      (assert-= (pool:pool-stats-acquired stats-before) 0)
      (assert-= (pool:pool-stats-released stats-before) 0))

    ;; Acquire and release
    (let ((resource (pool:acquire pool)))
      (pool:release pool resource))

    (let ((stats-after (pool:pool-stats pool)))
      (assert-= (pool:pool-stats-created stats-after) 1)
      (assert-= (pool:pool-stats-acquired stats-after) 1)
      (assert-= (pool:pool-stats-released stats-after) 1))

    (pool:destroy-pool pool)))

;;;; Validation Tests

(deftest test-pool-validation ()
  "Test resource validation on acquire/release"
  (let ((validation-calls 0)
        (should-validate t))

    (flet ((test-validator (resource)
             (incf validation-calls)
             (and should-validate (validate-test-resource resource))))

      (let ((pool (pool:create-pool :factory #'make-test-resource
                                 :validator #'test-validator
                                 :validation-on-acquire t
                                 :validation-on-release t))
            (*test-resource-counter* 0))

        ;; Normal operation
        (let ((resource (pool:acquire pool)))
          (assert-true (not (null resource)))
          (pool:release pool resource))

        ;; Should have called validator
        (assert-true (> validation-calls 0))

        ;; Make validation fail
        (setf should-validate nil)
        (setf validation-calls 0)

        ;; Should still get a resource (creates new one when validation fails)
        (let ((resource (pool:acquire pool)))
          (assert-true (not (null resource)))
          (pool:release pool resource))

        (pool:destroy-pool pool)))))

;;;; Error Handling Tests

(deftest test-pool-factory-error ()
  "Test handling of factory function errors"
  (let ((should-fail nil))
    (flet ((failing-factory ()
             (if should-fail
                 (error "Factory failed")
                 (make-test-resource))))

      (let ((pool (pool:create-pool :factory #'failing-factory))
            (*test-resource-counter* 0))

        ;; Normal operation
        (let ((resource (pool:acquire pool)))
          (assert-true (not (null resource)))
          (pool:release pool resource))

        ;; Make factory fail
        (setf should-fail t)

        ;; Should timeout when factory fails
        (assert-condition (error)
          (pool:acquire pool :timeout 1))

        (pool:destroy-pool pool)))))

;;;; Cleanup and Lifecycle Tests

(deftest test-pool-destruction ()
  "Test pool destruction cleans up resources"
  (let ((destroyed-count 0))
    (flet ((counting-destroyer (resource)
             (declare (ignore resource))
             (incf destroyed-count)))

      (let ((pool (pool:create-pool :factory #'make-test-resource
                                 :destroyer #'counting-destroyer
                                 :min-size 3
                                 :warm-up-p t)) ; Ensure resources are created
            (*test-resource-counter* 0))

        ;; Verify pool has resources
        (assert-true (>= (pool:pool-size pool) 3))

        ;; Destroy pool
        (pool:destroy-pool pool)

        ;; Should have destroyed resources
        (assert-true (> destroyed-count 0))))))

(deftest test-pool-warm-up ()
  "Test pool warm-up functionality"
  (let ((pool (pool:create-pool :factory #'make-test-resource
                             :min-size 3
                             :warm-up-p t))
        (*test-resource-counter* 0))

    ;; Pool should be warmed up
    (assert-true (>= (pool:pool-size pool) 3))

    (pool:destroy-pool pool)))

;;;; Concurrency Tests

(deftest test-pool-concurrent-acquire-release ()
  "Test concurrent acquire/release from multiple threads.
   Verifies pool stats match and no resources leak."
  (let* ((pool (pool:create-pool :factory #'make-test-resource
                              :destroyer #'destroy-test-resource
                              :validator #'validate-test-resource
                              :max-size 5))
         (*test-resource-counter* 0)
         (n-threads 8)
         (iterations-per-thread 50)
         (completed 0)
         (completed-lock (epsilon.sys.lock:make-lock "completed-lock"))
         (threads nil))
    (dotimes (i n-threads)
      (push (epsilon.sys.thread:make-thread
             (lambda ()
               (dotimes (j iterations-per-thread)
                 (let ((r (pool:acquire pool :timeout 5)))
                   (when r
                     (sleep 0.001)
                     (pool:release pool r)
                     (epsilon.sys.lock:with-lock (completed-lock)
                       (incf completed))))))
             :name (format nil "pool-worker-~D" i))
            threads))
    ;; Wait for all threads
    (dolist (th threads)
      (epsilon.sys.thread:join-thread th))
    ;; All iterations should have completed
    (assert-= completed (* n-threads iterations-per-thread))
    ;; Pool should have no resources in use
    (let ((stats (pool:pool-stats pool)))
      (assert-= (pool:pool-stats-acquired stats) (pool:pool-stats-released stats)))
    (pool:destroy-pool pool)))

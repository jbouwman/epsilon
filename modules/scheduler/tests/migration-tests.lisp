;;;; Tests for Stage 5: Channel and Primitive Migration
;;;;
;;;; Verifies that spawn, async, timeout-channel, and run-blocking work
;;;; correctly with the coroutine scheduler (via *task-run-hook*).

(defpackage :epsilon.scheduler.migration-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.scheduler sched)
            (epsilon.scheduler.coroutine coro)
            (epsilon.channel channel)
   (epsilon.sys.lock lock)
   (epsilon.sys.thread thread)))

;;; ============================================================================
;;; spawn inside scheduler
;;; ============================================================================

(deftest test-spawn-creates-coroutine
  "spawn inside with-scheduler creates a coroutine, not an OS thread."
  (let ((result nil))
    (sched:with-scheduler (s :num-carriers 2)
      (channel:spawn (setf result :from-coroutine)))
    (assert-eq :from-coroutine result)))

(deftest test-spawn-many-coroutines
  "1000 spawns inside scheduler all complete without creating 1000 threads."
  (let ((completed 0)
        (lock (lock:make-lock)))
    (sched:with-scheduler (s :num-carriers 4)
      (dotimes (i 1000)
        (channel:spawn
          (lock:with-lock (lock)
            (incf completed)))))
    (assert-= 1000 completed)))

(deftest test-spawn-outside-scheduler
  "spawn outside a scheduler still creates an OS thread (fallback)."
  (let ((result nil)
        (lock (lock:make-lock))
        (cv (lock:make-condition-variable)))
    (let ((thread
            (channel:spawn
              (lock:with-lock (lock)
                (setf result :from-thread)
                (lock:condition-notify cv)))))
      ;; Should return a thread object
      (assert-true (typep thread 'thread:thread))
      (lock:with-lock (lock)
        (loop until result
              do (lock:condition-wait cv lock :timeout 1)))
      (assert-eq :from-thread result)
      (thread:join-thread thread))))

(deftest test-spawn-nested-inside-scheduler
  "Spawning from within a spawned coroutine works."
  (let ((result nil))
    (sched:with-scheduler (s :num-carriers 2)
      (channel:spawn
        (channel:spawn
          (setf result :nested))))
    (assert-eq :nested result)))

;;; ============================================================================
;;; async/await inside scheduler
;;; ============================================================================

(deftest test-async-creates-coroutine
  "async inside scheduler creates a coroutine-backed promise."
  (let ((result nil))
    (sched:with-scheduler (s :num-carriers 2)
      (let ((p (channel:async (+ 1 2 3))))
        ;; await blocks the main thread until promise completes
        (setf result (channel:await p))))
    (assert-= 6 result)))

(deftest test-async-error-propagation
  "Errors in async coroutines propagate through await."
  (sched:with-scheduler (s :num-carriers 2)
    (let ((p (channel:async (error "test-error"))))
      (assert-condition (simple-error)
        (channel:await p)))))

(deftest test-multiple-async-promises
  "Multiple async promises all resolve inside scheduler."
  (let ((results nil))
    (sched:with-scheduler (s :num-carriers 4)
      (let ((promises (loop for i from 1 to 10
                            collect (let ((n i))
                                      (channel:async (* n n))))))
        (setf results (mapcar #'channel:await promises))))
    (assert-equal '(1 4 9 16 25 36 49 64 81 100) results)))

;;; ============================================================================
;;; run-blocking
;;; ============================================================================

(deftest test-run-blocking-basic
  "run-blocking offloads work to a thread and returns its value."
  (let ((result nil))
    (sched:with-scheduler (s :num-carriers 2)
      (sched:scheduler-submit s
        (lambda ()
          (setf result (sched:run-blocking (lambda () (* 7 6)))))))
    (assert-= 42 result)))

(deftest test-run-blocking-result-is-returned
  "run-blocking returns the worker function's value, available in-place
after the call returns."
  (let ((result nil))
    (sched:with-scheduler (s :num-carriers 2)
      (sched:scheduler-submit s
        (lambda ()
          (setf result (sched:run-blocking (lambda () :done))))))
    (assert-eq :done result)))

(deftest test-run-blocking-error
  "run-blocking re-signals errors from the worker thread."
  (let ((caught nil))
    (sched:with-scheduler (s :num-carriers 2)
      (sched:scheduler-submit s
        (lambda ()
          (handler-case
              (sched:run-blocking (lambda () (error "worker-error")))
            (error (e)
              (setf caught (princ-to-string e)))))))
    (assert-true (search "worker-error" (or caught "")))))

(deftest test-run-blocking-concurrent
  "Multiple coroutines using run-blocking concurrently."
  (let ((n 50)
        (completed 0)
        (lock (lock:make-lock)))
    (sched:with-scheduler (s :num-carriers 4)
      (dotimes (i n)
        (sched:scheduler-submit s
          (lambda ()
            (sched:run-blocking (lambda () (sleep 0.001)))
            (lock:with-lock (lock)
              (incf completed))))))
    (assert-= n completed)))

;;; ============================================================================
;;; Channel operations inside scheduler
;;; ============================================================================

(deftest test-channel-basic-inside-scheduler
  "Basic channel send/receive works inside the scheduler."
  (let ((result nil))
    (sched:with-scheduler (s :num-carriers 2)
      (let ((ch (channel:make-channel :capacity 1)))
        ;; Spawn a producer coroutine
        (channel:spawn (channel:send ch :hello))
        ;; Give it a moment to run
        (sleep 0.05)
        ;; Receive from main thread
        (multiple-value-bind (val ok) (channel:receive ch)
          (when ok (setf result val)))))
    (assert-eq :hello result)))

(deftest test-channel-producer-consumer-coroutines
  "Producer and consumer coroutines communicate via channel using try ops."
  (let ((results nil)
        (lock (lock:make-lock))
        (done (list nil)))
    (sched:with-scheduler (s :num-carriers 4)
      (let ((ch (channel:make-channel :capacity 10)))
        ;; Producer coroutine: send all values, then close
        (channel:spawn
          (dotimes (i 5)
            (channel:send ch i))
          (channel:close-channel ch))
        ;; Give producer time to run and fill the buffer
        (sleep 0.05)
        ;; Consumer coroutine: receive all values using try-receive
        (channel:spawn
          (loop
            (multiple-value-bind (val ok) (channel:try-receive ch)
              (cond
                (ok
                 (lock:with-lock (lock)
                   (push val results)))
                ((channel:channel-closed-p ch)
                 (setf (car done) t)
                 (return))
                (t (sleep 0.001))))))))
    (assert-true (car done))
    (assert-= 5 (length results))
    (assert-equal '(0 1 2 3 4) (sort results #'<))))

;;; ============================================================================
;;; timeout-channel inside scheduler
;;; ============================================================================

(deftest test-timeout-channel-inside-scheduler
  "timeout-channel works inside scheduler (uses coroutine instead of thread)."
  (let ((ch nil))
    (sched:with-scheduler (s :num-carriers 2)
      (setf ch (channel:timeout-channel 0.05))
      (sleep 0.1))
    (assert-true (channel:channel-closed-p ch))))

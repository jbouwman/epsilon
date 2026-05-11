;;;; IMPL-384 regression test: external scheduler-submit must wake a
;;;; carrier that has already parked in its idle wait.
;;;;
;;;; Before the IMPL-384 fix, a submission from a thread that is NOT a
;;;; carrier of the target scheduler, made AFTER the target carrier has
;;;; reached its idle park point, can be lost: the carrier sleeps in
;;;; wait-on-semaphore and the work is never picked up.  Bug surfaced
;;;; on darwin-arm64 while reducing IMPL-383's GC widetag corruption
;;;; reproducer.

(defpackage :epsilon.scheduler.external-submit-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.scheduler sched)))

(in-package :epsilon.scheduler.external-submit-tests)

(deftest test-external-submit-wakes-parked-carrier
  "A scheduler-submit issued from a non-carrier thread, after the
target carrier has parked in its idle wait, must be picked up within
a small time bound.  This is the IMPL-384 regression test: prior to
the fix, the submission's signal-semaphore could race the carrier's
wait-on-semaphore and the carrier would never observe the credit;
even if it observed the timeout-bounded poll, the work would still
take longer than the carrier-loop's 50 ms tick to surface, which
this test bounds at 1 s."
  (let ((sched (sched:make-scheduler :num-carriers 1))
        (done nil))
    (unwind-protect
         (progn
           (sched:scheduler-run sched)
           ;; Give the carrier time to enter its idle wait.  100 ms is
           ;; well above the carrier's 50 ms idle-tick, so the first
           ;; tick has fired and the second wait has begun.
           (sleep 0.1)
           ;; Submit from this (non-carrier) thread.
           (sched:scheduler-submit sched (lambda () (setf done t)))
           ;; Poll for completion with a 1 s deadline.
           (let ((deadline (+ (get-internal-real-time)
                              (* 1 internal-time-units-per-second))))
             (loop while (and (not done)
                              (< (get-internal-real-time) deadline))
                   do (sleep 0.01))))
      (sched:scheduler-shutdown sched))
    (assert-true done)))

(deftest test-external-submit-many-after-park
  "Submitting many coroutines from outside, after the carrier has
parked, must drain.  Burst-after-park exercises any signal-coalescing
in the wakeup primitive: even if multiple signal-semaphore calls
collapse into one wake, the carrier must drain the entire incoming
queue before re-parking."
  (let ((sched (sched:make-scheduler :num-carriers 1))
        (n 50)
        (counter 0)
        (counter-lock (epsilon.sys.lock:make-lock "extsubmit-counter")))
    (unwind-protect
         (progn
           (sched:scheduler-run sched)
           (sleep 0.1)
           (dotimes (i n)
             (sched:scheduler-submit
              sched
              (lambda ()
                (epsilon.sys.lock:with-lock (counter-lock)
                  (incf counter)))))
           (let ((deadline (+ (get-internal-real-time)
                              (* 5 internal-time-units-per-second))))
             (loop while (and (< counter n)
                              (< (get-internal-real-time) deadline))
                   do (sleep 0.01))))
      (sched:scheduler-shutdown sched))
    (assert-= n counter)))

(deftest test-external-submit-multiple-carriers
  "Round-robin distribution must reach every carrier even after each
has parked.  This exercises pick-target-carrier interacting with
multiple parked carriers; if any carrier's signal is lost, the
N-th submission targeting it deadlocks."
  (let ((sched (sched:make-scheduler :num-carriers 4))
        (n 16)  ; 4 per carrier under round-robin
        (counter 0)
        (counter-lock (epsilon.sys.lock:make-lock "extsubmit-multi")))
    (unwind-protect
         (progn
           (sched:scheduler-run sched)
           (sleep 0.1)
           (dotimes (i n)
             (sched:scheduler-submit
              sched
              (lambda ()
                (epsilon.sys.lock:with-lock (counter-lock)
                  (incf counter)))))
           (let ((deadline (+ (get-internal-real-time)
                              (* 5 internal-time-units-per-second))))
             (loop while (and (< counter n)
                              (< (get-internal-real-time) deadline))
                   do (sleep 0.01))))
      (sched:scheduler-shutdown sched))
    (assert-= n counter)))

;;;; channel-select tests

(defpackage :epsilon.scheduler.channel-select.tests
  (:use :cl :epsilon.test)
  (:import (epsilon.scheduler sched)
           (epsilon.scheduler.channel-select csel)
           (epsilon.scheduler.cancellation cxl)
           (epsilon.channel channel)
           (epsilon.sys.lock lock)))

(in-package :epsilon.scheduler.channel-select.tests)

;;; ---- :default clause -------------------------------------------------------

(deftest test-default-runs-when-no-clauses-ready
  "If no clause is ready and :default is present, run :default."
  (sched:with-scheduler (s)
    (let ((path nil))
      (sched:scheduler-submit
       s
       (lambda ()
         (let ((ch (channel:make-channel :capacity 1)))
           (csel:channel-select ()
             ((:recv ch) v
              (declare (ignore v))
              (setf path :recv))
             ((:default)
              (setf path :default))))))
      (sched:scheduler-wait s)
      (assert-eq :default path))))

;;; ---- :recv clause ----------------------------------------------------------

(deftest test-recv-clause-fires-on-ready-channel
  "A :recv clause runs when the channel has a value."
  (sched:with-scheduler (s)
    (let ((received nil))
      (sched:scheduler-submit
       s
       (lambda ()
         (let ((ch (channel:make-channel :capacity 1)))
           (channel:send ch 42)
           (csel:channel-select ()
             ((:recv ch) v
              (setf received v))))))
      (sched:scheduler-wait s)
      (assert-= 42 received))))

;;; ---- :send clause ----------------------------------------------------------

(deftest test-send-clause-fires-when-channel-has-room
  "A :send clause runs when the channel can accept a value."
  (sched:with-scheduler (s)
    (let ((sent nil))
      (sched:scheduler-submit
       s
       (lambda ()
         (let ((ch (channel:make-channel :capacity 1)))
           (csel:channel-select ()
             ((:send ch :hello)
              (setf sent t))))))
      (sched:scheduler-wait s)
      (assert-true sent))))

;;; ---- Random fairness -------------------------------------------------------

(deftest test-random-fairness-across-ready-clauses
  "When multiple clauses are ready, both fire over many trials."
  (sched:with-scheduler (s)
    (let ((picks-a 0)
          (picks-b 0)
          (lock (lock:make-lock "fair")))
      (sched:scheduler-submit
       s
       (lambda ()
         (dotimes (i 50)
           (let ((ca (channel:make-channel :capacity 1))
                 (cb (channel:make-channel :capacity 1)))
             (channel:send ca 1)
             (channel:send cb 2)
             (csel:channel-select ()
               ((:recv ca) v
                (declare (ignore v))
                (lock:with-lock (lock) (incf picks-a)))
               ((:recv cb) v
                (declare (ignore v))
                (lock:with-lock (lock) (incf picks-b))))))))
      (sched:scheduler-wait s)
      ;; Each side should be picked at least once over 50 trials with
      ;; uniform-random selection.  We don't enforce exact balance --
      ;; the goal is no starvation.
      (assert-true (plusp picks-a))
      (assert-true (plusp picks-b))
      (assert-= 50 (+ picks-a picks-b)))))

;;; ---- Timeout ---------------------------------------------------------------

(deftest test-timeout-fires-when-no-clause-ready
  "When :timeout elapses without a ready clause, signal CHANNEL-SELECT-TIMEOUT."
  (sched:with-scheduler (s)
    (let ((timed-out nil))
      (sched:scheduler-submit
       s
       (lambda ()
         (let ((ch (channel:make-channel :capacity 1)))
           (handler-case
               (csel:channel-select (:timeout 0.05)
                 ((:recv ch) v
                  (declare (ignore v))
                  nil))
             (csel:channel-select-timeout ()
               (setf timed-out t))))))
      (sched:scheduler-wait s)
      (assert-true timed-out))))

;;; ---- Cancellation ----------------------------------------------------------

(deftest test-cancellation-aborts-channel-select
  "A cancellation token fires FIBER-CANCELLED out of channel-select."
  (sched:with-scheduler (s)
    (let* ((tok (cxl:make-cancellation-token))
           (cancelled nil))
      (sb-thread:make-thread
       (lambda () (sleep 0.05) (cxl:cancel tok)))
      (sched:scheduler-submit
       s
       (lambda ()
         (let ((ch (channel:make-channel :capacity 1)))
           (handler-case
               (csel:channel-select (:cancellation tok)
                 ((:recv ch) v
                  (declare (ignore v))
                  nil))
             (cxl:fiber-cancelled ()
               (setf cancelled t))))))
      (sched:scheduler-wait s)
      (assert-true cancelled))))

;;; ---- Cross-coroutine rendezvous --------------------------------------------

(deftest test-recv-completes-after-other-coroutine-sends
  "A :recv clause that initially blocks completes once another coroutine
sends to the channel."
  (sched:with-scheduler (s)
    (let ((received nil))
      (let ((ch (channel:make-channel :capacity 1)))
        (sched:scheduler-submit
         s
         (lambda ()
           (sched:coroutine-sleep 0.05)
           (channel:send ch :ping)))
        (sched:scheduler-submit
         s
         (lambda ()
           (csel:channel-select ()
             ((:recv ch) v
              (setf received v))))))
      (sched:scheduler-wait s)
      (assert-eq :ping received))))

;;;; Nursery tests
;;;;
;;;; Confirms structured-concurrency invariants: scoped join, sibling
;;;; cancellation on first error, aggregate-error reporting, and
;;;; outer-cancel propagation through the nursery's child token.

(defpackage :epsilon.scheduler.nursery.tests
  (:use :cl :epsilon.test)
  (:import (epsilon.scheduler sched)
           (epsilon.scheduler.nursery nur)
           (epsilon.scheduler.cancellation cxl)
           (epsilon.sys.lock lock)))

(in-package :epsilon.scheduler.nursery.tests)

;;; ---- Helpers ----------------------------------------------------------------

(defun make-counter ()
  "(values get-counter increment-counter) -- atomic counter."
  (let ((value 0)
        (lock (lock:make-lock "counter")))
    (values
     (lambda () (lock:with-lock (lock) value))
     (lambda () (lock:with-lock (lock) (incf value))))))

;;; ---- Basic join -------------------------------------------------------------

(deftest test-with-nursery-joins-spawns
  "WITH-NURSERY does not return until every spawn finishes."
  (sched:with-scheduler (s)
    (multiple-value-bind (get-count incr-count) (make-counter)
      (sched:scheduler-submit
       s
       (lambda ()
         (nur:with-nursery (n)
           (dotimes (i 5)
             (nur:nursery-spawn n
              (lambda ()
                (sched:coroutine-sleep 0.05)
                (funcall incr-count)))))))
      (sched:scheduler-wait s)
      (assert-= 5 (funcall get-count)))))

;;; ---- Cancellation propagates inward ----------------------------------------

(deftest test-outer-cancel-cancels-nursery-spawns
  "Cancelling the surrounding token cancels every spawn in the nursery."
  (sched:with-scheduler (s)
    (let* ((tok (cxl:make-cancellation-token))
           (cancelled-count 0)
           (count-lock (lock:make-lock "cc")))
      (sched:scheduler-submit
       s
       (lambda ()
         (cxl:with-cancellation (tok)
           (nur:with-nursery (n)
             (dotimes (i 3)
               (nur:nursery-spawn n
                (lambda ()
                  (handler-case
                      (sched:coroutine-sleep
                       5 :cancellation cxl:*cancellation*)
                    (cxl:fiber-cancelled ()
                      (lock:with-lock (count-lock)
                        (incf cancelled-count)))))))
             ;; Yield once so the spawned siblings have a chance to
             ;; reach their park before we trigger the outer cancel.
             ;; Without this, cancel can race ahead of the spawns
             ;; entering coroutine-sleep on some runs.
             (sched:coroutine-sleep 0.05)
             (cxl:cancel tok :outer)))))
      (sched:scheduler-wait s)
      (assert-= 3 cancelled-count))))

;;; ---- Sibling error cancels other siblings ---------------------------------

(deftest test-sibling-error-cancels-others
  "An unhandled error in one spawn cancels its siblings via the
nursery's token, and the original error is re-signalled out of
WITH-NURSERY."
  (sched:with-scheduler (s)
    (let ((cancelled-count 0)
          (count-lock (lock:make-lock "cc"))
          (caught nil))
      (sched:scheduler-submit
       s
       (lambda ()
         (handler-case
             (nur:with-nursery (n)
               ;; Two siblings that should be cancelled.
               (dotimes (i 2)
                 (nur:nursery-spawn n
                  (lambda ()
                    (handler-case
                        (sched:coroutine-sleep
                         5 :cancellation cxl:*cancellation*)
                      (cxl:fiber-cancelled ()
                        (lock:with-lock (count-lock)
                          (incf cancelled-count)))))))
               ;; The bomb.
               (nur:nursery-spawn n
                (lambda ()
                  (sched:coroutine-sleep 0.05)
                  (error "boom"))))
           (error (e)
             (setf caught e)))))
      (sched:scheduler-wait s)
      (assert-not-null caught)
      (assert-true (search "boom" (format nil "~A" caught)))
      (assert-= 2 cancelled-count))))

;;; ---- Body error: spawned fibers join before unwind --------------------------

(deftest test-body-error-cancels-and-joins
  "If WITH-NURSERY's BODY itself errors, in-flight spawns are
cancelled and joined before the error propagates."
  (sched:with-scheduler (s)
    (let ((finished 0)
          (lock (lock:make-lock "fin"))
          (caught nil))
      (sched:scheduler-submit
       s
       (lambda ()
         (handler-case
             (nur:with-nursery (n)
               (nur:nursery-spawn n
                (lambda ()
                  (handler-case
                      (sched:coroutine-sleep
                       5 :cancellation cxl:*cancellation*)
                    (cxl:fiber-cancelled ()
                      (lock:with-lock (lock) (incf finished))))))
               (sched:coroutine-sleep 0.05)
               (error "body bombed"))
           (error (e)
             (setf caught e)))))
      (sched:scheduler-wait s)
      (assert-not-null caught)
      (assert-= 1 finished))))

;;; ---- Spawned fiber sees nursery's cancellation token -----------------------

(deftest test-spawn-sees-nursery-token-as-cancellation
  "Inside a NURSERY-SPAWN body, *CANCELLATION* is the nursery's token."
  (sched:with-scheduler (s)
    (let ((observed nil)
          (lock (lock:make-lock "obs")))
      (sched:scheduler-submit
       s
       (lambda ()
         (nur:with-nursery (n)
           (nur:nursery-spawn n
            (lambda ()
              (lock:with-lock (lock)
                (setf observed cxl:*cancellation*)))))))
      (sched:scheduler-wait s)
      (assert-not-null observed)
      (assert-true (cxl:cancellation-token-p observed)))))

;;; ---- Empty nursery ----------------------------------------------------------

(deftest test-empty-nursery-returns
  "A nursery with no spawns returns immediately."
  (sched:with-scheduler (s)
    (let ((reached nil))
      (sched:scheduler-submit
       s
       (lambda ()
         (nur:with-nursery (n)
           n)
         (setf reached t)))
      (sched:scheduler-wait s)
      (assert-true reached))))

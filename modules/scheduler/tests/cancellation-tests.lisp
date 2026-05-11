;;;; Cancellation token tests
;;;;
;;;; Token tree behaviour, with-cancellation/with-deadline macros, and
;;;; integration with COROUTINE-SLEEP / COROUTINE-WAIT-UNTIL via
;;;; FIBER-CANCELLED signalling.

(defpackage :epsilon.scheduler.cancellation.tests
  (:use :cl :epsilon.test)
  (:import (epsilon.scheduler sched)
           (epsilon.scheduler.cancellation cxl)
           (epsilon.sys.lock lock)
           (epsilon.sys.thread thread)))

(in-package :epsilon.scheduler.cancellation.tests)

;;; ---- Token tree -------------------------------------------------------------

(deftest test-token-starts-active
  "A fresh token is not cancelled."
  (let ((tok (cxl:make-cancellation-token)))
    (assert-true (not (cxl:cancellation-requested-p tok)))))

(deftest test-cancel-marks-token
  "Cancel sets the token's state to :cancelled."
  (let ((tok (cxl:make-cancellation-token)))
    (cxl:cancel tok)
    (assert-true (cxl:cancellation-requested-p tok))))

(deftest test-cancel-is-idempotent
  "Repeated cancels are no-ops; reason from the first call sticks."
  (let ((tok (cxl:make-cancellation-token)))
    (cxl:cancel tok :first)
    (cxl:cancel tok :second)
    (assert-eq :first (cxl:cancellation-token-reason tok))))

(deftest test-cancel-parent-cancels-children
  "Cancelling a parent walks the tree."
  (let* ((root  (cxl:make-cancellation-token))
         (mid   (cxl:make-cancellation-token root))
         (leaf  (cxl:make-cancellation-token mid))
         (sibling (cxl:make-cancellation-token root)))
    (cxl:cancel root :stop)
    (assert-true (cxl:cancellation-requested-p root))
    (assert-true (cxl:cancellation-requested-p mid))
    (assert-true (cxl:cancellation-requested-p leaf))
    (assert-true (cxl:cancellation-requested-p sibling))))

(deftest test-cancel-child-leaves-parent-active
  "Cancelling a child does not affect the parent."
  (let* ((root (cxl:make-cancellation-token))
         (child (cxl:make-cancellation-token root)))
    (cxl:cancel child)
    (assert-true (cxl:cancellation-requested-p child))
    (assert-true (not (cxl:cancellation-requested-p root)))))

(deftest test-child-of-cancelled-parent-born-cancelled
  "A token created under an already-cancelled parent inherits the state."
  (let* ((root (cxl:make-cancellation-token)))
    (cxl:cancel root :pre)
    (let ((child (cxl:make-cancellation-token root)))
      (assert-true (cxl:cancellation-requested-p child))
      (assert-eq :pre (cxl:cancellation-token-reason child)))))

;;; ---- with-cancellation / *cancellation* -------------------------------------

(deftest test-with-cancellation-binds-ambient
  "Within WITH-CANCELLATION, *CANCELLATION* sees the token."
  (let ((tok (cxl:make-cancellation-token)))
    (cxl:with-cancellation (tok)
      (assert-eq tok cxl:*cancellation*))))

;;; ---- signal-cancelled-if-requested ------------------------------------------

(deftest test-signal-cancelled-if-requested-no-op-active
  "Calling SIGNAL-CANCELLED-IF-REQUESTED on an active token does nothing."
  (let ((tok (cxl:make-cancellation-token)))
    (cxl:signal-cancelled-if-requested tok)  ; should not signal
    (assert-true t)))

(deftest test-signal-cancelled-if-requested-fires
  "Calling on a cancelled token signals FIBER-CANCELLED."
  (let ((tok (cxl:make-cancellation-token)))
    (cxl:cancel tok :testing)
    (handler-case
        (progn
          (cxl:signal-cancelled-if-requested tok)
          (assert-true nil))
      (cxl:fiber-cancelled (c)
        (assert-eq tok (cxl:fiber-cancelled-token c))
        (assert-eq :testing (cxl:fiber-cancelled-reason c))))))

;;; ---- fiber-cancelled is not caught by (handler-case ... (error () ...)) ----

(deftest test-fiber-cancelled-not-error
  "FIBER-CANCELLED is not a subtype of ERROR."
  (let ((tok (cxl:make-cancellation-token)))
    (cxl:cancel tok)
    ;; Generic (error () ...) handler should NOT catch fiber-cancelled.
    (let ((caught-as-error nil)
          (caught-as-cancelled nil))
      (handler-case
          (handler-case
              (cxl:signal-cancelled-if-requested tok)
            (error () (setf caught-as-error t)))
        (cxl:fiber-cancelled () (setf caught-as-cancelled t)))
      (assert-true (not caught-as-error))
      (assert-true caught-as-cancelled))))

;;; ---- Integration with the scheduler -----------------------------------------

(deftest test-coroutine-sleep-cancels-promptly
  "A coroutine sleeping with :cancellation wakes with FIBER-CANCELLED
within scheduler-loop latency of (cancel token)."
  (sched:with-scheduler (s)
    (let* ((tok (cxl:make-cancellation-token))
           (cancelled nil)
           (cancel-lock (lock:make-lock "cancel"))
           (done (lock:make-condition-variable :name "done")))
      (sched:scheduler-submit
       s
       (lambda ()
         (handler-case
             (sched:coroutine-sleep 5 :cancellation tok)
           (cxl:fiber-cancelled ()
             (lock:with-lock (cancel-lock)
               (setf cancelled t)
               (lock:condition-broadcast done))))))
      ;; Give the coroutine a moment to enter park.
      (sleep 0.1)
      (cxl:cancel tok :testing)
      (let ((deadline (+ (get-internal-real-time)
                         (* 5 internal-time-units-per-second))))
        (lock:with-lock (cancel-lock)
          (loop until (or cancelled
                          (>= (get-internal-real-time) deadline))
                do (lock:condition-wait done cancel-lock :timeout 0.5))))
      (assert-true cancelled))))

(deftest test-coroutine-wait-until-cancels
  "COROUTINE-WAIT-UNTIL with :cancellation is interruptible."
  (sched:with-scheduler (s)
    (let* ((tok (cxl:make-cancellation-token))
           (cancelled nil)
           (cancel-lock (lock:make-lock "cancel"))
           (done (lock:make-condition-variable :name "done")))
      (sched:scheduler-submit
       s
       (lambda ()
         (handler-case
             (sched:coroutine-wait-until (lambda () nil)  ; never true
                                          :timeout 5.0
                                          :cancellation tok)
           (cxl:fiber-cancelled ()
             (lock:with-lock (cancel-lock)
               (setf cancelled t)
               (lock:condition-broadcast done))))))
      (sleep 0.1)
      (cxl:cancel tok)
      (let ((deadline (+ (get-internal-real-time)
                         (* 5 internal-time-units-per-second))))
        (lock:with-lock (cancel-lock)
          (loop until (or cancelled
                          (>= (get-internal-real-time) deadline))
                do (lock:condition-wait done cancel-lock :timeout 0.5))))
      (assert-true cancelled))))

;;; ---- with-deadline ----------------------------------------------------------

(deftest test-with-deadline-cancels-after-elapsed
  "WITH-DEADLINE cancels its token after the timeout elapses."
  (sched:with-scheduler (s)
    (let* ((cancelled nil)
           (cancel-lock (lock:make-lock "cancel"))
           (done (lock:make-condition-variable :name "done")))
      (sched:scheduler-submit
       s
       (lambda ()
         (handler-case
             (cxl:with-deadline (0.1)
               (sched:coroutine-sleep 5
                                       :cancellation cxl:*cancellation*))
           (cxl:fiber-cancelled (c)
             (declare (ignore c))
             (lock:with-lock (cancel-lock)
               (setf cancelled t)
               (lock:condition-broadcast done))))))
      (let ((deadline (+ (get-internal-real-time)
                         (* 5 internal-time-units-per-second))))
        (lock:with-lock (cancel-lock)
          (loop until (or cancelled
                          (>= (get-internal-real-time) deadline))
                do (lock:condition-wait done cancel-lock :timeout 0.5))))
      (assert-true cancelled))))

;;; ---- Spawned coroutine inherits ambient cancellation -----------------------

(deftest test-spawn-inherits-ambient-cancellation
  "A coroutine spawned under a bound *cancellation* sees that token
inside its body."
  (sched:with-scheduler (s)
    (let* ((tok (cxl:make-cancellation-token))
           (observed nil)
           (lock-obs (lock:make-lock "obs"))
           (done (lock:make-condition-variable :name "done")))
      (cxl:with-cancellation (tok)
        (sched:scheduler-submit
         s
         (lambda ()
           (lock:with-lock (lock-obs)
             (setf observed cxl:*cancellation*)
             (lock:condition-broadcast done)))))
      (let ((deadline (+ (get-internal-real-time)
                         (* 5 internal-time-units-per-second))))
        (lock:with-lock (lock-obs)
          (loop until (or observed
                          (>= (get-internal-real-time) deadline))
                do (lock:condition-wait done lock-obs :timeout 0.5))))
      (assert-eq tok observed))))

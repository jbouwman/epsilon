;;;; Tests for epsilon.scheduler.io-wait -- fd-based parking.
;;;;
;;;; These tests use a stub reactor (a plain function stored in
;;;; *fd-wait-register*) so they don't require the platform reactor.
;;;; End-to-end integration with a real reactor is covered indirectly
;;;; by the proxy's TLS tests.

(defpackage :epsilon.scheduler.io-wait-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.scheduler sched)
            (epsilon.scheduler.coroutine coro)
            (epsilon.scheduler.io-wait io-wait)
   (epsilon.sys.lock lock)))

(defstruct stub-reactor
  (pending-callbacks nil :type list)
  (lock (lock:make-lock "stub-reactor") :type lock:lock))

(defun stub-registrar (reactor)
  "Return a function suitable for *fd-wait-register* that records calls
on REACTOR."
  (lambda (fd events cb)
    (lock:with-lock ((stub-reactor-lock reactor))
      (push (list fd events cb) (stub-reactor-pending-callbacks reactor)))))

(defun drain-and-fire (reactor event)
  "Invoke every pending callback with EVENT.  Returns the number fired."
  (let ((entries (lock:with-lock ((stub-reactor-lock reactor))
                   (prog1 (stub-reactor-pending-callbacks reactor)
                     (setf (stub-reactor-pending-callbacks reactor) nil)))))
    (dolist (entry entries)
      (funcall (third entry) event))
    (length entries)))

;;; ============================================================================
;;; park-on-fd error paths
;;; ============================================================================

(defmacro with-stub-register ((reactor-var) &body body)
  "Install a stub reactor as *fd-wait-register* for the duration of
BODY and restore it afterward.  Binds REACTOR-VAR to the stub so the
body can drain and fire pending callbacks; some tests don't need to
touch it directly so REACTOR-VAR is declared IGNORABLE at its binding
scope."
  `(let* ((,reactor-var (make-stub-reactor))
          (old-register io-wait:*fd-wait-register*))
     (declare (ignorable ,reactor-var))
     (unwind-protect
          (progn
            (setf io-wait:*fd-wait-register* (stub-registrar ,reactor-var))
            ,@body)
       (setf io-wait:*fd-wait-register* old-register))))

(deftest test-park-on-fd-without-register
  "park-on-fd signals an error when *fd-wait-register* is NIL."
  (let ((old-register io-wait:*fd-wait-register*)
        (caught nil))
    (unwind-protect
         (progn
           (setf io-wait:*fd-wait-register* nil)
           (sched:with-scheduler (s :num-carriers 1)
             (sched:scheduler-submit s
               (lambda ()
                 (handler-case
                     (io-wait:park-on-fd 0 '(:in) :timeout 0.01)
                   (error (e)
                     (setf caught (princ-to-string e))))))))
      (setf io-wait:*fd-wait-register* old-register))
    (assert-not-null caught)
    (assert-true (search "fd-wait-register" caught))))

(deftest test-park-on-fd-outside-coroutine
  "park-on-fd signals an error when called outside any coroutine."
  (let ((old-register io-wait:*fd-wait-register*)
        (caught nil))
    (unwind-protect
         (progn
           (setf io-wait:*fd-wait-register*
                 (lambda (fd events cb) (declare (ignore fd events cb)) nil))
           (handler-case
               (io-wait:park-on-fd 0 '(:in) :timeout 0.01)
             (error (e) (setf caught (princ-to-string e)))))
      (setf io-wait:*fd-wait-register* old-register))
    (assert-not-null caught)
    (assert-true (search "outside a coroutine" caught))))

;;; ============================================================================
;;; park-on-fd timeout and callback paths
;;; ============================================================================

(deftest test-park-on-fd-timeout
  "park-on-fd returns NIL when the timeout fires before any callback."
  (with-stub-register (reactor)
    (let ((result :unset))
      (sched:with-scheduler (s :num-carriers 1)
        (sched:scheduler-submit s
          (lambda ()
            (setf result (io-wait:park-on-fd 42 '(:in) :timeout 0.05)))))
      (assert-nil result))))

(deftest test-park-on-fd-wakes-on-callback
  "park-on-fd returns T when the reactor callback fires before timeout."
  (with-stub-register (reactor)
    (let ((result :unset))
      (sched:with-scheduler (s :num-carriers 1)
        (sched:scheduler-submit s
          (lambda ()
            (setf result (io-wait:park-on-fd 99 '(:in) :timeout 2.0))))
        (sleep 0.05)
        (drain-and-fire reactor :dummy-event))
      (assert-true result))))

;;;; Structured Concurrency -- Nurseries
;;;;
;;;; A nursery is a scoped fan-out container: WITH-NURSERY guarantees
;;;; that no spawned fiber outlives the lexical scope.  Its token is a
;;;; child of the ambient *CANCELLATION*, so an outer cancel propagates
;;;; inward; an unhandled error in any sibling fires the nursery's
;;;; token to cancel the rest, and the primary error is re-signalled
;;;; out of WITH-NURSERY after every sibling has joined.

(defpackage :epsilon.scheduler.nursery
  (:use :cl)
  (:import (epsilon.scheduler sched)
           (epsilon.scheduler.coroutine coro)
           (epsilon.scheduler.cancellation cxl)
           (epsilon.sys.lock lock)
           (epsilon.sys.semaphore sem))
  (:export
   #:nursery
   #:make-nursery
   #:nursery-p
   #:nursery-token
   #:with-nursery
   #:nursery-spawn
   #:nursery-await-all
   #:nursery-error-aggregate
   #:nursery-error-aggregate-primary
   #:nursery-error-aggregate-others))

(in-package :epsilon.scheduler.nursery)

;;; ---------------------------------------------------------------------------
;;; Aggregate-error condition
;;; ---------------------------------------------------------------------------

(define-condition nursery-error-aggregate (error)
  ((primary :initarg :primary :reader nursery-error-aggregate-primary)
   (others :initarg :others :reader nursery-error-aggregate-others
           :initform nil))
  (:report
   (lambda (c stream)
     (format stream "Nursery: ~A~@[ (and ~D other error~:P)~]"
             (nursery-error-aggregate-primary c)
             (let ((n (length (nursery-error-aggregate-others c))))
               (when (plusp n) n))))))

;;; ---------------------------------------------------------------------------
;;; Nursery
;;; ---------------------------------------------------------------------------

(defstruct (nursery (:constructor %make-nursery))
  "OUTSTANDING is the count of spawned coroutines that haven't yet
finished.  ERRORS is the list of unhandled errors signalled from
spawned bodies (excluding FIBER-CANCELLED, which is the cancellation
mechanism, not an error)."
  (token nil)
  (outstanding 0 :type fixnum)
  (errors nil :type list)
  (cancel-on-error t :type boolean)
  (lock (lock:make-lock "nursery") :type lock:lock)
  (cv (lock:make-condition-variable :name "nursery-cv")))

(defun make-nursery (&key cancel-on-error parent-token)
  "Create a nursery with a fresh cancellation token parented under
PARENT-TOKEN (defaults to the ambient *CANCELLATION*).
CANCEL-ON-ERROR (default T): an unhandled error in any spawned fiber
fires the nursery's token to cancel siblings."
  (%make-nursery :token (cxl:make-cancellation-token
                         (or parent-token cxl:*cancellation*))
                 :cancel-on-error (if cancel-on-error t nil)))

;;; ---------------------------------------------------------------------------
;;; Spawning
;;; ---------------------------------------------------------------------------

(defun %record-spawn-finished (nursery error-or-nil)
  "Decrement OUTSTANDING and, if the spawn errored, record the error
and (when CANCEL-ON-ERROR) fire the nursery's token."
  (lock:with-lock ((nursery-lock nursery))
    (when error-or-nil
      (push error-or-nil (nursery-errors nursery))
      (when (nursery-cancel-on-error nursery)
        (cxl:cancel (nursery-token nursery) :sibling-error)))
    (decf (nursery-outstanding nursery))
    (when (zerop (nursery-outstanding nursery))
      (lock:condition-broadcast (nursery-cv nursery)))))

(defun %wrap-spawn (nursery fn)
  (lambda ()
    (let ((cxl:*cancellation* (nursery-token nursery))
          (err nil))
      (handler-case
          (funcall fn)
        (cxl:fiber-cancelled ()
          ;; Cooperative cancellation is the protocol, not an error.
          nil)
        (error (e)
          (setf err e)))
      (%record-spawn-finished nursery err))))

(defun nursery-spawn (nursery fn &key name)
  "Submit FN as a fresh coroutine that lives within NURSERY.  Returns
the coroutine.  FN runs with *CANCELLATION* bound to the nursery's
token, so blocking primitives that pass :CANCELLATION cxl:*cancellation*
through pick up an outer or sibling-triggered cancel."
  (declare (ignore name))
  (let ((scheduler sched:*current-scheduler*))
    (unless scheduler
      (error "nursery-spawn: no current scheduler"))
    (lock:with-lock ((nursery-lock nursery))
      (incf (nursery-outstanding nursery)))
    (sched:scheduler-submit scheduler (%wrap-spawn nursery fn))))

;;; ---------------------------------------------------------------------------
;;; Joining
;;; ---------------------------------------------------------------------------

(defun nursery-await-all (nursery)
  "Block until every spawn in NURSERY has finished.  When called from
inside a coroutine, yields the fiber via COROUTINE-WAIT-UNTIL; outside
a coroutine, uses a condition-variable wait so test harnesses can
join from the main thread."
  (cond
    (coro:*current-coroutine*
     (sched:coroutine-wait-until
      (lambda ()
        (lock:with-lock ((nursery-lock nursery))
          (zerop (nursery-outstanding nursery))))))
    (t
     (lock:with-lock ((nursery-lock nursery))
       (loop until (zerop (nursery-outstanding nursery))
             do (lock:condition-wait (nursery-cv nursery)
                                     (nursery-lock nursery)
                                     :timeout 0.5)))))
  nil)

(defun %signal-aggregate-if-any (nursery)
  (let ((errs (nursery-errors nursery)))
    (when errs
      (let ((primary (car (last errs)))
            (others (butlast errs)))
        (if others
            (error 'nursery-error-aggregate :primary primary :others others)
            (error primary))))))

(defmacro with-nursery ((var &key (cancel-on-error t)) &body body)
  "Run BODY with VAR bound to a fresh nursery.  Cannot return until
every coroutine spawned via NURSERY-SPAWN has finished.  If any spawn
raises an unhandled error, the first one is re-signalled out of this
form (or wrapped in NURSERY-ERROR-AGGREGATE if multiple siblings
errored concurrently).  An outer cancel propagates inward via the
nursery's child token."
  `(let ((,var (make-nursery :cancel-on-error ,cancel-on-error)))
     (unwind-protect
          (progn
            (cxl:with-cancellation ((nursery-token ,var))
              ,@body)
            (nursery-await-all ,var)
            (%signal-aggregate-if-any ,var))
       ;; If BODY itself signalled, ensure spawned fibers finish before
       ;; we leave scope.  Cancel the nursery so they don't linger.
       (cxl:cancel (nursery-token ,var) :scope-exit)
       (nursery-await-all ,var))))

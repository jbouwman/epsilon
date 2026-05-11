;;;; channel-select -- multi-way rendezvous over EPSILON.CHANNEL
;;;;
;;;; A coroutine-aware CSP `select`.  Each clause is a tuple of
;;;;   (:recv chan)  bind to a fresh receive value
;;;;   (:send chan v) send V on chan
;;;;   (:default)    non-blocking branch (skip parking)
;;;;
;;;; Implementation: each iteration we try every clause non-blocking
;;;; via TRY-SEND / TRY-RECEIVE.  Ready clauses are collected; if any
;;;; are ready, we pick one uniformly at random (Go's fairness rule)
;;;; and run its body.  If none are ready and a :DEFAULT clause is
;;;; present, run it.  Otherwise yield via COROUTINE-SLEEP and re-poll.
;;;;
;;;; This is a polling implementation: the sleep cadence (5 ms) bounds
;;;; wake latency.  A future iteration can wire into the channel's
;;;; wait-queues for O(1) wake (see IMPL-348 followups), but the
;;;; polling design covers the common case correctly and avoids
;;;; touching channel internals.

(defpackage :epsilon.scheduler.channel-select
  (:use :cl)
  (:import (epsilon.scheduler sched)
           (epsilon.scheduler.coroutine coro)
           (epsilon.scheduler.cancellation cxl)
           (epsilon.channel channel))
  (:export
   #:channel-select
   #:channel-select-timeout))

(in-package :epsilon.scheduler.channel-select)

(define-condition channel-select-timeout (condition) ()
  (:documentation
   "Signalled out of CHANNEL-SELECT when its :TIMEOUT elapses without
any clause becoming ready.  Inherits from CONDITION (not ERROR), so
generic ERROR handlers do not catch it -- callers opt in via
HANDLER-CASE on this exact type."))

(defun %execute-ready (ready)
  "Pick one entry from READY at random and call its thunk.  Each entry
is (THUNK VALUE)."
  (let ((entry (nth (random (length ready)) ready)))
    (funcall (first entry) (second entry))))

(defun %expand-clause (clause ready-sym)
  "Expand a single clause to a form that, on the eager-attempt pass,
either pushes a (THUNK VALUE) entry onto READY-SYM or does nothing."
  (let ((head (first clause)))
    (unless (listp head)
      (error "channel-select: clause head must be a list, got ~S" head))
    (case (first head)
      (:recv
       (let ((chan-form (second head))
             (var       (second clause))
             (body      (cddr clause)))
         (let ((c (gensym "C")) (v (gensym "V")) (ok (gensym "OK")))
           `(let ((,c ,chan-form))
              (multiple-value-bind (,v ,ok) (channel:try-receive ,c)
                (when ,ok
                  (push (list (lambda (,var) ,@body) ,v) ,ready-sym)))))))
      (:send
       (let ((chan-form (second head))
             (val-form  (third head))
             (body      (rest clause)))
         (let ((c (gensym "C")) (v (gensym "V")))
           `(let ((,c ,chan-form) (,v ,val-form))
              (when (channel:try-send ,c ,v)
                (push (list (lambda (ignored)
                              (declare (ignore ignored))
                              ,@body)
                            nil)
                      ,ready-sym))))))
      (:default
       ;; Default clauses are handled separately by the caller.
       nil)
      (t
       (error "channel-select: unrecognised clause head ~S" head)))))

(defmacro channel-select ((&key timeout cancellation) &body clauses)
  "Multi-way select over channel send/receive operations.

Clause forms:
  ((:recv CHAN) VAR BODY...)       receive into VAR, run BODY
  ((:send CHAN VALUE) BODY...)     send VALUE, run BODY
  ((:default) BODY...)             non-blocking fallback

Options:
  :TIMEOUT seconds         signal CHANNEL-SELECT-TIMEOUT after the limit
  :CANCELLATION token      signal FIBER-CANCELLED if the token fires

When called inside a coroutine and no clause is immediately ready,
yields the fiber via COROUTINE-SLEEP on a 5 ms cadence.  Outside a
coroutine, busy-waits on a 1 ms cycle.

Random fairness: when multiple clauses are ready in the same poll,
one is selected uniformly at random."
  (let ((default-body
          (let ((dc (find-if (lambda (c)
                               (and (listp (first c))
                                    (eq (caar c) :default)))
                             clauses)))
            (when dc (rest dc))))
        (active (remove-if (lambda (c)
                             (and (listp (first c))
                                  (eq (caar c) :default)))
                           clauses))
        (ready  (gensym "READY"))
        (started (gensym "STARTED"))
        (tmout (gensym "TMOUT"))
        (ctok (gensym "CTOK")))
    `(let ((,tmout ,timeout)
           (,ctok ,cancellation)
           (,started (get-internal-real-time)))
       (block channel-select
         (loop
           (let ((,ready '()))
             ,@(mapcar (lambda (c) (%expand-clause c ready)) active)
             (when ,ready
               (return-from channel-select (%execute-ready ,ready))))
           ,@(when default-body
               `((return-from channel-select (progn ,@default-body))))
           (when ,ctok
             (cxl:signal-cancelled-if-requested ,ctok))
           (when (and ,tmout
                      (>= (- (get-internal-real-time) ,started)
                          (* ,tmout internal-time-units-per-second)))
             (signal 'channel-select-timeout)
             (return-from channel-select nil))
           (cond
             (coro:*current-coroutine*
              (sched:coroutine-sleep 0.005 :cancellation ,ctok))
             (t (sleep 0.001))))))))

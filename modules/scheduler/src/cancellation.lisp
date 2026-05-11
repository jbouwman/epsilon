;;;; Cancellation Tokens -- tree-shaped, propagating cooperative cancellation
;;;;
;;;; A token is a node in a tree.  Cancelling a parent atomically cancels
;;;; every descendant.  Coroutines that pass :CANCELLATION TOKEN to a
;;;; blocking primitive (COROUTINE-PARK, COROUTINE-SLEEP, PARK-ON-FD,
;;;; etc.) wake with FIBER-CANCELLED signalled out of that primitive when
;;;; the token fires.
;;;;
;;;; FIBER-CANCELLED inherits from CL:CONDITION (not CL:ERROR), so it
;;;; bypasses generic (HANDLER-CASE ... (ERROR () ...)) wrappers.  Code
;;;; that wants to catch cancellation does so explicitly via the
;;;; FIBER-CANCELLED type or via SERIOUS-CONDITION.
;;;;
;;;; Cancellation is cooperative: a fiber that never reaches a yield
;;;; point cannot be cancelled.  Runaway CPU loops remain a user bug.
;;;;
;;;; Wake latency is bounded by the carrier's polling cadence -- the
;;;; existing WAKE-WHEN path naturally observes the token's state every
;;;; carrier iteration.

(defpackage :epsilon.scheduler.cancellation
  (:use :cl)
  (:import (epsilon.sys.lock lock))
  (:export
   #:cancellation-token
   #:make-cancellation-token
   #:cancellation-token-p
   #:cancellation-token-reason
   #:cancellation-token-parent
   #:cancellation-requested-p
   #:cancel
   #:with-cancellation
   #:with-deadline
   #:fiber-cancelled
   #:fiber-cancelled-token
   #:fiber-cancelled-reason
   #:signal-cancelled-if-requested
   #:*cancellation*))

(in-package :epsilon.scheduler.cancellation)

;;; ---------------------------------------------------------------------------
;;; Condition type
;;; ---------------------------------------------------------------------------

(define-condition fiber-cancelled (serious-condition)
  ((token :initarg :token :reader fiber-cancelled-token :initform nil)
   (reason :initarg :reason :reader fiber-cancelled-reason :initform nil))
  (:report (lambda (c stream)
             (format stream "Fiber cancelled~@[: ~A~]"
                     (fiber-cancelled-reason c)))))

;;; ---------------------------------------------------------------------------
;;; Token
;;; ---------------------------------------------------------------------------

(defstruct (cancellation-token (:constructor %make-cancellation-token))
  "Node in the cancellation tree.
PARENT  the parent token, or NIL if root.
STATE   :ACTIVE or :CANCELLED.  Once cancelled, never reverts.
REASON  optional value attached at CANCEL time, surfaced on the
        FIBER-CANCELLED condition.
CHILDREN list of child tokens.  Cancelling walks the list to mark each
        child cancelled.
LOCK    serialises CANCEL with concurrent ADD-CHILD."
  (parent nil :type (or null cancellation-token))
  (state :active)
  (reason nil)
  (children nil :type list)
  (lock (lock:make-lock "cancellation-token") :type lock:lock))

(defun make-cancellation-token (&optional parent)
  "Create a new token.  When PARENT is supplied, the new token is
linked into the parent's child list and inherits the parent's
cancelled state if the parent is already cancelled."
  (let ((tok (%make-cancellation-token :parent parent)))
    (when parent
      (lock:with-lock ((cancellation-token-lock parent))
        (cond
          ((eq (cancellation-token-state parent) :cancelled)
           ;; Parent already cancelled: child is born cancelled, inheriting
           ;; the parent's reason if none was set on the child yet.
           (setf (cancellation-token-state tok) :cancelled
                 (cancellation-token-reason tok)
                 (cancellation-token-reason parent)))
          (t
           (push tok (cancellation-token-children parent))))))
    tok))

(defun cancellation-requested-p (token)
  "Return T if TOKEN (or, by virtue of inherited tree state, any of its
ancestors at the time of cancellation) has been cancelled."
  (and token (eq (cancellation-token-state token) :cancelled)))

(defun cancel (token &optional reason)
  "Mark TOKEN cancelled and recursively cancel every descendant.
Idempotent: cancelling an already-cancelled token is a no-op."
  (let ((to-walk (list token)))
    (loop while to-walk do
      (let ((tok (pop to-walk)))
        (lock:with-lock ((cancellation-token-lock tok))
          (when (eq (cancellation-token-state tok) :active)
            (setf (cancellation-token-state tok) :cancelled
                  (cancellation-token-reason tok) reason)
            (dolist (child (cancellation-token-children tok))
              (push child to-walk))))))))

(defmacro with-cancellation ((token) &body body)
  "Bind *CANCELLATION* to TOKEN within BODY.  Coroutines spawned inside
inherit TOKEN as their ambient cancellation token."
  `(let ((*cancellation* ,token))
     ,@body))

;;; ---------------------------------------------------------------------------
;;; Ambient token
;;; ---------------------------------------------------------------------------

(defvar *cancellation* nil
  "Ambient cancellation token.  Bound by WITH-CANCELLATION /
WITH-DEADLINE.  Captured at coroutine-submit time so a spawned
coroutine inherits its spawner's token.")

;;; ---------------------------------------------------------------------------
;;; Cooperative check
;;; ---------------------------------------------------------------------------

(defun signal-cancelled-if-requested (token)
  "If TOKEN is cancelled, signal FIBER-CANCELLED out of the caller.
Intended use is at the top of long CPU-bound loops where no blocking
primitive runs."
  (when (cancellation-requested-p token)
    (error 'fiber-cancelled
           :token token
           :reason (cancellation-token-reason token))))

;;; ---------------------------------------------------------------------------
;;; with-deadline
;;; ---------------------------------------------------------------------------

(defmacro with-deadline ((seconds &key reason) &body body)
  "Execute BODY with a fresh child cancellation token that fires after
SECONDS.  The child is parented under the current *CANCELLATION* (if
any), so an outer cancellation also propagates inward."
  (let ((tok (gensym "TOK"))
        (timer (gensym "TIMER")))
    `(let* ((,tok (make-cancellation-token *cancellation*))
            (,timer (sb-ext:make-timer
                     (lambda () (cancel ,tok (or ,reason :deadline)))
                     :name "cancellation-deadline")))
       (sb-ext:schedule-timer ,timer ,seconds)
       (unwind-protect
            (with-cancellation (,tok) ,@body)
         (sb-ext:unschedule-timer ,timer)))))

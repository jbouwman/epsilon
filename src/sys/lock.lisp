(cl:defpackage epsilon.sys.lock
  (:use
   cl)
  (:export
   acquire-lock
   lock
   lock-name
   lock-p
   make-lock
   make-recursive-lock
   release-lock
   with-lock
   with-recursive-lock-held
   ;; Condition variables (always paired with a lock)
   condition-variable
   make-condition-variable
   condition-wait
   condition-notify
   condition-broadcast
   ;; Slow-wait instrumentation
   *slow-acquire-warn-seconds*
   *slow-acquire-handler*))

(in-package epsilon.sys.lock)

;;; ---------------------------------------------------------------------------
;;; Slow-wait instrumentation
;;;
;;; A waiter that blocks longer than *SLOW-ACQUIRE-WARN-SECONDS* on
;;; ACQUIRE-LOCK or CONDITION-WAIT is reported via *SLOW-ACQUIRE-HANDLER*
;;; (a function of NAME WAITED-SECONDS THREAD-NAME KIND).  The default
;;; handler is NIL (no reporting); the web framework installs a handler
;;; that emits a structured log line with a stack identifier, so a
;;; deadlocked condvar surfaces in the journal instead of silently
;;; pinning a carrier.
;;;
;;; The instrumentation is opt-in: with the default handler the wrappers
;;; collapse to the underlying SB-THREAD primitives and add no allocation.
;;; ---------------------------------------------------------------------------

(defparameter *slow-acquire-warn-seconds* 1.0
  "Threshold (seconds) above which a lock acquisition or condition-wait
   is reported via *SLOW-ACQUIRE-HANDLER*.")

(defparameter *slow-acquire-handler* nil
  "Function called when a wait exceeds *SLOW-ACQUIRE-WARN-SECONDS*, or
   NIL to disable instrumentation.  Signature: (NAME WAITED THREAD KIND)
   where NAME is the lock or cv name (string), WAITED is seconds elapsed,
   THREAD is the waiter thread's name, and KIND is :ACQUIRE or :CONDITION-WAIT.
   The handler must not signal -- exceptions are muffled.")

(declaim (inline %record-wait-start))
(defun %record-wait-start ()
  (and *slow-acquire-handler* (get-internal-real-time)))

(defun %report-slow-wait (start name kind)
  (when (and start *slow-acquire-handler*)
    (let* ((elapsed (/ (- (get-internal-real-time) start)
                       (float internal-time-units-per-second 0.0))))
      (when (>= elapsed *slow-acquire-warn-seconds*)
        (handler-case
            (funcall *slow-acquire-handler*
                     name elapsed
                     (sb-thread:thread-name sb-thread:*current-thread*)
                     kind)
          (error () nil))))))

(defun acquire-lock (lock &key (wait t) timeout)
  "Acquire the lock LOCK for the calling thread.

  WAIT governs what happens if the lock is not available: if WAIT
  is true, the calling thread will wait until the lock is available
  and then acquire it; if WAIT is NIL, ACQUIRE-LOCK will return
  immediately.

  If WAIT is true, TIMEOUT may specify a maximum amount of seconds to
  wait for the lock to become available.

  ACQUIRE-LOCK returns T if the lock was acquired and NIL
  otherwise.

  This specification does not define what happens if a thread
  attempts to acquire a lock that it already holds. For applications
  that require locks to be safe when acquired recursively, see instead
  MAKE-RECURSIVE-LOCK and friends."
  (check-type timeout (or null (real 0)))
  (let ((start (%record-wait-start)))
    (multiple-value-prog1
        (sb-thread:grab-mutex lock :waitp wait :timeout timeout)
      (%report-slow-wait start (sb-thread:mutex-name lock) :acquire))))

(deftype lock ()
  'sb-thread:mutex)

(defun lock-name (lock)
  "Return the human-readable name LOCK was created with, or whatever
   default the underlying SB-THREAD primitive supplied."
  (sb-thread:mutex-name lock))

(defun lock-p (object)
  "True iff OBJECT is a lock created by MAKE-LOCK or MAKE-RECURSIVE-LOCK."
  (typep object 'sb-thread:mutex))

(defun make-lock (&optional name)
  "Allocate and return a fresh non-recursive lock.  NAME (a string) is
   used in slow-wait diagnostics and SBCL inspection output; defaults
   to \"Anonymous lock\" if not supplied.  See MAKE-RECURSIVE-LOCK if
   the same thread might re-enter the lock."
  (sb-thread:make-mutex :name (or name "Anonymous lock")))

(defun release-lock (lock)
  "Release LOCK. It is an error to call this unless
  the lock has previously been acquired (and not released) by the same
  thread. If other threads are waiting for the lock, the
  ACQUIRE-LOCK call in one of them will now be able to continue.

  Returns the lock."
  (sb-thread:release-mutex lock)
  lock)

(defmacro with-lock ((place) &body body)
  "Evaluates BODY with the lock named by PLACE, the value of which
  is a lock created by MAKE-LOCK. Before the forms in BODY are
  evaluated, the lock is acquired as if by using ACQUIRE-LOCK. After the
  forms in BODY have been evaluated, or if a non-local control transfer
  is caused (e.g. by THROW or SIGNAL), the lock is released as if by
  RELEASE-LOCK.

  Note that if the debugger is entered, it is unspecified whether the
  lock is released at debugger entry or at debugger exit when execution
  is restarted."
  `(sb-thread:with-mutex (,place) ,@body))

(defun make-recursive-lock (&optional name)
  "Allocate and return a fresh recursive lock -- the same thread can
   acquire it multiple times without deadlocking, and must release it
   the matching number of times.  See MAKE-LOCK for the cheaper
   non-recursive variant."
  (sb-thread:make-mutex :name (or name "Anonymous lock")))

(defmacro with-recursive-lock-held ((place) &body body)
  "Like WITH-LOCK but uses the recursive-acquire primitive: BODY can
   call into other code that re-enters the same lock without
   deadlocking.  PLACE is evaluated to obtain the lock object."
  `(sb-thread:with-recursive-lock (,place)
     ,@body))

;;; Condition variables.  Each cv is associated with a lock at use-time:
;;; the caller acquires the lock, checks a predicate, calls
;;; `condition-wait' (which atomically releases the lock and blocks),
;;; and on wakeup re-acquires the lock and re-checks.

(deftype condition-variable ()
  'sb-thread:waitqueue)

(defun make-condition-variable (&key name)
  "Allocate and return a fresh condition variable for the
   acquire-lock / check-predicate / condition-wait pattern.  NAME is
   used in slow-wait diagnostics and inspector output."
  (sb-thread:make-waitqueue :name (or name "Anonymous condition")))

(defun condition-wait (cv lock &key timeout)
  "Atomically release LOCK and block on CV until notified or TIMEOUT
elapses.  On wakeup, LOCK is re-acquired before this call returns.  The
caller must hold LOCK on entry."
  (let ((start (%record-wait-start)))
    (multiple-value-prog1
        (sb-thread:condition-wait cv lock :timeout timeout)
      (%report-slow-wait start
                         (sb-thread:waitqueue-name cv)
                         :condition-wait))))

(defun condition-notify (cv)
  "Wake one thread waiting on CV (if any).  The notifier should hold the
associated lock so the wakee's predicate-check happens-after the state
change that motivated the notification."
  (sb-thread:condition-notify cv))

(defun condition-broadcast (cv)
  "Wake every thread waiting on CV."
  (sb-thread:condition-broadcast cv))

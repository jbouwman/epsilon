;;;; Coroutine -- lightweight cooperative task backed by an sb-fiber
;;;;
;;;; Each coroutine owns an sb-fiber (a user-space stack with its own
;;;; control and binding stacks).  Suspension is implemented by switching
;;;; to the carrier's scheduler fiber; resumption switches back into the
;;;; coroutine's fiber.  Because the fiber preserves the entire call
;;;; stack -- loop variables, let bindings, handler frames, and dynamic
;;;; bindings -- a coroutine can park from arbitrarily deep call chains
;;;; (e.g. inside the TLS record layer) and resume in place.
;;;;
;;;; Divergence from upstream sb-fiber-runtime planning notes
;;;; (manual/architecture/sb-fiber-runtime.md):
;;;;
;;;; That document assumes single-thread affinity -- one scheduler per
;;;; OS thread, the reactor on the same thread as the parked fiber, and
;;;; SB-FIBER:FIBER-PARK / FIBER-UNPARK as the load-bearing
;;;; suspend/wake primitives.  Kreisler instead runs M:N: the scheduler
;;;; pools several carrier threads and migrates coroutines between them
;;;; via WAKE-COROUTINE-FROM-FOREIGN; the reactor lives on its own
;;;; thread.  In that model, SB-FIBER:FIBER-PARK is awkward because
;;;; every wake site (timer-heap, owner-carrier deque, foreign-thread
;;;; push) would need a paired SB-FIBER:FIBER-UNPARK before its
;;;; FIBER-SWITCH, otherwise the next FIBER-PARK consumes a stale
;;;; credit and short-circuits.  We instead use plain FIBER-SWITCH and
;;;; carry a WAKE-PREDICATE that the carrier polls between iterations
;;;; -- the polling backstop closes the lost-wakeup window that
;;;; FIBER-PARK's atomicity would otherwise close.
;;;;
;;;; manual/implement/352_fiber-park-m-n-integration.md tracks the work
;;;; to switch over to FIBER-PARK / FIBER-UNPARK if Kreisler ever moves
;;;; to thread-per-core.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-fiber))

(defpackage :epsilon.scheduler.coroutine
  (:use :cl)
  (:import (epsilon.scheduler.cancellation cxl))
  (:export
   ;; Coroutine struct
   #:coroutine
   #:make-coroutine
   #:coroutine-p
   #:coroutine-id
   #:coroutine-state
   #:coroutine-result
   #:coroutine-error-value
   #:coroutine-wake-predicate
   #:coroutine-wake-timeout
   #:coroutine-scheduler
   #:coroutine-priority
   #:coroutine-fiber
   #:coroutine-owner-carrier
   #:coroutine-scheduler-fiber
   #:coroutine-run-count

   ;; State predicates
   #:coroutine-ready-p
   #:coroutine-running-p
   #:coroutine-suspended-p
   #:coroutine-completed-p
   #:coroutine-failed-p
   #:coroutine-done-p

   ;; Suspension API (called from within a running coroutine)
   #:coroutine-yield
   #:coroutine-park

   ;; Running coroutine binding
   #:*current-coroutine*

   ;; Execution
   #:run-coroutine
   #:destroy-coroutine

   ;; Stage 1 instrumentation (IMPL-382)
   #:*coroutine-trace*
   #:fiber-stack-stats))

;;; ---------------------------------------------------------------------------
;;; Coroutine ID generator
;;; ---------------------------------------------------------------------------

(defstruct (id-gen (:constructor %make-id-gen))
  (counter 0 :type sb-ext:word))

(defvar *coroutine-id-gen* (%make-id-gen))

(defun next-coroutine-id ()
  (sb-ext:atomic-incf (id-gen-counter *coroutine-id-gen*)))

;;; ---------------------------------------------------------------------------
;;; Coroutine struct
;;; ---------------------------------------------------------------------------

(deftype coroutine-state ()
  '(member :ready :running :suspended :completed :failed))

(defstruct (coroutine (:constructor %make-coroutine))
  "A lightweight cooperative task backed by an sb-fiber.

STATE transitions:
  :ready -> :running -> :ready       (yield)
  :ready -> :running -> :suspended   (park)
  :ready -> :running -> :completed   (normal return)
  :ready -> :running -> :failed      (unhandled error)
  :suspended -> :ready               (wake condition met)

FN is the user function (zero-argument), captured at construction.
FIBER is the underlying sb-fiber, allocated lazily on first
RUN-COROUTINE call (because sb-fiber is per-thread and must be
allocated on the OS thread that will switch into it).
OWNER-CARRIER is the carrier on which the fiber was allocated; it
remains pinned to that carrier for its lifetime.
SCHEDULER-FIBER is the carrier's main fiber; the coroutine switches
back to it on yield/park.  It is set by RUN-COROUTINE on every entry."
  (id 0 :type fixnum)
  (state :ready :type coroutine-state)
  (fn nil :type (or null function))
  (initial-bindings nil :type list)
  (result nil)
  (error-value nil)
  (wake-predicate nil :type (or null function))
  (wake-timeout nil :type (or null fixnum))  ; internal-real-time deadline
  (scheduler nil)
  (priority 0 :type fixnum)
  (fiber nil)
  (owner-carrier nil)
  (scheduler-fiber nil)
  ;; Stage 1 instrumentation (IMPL-382): incremented on every entry into
  ;; the coroutine's fiber, so we can correlate stack-usage growth with
  ;; the number of resume cycles.
  (run-count 0 :type fixnum))

(defmethod print-object ((c coroutine) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (format stream "~D ~S" (coroutine-id c) (coroutine-state c))))

(defvar *current-coroutine* nil
  "The coroutine currently executing on this carrier thread, or NIL.")

(defvar *captured-specials*
  '(*standard-output* *error-output* *trace-output*
    *debug-io* *query-io* *package* *readtable*
    *print-base* *print-radix* *print-length* *print-level*
    epsilon.scheduler.cancellation:*cancellation*)
  "Special variables captured from the submitting thread at MAKE-COROUTINE
time and re-established on the coroutine's fiber binding stack on first
run, so the coroutine inherits the caller's dynamic environment (including
the ambient cancellation token).")

(defun capture-current-specials ()
  (loop for sym in *captured-specials*
        when (boundp sym)
          collect (cons sym (symbol-value sym))))

(defun make-coroutine (fn &key (priority 0))
  "Create a coroutine that will execute FN.

FN is a function of zero arguments.  It may call COROUTINE-YIELD or
COROUTINE-PARK to suspend.  When it returns normally, the coroutine
completes with its return value as the result; on unhandled error,
it transitions to :failed and the error is stored.

The fiber is allocated lazily on first RUN-COROUTINE, which pins the
coroutine to that carrier thread.  Selected special variables (see
*CAPTURED-SPECIALS*) are snapshotted at this call site and re-bound
inside the fiber's body so the coroutine inherits the submitter's
dynamic environment."
  (%make-coroutine
   :id (next-coroutine-id)
   :state :ready
   :fn fn
   :initial-bindings (capture-current-specials)
   :priority priority))

(defun ensure-fiber (coro)
  "Allocate the coroutine's fiber on the current OS thread if not yet
allocated.  Must be called from the carrier that will own the
coroutine.  The outer HANDLER-CASE converts a truly-unhandled
condition (the user function escapes without a handler) into the
coroutine's :FAILED state, keeping the carrier from running off the
fiber stack."
  (unless (coroutine-fiber coro)
    (let ((fn (coroutine-fn coro))
          (bindings (coroutine-initial-bindings coro)))
      (setf (coroutine-fiber coro)
            (sb-fiber:make-fiber
             (lambda ()
               (progv (mapcar #'car bindings)
                      (mapcar #'cdr bindings)
                 (let ((*current-coroutine* coro))
                   ;; Muffle transient SIMPLE-WARNINGS emitted by user
                   ;; code (e.g. a reactor unregister on an already-
                   ;; closed fd) so they don't reach SBCL's debugger,
                   ;; which under --disable-debugger promotes them to
                   ;; fatal process exits.
                   (handler-bind
                       ((simple-warning
                          (lambda (c)
                            (declare (ignore c))
                            (let ((restart (find-restart 'muffle-warning)))
                              (when restart (invoke-restart restart))))))
                     (handler-case
                         (let ((result (funcall fn)))
                           (setf (coroutine-result coro) result)
                           (setf (coroutine-state coro) :completed))
                       (error (e)
                         (setf (coroutine-error-value coro) e)
                         (setf (coroutine-state coro) :failed))))))))))))

;;; ---------------------------------------------------------------------------
;;; State predicates
;;; ---------------------------------------------------------------------------

(declaim (inline coroutine-ready-p coroutine-running-p
                 coroutine-suspended-p coroutine-completed-p
                 coroutine-failed-p coroutine-done-p))

(defun coroutine-ready-p (coro)
  (eq (coroutine-state coro) :ready))

(defun coroutine-running-p (coro)
  (eq (coroutine-state coro) :running))

(defun coroutine-suspended-p (coro)
  (eq (coroutine-state coro) :suspended))

(defun coroutine-completed-p (coro)
  (eq (coroutine-state coro) :completed))

(defun coroutine-failed-p (coro)
  (eq (coroutine-state coro) :failed))

(defun coroutine-done-p (coro)
  "True if coroutine is in a terminal state."
  (member (coroutine-state coro) '(:completed :failed)))

;;; ---------------------------------------------------------------------------
;;; Suspension API
;;; ---------------------------------------------------------------------------

(defun coroutine-yield ()
  "Yield the current coroutine, allowing other coroutines to run.
The coroutine is moved to :ready and re-enqueued by the scheduler.
Execution resumes in-place when the carrier picks it up again."
  (let ((coro *current-coroutine*))
    (unless coro
      (error "coroutine-yield called outside a coroutine"))
    (setf (coroutine-state coro) :ready)
    (sb-fiber:fiber-switch (coroutine-fiber coro)
                           (coroutine-scheduler-fiber coro))))

(defun coroutine-park (&key wake-when timeout cancellation)
  "Park the current coroutine until WAKE-WHEN returns true (or TIMEOUT
seconds elapse, or CANCELLATION fires, if any are specified).

WAKE-WHEN: a function of zero arguments.  The scheduler periodically
  evaluates it; when it returns true, the coroutine is moved to :ready
  and resumed in-place at the call site.
TIMEOUT: optional timeout in seconds.  If the coroutine is not woken
  within this time, it is woken anyway.
CANCELLATION: optional EPSILON.SCHEDULER.CANCELLATION:CANCELLATION-TOKEN.
  If the token fires before any other wake condition, COROUTINE-PARK
  signals FIBER-CANCELLED out of the call.

At least one of WAKE-WHEN, TIMEOUT, or CANCELLATION must be supplied."
  (let ((coro *current-coroutine*))
    (unless coro
      (error "coroutine-park called outside a coroutine"))
    (unless (or wake-when timeout cancellation)
      (error "coroutine-park requires :wake-when, :timeout, or :cancellation"))
    (let ((effective-wake-when
            (cond
              ((and cancellation wake-when)
               (lambda ()
                 (or (cxl:cancellation-requested-p cancellation)
                     (funcall wake-when))))
              (cancellation
               (lambda () (cxl:cancellation-requested-p cancellation)))
              (t wake-when))))
      (setf (coroutine-wake-predicate coro) effective-wake-when))
    (when timeout
      (setf (coroutine-wake-timeout coro)
            (+ (get-internal-real-time)
               (ceiling (* timeout internal-time-units-per-second)))))
    (setf (coroutine-state coro) :suspended)
    (sb-fiber:fiber-switch (coroutine-fiber coro)
                           (coroutine-scheduler-fiber coro))
    ;; Re-entry: if cancellation fired during the park, surface it now.
    (when cancellation
      (cxl:signal-cancelled-if-requested cancellation))))

;;; ---------------------------------------------------------------------------
;;; Coroutine execution
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; Stage 1 instrumentation (IMPL-382)
;;;
;;; Tools for diagnosing the keepalive hang: a per-coroutine fiber stack
;;; trace and a structured snapshot of fiber stack usage.  Both are
;;; opt-in so they have zero cost when disabled.
;;; ---------------------------------------------------------------------------

(defvar *coroutine-trace* nil
  "When bound to a stream, run-coroutine writes one line per fiber entry
and one line per fiber exit summarising the coroutine id, the carrier id,
the run-count, the action that ended the run, and the fiber's control
and binding stack usage.  Intended for IMPL-382 diagnosis -- leave NIL
in production.

Each line is a plist-style record on a single line, prefixed with
\"#fiber-trace\" for easy grep, e.g.:

  #fiber-trace :phase :enter :coro 42 :carrier 0 :run 3 :csu 12288 :bsu 256
  #fiber-trace :phase :exit  :coro 42 :carrier 0 :run 3 :action :yield :csu 16384 :bsu 320")

(defun fiber-stack-stats (coro)
  "Return a plist describing CORO's fiber stack usage and capacity.
Returns NIL if the fiber has not yet been allocated.

The sb-fiber stack-usage accessors that this once read were dropped in
the post-IMPL-382 sb-fiber rewrite (commit 18c0e635 of the jbouwman/sbcl
fork).  Until they're reintroduced the four numeric fields report 0."
  (let ((fiber (coroutine-fiber coro)))
    (when fiber
      (list :id (coroutine-id coro)
            :run-count (coroutine-run-count coro)
            :state (coroutine-state coro)
            :control-stack-usage 0
            :control-stack-size 0
            :binding-stack-usage 0
            :binding-stack-size 0))))

(defun %trace-fiber-event (stream phase coro action)
  "Emit one line to STREAM describing a fiber entry or exit.  Output is
deliberately compact and allocates only the format string."
  (let ((fiber (coroutine-fiber coro))
        (carrier-id (let ((c (coroutine-owner-carrier coro)))
                      (when c
                        (and (find-symbol "CARRIER-ID" :epsilon.scheduler)
                             (funcall (find-symbol "CARRIER-ID" :epsilon.scheduler)
                                      c))))))
    (when fiber
      (format stream
              "~&#fiber-trace :phase ~S :coro ~D :carrier ~A :run ~D~@[ :action ~S~] :csu ~D :bsu ~D~%"
              phase
              (coroutine-id coro)
              carrier-id
              (coroutine-run-count coro)
              action
              0
              0)
      (force-output stream))))

(defun run-coroutine (coro scheduler-fiber)
  "Switch into CORO's fiber from SCHEDULER-FIBER and run until the
coroutine yields, parks, completes, or fails.  Returns one of:
  :YIELD     coroutine called COROUTINE-YIELD
  :PARK      coroutine called COROUTINE-PARK
  NIL        coroutine completed normally
  :FAILED    coroutine signaled an unhandled error

The fiber is allocated on first call (binding the coroutine to the
calling carrier thread for life).  SCHEDULER-FIBER must be the fiber
currently running on the calling thread (typically obtained with
SB-FIBER:MAKE-MAIN-FIBER once per carrier and reused)."
  (ensure-fiber coro)
  (setf (coroutine-scheduler-fiber coro) scheduler-fiber)
  (setf (coroutine-state coro) :running)
  (incf (coroutine-run-count coro))
  (let ((trace *coroutine-trace*))
    (when trace
      (%trace-fiber-event trace :enter coro nil))
    (sb-fiber:fiber-switch scheduler-fiber (coroutine-fiber coro))
    ;; When fiber-switch returns, the coroutine has either yielded,
    ;; parked, completed, or failed; the state slot reflects which.
    (let ((action (case (coroutine-state coro)
                    (:ready :yield)
                    (:suspended :park)
                    (:completed nil)
                    (:failed :failed)
                    (t (error "Unexpected coroutine state after switch: ~S"
                              (coroutine-state coro))))))
      (when trace
        (%trace-fiber-event trace :exit coro action))
      action)))

(defun destroy-coroutine (coro)
  "Release the fiber's stacks.  Must only be called once the coroutine
is in a terminal state (:completed or :failed)."
  (let ((fiber (coroutine-fiber coro)))
    (when fiber
      (sb-fiber:destroy-fiber fiber)
      (setf (coroutine-fiber coro) nil))))

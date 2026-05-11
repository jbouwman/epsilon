;;;; Scheduler -- M:N coroutine scheduler with per-carrier pinning
;;;;
;;;; Multiplexes N coroutines onto M carrier threads.  Each coroutine
;;;; owns an sb-fiber, which sb-fiber pins to the OS thread that
;;;; allocated it; consequently, coroutines cannot migrate between
;;;; carriers once they have started running.
;;;;
;;;; Submission picks a carrier (round-robin across the pool) and
;;;; routes the coroutine to that carrier's incoming queue.  The
;;;; carrier owns the coroutine for life: subsequent yields, parks,
;;;; and wakeups all happen on the same carrier thread, so every
;;;; fiber-switch is intra-thread.
;;;;
;;;; Each carrier owns its own deque, timer-heap, and parked-list, so
;;;; no cross-carrier locking is needed for run-time scheduling.  The
;;;; only shared state is the round-robin counter and the per-carrier
;;;; incoming queue (mutex-protected, used for cross-thread submission
;;;; and inter-carrier sends from the channel hook).

(defpackage :epsilon.scheduler
  (:use :cl)
  (:import (epsilon.scheduler.coroutine coro)
            (epsilon.scheduler.timer timer)
            (epsilon.channel channel))
  (:import
   (epsilon.sys.lock lock)
   (epsilon.sys.semaphore sem)
   (epsilon.sys.thread thread))
  (:export
   ;; Scheduler
   #:make-scheduler
   #:scheduler
   #:scheduler-p
   #:scheduler-submit
   #:scheduler-try-submit
   #:scheduler-run
   #:scheduler-shutdown
   #:scheduler-wait
   #:scheduler-saturated-p
   #:scheduler-max-incoming-queue
   #:scheduler-overflow-count

   ;; Lifecycle macro
   #:with-scheduler

   ;; Current scheduler / carrier
   #:*current-scheduler*
   #:*current-carrier*

   ;; Scheduler accessors used by watchdogs / introspection
   #:scheduler-carriers
   #:scheduler-num-carriers
   #:scheduler-running-p

   ;; Carrier accessors needed by io-wait
   #:carrier
   #:carrier-id
   #:carrier-incoming-queue
   #:carrier-incoming-lock
   #:carrier-idle-sem

   ;; Liveness instrumentation (used by watchdogs and /health/ready)
   #:carrier-iter-counter
   #:carrier-progress-stamp

   ;; Coroutine sleep and wait
   #:coroutine-sleep
   #:coroutine-wait-until

   ;; Cross-thread / cross-carrier wakeup primitive used by io-wait
   #:wake-coroutine-from-foreign

   ;; Carrier idle hook for I/O polling integration
   #:scheduler-idle-hook

   ;; CPU-bound offload
   #:run-blocking

   ;; Observability
   #:scheduler-active-coroutine-count
   #:scheduler-submit-queue-depth

   ;; Stage 1 instrumentation (IMPL-382)
   #:dump-carrier-backtraces
   #:carrier-fiber-stack-snapshot
   #:carrier-backtrace))

;;; ---------------------------------------------------------------------------
;;; Carrier -- OS thread that runs coroutines
;;; ---------------------------------------------------------------------------

(defstruct (carrier (:constructor %make-carrier))
  "A carrier thread in the scheduler's thread pool.

INCOMING-QUEUE  list of coroutines awaiting first run on this carrier
                (mutex-protected; pushed by external submitters or by
                wake events from non-owner carriers).
DEQUE           list of :ready coroutines pinned to this carrier
                (single-owner, no lock required).
TIMER-HEAP      min-heap of parked coroutines with timeout deadlines.
PARKED          list of parked coroutines awaiting predicate wakeups.
SCHEDULER-FIBER the carrier thread's main fiber (created on entry to
                CARRIER-LOOP), used as the FROM/TO of every fiber switch
                into a coroutine."
  (id 0 :type fixnum)
  (thread nil)
  (deque nil :type list)
  (incoming-queue nil :type list)
  (incoming-queue-length 0 :type fixnum)
  (incoming-lock nil)
  (idle-sem nil)
  (timer-heap nil)
  (parked nil :type list)
  (scheduler nil)
  (running-p nil :type boolean)
  (scheduler-fiber nil)
  ;; Liveness counters: bumped by the carrier itself on every loop
  ;; iteration so a watchdog thread can detect a wedged carrier without
  ;; having to interrupt-thread it.  PROGRESS-STAMP is universal-time
  ;; sampled when the iter counter advances.
  (iter-counter 0 :type sb-ext:word)
  (progress-stamp 0 :type integer)
  ;; Preallocated scratch for %CAPTURE-BACKTRACE-INTO-BUFFER (filled
  ;; from inside a SIGURG handler, drained by the caller of
  ;; CARRIER-BACKTRACE / DUMP-CARRIER-BACKTRACES).  Kept off the heap
  ;; so the watchdog's diagnostic path cannot itself trigger an
  ;; allocation under near-OOM and crash the runtime.
  (backtrace-buffer nil :type (or null string))
  (backtrace-seq 0 :type sb-ext:word))

(defmethod print-object ((c carrier) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (format stream "~D ~:[stopped~;running~]" (carrier-id c) (carrier-running-p c))))

(defconstant +carrier-backtrace-max-frames+ 32
  "Maximum number of frames captured into a carrier's backtrace
buffer.  Anything past this is silently truncated.")

(defconstant +carrier-backtrace-line-bytes+ 128
  "Maximum bytes written per frame line in a carrier's backtrace
buffer.  Long package-qualified names past this are truncated.")

(defun make-carrier (id scheduler)
  (%make-carrier
   :id id
   :deque nil
   :incoming-queue nil
   :incoming-lock (lock:make-lock (format nil "carrier-~D-incoming" id))
   :idle-sem (sem:make-semaphore :name (format nil "carrier-~D-idle" id) :count 0)
   :timer-heap (timer:make-timer-heap)
   :scheduler scheduler
   :backtrace-buffer (make-array (* +carrier-backtrace-max-frames+
                                    +carrier-backtrace-line-bytes+)
                                 :element-type 'character
                                 :fill-pointer 0
                                 :initial-element #\Space)
   :running-p t))

;;; ---------------------------------------------------------------------------
;;; Scheduler struct
;;; ---------------------------------------------------------------------------

(defvar *current-scheduler* nil
  "The scheduler bound in the current dynamic scope, or NIL.")

(defvar *current-carrier* nil
  "The carrier running on the current OS thread, or NIL.")

(defstruct (scheduler (:constructor %make-scheduler))
  "M:N coroutine scheduler with per-carrier ownership.

CARRIERS            vector of carrier structs, one per OS thread.
NEXT-CARRIER        atomic round-robin counter for submission.
ACTIVE-COUNT        number of coroutines not yet in a terminal state.
DONE-SEM            signalled when ACTIVE-COUNT reaches 0.
MAX-INCOMING-QUEUE  per-carrier cap on the incoming queue, or NIL for
                    unbounded.  When the round-robin target is at or
                    above the cap, SCHEDULER-TRY-SUBMIT returns NIL so
                    callers (e.g. the HTTP accept loop) can shed load
                    rather than letting the queue grow without bound.
OVERFLOW-COUNT      monotonically increasing count of refused submits
                    (useful for /metrics and structured logging)."
  (carriers #() :type simple-vector)
  (num-carriers 0 :type fixnum)
  (running-p nil :type boolean)
  (next-carrier 0 :type sb-ext:word)
  (active-count 0 :type fixnum)
  (active-lock (lock:make-lock "scheduler-active"))
  (done-sem (sem:make-semaphore :name "scheduler-done" :count 0))
  (idle-hook nil :type (or null function))
  (max-incoming-queue nil :type (or null (integer 1)))
  (overflow-count 0 :type sb-ext:word))

(defun scheduler-active-coroutine-count (scheduler)
  "Return the number of active (non-terminal) coroutines."
  (scheduler-active-count scheduler))

(defun scheduler-submit-queue-depth (scheduler)
  "Return the total number of coroutines awaiting first run across all
carrier incoming queues.  Reads each carrier's cached length without the
incoming-lock; the value is approximate (a concurrent push or drain may
update it during the walk) but suitable for /metrics and overflow logs."
  (let ((carriers (scheduler-carriers scheduler))
        (total 0))
    (dotimes (i (scheduler-num-carriers scheduler))
      (incf total (carrier-incoming-queue-length (aref carriers i))))
    total))

;;; ---- Platform ----

(defun cpu-count ()
  "Return the number of available CPU cores."
  (max 1 (handler-case
              (sb-alien:alien-funcall
               (sb-alien:extern-alien
                "sysconf"
                (function sb-alien:long sb-alien:long))
               ;; _SC_NPROCESSORS_ONLN = 58 on Darwin, 84 on Linux
               #+darwin 58
               #+linux 84)
            (error () 4))))

(defmethod print-object ((s scheduler) stream)
  (print-unreadable-object (s stream :type t :identity t)
    (format stream "carriers=~D active=~D ~:[stopped~;running~]"
            (scheduler-num-carriers s)
            (scheduler-active-count s)
            (scheduler-running-p s))))

(defun make-scheduler (&key (num-carriers (cpu-count)) idle-hook
                            max-incoming-queue)
  "Create a scheduler with NUM-CARRIERS carrier threads.
Defaults to the number of available CPU cores.
IDLE-HOOK, if provided, is a function called during the carrier idle path.
MAX-INCOMING-QUEUE, if provided, caps each carrier's incoming queue;
SCHEDULER-TRY-SUBMIT returns NIL when the target carrier is full."
  (let* ((n (max 1 num-carriers))
         (sched (%make-scheduler :num-carriers n :running-p t
                                 :idle-hook idle-hook
                                 :max-incoming-queue max-incoming-queue)))
    (setf (scheduler-carriers sched)
          (let ((v (make-array n)))
            (dotimes (i n)
              (setf (aref v i) (make-carrier i sched)))
            v))
    sched))

;;; ---------------------------------------------------------------------------
;;; Submission
;;; ---------------------------------------------------------------------------

(defun pick-target-carrier (scheduler)
  "Pick a carrier for a new coroutine via round-robin."
  (let* ((n (scheduler-num-carriers scheduler))
         (idx (mod (sb-ext:atomic-incf (scheduler-next-carrier scheduler)) n)))
    (aref (scheduler-carriers scheduler) idx)))

(defun push-incoming (carrier coro)
  "Push CORO onto CARRIER's incoming queue and signal its idle-sem."
  (lock:with-lock ((carrier-incoming-lock carrier))
    (push coro (carrier-incoming-queue carrier))
    (incf (carrier-incoming-queue-length carrier)))
  (sem:signal-semaphore (carrier-idle-sem carrier)))

(defun %try-push-incoming (carrier coro max-len)
  "Try to push CORO onto CARRIER's incoming queue, refusing if the queue
is at or above MAX-LEN.  Returns T on success, NIL on overflow.  The
length check is performed under the same lock that does the push, so
two concurrent submitters cannot both squeak past the cap."
  (let ((accepted nil))
    (lock:with-lock ((carrier-incoming-lock carrier))
      (when (or (null max-len)
                (< (carrier-incoming-queue-length carrier) max-len))
        (push coro (carrier-incoming-queue carrier))
        (incf (carrier-incoming-queue-length carrier))
        (setf accepted t)))
    (when accepted
      (sem:signal-semaphore (carrier-idle-sem carrier)))
    accepted))

(defun scheduler-saturated-p (scheduler)
  "Return T when every carrier's incoming queue has reached
MAX-INCOMING-QUEUE.  Cheap to call from observability/health code; under
unbounded scheduling (the default) always returns NIL."
  (let ((cap (scheduler-max-incoming-queue scheduler)))
    (and cap
         (let ((carriers (scheduler-carriers scheduler))
               (saturated t))
           (dotimes (i (scheduler-num-carriers scheduler) saturated)
             (let ((c (aref carriers i)))
               (when (< (carrier-incoming-queue-length c) cap)
                 (setf saturated nil)
                 (return saturated))))))))

(defun scheduler-submit (scheduler coroutine-or-fn &key (priority 0))
  "Submit work to SCHEDULER.  Returns the coroutine.

COROUTINE-OR-FN may be:
  - A coroutine struct (submitted directly)
  - A function (wrapped in a new coroutine)

A target carrier is chosen via round-robin and the coroutine is added
to its incoming queue.  If submission happens from inside a carrier
running on this scheduler, the local carrier's deque is used directly
to keep work cache-warm.

This entry point is unconditional: no queue cap is enforced.  Callers
that want to shed load on overflow should use SCHEDULER-TRY-SUBMIT
instead."
  (let ((coro (if (coro:coroutine-p coroutine-or-fn)
                  coroutine-or-fn
                  (coro:make-coroutine coroutine-or-fn :priority priority))))
    (setf (coro:coroutine-scheduler coro) scheduler)
    (lock:with-lock ((scheduler-active-lock scheduler))
      (incf (scheduler-active-count scheduler)))
    (let ((local *current-carrier*))
      (cond
        ;; Submitted from inside a carrier of this scheduler: push to the
        ;; local deque (single-owner; no lock needed).  This keeps freshly
        ;; spawned work on the producing thread, which both improves
        ;; locality and lets newly spawned coroutines inherit the
        ;; producer's owner-carrier on first run.
        ((and local (eq (carrier-scheduler local) scheduler))
         (push coro (carrier-deque local)))
        (t
         (push-incoming (pick-target-carrier scheduler) coro))))
    coro))

(defun scheduler-try-submit (scheduler coroutine-or-fn &key (priority 0))
  "Like SCHEDULER-SUBMIT but enforces SCHEDULER-MAX-INCOMING-QUEUE.

When the round-robin target carrier's incoming queue is at or above
the cap, refuses the submission and returns NIL; the scheduler's
OVERFLOW-COUNT is incremented.  On success returns the coroutine.

Submitters running on a carrier of this scheduler always succeed -- the
local-deque path is single-owner and unbounded by design (workers
enqueueing follow-up work for themselves should not be load-shed by the
accept-side cap).  The cap exists to bound external producers, in
particular the HTTP accept loop."
  (let* ((coro (if (coro:coroutine-p coroutine-or-fn)
                   coroutine-or-fn
                   (coro:make-coroutine coroutine-or-fn :priority priority)))
         (local *current-carrier*)
         (local-p (and local (eq (carrier-scheduler local) scheduler)))
         (cap (scheduler-max-incoming-queue scheduler)))
    (setf (coro:coroutine-scheduler coro) scheduler)
    (cond
      (local-p
       (lock:with-lock ((scheduler-active-lock scheduler))
         (incf (scheduler-active-count scheduler)))
       (push coro (carrier-deque local))
       coro)
      (t
       (let ((target (pick-target-carrier scheduler)))
         (cond
           ((%try-push-incoming target coro cap)
            (lock:with-lock ((scheduler-active-lock scheduler))
              (incf (scheduler-active-count scheduler)))
            coro)
           (t
            (sb-ext:atomic-incf (scheduler-overflow-count scheduler))
            (ignore-errors (coro:destroy-coroutine coro))
            nil)))))))

;;; ---------------------------------------------------------------------------
;;; Cross-thread wake (used by io-wait when a reactor thread wakes a
;;; coroutine).  The carrier's idle-sem is signalled so it leaves any
;;; semaphore wait promptly.
;;; ---------------------------------------------------------------------------

(defun wake-coroutine-from-foreign (coro)
  "Mark CORO as :ready and route it back to its owner carrier's
incoming queue.  Safe to call from any thread (e.g. from a reactor
callback running on a different OS thread).  The owner carrier picks
it up on its next loop iteration and switches into its fiber."
  (when (coro:coroutine-suspended-p coro)
    (setf (coro:coroutine-state coro) :ready)
    (setf (coro:coroutine-wake-predicate coro) nil)
    (setf (coro:coroutine-wake-timeout coro) nil)
    (let ((owner (coro:coroutine-owner-carrier coro)))
      (if owner
          (push-incoming owner coro)
          ;; Not yet pinned -- shouldn't happen in practice (a coroutine
          ;; can only park after running once, which assigns owner) but
          ;; route through round-robin as a safe fallback.
          (push-incoming
           (pick-target-carrier (coro:coroutine-scheduler coro))
           coro)))
    t))

;;; ---------------------------------------------------------------------------
;;; Per-carrier wake (called from the owner carrier's idle path).
;;; Single-owner, no lock needed.
;;; ---------------------------------------------------------------------------

(defun wake-on-owner (carrier coro)
  "Wake CORO on its owner carrier (the calling thread).  Pushes onto
the local deque.  Returns T if the coroutine was woken, NIL if it was
already woken via another path."
  (when (coro:coroutine-suspended-p coro)
    (setf (coro:coroutine-state coro) :ready)
    (setf (coro:coroutine-wake-predicate coro) nil)
    (setf (coro:coroutine-wake-timeout coro) nil)
    (push coro (carrier-deque carrier))
    t))

(defun park-coroutine (carrier coro)
  "Route a freshly suspended coroutine to the owner carrier's parked
list and/or timer heap.  Called from CARRIER-RUN-ONE, so we are on
the owner."
  (let ((has-timeout (coro:coroutine-wake-timeout coro))
        (has-predicate (coro:coroutine-wake-predicate coro)))
    (when has-timeout
      (timer:timer-heap-insert (carrier-timer-heap carrier)
                                (coro:coroutine-wake-timeout coro)
                                coro))
    (when has-predicate
      (push coro (carrier-parked carrier)))))

(defun check-timer-heap (carrier)
  "Drain expired entries from the carrier's timer heap and wake them."
  (let ((expired (timer:timer-heap-drain (carrier-timer-heap carrier)
                                          (get-internal-real-time))))
    (dolist (coro expired)
      (wake-on-owner carrier coro))))

(defun check-parked-coroutines (carrier)
  "Check predicate-parked coroutines on this carrier for wake."
  (let ((woken nil))
    (setf (carrier-parked carrier)
          (loop for coro in (carrier-parked carrier)
                ;; Skip coroutines woken via another path (e.g. timer or
                ;; foreign wake from io-wait).
                unless (coro:coroutine-suspended-p coro)
                  do (progn)
                else if (and (coro:coroutine-wake-predicate coro)
                             (funcall (coro:coroutine-wake-predicate coro)))
                  do (push coro woken)
                else
                  collect coro))
    (dolist (coro woken)
      (wake-on-owner carrier coro))))

;;; ---------------------------------------------------------------------------
;;; Coroutine completion
;;; ---------------------------------------------------------------------------

(defun coroutine-finished (scheduler coro)
  "Handle a completed or failed coroutine.  Releases the coroutine's
fiber stacks and decrements the scheduler's active-count."
  (coro:destroy-coroutine coro)
  (let ((remaining
          (lock:with-lock ((scheduler-active-lock scheduler))
            (decf (scheduler-active-count scheduler)))))
    (when (<= remaining 0)
      (sem:signal-semaphore (scheduler-done-sem scheduler)))))

;;; ---------------------------------------------------------------------------
;;; Carrier loop
;;; ---------------------------------------------------------------------------

(defun drain-incoming (carrier)
  "Move all entries from the incoming queue onto the local deque."
  (let ((pending nil))
    (lock:with-lock ((carrier-incoming-lock carrier))
      (when (carrier-incoming-queue carrier)
        (setf pending (carrier-incoming-queue carrier))
        (setf (carrier-incoming-queue carrier) nil)
        (setf (carrier-incoming-queue-length carrier) 0)))
    (dolist (coro pending)
      ;; Pin coroutines on first sight: they will run on this carrier,
      ;; so future wakeups must come back here.
      (unless (coro:coroutine-owner-carrier coro)
        (setf (coro:coroutine-owner-carrier coro) carrier))
      (push coro (carrier-deque carrier)))))

(defun pop-deque (carrier)
  "Pop one ready coroutine from the carrier's local deque, or NIL."
  (let ((d (carrier-deque carrier)))
    (when d
      (setf (carrier-deque carrier) (cdr d))
      (car d))))

(defun carrier-run-one (carrier scheduler coro)
  "Run a single coroutine and handle the result.  Pins the coroutine
to this carrier on first run."
  (unless (coro:coroutine-owner-carrier coro)
    (setf (coro:coroutine-owner-carrier coro) carrier))
  (let ((*current-carrier* carrier)
        (*current-scheduler* scheduler)
        (channel:*task-run-hook*
          (lambda (thunk &key (name "coroutine"))
            (declare (ignore name))
            (scheduler-submit scheduler thunk))))
    (handler-case
        (let ((action (coro:run-coroutine coro
                                           (carrier-scheduler-fiber carrier))))
          (case action
            (:yield
             (push coro (carrier-deque carrier)))
            (:park
             (park-coroutine carrier coro))
            ((nil :failed)
             (coroutine-finished scheduler coro))))
      (error (e)
        (unless (coro:coroutine-done-p coro)
          (setf (coro:coroutine-state coro) :failed)
          (setf (coro:coroutine-error-value coro) e))
        (coroutine-finished scheduler coro)))))

(defun %drain-carrier-fibers (carrier)
  "Destroy all fibers still held by this carrier's queues.  Must be
called before the main (scheduler) fiber is destroyed, since sb-fiber
requires the main fiber to outlive every coroutine fiber on the same
thread."
  (labels ((destroy-all (list)
             (dolist (coro list)
               (ignore-errors (coro:destroy-coroutine coro)))))
    (destroy-all (carrier-deque carrier))
    (setf (carrier-deque carrier) nil)
    (lock:with-lock ((carrier-incoming-lock carrier))
      (destroy-all (carrier-incoming-queue carrier))
      (setf (carrier-incoming-queue carrier) nil)
      (setf (carrier-incoming-queue-length carrier) 0))
    (destroy-all (carrier-parked carrier))
    (setf (carrier-parked carrier) nil)
    ;; Timer-heap entries reference coroutines; drain what remains.
    (let ((remaining (timer:timer-heap-drain (carrier-timer-heap carrier)
                                              most-positive-fixnum)))
      (destroy-all remaining))))

(defun carrier-loop (carrier)
  "Main loop for a carrier thread.  Allocates the carrier's main
fiber once and runs until the scheduler shuts down.  Before exiting
destroys any coroutine fibers still owned by this carrier so the
sb-fiber thread_list is empty when the main fiber is torn down."
  (let ((scheduler (carrier-scheduler carrier))
        (scheduler-fiber (sb-fiber:make-main-fiber)))
    (setf (carrier-scheduler-fiber carrier) scheduler-fiber)
    (let ((*current-carrier* carrier)
          (*current-scheduler* scheduler))
      (unwind-protect
           (handler-case
               (loop while (carrier-running-p carrier)
                     do (carrier-loop-iter carrier scheduler))
             (error (e)
               (format *error-output* "~&Carrier ~D died: ~A~%"
                       (carrier-id carrier) e)))
        (%drain-carrier-fibers carrier)
        (setf (carrier-scheduler-fiber carrier) nil)
        (ignore-errors (sb-fiber:destroy-fiber scheduler-fiber))))))

(defun carrier-loop-iter (carrier scheduler)
  "One iteration of the carrier loop: drain incoming, check timers/parked,
run a coroutine if one is ready, otherwise idle.

The timer/parked checks run on EVERY iter (not only when the deque is
empty) to close a wake/park race: when WAKE-COROUTINE-FROM-FOREIGN is
called from a reactor callback while the target coroutine is still
:running (between its CONTAINING-COROUTINE-PARK setting the wake
predicate and transitioning state to :suspended), the foreign wake is
dropped because COROUTINE-SUSPENDED-P returns NIL.  The coroutine
finishes parking with the predicate already firing (e.g. fd-wait-slot's
fired flag is T), but it sits on the parked list until something
prompts a check.  Under burst load, the deque rarely empties, so the
check could be deferred up to the timer's deadline -- 5s in the TLS
async-transport handshake path, manifesting as deadline-exceeded
errors.  Polling on every iter is O(parked-size) per tick, which is
trivial for typical workloads (a few parked fibers per carrier)."
  ;; Liveness: bump the per-carrier counter so an external watchdog can
  ;; tell whether this carrier is still making progress.  PROGRESS-STAMP
  ;; is the universal-time of the last bump; it is what watchdog code
  ;; samples (microsecond-grained latency would require a faster clock).
  (sb-ext:atomic-incf (carrier-iter-counter carrier))
  (setf (carrier-progress-stamp carrier) (get-universal-time))
  (drain-incoming carrier)
  (check-timer-heap carrier)
  (check-parked-coroutines carrier)
  (let ((coro (pop-deque carrier)))
    (cond
      (coro
       (carrier-run-one carrier scheduler coro))
      (t
       (let ((hook (scheduler-idle-hook scheduler)))
         (when hook (funcall hook)))
       ;; Sleep until something arrives or the next timer fires.
       (let* ((heap (carrier-timer-heap carrier))
              (next (timer:timer-heap-peek heap))
              (now (get-internal-real-time))
              (timeout (cond
                         ((null next) 0.05)
                         ((<= next now) 0)
                         (t (min 0.05
                                 (/ (- next now)
                                    (float internal-time-units-per-second)))))))
         (when (> timeout 0)
           (sem:wait-on-semaphore (carrier-idle-sem carrier)
                                  :timeout timeout)))))))

;;; ---------------------------------------------------------------------------
;;; Scheduler lifecycle
;;; ---------------------------------------------------------------------------

(defun scheduler-start-carriers (scheduler)
  "Start the carrier threads."
  (let ((carriers (scheduler-carriers scheduler)))
    (dotimes (i (scheduler-num-carriers scheduler))
      (let ((carrier (aref carriers i)))
        (setf (carrier-thread carrier)
              (thread:make-thread
               (lambda () (carrier-loop carrier))
               :name (format nil "carrier-~d" (carrier-id carrier))))))))

(defun scheduler-run (scheduler &key wait)
  "Start the scheduler's carrier threads.

If WAIT is true, block until all submitted work completes."
  (scheduler-start-carriers scheduler)
  (when wait
    (scheduler-wait scheduler)))

(defun scheduler-wait (scheduler)
  "Block until all active coroutines have completed."
  (loop
    (let ((count (lock:with-lock ((scheduler-active-lock scheduler))
                   (scheduler-active-count scheduler))))
      (when (<= count 0)
        (return)))
    (sem:wait-on-semaphore (scheduler-done-sem scheduler) :timeout 0.01)))

;;; ---------------------------------------------------------------------------
;;; Stage 1 instrumentation (IMPL-382)
;;;
;;; Two facilities for diagnosing the keepalive hang where a carrier wedges
;;; in a tight call_into_lisp loop and never reaches a GC safepoint:
;;;
;;; 1. DUMP-CARRIER-BACKTRACES asks every carrier to print its current
;;;    Lisp backtrace to a stream.  Useful WHILE the process is still
;;;    making progress (call from a separate REPL connection or schedule
;;;    a coroutine to call it).  Once a carrier is genuinely wedged the
;;;    rest of the runtime is GC-stopped and even interrupt-thread will
;;;    not deliver, so capture the snapshot mid-run, before the hang.
;;;
;;; 2. CARRIER-FIBER-STACK-SNAPSHOT walks each carrier's currently-known
;;;    coroutines and returns a plist with their fiber stack-usage and
;;;    capacity.  Read-only and lock-light: confirms whether per-fiber
;;;    stacks grow unbounded across requests.
;;; ---------------------------------------------------------------------------

(defun %carrier-known-coroutines (carrier)
  "Return a fresh list of all coroutines currently tracked by CARRIER:
its deque, parked list, and incoming queue.  Held briefly under the
incoming-lock; deque and parked are owner-thread-private so no lock is
required for a snapshot read."
  (let ((from-incoming
          (lock:with-lock ((carrier-incoming-lock carrier))
            (copy-list (carrier-incoming-queue carrier)))))
    (append (copy-list (carrier-deque carrier))
            (copy-list (carrier-parked carrier))
            from-incoming)))

(defun carrier-fiber-stack-snapshot (scheduler)
  "Walk SCHEDULER's carriers and collect fiber stack-usage stats for every
coroutine they currently know about.  Returns a list of plists shaped
like (:carrier <id> :coroutines (<plist> ...)).  Each coroutine plist is
the result of COROUTINE-FIBER-STACK-STATS.  Coroutines whose fiber has
not yet been allocated are omitted.  Suitable for logging across the
lifetime of the reproducer to confirm the unbounded-growth hypothesis."
  (let ((carriers (scheduler-carriers scheduler))
        (results nil))
    (dotimes (i (scheduler-num-carriers scheduler))
      (let* ((carrier (aref carriers i))
             (coros (%carrier-known-coroutines carrier))
             (stats (loop for c in coros
                          for s = (coro:fiber-stack-stats c)
                          when s collect s)))
        (push (list :carrier (carrier-id carrier)
                    :coroutines stats)
              results)))
    (nreverse results)))

;;; ---------------------------------------------------------------------------
;;; Allocation-free carrier backtrace.
;;;
;;; The watchdog asks for a backtrace from a carrier that has been
;;; stuck for STALL-WARN-SECONDS.  The naive implementation
;;; (PRINC-TO-STRING each frame) is unsafe under near-OOM: every
;;; printed frame allocates fresh string buffers from inside the
;;; SIGURG handler, and one of those allocations is what tipped a
;;; 99%-full SBCL heap into "Heap exhausted, game over" in the
;;; sibley-upload incident.
;;;
;;; The replacement avoids heap allocation in the signal handler.
;;; Each carrier owns a preallocated character buffer (sized at
;;; carrier creation, see +CARRIER-BACKTRACE-MAX-FRAMES+ *
;;; +CARRIER-BACKTRACE-LINE-BYTES+).  The SIGURG handler walks frames
;;; via SB-DI:TOP-FRAME / SB-DI:FRAME-DOWN and writes the function
;;; name into the buffer through VECTOR-PUSH on a non-adjustable
;;; string with a fill pointer -- VECTOR-PUSH on such a vector either
;;; succeeds or returns NIL, but it never grows the underlying
;;; storage and never conses.  When the SIGURG handler is done it
;;; bumps BACKTRACE-SEQ via SB-EXT:ATOMIC-INCF; the watchdog spins on
;;; that counter and then splits the buffer into a list of frame
;;; strings (which CAN allocate, because the watchdog thread is not
;;; the one whose heap is starving).
;;;
;;; Trade: argument values and source-line metadata that
;;; PRINT-BACKTRACE used to emit are gone.  Function names alone are
;;; what CLASSIFY-STALL-REASON looks at, and the operator gets enough
;;; to triage; a deeper post-mortem can use the live-process
;;; introspection endpoint.
;;; ---------------------------------------------------------------------------

(declaim (inline %write-string-bounded))
(defun %write-string-bounded (buf s limit)
  "Push the characters of S into BUF (a string with a fill pointer)
as long as BUF's fill pointer is below LIMIT.  Allocation-free."
  (declare (type string buf s) (type fixnum limit) (optimize speed))
  (let ((sl (length s)))
    (declare (type fixnum sl))
    (loop for i fixnum from 0 below sl
          while (< (fill-pointer buf) limit)
          do (vector-push (char s i) buf))))

(defun %write-symbol-name-bounded (buf sym limit)
  "Write SYM's package-qualified name into BUF (no allocation:
SYMBOL-PACKAGE / PACKAGE-NAME / SYMBOL-NAME are all slot reads)."
  (declare (type string buf) (type symbol sym) (type fixnum limit))
  (let* ((pkg (symbol-package sym))
         (pname (and pkg (package-name pkg))))
    (cond
      ((null pkg)
       (%write-string-bounded buf "#:" limit)
       (%write-string-bounded buf (symbol-name sym) limit))
      ((string= pname "KEYWORD")
       (when (< (fill-pointer buf) limit)
         (vector-push #\: buf))
       (%write-string-bounded buf (symbol-name sym) limit))
      (t
       (%write-string-bounded buf pname limit)
       (when (< (fill-pointer buf) limit)
         (vector-push #\: buf)
         (when (< (fill-pointer buf) limit)
           (vector-push #\: buf)))
       (%write-string-bounded buf (symbol-name sym) limit)))))

(defun %write-name-bounded (buf name limit depth)
  "Write a function name (symbol, string, or list) into BUF, capped
at LIMIT bytes and DEPTH levels of cons nesting.  Allocation-free.
Renders lists structurally (e.g. (LAMBDA NIL :IN SLEEP)) so the
stall-reason classifier can substring-match the inner symbol names."
  (declare (type string buf) (type fixnum limit depth))
  (cond
    ((symbolp name)
     (%write-symbol-name-bounded buf name limit))
    ((stringp name)
     (%write-string-bounded buf name limit))
    ((and (consp name) (plusp depth))
     (when (< (fill-pointer buf) limit)
       (vector-push #\( buf))
     (let ((first t)
           (rest name))
       (loop while (and (consp rest)
                        (< (fill-pointer buf) limit))
             do (cond
                  (first (setf first nil))
                  (t (when (< (fill-pointer buf) limit)
                       (vector-push #\Space buf))))
                (%write-name-bounded buf (car rest) limit (1- depth))
                (setf rest (cdr rest)))
       (when (and rest (not (consp rest))
                  (< (fill-pointer buf) limit))
         (%write-string-bounded buf " . " limit)
         (%write-name-bounded buf rest limit (1- depth))))
     (when (< (fill-pointer buf) limit)
       (vector-push #\) buf)))
    ((null name)
     (%write-string-bounded buf "NIL" limit))
    (t
     (%write-string-bounded buf "<unprintable>" limit))))

(defun %write-frame-name-bounded (buf frame line-limit)
  "Write FRAME's function name into BUF, capped at LINE-LIMIT
bytes.  Allocation-free."
  (declare (type string buf) (type fixnum line-limit))
  (handler-case
      (let* ((debug-fun (sb-di:frame-debug-fun frame))
             (name (sb-di:debug-fun-name debug-fun)))
        (%write-name-bounded buf name line-limit 4))
    (condition ()
      (%write-string-bounded buf "<error>" line-limit))))

(defun %frame-is-bogus-marker-p (frame)
  "Return T when FRAME's debug-fun-name is the string \"bogus
stack frame\" -- SBCL inserts this at the C/Lisp boundary that
separates a signal-handler chain from the interrupted Lisp code."
  (handler-case
      (let* ((debug-fun (sb-di:frame-debug-fun frame))
             (name (sb-di:debug-fun-name debug-fun)))
        (and (stringp name) (search "bogus" name :test #'char-equal)))
    (condition () nil)))

(defun %skip-interrupt-handler-frames (frame)
  "When FRAME is the top of an SB-THREAD:INTERRUPT-THREAD-delivered
SIGURG stack, walk past the signal-handler chain (including the
trailing \"bogus stack frame\" marker SBCL inserts) and return the
frame for the interrupted code.  When no bogus marker is found
within the first 32 frames, return FRAME unchanged so a non-interrupt
caller still gets a useful backtrace."
  (let ((cur frame))
    (dotimes (i 32 frame)
      (when (null cur) (return frame))
      (when (%frame-is-bogus-marker-p cur)
        (return (or (sb-di:frame-down cur) frame)))
      (setf cur (handler-case (sb-di:frame-down cur)
                  (condition () nil))))))

(defun %capture-backtrace-into-buffer (carrier)
  "Walk the current thread's stack and write at most
+CARRIER-BACKTRACE-MAX-FRAMES+ function-name lines into CARRIER's
preallocated buffer, deepest-first.  Each line is capped at
+CARRIER-BACKTRACE-LINE-BYTES+ bytes and terminated with #\\Newline.
After writing, BACKTRACE-SEQ is bumped via SB-EXT:ATOMIC-INCF so
the caller in another thread observes the buffer as complete.

Designed to run from inside SB-THREAD:INTERRUPT-THREAD, i.e.
from a SIGURG handler -- no heap allocation beyond the fixed-size
SB-DI frame structs that SB-DI:FRAME-DOWN cons up internally.
Even those are bounded by MAX-FRAMES.

When called from a SIGURG handler the top of the stack is a long
chain of SBCL signal-handler internals (RUN-INTERRUPTION,
INVOKE-INTERRUPTION, the C trampolines, a \"bogus stack frame\"
boundary marker).  %SKIP-INTERRUPT-HANDLER-FRAMES walks past those
so the recorded frames begin with the actual interrupted code --
which is what the stall classifier expects."
  (let ((buf (carrier-backtrace-buffer carrier)))
    (when buf
      (setf (fill-pointer buf) 0)
      (handler-case
          (let ((frame (%skip-interrupt-handler-frames (sb-di:top-frame)))
                (count 0)
                (total-limit (array-total-size buf)))
            (declare (type fixnum count total-limit))
            (loop while (and frame
                             (< count +carrier-backtrace-max-frames+)
                             (< (+ (fill-pointer buf)
                                   +carrier-backtrace-line-bytes+)
                                total-limit))
                  do (let ((line-limit
                             (min total-limit
                                  (+ (fill-pointer buf)
                                     (1- +carrier-backtrace-line-bytes+)))))
                       (%write-frame-name-bounded buf frame line-limit))
                     (when (< (fill-pointer buf) total-limit)
                       (vector-push #\Newline buf))
                     (incf count)
                     (setf frame (sb-di:frame-down frame))))
        (condition () nil))
      (sb-ext:atomic-incf (carrier-backtrace-seq carrier)))))

(defun %split-buffer-frames (buf)
  "Split a freshly-captured backtrace buffer into a list of frame
strings, one per #\\Newline.  Runs in the watchdog/dumper thread,
so allocation here is fine."
  (let ((out nil)
        (start 0)
        (end (fill-pointer buf)))
    (loop for i fixnum from 0 below end
          do (when (char= (char buf i) #\Newline)
               (push (subseq buf start i) out)
               (setf start (1+ i))))
    (when (< start end)
      (push (subseq buf start end) out))
    (nreverse out)))

(defun %wait-for-backtrace (carrier start-seq deadline-internal)
  "Spin-sleep in the watchdog/dumper thread until CARRIER's
BACKTRACE-SEQ exceeds START-SEQ or DEADLINE-INTERNAL passes.
Returns T on capture, NIL on timeout."
  (loop
    (when (/= (carrier-backtrace-seq carrier) start-seq)
      (return t))
    (when (>= (get-internal-real-time) deadline-internal)
      (return nil))
    (sleep 0.005)))

(defun carrier-backtrace (carrier &key (max-frames 20) (timeout 1.0))
  "Capture CARRIER's current Lisp backtrace as a list of strings,
one per frame, deepest-first.  Returns NIL if the carrier thread is
not alive, the interrupt could not be delivered, or the timeout
elapsed before the SIGURG handler finished writing.

  MAX-FRAMES   informational; the buffer is pre-sized for at most
               +CARRIER-BACKTRACE-MAX-FRAMES+ frames.  Smaller
               values are honoured as a post-walk truncation.
  TIMEOUT      seconds to wait for the carrier's thread to honour
               the interrupt (default 1.0).

The capture is allocation-free in the SIGURG handler (writes into
a preallocated per-carrier buffer), so it survives near-OOM
conditions that the previous PRINC-TO-STRING-based implementation
would have crashed on.  Argument values and source-line info are
not included; CLASSIFY-STALL-REASON only consults function names."
  (let* ((thread (carrier-thread carrier))
         (buf (carrier-backtrace-buffer carrier)))
    (when (and thread (thread:thread-alive-p thread) buf)
      (let ((start-seq (carrier-backtrace-seq carrier)))
        (handler-case
            (thread:interrupt-thread
             thread
             (lambda () (%capture-backtrace-into-buffer carrier)))
          (error () (return-from carrier-backtrace nil)))
        (let ((deadline (+ (get-internal-real-time)
                           (truncate (* timeout
                                        internal-time-units-per-second)))))
          (when (%wait-for-backtrace carrier start-seq deadline)
            (let ((frames (%split-buffer-frames buf)))
              (cond ((null frames) nil)
                    ((and max-frames (> (length frames) max-frames))
                     (subseq frames 0 max-frames))
                    (t frames)))))))))

(defun %print-carrier-backtrace (carrier stream)
  "Write CARRIER's most recently captured backtrace buffer to
STREAM with a header.  Called from the dumper thread (the watchdog
or the introspection endpoint), NOT from inside an interrupt --
the SIGURG handler only fills the buffer."
  (handler-case
      (progn
        (format stream "~&;; ============================================================~%")
        (format stream ";; Backtrace of carrier ~D (thread ~A) at ~A~%"
                (carrier-id carrier)
                (ignore-errors (thread:thread-name (carrier-thread carrier)))
                (get-universal-time))
        (format stream ";; ============================================================~%")
        (let ((buf (carrier-backtrace-buffer carrier)))
          (when buf
            (write-sequence buf stream :end (fill-pointer buf))))
        (format stream "~&;; ------------------------------------------------------------~%")
        (force-output stream))
    (condition (c)
      (format stream "~&;; Backtrace failed for carrier ~D: ~A~%"
              (carrier-id carrier) c))))

(defun dump-carrier-backtraces (scheduler &key (stream *error-output*)
                                               (timeout 0.5))
  "Ask every carrier in SCHEDULER to capture its current Lisp
backtrace into its preallocated buffer, then print the buffers to
STREAM from this thread.  Uses %CAPTURE-BACKTRACE-INTO-BUFFER
inside SB-THREAD:INTERRUPT-THREAD so the per-carrier capture is
allocation-free; printing happens here (not inside the interrupt).

Carriers that do not honour the interrupt within TIMEOUT seconds
are reported as unresponsive but do not block the dump.  Returns
the list of carriers whose buffer was successfully captured."
  (let* ((carriers (scheduler-carriers scheduler))
         (n (scheduler-num-carriers scheduler))
         (start-seqs (make-array n :initial-element 0))
         (interrupted (make-array n :initial-element nil)))
    (dotimes (i n)
      (let* ((carrier (aref carriers i))
             (thread (carrier-thread carrier)))
        (when (and thread (thread:thread-alive-p thread)
                   (carrier-backtrace-buffer carrier))
          (setf (aref start-seqs i) (carrier-backtrace-seq carrier))
          (handler-case
              (progn
                (thread:interrupt-thread
                 thread
                 (lambda () (%capture-backtrace-into-buffer carrier)))
                (setf (aref interrupted i) t))
            (error (e)
              (format stream
                      "~&;; Could not interrupt carrier ~D: ~A~%"
                      (carrier-id carrier) e))))))
    (let ((deadline (+ (get-internal-real-time)
                       (truncate (* timeout
                                    internal-time-units-per-second))))
          (captured nil))
      (dotimes (i n)
        (when (aref interrupted i)
          (let ((carrier (aref carriers i)))
            (cond
              ((%wait-for-backtrace carrier (aref start-seqs i) deadline)
               (%print-carrier-backtrace carrier stream)
               (push carrier captured))
              (t
               (format stream
                       "~&;; Carrier ~D unresponsive within ~Fs~%"
                       (carrier-id carrier) timeout))))))
      (nreverse captured))))

(defun scheduler-shutdown (scheduler &key (timeout 5))
  "Shut down the scheduler, stopping all carrier threads."
  (setf (scheduler-running-p scheduler) nil)
  (let ((carriers (scheduler-carriers scheduler)))
    (dotimes (i (scheduler-num-carriers scheduler))
      (let ((c (aref carriers i)))
        (setf (carrier-running-p c) nil)
        ;; Wake the carrier so it notices running-p has flipped.
        (sem:signal-semaphore (carrier-idle-sem c))))
    (dotimes (i (scheduler-num-carriers scheduler))
      (let ((thread (carrier-thread (aref carriers i))))
        (when (and thread (thread:thread-alive-p thread))
          (handler-case
              (sb-ext:with-timeout timeout
                (thread:join-thread thread))
            (sb-ext:timeout ()
              (format *error-output*
                      "~&Warning: Carrier thread ~D did not exit within ~Ds~%"
                      i timeout))))))))

(defun coroutine-wait-until (predicate &key timeout cancellation)
  "Suspend the current coroutine until PREDICATE returns true (or
TIMEOUT seconds elapse, or CANCELLATION fires).  Execution resumes
in-place after this call returns."
  (unless coro:*current-coroutine*
    (error "coroutine-wait-until called outside a coroutine"))
  (coro:coroutine-park :wake-when predicate
                       :timeout timeout
                       :cancellation cancellation))

(defun coroutine-sleep (seconds &key cancellation)
  "Suspend the current coroutine for approximately SECONDS.
Coroutine equivalent of CL:SLEEP -- yields to the scheduler instead of
blocking the carrier thread.  If CANCELLATION fires, signals
FIBER-CANCELLED."
  (unless coro:*current-coroutine*
    (error "coroutine-sleep called outside a coroutine"))
  (coro:coroutine-park :timeout seconds :cancellation cancellation))

(defun run-blocking (fn)
  "Offload FN to a separate OS thread and park the current coroutine
until it completes.  Returns the value FN returned, or re-signals the
error FN raised."
  (unless coro:*current-coroutine*
    (error "run-blocking called outside a coroutine"))
  (let ((done (list nil))
        (result (list nil))
        (err (list nil))
        (carrier *current-carrier*))
    (thread:make-thread
     (lambda ()
       (handler-case
           (setf (car result) (funcall fn))
         (error (e)
           (setf (car err) e)))
       (setf (car done) t)
       ;; Poke the owner carrier so it doesn't sleep through the wake.
       (when carrier
         (sem:signal-semaphore (carrier-idle-sem carrier))))
     :name "blocking-worker")
    (coro:coroutine-park :wake-when (lambda () (car done)))
    (when (car err) (error (car err)))
    (car result)))

(defmacro with-scheduler ((var &key (num-carriers nil num-carriers-p)
                                     (idle-hook nil idle-hook-p))
                          &body body)
  "Execute BODY with VAR bound to a new scheduler.

The scheduler's carrier threads are started before BODY executes.
On exit (normal or error), all active coroutines are allowed to complete
and carrier threads are joined."
  `(let* ((,var (make-scheduler ,@(when num-carriers-p
                                    `(:num-carriers ,num-carriers))
                                ,@(when idle-hook-p
                                    `(:idle-hook ,idle-hook))))
          (*current-scheduler* ,var)
          (channel:*task-run-hook*
            (lambda (thunk &key (name "coroutine"))
              (declare (ignore name))
              (scheduler-submit ,var thunk))))
     (scheduler-run ,var)
     (unwind-protect
          (progn ,@body)
       (scheduler-wait ,var)
       (scheduler-shutdown ,var))))

;;;; Tests for the coroutine scheduler.

(defpackage :epsilon.scheduler.tests
  (:use :cl :epsilon.test)
  (:import (epsilon.scheduler sched)
            (epsilon.scheduler.coroutine coro)
   (epsilon.sys.lock lock)
   (epsilon.sys.thread thread)))

;;; ============================================================================
;;; Coroutine Basics (no scheduler needed -- exercise the fiber wrapper
;;; directly using a manually-allocated main fiber).
;;; ============================================================================

(defmacro with-direct-coroutine ((coro fn) &body body)
  "Run BODY with CORO bound to a fiber-backed coroutine and a
freshly-allocated main fiber.  RUN-CORO is a local helper that
switches into the coroutine once and returns RUN-COROUTINE's result."
  (let ((mf (gensym "MF")))
    `(let* ((,coro (coro:make-coroutine ,fn))
            (,mf (sb-fiber:make-main-fiber)))
       (flet ((run-coro () (coro:run-coroutine ,coro ,mf)))
         (declare (ignorable (function run-coro)))
         (unwind-protect
              (progn ,@body)
           (when (coro:coroutine-fiber ,coro)
             (coro:destroy-coroutine ,coro))
           (sb-fiber:destroy-fiber ,mf))))))

(deftest test-coroutine-creation
  "A new coroutine starts in :ready state."
  (with-direct-coroutine (c (lambda () 42))
    (assert-true (coro:coroutine-p c))
    (assert-true (coro:coroutine-ready-p c))
    (assert-true (not (coro:coroutine-done-p c)))))

(deftest test-coroutine-run-complete
  "Running a coroutine that returns immediately completes it."
  (with-direct-coroutine (c (lambda () 42))
    (let ((action (run-coro)))
      (assert-nil action)
      (assert-true (coro:coroutine-completed-p c))
      (assert-= 42 (coro:coroutine-result c)))))

(deftest test-coroutine-run-error
  "A coroutine that signals an error enters :failed state."
  (with-direct-coroutine (c (lambda () (error "boom")))
    (let ((action (run-coro)))
      (assert-eq :failed action)
      (assert-true (coro:coroutine-failed-p c))
      (assert-not-null (coro:coroutine-error-value c)))))

(deftest test-coroutine-yield-resumes-in-place
  "After a yield, execution resumes in-place at the call site (the
fiber preserves the call stack)."
  (let* ((step 0))
    (with-direct-coroutine (c (lambda ()
                                (incf step)
                                (coro:coroutine-yield)
                                (incf step)
                                (coro:coroutine-yield)
                                (incf step)))
      (let ((action (run-coro)))
        (assert-eq :yield action)
        (assert-= 1 step))
      (let ((action (run-coro)))
        (assert-eq :yield action)
        (assert-= 2 step))
      (let ((action (run-coro)))
        (assert-nil action)
        (assert-= 3 step)
        (assert-true (coro:coroutine-completed-p c))))))

(deftest test-coroutine-park-suspends
  "A parked coroutine enters :suspended state with its wake predicate set."
  (let* ((flag nil))
    (with-direct-coroutine (c (lambda ()
                                (coro:coroutine-park
                                 :wake-when (lambda () flag))
                                :woke-up))
      (let ((action (run-coro)))
        (assert-eq :park action)
        (assert-true (coro:coroutine-suspended-p c))
        (assert-not-null (coro:coroutine-wake-predicate c)))
      (assert-nil (funcall (coro:coroutine-wake-predicate c)))
      (setf flag t)
      (assert-true (funcall (coro:coroutine-wake-predicate c)))
      ;; Manually transition to :ready and re-enter -- execution resumes
      ;; right after the park call, so we get :woke-up as the result.
      (setf (coro:coroutine-state c) :ready)
      (let ((action (run-coro)))
        (assert-nil action)
        (assert-true (coro:coroutine-completed-p c))
        (assert-eq :woke-up (coro:coroutine-result c))))))

;;; ============================================================================
;;; Scheduler Basics
;;; ============================================================================

(deftest test-scheduler-creation
  "Create a scheduler with specified carrier count."
  (let ((s (sched:make-scheduler :num-carriers 2)))
    (assert-true (sched:scheduler-p s))))

(deftest test-scheduler-single-coroutine
  "Submit a single coroutine and wait for it to complete."
  (let ((result nil))
    (sched:with-scheduler (s :num-carriers 2)
      (sched:scheduler-submit s
        (lambda () (setf result 42))))
    (assert-= 42 result)))

(deftest test-scheduler-multiple-coroutines
  "Submit multiple coroutines and verify all complete."
  (let ((counter 0)
        (lock (lock:make-lock)))
    (sched:with-scheduler (s :num-carriers 2)
      (dotimes (i 100)
        (sched:scheduler-submit s
          (lambda ()
            (lock:with-lock (lock)
              (incf counter))))))
    (assert-= 100 counter)))

(deftest test-scheduler-yielding-coroutines
  "Coroutines that yield N times still complete with the right post-yield value."
  (let ((results (make-array 10 :initial-element 0)))
    (sched:with-scheduler (s :num-carriers 2)
      (dotimes (i 10)
        (let ((idx i))
          (sched:scheduler-submit s
            (lambda ()
              (dotimes (_ 4)
                (coro:coroutine-yield))
              (setf (aref results idx) 5))))))
    (dotimes (i 10)
      (assert-= 5 (aref results i)))))

(deftest test-scheduler-parking
  "Parked coroutines are woken when their predicate becomes true; they
continue in-place after the park call."
  (let* ((state (vector nil nil)))  ; (0)=flag  (1)=result
    (sched:with-scheduler (s :num-carriers 2)
      (sched:scheduler-submit s
        (lambda ()
          (coro:coroutine-park :wake-when (lambda () (svref state 0)))
          (setf (svref state 1) :woken)))
      (sleep 0.05)
      (setf (svref state 0) t))
    (assert-eq :woken (svref state 1))))

(deftest test-scheduler-error-handling
  "Coroutines that error are marked as failed without crashing the scheduler."
  (let ((good-result nil))
    (sched:with-scheduler (s :num-carriers 2)
      (sched:scheduler-submit s
        (lambda () (error "intentional error")))
      (sched:scheduler-submit s
        (lambda () (setf good-result :ok))))
    (assert-eq :ok good-result)))

;;; ============================================================================
;;; Work Distribution
;;; ============================================================================

(deftest test-scheduler-work-distribution
  "Work is distributed across multiple carriers via round-robin pinning."
  (let ((carrier-ids (make-array 0 :fill-pointer 0 :adjustable t))
        (lock (lock:make-lock)))
    (sched:with-scheduler (s :num-carriers 4)
      (dotimes (i 100)
        (sched:scheduler-submit s
          (lambda ()
            (let ((name (thread:thread-name (thread:current-thread))))
              (lock:with-lock (lock)
                (vector-push-extend name carrier-ids)))))))
    (let ((unique (remove-duplicates (coerce carrier-ids 'list) :test #'equal)))
      (assert-true (>= (length unique) 2)))))

;;; ============================================================================
;;; Scale Test
;;; ============================================================================

(deftest test-scheduler-10k-coroutines
  "Spawn 10,000 coroutines that each yield once then complete."
  (let ((n 10000)
        (completed 0)
        (lock (lock:make-lock)))
    (sched:with-scheduler (s :num-carriers 4)
      (dotimes (i n)
        (sched:scheduler-submit s
          (lambda ()
            (coro:coroutine-yield)
            (lock:with-lock (lock)
              (incf completed))))))
    (assert-= n completed)))

;;; ============================================================================
;;; Special Variable Propagation
;;; ============================================================================

(deftest test-scheduler-special-variables
  "Dynamic bindings established before SCHEDULER-SUBMIT remain visible
inside the coroutine body via the fiber's own binding stack."
  (let ((result nil))
    (let ((*package* (find-package :keyword)))
      (sched:with-scheduler (s :num-carriers 2)
        (sched:scheduler-submit s
          (lambda ()
            (setf result (package-name *package*))))))
    ;; *package* is captured by the closure; verify the body ran with
    ;; the lambda's lexical environment intact.
    (assert-equal "KEYWORD" result)))

;;; ============================================================================
;;; Carrier visibility across fiber switch (IMPL-382 finding)
;;; ============================================================================

(deftest test-scheduler-current-carrier-visibility
  "sched:*current-carrier* established by carrier-run-one's LET wrapper
IS visible from inside the coroutine fiber's body, even though it is
not in *captured-specials*.

The mechanism: SBCL stores the live value of a special variable in a
per-OS-thread TLS cell.  A LET binding writes the new value to that TLS
cell and pushes a (symbol, old-value) pair on the binding stack so the
unwind path can restore it.  sb-fiber:fiber-switch swaps the binding
stack pointer (so unwind targets the right fiber's bindings) but does
not swap the TLS cells -- those remain at whatever the host OS thread
last wrote them.  Because every coroutine pinned to a carrier runs on
that carrier's OS thread, it reads the carrier's TLS cells.

Consequence for IMPL-382: epsilon.web:make-dispatching-handler's check
\"if sched:*current-carrier* then dispatch synchronously\" DOES fire on
every request.  Per-request coroutine spawning was hypothesised earlier
in this investigation but it is not actually happening; the connection
coroutine handles each request in-place."
  (let ((observed-carrier :sentinel)
        (observed-symbol-bound-p :sentinel))
    (sched:with-scheduler (s :num-carriers 1)
      (sched:scheduler-submit s
        (lambda ()
          (setf observed-symbol-bound-p (boundp 'sched:*current-carrier*))
          (setf observed-carrier sched:*current-carrier*))))
    (assert-true observed-symbol-bound-p)
    (assert-not-null observed-carrier)
    (assert-true (sched::carrier-p observed-carrier))))

(deftest test-scheduler-current-coroutine-visibility
  "By contrast, coro:*current-coroutine* IS visible inside the coroutine
fiber, because ensure-fiber wraps the user body with an explicit
(let ((*current-coroutine* coro)) ...) inside the fiber's lambda.  This
is the binding callers should consult to detect they are running on a
scheduler carrier, not *current-carrier*."
  (let ((observed nil))
    (sched:with-scheduler (s :num-carriers 1)
      (sched:scheduler-submit s
        (lambda ()
          (setf observed coro:*current-coroutine*))))
    (assert-not-null observed)
    (assert-true (coro:coroutine-p observed))))

;;; ============================================================================
;;; Bounded accept queue: SCHEDULER-TRY-SUBMIT and 503-style overflow.
;;;
;;; The bounded queue is the in-process backstop against accept-side
;;; flooding: when a carrier's incoming queue is at the cap, new
;;; submissions get refused (NIL) instead of growing the queue without
;;; bound.  We exercise it by holding a 1-carrier scheduler busy on a
;;; never-completing coroutine, filling the carrier's incoming queue to
;;; the cap, and verifying the next submit is refused.
;;; ============================================================================

(deftest test-scheduler-try-submit-unbounded-default
  "Without :MAX-INCOMING-QUEUE the cap is NIL and try-submit always
   succeeds, matching SCHEDULER-SUBMIT semantics."
  (let ((sched (sched:make-scheduler :num-carriers 1)))
    (unwind-protect
         (loop repeat 64
               do (assert-not-null
                   (sched:scheduler-try-submit sched (lambda ()))))
      (sched:scheduler-shutdown sched))))

(deftest test-scheduler-try-submit-respects-cap
  "When :MAX-INCOMING-QUEUE is N, the (N+1)th external submission is
   refused while the carrier is blocked.  OVERFLOW-COUNT increments on
   each refusal."
  (let ((sched (sched:make-scheduler :num-carriers 1
                                     :max-incoming-queue 4))
        (gate-lock (lock:make-lock "test-gate"))
        (gate (sb-thread:make-waitqueue :name "test-gate"))
        (gate-open nil))
    (unwind-protect
         (progn
           ;; Start the carriers running so the first submission is
           ;; picked up and parks on the gate; subsequent submissions
           ;; queue up behind it.
           (sched:scheduler-run sched)
           (sched:scheduler-submit
            sched
            (lambda ()
              (lock:with-lock (gate-lock)
                (loop until gate-open
                      do (lock:condition-wait gate gate-lock)))))
           ;; Give the carrier a moment to drain that submission.
           (sleep 0.05)
           ;; Now flood the incoming queue.  Each of these is just a
           ;; lambda that would terminate quickly if it ever ran.
           (loop repeat 4
                 do (assert-not-null
                     (sched:scheduler-try-submit sched (lambda ()))))
           ;; The next submit should be refused.
           (assert-nil (sched:scheduler-try-submit sched (lambda ())))
           (assert-= 1 (sched:scheduler-overflow-count sched))
           ;; And again: overflow count rises.
           (assert-nil (sched:scheduler-try-submit sched (lambda ())))
           (assert-= 2 (sched:scheduler-overflow-count sched))
           ;; SCHEDULER-SATURATED-P agrees.
           (assert-true (sched:scheduler-saturated-p sched))
           ;; Release the carrier so shutdown completes promptly.
           (lock:with-lock (gate-lock)
             (setf gate-open t)
             (sb-thread:condition-broadcast gate)))
      (sched:scheduler-shutdown sched))))

(deftest test-scheduler-saturated-p-default
  "Without a cap, scheduler-saturated-p is NIL even after many
   submissions."
  (let ((sched (sched:make-scheduler :num-carriers 2)))
    (unwind-protect
         (progn
           (loop repeat 100
                 do (sched:scheduler-submit sched (lambda ())))
           (assert-nil (sched:scheduler-saturated-p sched)))
      (sched:scheduler-shutdown sched))))

;;; ============================================================================
;;; Carrier liveness counters
;;; ============================================================================

(deftest test-carrier-iter-counter-advances
  "Every CARRIER-LOOP-ITER bumps CARRIER-ITER-COUNTER and updates
   CARRIER-PROGRESS-STAMP.  An external watchdog reads these to detect
   wedged carriers."
  (let ((sched (sched:make-scheduler :num-carriers 1)))
    (unwind-protect
         (progn
           (sched:scheduler-run sched)
           (let* ((carriers (sched:scheduler-carriers sched))
                  (c (aref carriers 0))
                  (initial (sched:carrier-iter-counter c)))
             ;; Wait long enough for at least one idle iter to elapse
             ;; (the idle wait is bounded at 50ms in carrier-loop-iter).
             (sleep 0.2)
             (assert-true (> (sched:carrier-iter-counter c) initial))
             (assert-true (plusp (sched:carrier-progress-stamp c)))))
      (sched:scheduler-shutdown sched))))

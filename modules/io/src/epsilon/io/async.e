;;;; epsilon.io.async - Structured async IO runtime
;;;;
;;;; - Tasks have explicit ownership and lifecycle
;;;; - Context scopes ensure cleanup on exit
;;;; - Cancellation propagates through task trees
;;;; - Await-style interface

(package epsilon.io.async
  (import (epsilon.async async)
          (epsilon.sys.lock lock)
          (epsilon.sys.variable cv)))

;;; ============================================================================
;;; Local Condition Types
;;; ============================================================================

(define-condition async-error (error)
  ((operation :initarg :operation :initform nil :reader async-error-operation))
  (:report (lambda (c s)
             (format s "Async error during ~A" (async-error-operation c)))))

(define-condition cancelled-error (async-error)
  ()
  (:report (lambda (c s)
             (format s "Operation ~A was cancelled" (async-error-operation c)))))

(define-condition closed-error (async-error)
  ((stream :initarg :stream :initform nil :reader closed-error-stream))
  (:report (lambda (c s)
             (format s "Operation ~A on closed stream" (async-error-operation c)))))

(define-condition timeout-error (async-error)
  ((duration :initarg :duration :initform nil :reader timeout-error-duration))
  (:report (lambda (c s)
             (format s "Operation ~A timed out after ~A"
                     (async-error-operation c)
                     (timeout-error-duration c)))))

;;; ============================================================================
;;; Task States and Results
;;; ============================================================================

(deftype task-state ()
  "Possible states for an async task."
  '(member :pending :running :completed :failed :cancelled))

(defstruct (task-result (:constructor %make-task-result))
  "Result of a completed task."
  (value nil)
  (error nil))

(defun make-success-result (value)
  (%make-task-result :value value))

(defun make-error-result (error)
  (%make-task-result :error error))

;;; ============================================================================
;;; Task Handle
;;; ============================================================================

(defvar *task-id-counter* 0)

(defstruct (task (:constructor %make-task))
  "Handle to an async operation.

   Tasks represent in-flight async operations. They can be awaited,
   polled, or cancelled."
  (id 0 :type fixnum)
  (state :pending :type task-state)
  (result nil :type (or null task-result))
  (context nil)                              ; owning io-context
  (operation nil)                            ; platform operation handle
  (cancel-fn nil :type (or null function))   ; how to cancel
  (waiters nil :type list)                   ; threads waiting on this task
  (lock (lock:make-lock "task")))

(defun make-task (context &key operation cancel-fn)
  "Create a new task owned by CONTEXT."
  (%make-task :id (lock:with-lock ((io-context-lock context))
                    (incf *task-id-counter*))
              :context context
              :operation operation
              :cancel-fn cancel-fn))

(defun task-complete-p (task)
  "Return T if task is in a terminal state."
  (member (task-state task) '(:completed :failed :cancelled)))

(defun task-complete (task value)
  "Mark task as successfully completed with VALUE."
  (lock:with-lock ((task-lock task))
    (unless (task-complete-p task)
      (setf (task-state task) :completed)
      (setf (task-result task) (make-success-result value))
      (task-wake-waiters task))))

(defun task-fail (task error)
  "Mark task as failed with ERROR."
  (lock:with-lock ((task-lock task))
    (unless (task-complete-p task)
      (setf (task-state task) :failed)
      (setf (task-result task) (make-error-result error))
      (task-wake-waiters task))))

(defun task-cancel (task)
  "Request cancellation of TASK.
   Returns T if cancellation was initiated, NIL if already complete."
  (lock:with-lock ((task-lock task))
    (when (member (task-state task) '(:pending :running))
      (setf (task-state task) :cancelled)
      (setf (task-result task) (make-error-result
                                (make-condition 'cancelled-error
                                                :operation :async)))
      (when (task-cancel-fn task)
        (funcall (task-cancel-fn task)))
      (task-wake-waiters task)
      t)))

(defun task-wake-waiters (task)
  "Wake all threads waiting on TASK. Called with lock held."
  (dolist (c (task-waiters task))
    (cv:condition-notify c)))

;;; ============================================================================
;;; IO Context - Structured Concurrency Scope
;;; ============================================================================

(defstruct (io-context (:constructor %make-io-context))
  "Scoped container for async operations.

   An io-context owns a set of tasks. When the context is closed,
   all tasks are cancelled and awaited. This ensures structured
   concurrency: no task outlives its parent scope."
  (id 0 :type fixnum)
  (backend nil)                              ; platform backend
  (tasks nil :type list)                     ; owned tasks
  (running-p t :type boolean)
  (closed-p nil :type boolean)
  (lock (lock:make-lock "io-context"))
  (completion-cv (cv:make-condition-variable :name "io-completion")))

(defvar *context-id-counter* 0)

(defun make-io-context (&key backend)
  "Create a new IO context.
   BACKEND is the platform-specific event backend (kqueue/epoll/iocp)."
  (let ((ctx (%make-io-context
              :id (incf *context-id-counter*)
              :backend (or backend (create-default-backend)))))
    (async:ensure-async-system)
    ctx))

(defun create-default-backend ()
  "Create platform-appropriate async backend."
  ;; The platform module (darwin/linux/windows) provides this
  nil)

(defun io-context-close (ctx)
  "Close context, cancelling all tasks and waiting for completion."
  (lock:with-lock ((io-context-lock ctx))
    (when (io-context-closed-p ctx)
      (return-from io-context-close nil))
    (setf (io-context-running-p ctx) nil)
    ;; Cancel all tasks
    (dolist (task (io-context-tasks ctx))
      (task-cancel task))
    (setf (io-context-closed-p ctx) t))
  t)

(defun io-context-add-task (ctx task)
  "Register TASK with context CTX."
  (lock:with-lock ((io-context-lock ctx))
    (push task (io-context-tasks ctx))))

(defun io-context-remove-task (ctx task)
  "Remove TASK from context CTX."
  (lock:with-lock ((io-context-lock ctx))
    (setf (io-context-tasks ctx)
          (delete task (io-context-tasks ctx)))))

;;; ============================================================================
;;; Task Spawning
;;; ============================================================================

(defun spawn (ctx operation-fn)
  "Spawn a new async task in context CTX.
   OPERATION-FN is called to start the operation and should return
   a platform operation handle."
  (check-context-open ctx)
  (let ((task (make-task ctx)))
    (setf (task-state task) :running)
    (handler-case
        (let ((op (funcall operation-fn task)))
          (setf (task-operation task) op))
      (error (e)
        (task-fail task e)))
    (io-context-add-task ctx task)
    task))

(defun check-context-open (ctx)
  "Signal error if context is closed."
  (when (io-context-closed-p ctx)
    (error 'closed-error :operation :spawn :stream ctx)))

;;; ============================================================================
;;; Awaiting Tasks
;;; ============================================================================

(defun await (task &key timeout)
  "Wait for TASK to complete and return its result.

   If task completed successfully, returns the value.
   If task failed, signals the error.
   If task was cancelled, signals cancelled-error.

   TIMEOUT: optional timeout in seconds. Signals timeout-error if exceeded."
  (let ((deadline (when timeout
                    (+ (get-internal-real-time)
                       (* timeout internal-time-units-per-second)))))
    (loop
      ;; Check if already complete
      (when (task-complete-p task)
        (return (task-unwrap-result task)))
      ;; Poll for completions
      (poll-once (task-context task) :timeout-ms 10)
      ;; Check timeout
      (when (and deadline (> (get-internal-real-time) deadline))
        (error 'timeout-error
               :operation :await
               :duration timeout)))))

(defun task-unwrap-result (task)
  "Extract result from completed task, signaling error if failed."
  (let ((result (task-result task)))
    (if (task-result-error result)
        (error (task-result-error result))
        (task-result-value result))))

(defun await-any (tasks &key timeout)
  "Wait for any of TASKS to complete.
   Returns (values completed-task remaining-tasks)."
  (let ((deadline (when timeout
                    (+ (get-internal-real-time)
                       (* timeout internal-time-units-per-second)))))
    (loop
      ;; Check for any completed
      (dolist (task tasks)
        (when (task-complete-p task)
          (return (values task (remove task tasks)))))
      ;; Poll (use first task's context)
      (when tasks
        (poll-once (task-context (first tasks)) :timeout-ms 10))
      ;; Check timeout
      (when (and deadline (> (get-internal-real-time) deadline))
        (error 'timeout-error :operation :await-any :duration timeout)))))

(defun await-all (tasks &key timeout)
  "Wait for all TASKS to complete.
   Returns list of results (in task order).
   If any task fails, the first error is signaled after cancelling remaining."
  (let ((results (make-list (length tasks)))
        (remaining (copy-list tasks))
        (deadline (when timeout
                    (+ (get-internal-real-time)
                       (* timeout internal-time-units-per-second)))))
    (loop while remaining
          do (multiple-value-bind (completed rest)
                 (await-any remaining
                            :timeout (when deadline
                                       (/ (- deadline (get-internal-real-time))
                                          internal-time-units-per-second)))
               (declare (ignore rest))
               ;; Check for failure
               (when (eq (task-state completed) :failed)
                 ;; Cancel remaining tasks
                 (dolist (task remaining)
                   (unless (eq task completed)
                     (task-cancel task)))
                 ;; Signal the error
                 (task-unwrap-result completed))
               ;; Store result
               (let ((idx (position completed tasks)))
                 (setf (nth idx results) (task-result-value (task-result completed))))
               ;; Remove completed from remaining
               (setf remaining (remove completed remaining))))
    results))

;;; ============================================================================
;;; Polling
;;; ============================================================================

(defun poll-once (ctx &key (timeout-ms 0))
  "Poll for completions on context CTX.
   Returns number of completions processed."
  (when (io-context-closed-p ctx)
    (return-from poll-once 0))
  (let ((completions (async:poll-completions timeout-ms))
        (count 0))
    (dolist (completion completions)
      (let ((task (find-task-for-operation ctx completion)))
        (when task
          (handler-case
              (let ((result (async:async-operation-result completion)))
                (task-complete task result))
            (error (e)
              (task-fail task e)))
          (incf count))))
    count))

(defun find-task-for-operation (ctx operation)
  "Find task in CTX that owns OPERATION."
  (lock:with-lock ((io-context-lock ctx))
    (find operation (io-context-tasks ctx)
          :key #'task-operation
          :test #'eq)))

(defun run-until-complete (ctx task)
  "Run context event loop until TASK completes.
   Returns task result."
  (loop until (task-complete-p task)
        do (poll-once ctx :timeout-ms 10))
  (task-unwrap-result task))

;;; ============================================================================
;;; Async IO Operations
;;; ============================================================================

(defun async-read (ctx fd buffer &key (start 0) (end (length buffer)))
  "Start async read from FD into BUFFER[start:end].
   Returns a task that completes with bytes read."
  (spawn ctx
         (lambda (task)
           (async:async-read
            fd buffer
            :start start
            :end end
            :on-complete (lambda (n)
                           (task-complete task n))
            :on-error (lambda (e)
                        (task-fail task e))))))

(defun async-write (ctx fd buffer &key (start 0) (end (length buffer)))
  "Start async write of BUFFER[start:end] to FD.
   Returns a task that completes with bytes written."
  (spawn ctx
         (lambda (task)
           (async:async-write
            fd buffer
            :start start
            :end end
            :on-complete (lambda (n)
                           (task-complete task n))
            :on-error (lambda (e)
                        (task-fail task e))))))

(defun async-accept (ctx listener-fd)
  "Start async accept on LISTENER-FD.
   Returns a task that completes with (client-fd . address)."
  (spawn ctx
         (lambda (task)
           (async:async-accept
            listener-fd
            :on-complete (lambda (result)
                           (task-complete task result))
            :on-error (lambda (e)
                        (task-fail task e))))))

(defun async-connect (ctx fd address)
  "Start async connect of FD to ADDRESS.
   Returns a task that completes when connected."
  (spawn ctx
         (lambda (task)
           (async:async-connect
            fd address
            :on-complete (lambda (result)
                           (task-complete task result))
            :on-error (lambda (e)
                        (task-fail task e))))))

;;; ============================================================================
;;; Timer Tasks
;;; ============================================================================

(defun async-sleep (ctx duration-seconds)
  "Create a task that completes after DURATION-SECONDS.
   Useful for timeouts and delays."
  (let ((task (make-task ctx))
        (deadline (+ (get-internal-real-time)
                     (* duration-seconds internal-time-units-per-second))))
    (setf (task-state task) :running)
    ;; For now, use polling. Platform backends can provide efficient timers.
    (setf (task-operation task)
          (list :timer deadline))
    (io-context-add-task ctx task)
    task))

(defun async-timeout (ctx duration-seconds task)
  "Wrap TASK with a timeout. If TASK doesn't complete within
   DURATION-SECONDS, it is cancelled and timeout-error is signaled."
  (let ((timer (async-sleep ctx duration-seconds)))
    (multiple-value-bind (completed remaining)
        (await-any (list task timer))
      (if (eq completed timer)
          (progn
            (task-cancel task)
            (error 'timeout-error :operation :async :duration duration-seconds))
          (progn
            (task-cancel timer)
            (task-unwrap-result task))))))

;;; ============================================================================
;;; Convenience Macros
;;; ============================================================================

(defmacro with-io-context ((ctx-var &rest args) &body body)
  "Execute BODY with CTX-VAR bound to a new IO context.
   The context is closed when BODY exits (normal or error),
   which cancels all pending tasks."
  `(let ((,ctx-var (make-io-context ,@args)))
     (unwind-protect
          (progn ,@body)
       (io-context-close ,ctx-var))))

(defmacro async-let (ctx bindings &body body)
  "Spawn tasks for BINDINGS in parallel, await all, then execute BODY.

   Example:
     (async-let ctx
         ((data1 (async-read ctx fd1 buf1))
          (data2 (async-read ctx fd2 buf2)))
       (process data1 data2))"
  (let ((tasks (gensym "TASKS"))
        (results (gensym "RESULTS")))
    `(let* ((,tasks (list ,@(mapcar #'cadr bindings)))
            (,results (await-all ,tasks)))
       (let ,(loop for (var form) in bindings
                   for i from 0
                   collect `(,var (nth ,i ,results)))
         ,@body))))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(defun set-nonblocking (fd)
  "Set file descriptor to non-blocking mode."
  (async:set-nonblocking fd))

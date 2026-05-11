;;;; Darwin Async I/O Implementation using kqueue
;;;;
;;;; Parallel to epsilon/modules/linux/src/async.lisp -- delegates all
;;;; kqueue bookkeeping to epsilon.net.reactor, whose reactor
;;;; thread is the sole caller of kevent().  This keeps the event-loop
;;;; and per-FD dispatch logic in one place and lets both the async
;;;; submit/poll API and the scheduler's io-wait *fd-wait-register* hook
;;;; share a single kqueue fd.

(defpackage #:epsilon.async
  (:use #:cl)
  (:import
   (epsilon.kqueue kqueue)
   (epsilon.net.reactor reactor)
   (epsilon.net.constants constants)
   (epsilon.log log)
   (epsilon.sys.thread thread)
   (epsilon.sys.lock lock))
  (:export
   ;; Async operation struct
   #:async-operation
   #:async-operation-p
   #:make-async-operation
   #:async-operation-fd
   #:async-operation-type
   #:async-operation-buffer
   #:async-operation-start
   #:async-operation-end
   #:async-operation-callback
   #:async-operation-error-callback
   #:async-operation-result

   ;; Async system management
   #:ensure-async-system
   #:stop-async-system
   #:submit-async-operation
   #:cancel-async-operation
   #:poll-completions

   ;; Async operations
   #:async-read
   #:async-write
   #:async-accept
   #:async-connect

   ;; Pool teardown support
   #:cleanup-fd-operations
   #:quiesce-fd
   #:fence-async-event-loop

   ;; Utilities
   #:set-nonblocking

   ;; Process monitoring (synchronous subprocess I/O multiplexing)
   #:+eagain+
   #:make-process-monitor
   #:process-monitor-add-read
   #:process-monitor-add-pid
   #:process-monitor-remove-read
   #:process-monitor-wait
   #:process-monitor-close))

;;; ============================================================================
;;; Async Operation Struct
;;; ============================================================================

(defstruct async-operation
  "Represents a pending async I/O operation"
  (fd nil :type (or null fixnum))
  (type nil :type keyword)
  (buffer nil)
  (start 0 :type fixnum)
  (end nil :type (or null fixnum))
  (callback nil :type (or null function))
  (error-callback nil :type (or null function))
  (result nil))

;;; ============================================================================
;;; Darwin Kqueue-based Async System
;;; ============================================================================

(defvar *pending-operations* (make-hash-table :test 'equal)
  "Hash table mapping (fd . operation-type) to async-operation.
   All access must be guarded by *pending-lock*.")

(defvar *pending-lock* (lock:make-lock "pending-operations")
  "Mutex protecting *pending-operations*.")

(defvar *async-thread* nil
  "Background thread for async event processing")

(defvar *async-running* nil
  "Flag to control async thread lifecycle")

(defvar *completion-queue* '()
  "Queue of completed operations")

(defvar *completion-lock* (lock:make-lock "completion-queue"))

(defvar *cancel-in-progress* nil
  "Defense-in-depth guard: when non-nil, cancel-async-operation is already
   on the call stack and re-entrant invocations return immediately.")

(defvar *event-loop-generation* 0
  "Monotonically increasing counter incremented each async event loop iteration.")

(defvar *event-loop-generation-lock*
  (lock:make-lock "event-loop-generation"))

(defvar *event-loop-generation-cv*
  (lock:make-condition-variable :name "event-loop-generation-cv"))

(defun validate-file-descriptor (fd)
  "Validate that a file descriptor is valid before using with kqueue"
  (and (integerp fd)
       (>= fd 0)
       (handler-case
           (progn
             (constants:%fcntl fd constants:+f-getfl+ 0)
             t)
         (error () nil))))

(defun ensure-async-system ()
  "Ensure the async system is initialized"
  (handler-case
      (progn
        (reactor:boot-reactor)
        (start-async-thread))
    (error (e)
      (warn "Failed to initialize async system: ~A" e)
      (error e))))

(defun start-async-thread ()
  "Start the background async event processing thread"
  (unless (and *async-thread* (thread:thread-alive-p *async-thread*))
    (setf *async-running* t)
    (setf *async-thread*
          (thread:make-thread #'async-event-loop :name "darwin-async-loop"))))

(defun stop-async-system ()
  "Stop the async system and clean up resources"
  (setf *async-running* nil)
  (when (and *async-thread* (thread:thread-alive-p *async-thread*))
    (handler-case
        (thread:join-thread *async-thread*)
      (error (e)
        (warn "Error joining async thread: ~A" e))))
  (setf *async-thread* nil)
  (reactor:shutdown-reactor)
  (lock:with-lock (*pending-lock*)
    (clrhash *pending-operations*))
  (lock:with-lock (*completion-lock*)
    (setf *completion-queue* '()))
  (lock:with-lock (*event-loop-generation-lock*)
    (setf *event-loop-generation* 0)))

(defun async-event-loop ()
  "Main async event processing loop.
   Consumes events from the kqueue manager's any-queue (i.e. events for
   FDs that have no specific register-socket-callback waiter) and routes
   them to submit-async-operation-registered pending operations."
  (loop while *async-running* do
    (handler-case
        (let ((events (reactor:wait-for-any-socket 100)))
          (dolist (event events)
            (process-async-event event))
          (when (null events)
            (sleep 0.001)))
      (error (e)
        (log:error "Darwin async event loop error: ~A" e)
        (sleep 0.1)))
    (lock:with-lock (*event-loop-generation-lock*)
      (incf *event-loop-generation*)
      (lock:condition-broadcast *event-loop-generation-cv*))))

(defun %filter-to-op-types (filter)
  "Return the list of operation types that match a kqueue filter."
  (cond
    ((= filter kqueue:+evfilt-read+)  '(:read :accept))
    ((= filter kqueue:+evfilt-write+) '(:write :connect))
    (t '())))

(defun process-async-event (event)
  "Process a single kqueue event from the manager's any-queue."
  (let* ((fd (kqueue:kevent-struct-ident event))
         (filter (kqueue:kevent-struct-filter event))
         (flags (kqueue:kevent-struct-flags event))
         (op-types (%filter-to-op-types filter))
         (errored (not (zerop (logand flags kqueue:+ev-error+)))))
    (dolist (op-type op-types)
      (let* ((key (cons fd op-type))
             (async-op (lock:with-lock (*pending-lock*)
                         (prog1 (gethash key *pending-operations*)
                           (remhash key *pending-operations*)))))
        (when async-op
          (cond
            (errored
             (when (async-operation-error-callback async-op)
               (handler-case
                   (funcall (async-operation-error-callback async-op)
                            "I/O error")
                 (error (e)
                   (warn "Error in error callback: ~A" e)))))
            (t
             (case op-type
               (:read    (perform-read-operation async-op))
               (:write   (perform-write-operation async-op))
               (:accept  (perform-accept-operation async-op))
               (:connect (perform-write-operation async-op)))))
          (lock:with-lock (*completion-lock*)
            (push async-op *completion-queue*)))))))

(defun perform-read-operation (async-op)
  (handler-case
      (progn
        (setf (async-operation-result async-op) :ready)
        (when (async-operation-callback async-op)
          (funcall (async-operation-callback async-op))))
    (error (e)
      (setf (async-operation-result async-op) e)
      (when (async-operation-error-callback async-op)
        (funcall (async-operation-error-callback async-op) e)))))

(defun perform-write-operation (async-op)
  (handler-case
      (progn
        (setf (async-operation-result async-op) :ready)
        (when (async-operation-callback async-op)
          (funcall (async-operation-callback async-op))))
    (error (e)
      (setf (async-operation-result async-op) e)
      (when (async-operation-error-callback async-op)
        (funcall (async-operation-error-callback async-op) e)))))

(defun perform-accept-operation (async-op)
  (handler-case
      (progn
        (setf (async-operation-result async-op) :ready)
        (when (async-operation-callback async-op)
          (funcall (async-operation-callback async-op))))
    (error (e)
      (setf (async-operation-result async-op) e)
      (when (async-operation-error-callback async-op)
        (funcall (async-operation-error-callback async-op) e)))))

;;; ============================================================================
;;; Async Operation Interface
;;; ============================================================================

(defun %op-type-events (op-type)
  "Map an operation type to the reactor event keyword list."
  (case op-type
    (:read    '(:in))
    (:write   '(:out))
    (:accept  '(:in))
    (:connect '(:out))
    (t (error "Unknown operation type: ~A" op-type))))

(defun submit-async-operation (operation)
  "Submit an async operation for processing.
   If the fd is already registered with the kqueue manager, uses
   modify-socket-events to add the new interest; otherwise registers."
  (ensure-async-system)
  (let ((fd (async-operation-fd operation))
        (op-type (async-operation-type operation)))
    (unless (validate-file-descriptor fd)
      (error "Invalid file descriptor: ~A" fd))
    (let ((key (cons fd op-type))
          (events (%op-type-events op-type)))
      (lock:with-lock (*pending-lock*)
        (setf (gethash key *pending-operations*) operation))
      (handler-case
          (if (reactor:socket-registered-p fd)
              (reactor:modify-socket-events fd events)
              (reactor:register-socket fd events))
        (error (e)
          ;; Best-effort: fall back to immediate completion so callers
          ;; using stdin/stdout or already-closed fds don't hang.
          (warn "Socket registration failed for fd ~A: ~A" fd e)
          (lock:with-lock (*pending-lock*)
            (remhash key *pending-operations*))
          (lock:with-lock (*completion-lock*)
            (push operation *completion-queue*)))))))

(defun cancel-async-operation (operation)
  "Cancel a specific async operation.  Removes it from the pending table
   and invokes its error callback.  Uses *cancel-in-progress* as a
   re-entrancy guard."
  (when *cancel-in-progress*
    (return-from cancel-async-operation nil))
  (let* ((*cancel-in-progress* t)
         (fd (async-operation-fd operation))
         (op-type (async-operation-type operation))
         (key (cons fd op-type))
         (found (lock:with-lock (*pending-lock*)
                  (when (gethash key *pending-operations*)
                    (remhash key *pending-operations*)
                    t))))
    (when found
      (when (async-operation-error-callback operation)
        (handler-case
            (funcall (async-operation-error-callback operation)
                     "Operation cancelled")
          (error (e)
            (warn "Error in cancel error callback: ~A" e))))
      t)))

(defun poll-completions (&optional (timeout-ms 0))
  "Poll for completed operations"
  (declare (ignore timeout-ms))
  (lock:with-lock (*completion-lock*)
    (prog1 *completion-queue*
      (setf *completion-queue* '()))))

;;; ============================================================================
;;; High-Level Async Operations
;;; ============================================================================

(defun async-read (fd buffer &key (start 0) (end (length buffer)) on-complete on-error)
  "Perform async read operation."
  (let ((operation (make-async-operation
                    :fd fd
                    :type :read
                    :buffer buffer
                    :start start
                    :end end
                    :callback on-complete
                    :error-callback on-error)))
    (submit-async-operation operation)
    operation))

(defun async-write (fd buffer &key (start 0) (end (length buffer)) on-complete on-error)
  "Perform async write operation."
  (let ((operation (make-async-operation
                    :fd fd
                    :type :write
                    :buffer buffer
                    :start start
                    :end end
                    :callback on-complete
                    :error-callback on-error)))
    (submit-async-operation operation)
    operation))

(defun async-accept (listener-fd &key on-complete on-error)
  "Perform async accept operation"
  (let ((operation (make-async-operation
                    :fd listener-fd
                    :type :accept
                    :callback on-complete
                    :error-callback on-error)))
    (submit-async-operation operation)
    operation))

(defun async-connect (fd address &key on-complete on-error)
  "Perform async connect operation"
  (declare (ignore address))
  (let ((operation (make-async-operation
                    :fd fd
                    :type :connect
                    :callback on-complete
                    :error-callback on-error)))
    (submit-async-operation operation)
    operation))

;;; ============================================================================
;;; Pool Teardown Support
;;; ============================================================================

(defun cleanup-fd-operations (fd)
  "Clean up all pending operations for a specific file descriptor.
   Invokes error callbacks on each pending operation, then removes them
   from the pending table and unregisters the fd.  Returns the number of
   operations removed."
  (let ((keys-to-remove '())
        (callbacks '()))
    (lock:with-lock (*pending-lock*)
      (maphash (lambda (key operation)
                 (when (= (car key) fd)
                   (push key keys-to-remove)
                   (when (async-operation-error-callback operation)
                     (push (async-operation-error-callback operation) callbacks))))
               *pending-operations*)
      (dolist (key keys-to-remove)
        (remhash key *pending-operations*)))
    (dolist (cb callbacks)
      (handler-case (funcall cb "File descriptor closed")
        (error (e) (warn "Error in cleanup error callback: ~A" e))))
    (handler-case (reactor:unregister-socket fd) (error () nil))
    (length keys-to-remove)))

(defun quiesce-fd (fd)
  "Silently remove FD from the async system without invoking callbacks.
   Unlike cleanup-fd-operations, this does NOT invoke error callbacks,
   avoiding re-entrancy issues during pool teardown.  Use
   fence-async-event-loop after quiescing to ensure any currently-executing
   callbacks for FD have completed."
  (let ((keys-to-remove '()))
    (lock:with-lock (*pending-lock*)
      (maphash (lambda (key op)
                 (declare (ignore op))
                 (when (= (car key) fd)
                   (push key keys-to-remove)))
               *pending-operations*)
      (dolist (key keys-to-remove)
        (remhash key *pending-operations*))))
  (handler-case (reactor:unregister-socket fd) (error () nil)))

(defun fence-async-event-loop ()
  "Wait for the async event loop to complete its current iteration.
   Returns immediately if the async system is not running."
  (unless (and *async-thread* (thread:thread-alive-p *async-thread*))
    (return-from fence-async-event-loop))
  (lock:with-lock (*event-loop-generation-lock*)
    (let ((target (1+ *event-loop-generation*)))
      (loop while (and *async-running*
                       (< *event-loop-generation* target))
            do (lock:condition-wait *event-loop-generation-cv*
                                    *event-loop-generation-lock*
                                    :timeout 1)))))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(defun set-nonblocking (fd)
  "Set a file descriptor to non-blocking mode.
   Uses direct SBCL FFI because epsilon.foreign's fcntl wrapper has a
   known bug where F_SETFL returns success but doesn't actually modify
   flags."
  (let ((flags (sb-alien:alien-funcall
                (sb-alien:extern-alien "fcntl"
                                       (function sb-alien:int sb-alien:int sb-alien:int sb-alien:int))
                fd constants:+f-getfl+ 0)))
    (when (< flags 0)
      (error "Failed to get file descriptor flags for fd ~A" fd))
    (let ((result (sb-alien:alien-funcall
                   (sb-alien:extern-alien "fcntl"
                                          (function sb-alien:int sb-alien:int sb-alien:int sb-alien:int))
                   fd constants:+f-setfl+ (logior flags constants:+o-nonblock+))))
      (when (< result 0)
        (error "Failed to set non-blocking mode for fd ~A" fd))
      fd)))

;;; ============================================================================
;;; Process Monitor (synchronous multiplexer for subprocess I/O)
;;; ============================================================================

(defconstant +eagain+ 35
  "EAGAIN errno value on Darwin.")

(defconstant +note-exit+ #x80000000
  "NOTE_EXIT kqueue fflags value for EVFILT_PROC.")

(defstruct (process-monitor (:constructor %make-process-monitor))
  "Synchronous I/O multiplexer for subprocess stdout/stderr and process exit.
   Backed by a dedicated kqueue, independent of the global async system."
  kqueue-fd)

(defun make-process-monitor ()
  "Create a new process monitor backed by a dedicated kqueue fd."
  (%make-process-monitor :kqueue-fd (kqueue:kqueue)))

(defun process-monitor-add-read (monitor fd)
  "Register FD for read-readiness events."
  (kqueue:add-event (process-monitor-kqueue-fd monitor) fd kqueue:+evfilt-read+))

(defun process-monitor-add-pid (monitor pid pidfd)
  "Register PID for process exit notification via EVFILT_PROC.
   PIDFD is unused on Darwin. Returns T on success, NIL if the process
   already exited (ESRCH)."
  (declare (ignore pidfd))
  (handler-case
      (progn
        (kqueue:kevent (process-monitor-kqueue-fd monitor)
                       (list (kqueue:make-kevent-struct
                              :ident pid
                              :filter kqueue:+evfilt-proc+
                              :flags kqueue:+ev-add+
                              :fflags +note-exit+
                              :data 0
                              :udata 0))
                       '())
        t)
    (error (e)
      (if (search "errno 3" (princ-to-string e))
          nil
          (error e)))))

(defun process-monitor-remove-read (monitor fd)
  "Remove FD from read-readiness monitoring. Ignores errors."
  (handler-case
      (kqueue:remove-event (process-monitor-kqueue-fd monitor) fd kqueue:+evfilt-read+)
    (error () nil)))

(defun process-monitor-wait (monitor timeout-ms)
  "Wait up to TIMEOUT-MS milliseconds for events. Pass -1 to block indefinitely.
   Returns a list of (fd . event-type) where event-type is :readable, :eof,
   :pid-exit, or :error. For :pid-exit events the car is nil."
  (let ((kq (process-monitor-kqueue-fd monitor))
        (results nil))
    (dolist (ev (kqueue:wait-for-events kq 16
                                        (when (and timeout-ms (>= timeout-ms 0))
                                          (/ timeout-ms 1000.0))))
      (let ((ident  (kqueue:kevent-struct-ident ev))
            (filter (kqueue:kevent-struct-filter ev))
            (flags  (kqueue:kevent-struct-flags ev)))
        (cond
          ((= filter kqueue:+evfilt-proc+)
           (push (cons nil :pid-exit) results))
          ((and (= filter kqueue:+evfilt-read+)
                (not (zerop (logand flags kqueue:+ev-eof+)))
                (= (kqueue:kevent-struct-data ev) 0))
           (push (cons ident :eof) results))
          ((= filter kqueue:+evfilt-read+)
           (push (cons ident :readable) results))
          (t
           (push (cons ident :error) results)))))
    (nreverse results)))

(defun process-monitor-close (monitor)
  "Release the kqueue fd held by MONITOR."
  (kqueue:kqueue-close (process-monitor-kqueue-fd monitor)))

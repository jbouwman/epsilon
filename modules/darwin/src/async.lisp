;;;; Darwin Async I/O Implementation using kqueue
;;;;
;;;; This module provides the epsilon.async interface for Darwin systems
;;;; using direct kqueue integration for all file descriptor types.

(defpackage #:epsilon.async
  (:use #:cl)
  (:local-nicknames
   (#:kqueue #:epsilon.kqueue)
   (#:constants #:epsilon.net.constants)
   (#:log #:epsilon.log))
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
   #:process-monitor-close)
  (:enter t))

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

(defvar *global-kqueue* nil
  "Global kqueue file descriptor for async operations")

(defvar *pending-operations* (make-hash-table :test 'equal)
  "Hash table mapping (fd . filter) to async-operation.
   All access must be guarded by *pending-lock*.")

(defvar *pending-lock* (sb-thread:make-mutex :name "pending-operations")
  "Mutex protecting *pending-operations*.")

(defvar *async-thread* nil
  "Background thread for async event processing")

(defvar *async-running* nil
  "Flag to control async thread lifecycle")

(defvar *completion-queue* '()
  "Queue of completed operations")

(defvar *completion-lock* (sb-thread:make-mutex :name "completion-queue"))

(defvar *event-loop-generation* 0
  "Monotonically increasing counter incremented each async event loop iteration.")

(defvar *event-loop-generation-lock*
  (sb-thread:make-mutex :name "event-loop-generation"))

(defvar *event-loop-generation-cv*
  (sb-thread:make-waitqueue :name "event-loop-generation-cv"))

(defun validate-file-descriptor (fd)
  "Validate that a file descriptor is valid before using with kqueue"
  (and (integerp fd)
       (>= fd 0)
       ;; Simple validation - try to get flags
       (handler-case
           (progn
             (constants:%fcntl fd constants:+f-getfl+ 0)
             t)
         (error () nil))))

(defun ensure-async-system ()
  "Ensure the async system is initialized"
  (unless *global-kqueue*
    (setf *global-kqueue* (kqueue:kqueue))
    (start-async-thread)))

(defun start-async-thread ()
  "Start the background async event processing thread"
  (unless (and *async-thread* (sb-thread:thread-alive-p *async-thread*))
    (setf *async-running* t)
    (setf *async-thread*
          (sb-thread:make-thread #'async-event-loop :name "darwin-async-loop"))))

(defun stop-async-system ()
  "Stop the async system and clean up resources"
  (setf *async-running* nil)
  (when (and *async-thread* (sb-thread:thread-alive-p *async-thread*))
    (sb-thread:join-thread *async-thread*))
  (when *global-kqueue*
    (kqueue:kqueue-close *global-kqueue*)
    (setf *global-kqueue* nil))
  (clrhash *pending-operations*)
  (setf *completion-queue* '()))

(defun async-event-loop ()
  "Main async event processing loop"
  (loop while *async-running* do
        (handler-case
            (let ((events (kqueue:wait-for-events *global-kqueue* 64 1))) ; 1 second timeout
              (dolist (event events)
                (process-async-event event)))
          (error (e)
            (log:error "Async event loop error: ~A" e)
            (sleep 0.1)))
        (sb-thread:with-mutex (*event-loop-generation-lock*)
          (incf *event-loop-generation*)
          (sb-thread:condition-broadcast *event-loop-generation-cv*))))

(defun process-async-event (event)
  "Process a single kqueue event"
  (let* ((fd (kqueue:kevent-struct-ident event))
         (filter (kqueue:kevent-struct-filter event))
         (flags (kqueue:kevent-struct-flags event))
         (key (cons fd filter)))

    ;; Check for real errors (EV_ERROR indicates actual error)
    ;; Note: EV_EOF for read operations is NOT an error - it means data may be
    ;; available and connection was closed. We should still process it normally.
    (when (not (zerop (logand flags kqueue:+ev-error+)))
      (let ((async-op (sb-thread:with-mutex (*pending-lock*)
                        (prog1 (gethash key *pending-operations*)
                          (remhash key *pending-operations*)))))
        (when async-op
          (sb-thread:with-mutex (*completion-lock*)
            (push async-op *completion-queue*))
          (when (async-operation-error-callback async-op)
            (funcall (async-operation-error-callback async-op) "I/O error"))))
      (return-from process-async-event))

    ;; Handle successful events (normal read/write readiness)
    (let ((async-op (sb-thread:with-mutex (*pending-lock*)
                      (prog1 (gethash key *pending-operations*)
                        (remhash key *pending-operations*)))))
      (when async-op
        ;; Perform the actual I/O operation based on stored operation type
        (case (async-operation-type async-op)
          (:read
           (perform-read-operation async-op))
          (:write
           (perform-write-operation async-op))
          (:accept
           (perform-accept-operation async-op))
          (:connect
           (perform-write-operation async-op)))

        (sb-thread:with-mutex (*completion-lock*)
          (push async-op *completion-queue*))))))

(defun perform-read-operation (async-op)
  "Perform the actual read operation"
  (handler-case
      ;; Signal completion - the fd is ready for reading
      ;; The callback (waker) is a nullary function
      (progn
        (setf (async-operation-result async-op) :ready)
        (when (async-operation-callback async-op)
          (funcall (async-operation-callback async-op))))
    (error (e)
      (setf (async-operation-result async-op) e)
      (when (async-operation-error-callback async-op)
        (funcall (async-operation-error-callback async-op) e)))))

(defun perform-write-operation (async-op)
  "Perform the actual write operation"
  (handler-case
      ;; Signal completion - the fd is ready for writing
      ;; The callback (waker) is a nullary function
      (progn
        (setf (async-operation-result async-op) :ready)
        (when (async-operation-callback async-op)
          (funcall (async-operation-callback async-op))))
    (error (e)
      (setf (async-operation-result async-op) e)
      (when (async-operation-error-callback async-op)
        (funcall (async-operation-error-callback async-op) e)))))

(defun perform-accept-operation (async-op)
  "Perform the actual accept operation"
  (handler-case
      ;; Signal completion - the fd is ready for accepting
      ;; The callback (waker) is a nullary function
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

(defun submit-async-operation (operation)
  "Submit an async operation for processing"
  (ensure-async-system)
  (let ((fd (async-operation-fd operation))
        (op-type (async-operation-type operation)))

    ;; Validate file descriptor
    (unless (validate-file-descriptor fd)
      (error "Invalid file descriptor: ~A" fd))

    ;; Register with kqueue
    (let ((filter (case op-type
                    (:read kqueue:+evfilt-read+)
                    (:write kqueue:+evfilt-write+)
                    (:accept kqueue:+evfilt-read+) ; Accept uses read filter
                    (:connect kqueue:+evfilt-write+) ; Connect uses write filter
                    (t (error "Unknown operation type: ~A" op-type)))))
      (let ((key (cons fd filter)))
        ;; Store the operation and register with kqueue
        (sb-thread:with-mutex (*pending-lock*)
          (setf (gethash key *pending-operations*) operation))

        (handler-case
            (kqueue:add-event *global-kqueue* fd filter)
          (error (e)
            (sb-thread:with-mutex (*pending-lock*)
              (remhash key *pending-operations*))
            (error "Failed to register with kqueue: ~A" e)))))))

(defun cancel-async-operation (operation)
  "Cancel a specific async operation. Removes it from the pending table
   and invokes its error callback."
  (let* ((fd (async-operation-fd operation))
         (op-type (async-operation-type operation))
         (filter (case op-type
                   (:read kqueue:+evfilt-read+)
                   (:write kqueue:+evfilt-write+)
                   (:accept kqueue:+evfilt-read+)
                   (:connect kqueue:+evfilt-write+)
                   (t (return-from cancel-async-operation nil))))
         (key (cons fd filter)))
    (let ((found (sb-thread:with-mutex (*pending-lock*)
                   (when (gethash key *pending-operations*)
                     (remhash key *pending-operations*)
                     t))))
      (when found
        ;; Remove from kqueue (ignore errors -- fd may already be closed)
        (when *global-kqueue*
          (handler-case (kqueue:remove-event *global-kqueue* fd filter)
            (error () nil)))
        ;; Invoke error callback
        (when (async-operation-error-callback operation)
          (handler-case
              (funcall (async-operation-error-callback operation) "Operation cancelled")
            (error (e)
              (warn "Error in cancel error callback: ~A" e))))
        t))))

(defun poll-completions (&optional (timeout-ms 0))
  "Poll for completed operations"
  (declare (ignore timeout-ms))
  (sb-thread:with-mutex (*completion-lock*)
    (prog1 *completion-queue*
      (setf *completion-queue* '()))))

;;; ============================================================================
;;; High-Level Async Operations
;;; ============================================================================

(defun async-read (fd buffer &key (start 0) (end (length buffer)) on-complete on-error)
  "Perform async read operation.
   START and END specify the region of BUFFER to read into."
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
  "Perform async write operation.
   START and END specify the region of BUFFER to write from."
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
   from the pending table and unregisters each kqueue filter for FD.
   Returns the number of operations removed."
  (let ((keys-to-remove '())
        (callbacks '()))
    (sb-thread:with-mutex (*pending-lock*)
      (maphash (lambda (key operation)
                 (when (= (car key) fd)
                   (push key keys-to-remove)
                   (when (async-operation-error-callback operation)
                     (push (async-operation-error-callback operation) callbacks))))
               *pending-operations*)
      (dolist (key keys-to-remove)
        (remhash key *pending-operations*)))
    ;; Invoke callbacks and remove kqueue filters outside the lock
    (dolist (cb callbacks)
      (handler-case (funcall cb "File descriptor closed")
        (error (e) (warn "Error in cleanup error callback: ~A" e))))
    (dolist (key keys-to-remove)
      (when *global-kqueue*
        (handler-case (kqueue:remove-event *global-kqueue* fd (cdr key))
          (error () nil))))
    (length keys-to-remove)))

(defun quiesce-fd (fd)
  "Silently remove FD from the async system without invoking callbacks.
   Removes all pending operations for FD from the pending table and
   unregisters each kqueue filter for FD.  Unlike cancel-async-operation,
   this does NOT invoke error callbacks, avoiding re-entrancy issues during
   pool teardown.  Use fence-async-event-loop after quiescing to ensure any
   currently-executing callbacks for FD have completed."
  (let ((keys-to-remove '()))
    (sb-thread:with-mutex (*pending-lock*)
      (maphash (lambda (key op)
                 (declare (ignore op))
                 (when (= (car key) fd)
                   (push key keys-to-remove)))
               *pending-operations*)
      (dolist (key keys-to-remove)
        (remhash key *pending-operations*)))
    ;; Remove kqueue filters outside the lock
    (dolist (key keys-to-remove)
      (when *global-kqueue*
        (handler-case (kqueue:remove-event *global-kqueue* fd (cdr key))
          (error () nil))))))

(defun fence-async-event-loop ()
  "Wait for the async event loop to complete its current iteration.
   Guarantees that any callbacks in-flight when this function is called
   have finished executing before this function returns.
   Returns immediately if the async system is not running."
  (unless (and *async-thread* (sb-thread:thread-alive-p *async-thread*))
    (return-from fence-async-event-loop))
  (sb-thread:with-mutex (*event-loop-generation-lock*)
    (let ((target (1+ *event-loop-generation*)))
      (loop while (and *async-running*
                       (< *event-loop-generation* target))
            do (sb-thread:condition-wait *event-loop-generation-cv*
                                        *event-loop-generation-lock*
                                        :timeout 1)))))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(defun set-nonblocking (fd)
  "Set a file descriptor to non-blocking mode.
   Uses direct SBCL FFI for simplicity."
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
      ;; ESRCH (errno 3) means the process already exited before we could
      ;; register the kevent - a normal race for fast commands.
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

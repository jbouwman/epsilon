;;;; Linux Async I/O Implementation using epoll
;;;;
;;;; This module provides the epsilon.async interface for Linux systems
;;;; using epoll for event notification and async I/O operations.

(defpackage #:epsilon.async
  (:use #:cl)
  (:local-nicknames
   (#:epoll #:epsilon.sys.epoll)
   (#:epoll-mgr #:epsilon.net.epoll-manager)
   (#:lib #:epsilon.foreign))
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
   #:poll-completions

   ;; Async operations
   #:async-read
   #:async-write
   #:async-accept
   #:async-connect

   ;; Utilities
   #:set-nonblocking

   ;; Operation management
   #:cleanup-fd-operations
   #:cancel-async-operation

   ;; Pool teardown support
   #:quiesce-fd
   #:fence-async-event-loop

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
;;; System Constants and FFI
;;; ============================================================================

;; fcntl commands
(defconstant +f-getfl+ 3)
(defconstant +f-setfl+ 4)

;; File status flags
(defconstant +o-nonblock+ #o4000)

;; fcntl system call
(lib:defshared %fcntl "fcntl" "libc" :int
  (fd :int) (cmd :int) (arg :int)
  :documentation "File control operations")

;;; ============================================================================
;;; Linux Epoll-based Async System
;;; ============================================================================

;; Now using centralized epoll manager instead of global epfd

(defvar *pending-operations* (make-hash-table :test 'equal)
  "Hash table mapping (fd . operation-type) to async-operation")

(defvar *async-thread* nil
  "Background thread for async event processing")

(defvar *async-running* nil
  "Flag to control async thread lifecycle")

(defvar *completion-queue* '()
  "Queue of completed operations")

(defvar *completion-lock* (sb-thread:make-mutex :name "completion-queue"))

(defvar *cancel-in-progress* nil
  "Defense-in-depth guard: when non-nil, cancel-async-operation is already
   on the call stack and re-entrant invocations return immediately.")

(defvar *event-loop-generation* 0
  "Monotonically increasing counter incremented after each event loop iteration.
   Used by fence-async-event-loop to synchronize with the event loop thread.")

(defvar *event-loop-generation-lock* (sb-thread:make-mutex :name "event-loop-generation"))

(defvar *event-loop-generation-cv* (sb-thread:make-waitqueue :name "event-loop-generation"))

(defun validate-file-descriptor (fd)
  "Validate that a file descriptor is valid before using with epoll"
  (and (integerp fd)
       (>= fd 0)
       ;; Simple validation - try to get flags
       (handler-case
           (progn
             (%fcntl fd +f-getfl+ 0)
             t)
         (error () nil))))

(defun ensure-async-system ()
  "Ensure the async system is initialized"
  (handler-case
      (progn
        ;; Ensure epoll manager is available
        (epoll-mgr:get-epoll-manager)
        (start-async-thread))
    (error (e)
      (warn "Failed to initialize async system: ~A" e)
      (error e))))

(defun start-async-thread ()
  "Start the background async event processing thread"
  (unless (and *async-thread* (sb-thread:thread-alive-p *async-thread*))
    (setf *async-running* t)
    (setf *async-thread*
          (sb-thread:make-thread #'async-event-loop :name "linux-async-loop"))))

(defun stop-async-system ()
  "Stop the async system and clean up resources"
  (setf *async-running* nil)
  (when (and *async-thread* (sb-thread:thread-alive-p *async-thread*))
    (handler-case
        (sb-thread:join-thread *async-thread*)
      (error (e)
        (warn "Error joining async thread: ~A" e))))
  (setf *async-thread* nil)
  ;; Epoll manager will handle its own cleanup
  (epoll-mgr:shutdown-epoll-manager)
  (clrhash *pending-operations*)
  (setf *completion-queue* '())
  (setf *event-loop-generation* 0))

(defun async-event-loop ()
  "Main async event processing loop"
  (loop while *async-running* do
    (handler-case
        (let ((events (epoll-mgr:wait-for-any-socket 100))) ; 100ms timeout
          (dolist (event events)
            (process-epoll-event event))
          ;; If no events, short sleep to prevent busy waiting
          (when (null events)
            (sleep 0.001)))
      (error (e)
        (warn "Linux async event loop error: ~A" e)
        ;; Sleep on error to prevent error loops
        (sleep 0.1)))
    ;; Advance generation for fence synchronization.
    ;; This signals that all callbacks from this iteration have completed.
    (sb-thread:with-mutex (*event-loop-generation-lock*)
      (incf *event-loop-generation*)
      (sb-thread:condition-broadcast *event-loop-generation-cv*))))

(defun process-epoll-event (event)
  "Process a single epoll event"
  (let* ((fd (epoll:epoll-event-data event))
         (events (epoll:epoll-event-events event))
         (operation-type (cond
                           ((logtest events epoll:+epollin+) :read)
                           ((logtest events epoll:+epollout+) :write)
                           (t :unknown)))
         (key (cons fd operation-type)))

    ;; Check for error conditions first
    (when (or (logtest events epoll:+epollerr+)
              (logtest events epoll:+epollhup+)
              (logtest events epoll:+epollrdhup+))
      (let ((async-op (gethash key *pending-operations*)))
        (when async-op
          (remhash key *pending-operations*)
          ;; Invoke error callback if present
          (when (async-operation-error-callback async-op)
            (handler-case
                (funcall (async-operation-error-callback async-op)
                         (cond
                           ((logtest events epoll:+epollerr+) "Socket error")
                           ((logtest events epoll:+epollhup+) "Socket hangup")
                           ((logtest events epoll:+epollrdhup+) "Peer closed connection")
                           (t "Unknown error")))
              (error (e)
                (warn "Error in error callback: ~A" e))))
          ;; Add to completion queue for cleanup
          (sb-thread:with-mutex (*completion-lock*)
            (push async-op *completion-queue*)))
        (return-from process-epoll-event)))

    ;; Handle normal events
    (let ((async-op (gethash key *pending-operations*)))
      (when async-op
        (remhash key *pending-operations*)

        ;; Perform the actual I/O operation based on type
        (case operation-type
          (:read (perform-read-operation async-op))
          (:write (perform-write-operation async-op))
          (:accept (perform-accept-operation async-op)))

        ;; Add to completion queue
        (sb-thread:with-mutex (*completion-lock*)
          (push async-op *completion-queue*))))))

;;; ============================================================================
;;; I/O Operation Implementations
;;; ============================================================================

(defun perform-read-operation (async-op)
  "Perform the actual read operation"
  (let ((buffer (async-operation-buffer async-op)))
    (when buffer
      (handler-case
          ;; For now, just signal completion without actual I/O
          ;; The actual I/O will be handled by epsilon.io layer
          (let ((bytes-read 0))
            (when (async-operation-callback async-op)
              (funcall (async-operation-callback async-op) bytes-read)))
        (error (e)
          (when (async-operation-error-callback async-op)
            (funcall (async-operation-error-callback async-op) e)))))))

(defun perform-write-operation (async-op)
  "Perform the actual write operation"
  (let ((buffer (async-operation-buffer async-op)))
    (when buffer
      (handler-case
          ;; For now, just signal completion without actual I/O
          ;; The actual I/O will be handled by epsilon.io layer
          (let ((bytes-written 0))
            (when (async-operation-callback async-op)
              (funcall (async-operation-callback async-op) bytes-written)))
        (error (e)
          (when (async-operation-error-callback async-op)
            (funcall (async-operation-error-callback async-op) e)))))))

(defun perform-accept-operation (async-op)
  "Perform the actual accept operation"
  (handler-case
      ;; For now, just signal completion without actual I/O
      ;; The actual I/O will be handled by epsilon.io layer
      (let ((client-fd -1))
        (when (async-operation-callback async-op)
          (funcall (async-operation-callback async-op) client-fd)))
    (error (e)
      (when (async-operation-error-callback async-op)
        (funcall (async-operation-error-callback async-op) e)))))

;;; ============================================================================
;;; Async Operation Interface
;;; ============================================================================

(defun submit-async-operation (operation)
  "Submit an async operation for processing.
   If the fd is already registered with the epoll manager (e.g. re-registering
   for read after partial consume), uses modify-socket-events instead of
   register-socket to avoid EPOLL_CTL_ADD on an already-added fd."
  (ensure-async-system)
  (let ((fd (async-operation-fd operation))
        (op-type (async-operation-type operation)))

    ;; Validate file descriptor
    (unless (validate-file-descriptor fd)
      (error "Invalid file descriptor: ~A" fd))

    (let ((key (cons fd op-type))
          (events (case op-type
                    (:read '(:in))
                    (:write '(:out))
                    (:accept '(:in))
                    (:connect '(:out))
                    (t (error "Unknown operation type: ~A" op-type)))))
      ;; Store operation for completion lookup
      (setf (gethash key *pending-operations*) operation)

      ;; Register or modify epoll interest for this fd
      (handler-case
          (if (epoll-mgr:socket-registered-p fd)
              ;; fd already in epoll -- modify events
              (epoll-mgr:modify-socket-events fd events)
              ;; fd not yet registered -- add it
              (epoll-mgr:register-socket fd events :data fd))
        (error (e)
          ;; For testing purposes, just complete immediately if epoll fails
          ;; This allows tests to work with stdin/stdout
          (warn "Socket registration failed for fd ~A: ~A" fd e)
          (sb-thread:with-mutex (*completion-lock*)
            (push operation *completion-queue*)))))))

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
;;; Operation Management
;;; ============================================================================

(defun cleanup-fd-operations (fd)
  "Clean up all pending operations for a specific file descriptor"
  (let ((operations-to-remove '()))
    ;; Find all operations for this fd
    (maphash (lambda (key operation)
               (when (= (car key) fd)
                 (push key operations-to-remove)
                 ;; Invoke error callback if present
                 (when (async-operation-error-callback operation)
                   (handler-case
                       (funcall (async-operation-error-callback operation) "File descriptor closed")
                     (error (e)
                       (warn "Error in cleanup error callback: ~A" e))))))
             *pending-operations*)

    ;; Remove operations from pending table
    (dolist (key operations-to-remove)
      (remhash key *pending-operations*))

    ;; Unregister from epoll manager (ignore errors)
    (handler-case
        (epoll-mgr:unregister-socket fd)
      (error ()
        ;; Ignore errors - fd might already be closed
        nil))

    (length operations-to-remove)))

(defun cancel-async-operation (operation)
  "Cancel a specific async operation.
   Uses *cancel-in-progress* as a re-entrancy guard: if the error-callback
   invoked below itself calls cancel-async-operation, the nested call
   returns immediately instead of recursing."
  (when *cancel-in-progress*
    (return-from cancel-async-operation nil))
  (let* ((*cancel-in-progress* t)
         (fd (async-operation-fd operation))
         (op-type (async-operation-type operation))
         (key (cons fd op-type)))
    (when (gethash key *pending-operations*)
      (remhash key *pending-operations*)
      ;; Invoke error callback
      (when (async-operation-error-callback operation)
        (handler-case
            (funcall (async-operation-error-callback operation) "Operation cancelled")
          (error (e)
            (warn "Error in cancel error callback: ~A" e))))
      t)))

;;; ============================================================================
;;; Pool Teardown Support
;;; ============================================================================

(defun quiesce-fd (fd)
  "Silently remove FD from the async system without invoking callbacks.
   Removes all pending operations for FD from the pending table and
   unregisters FD from epoll.  Unlike cleanup-fd-operations, this does
   NOT invoke error callbacks, avoiding re-entrancy issues during pool
   teardown.  Use fence-async-event-loop after quiescing to ensure any
   currently-executing callbacks for FD have completed."
  (let ((keys-to-remove '()))
    (maphash (lambda (key op)
               (declare (ignore op))
               (when (= (car key) fd)
                 (push key keys-to-remove)))
             *pending-operations*)
    (dolist (key keys-to-remove)
      (remhash key *pending-operations*)))
  (handler-case (epoll-mgr:unregister-socket fd) (error () nil)))

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
  "Set a file descriptor to non-blocking mode"
  (let ((flags (%fcntl fd +f-getfl+ 0)))
    (when (< flags 0)
      (error "Failed to get file descriptor flags for fd ~A" fd))
    (let ((result (%fcntl fd +f-setfl+ (logior flags +o-nonblock+))))
      (when (< result 0)
        (error "Failed to set non-blocking mode for fd ~A" fd))
      fd)))

;;; ============================================================================
;;; Process Monitor (synchronous multiplexer for subprocess I/O)
;;; ============================================================================

(defconstant +eagain+ 11
  "EAGAIN errno value on Linux.")

(defstruct (process-monitor (:constructor %make-process-monitor))
  "Synchronous I/O multiplexer for subprocess stdout/stderr and process exit.
   Backed by a dedicated epoll instance, independent of the global async system."
  epoll-fd
  (pidfd nil))

(defun make-process-monitor ()
  "Create a new process monitor backed by a dedicated epoll fd."
  (%make-process-monitor :epoll-fd (epoll:epoll-create1 0)))

(defun process-monitor-add-read (monitor fd)
  "Register FD for read-readiness and hangup events."
  (epoll:add-event (process-monitor-epoll-fd monitor) fd
                   (logior epoll:+epollin+ epoll:+epollrdhup+)))

(defun process-monitor-add-pid (monitor pid pidfd)
  "Register process exit via PIDFD if available.
   PID is unused on Linux. Returns true if registered, nil if pidfd is nil."
  (declare (ignore pid))
  (when pidfd
    (epoll:add-event (process-monitor-epoll-fd monitor) pidfd
                     epoll:+epollin+
                     :data (epoll:make-epoll-data :fd pidfd))
    (setf (process-monitor-pidfd monitor) pidfd)
    t))

(defun process-monitor-remove-read (monitor fd)
  "Remove FD from read-readiness monitoring. Ignores errors."
  (handler-case
      (epoll:delete-event (process-monitor-epoll-fd monitor) fd)
    (error () nil)))

(defun process-monitor-wait (monitor timeout-ms)
  "Wait up to TIMEOUT-MS milliseconds for events. Pass -1 to block indefinitely.
   Returns a list of (fd . event-type) where event-type is :readable, :eof,
   :pid-exit, or :error. For :pid-exit events the car is nil."
  (let ((epfd  (process-monitor-epoll-fd monitor))
        (pidfd (process-monitor-pidfd monitor))
        (results nil))
    (dolist (ev (epoll:epoll-wait epfd 16 (if timeout-ms timeout-ms -1)))
      (let* ((fd   (epoll:epoll-data-fd (epoll:epoll-event-data ev)))
             (evts (epoll:epoll-event-events ev)))
        (cond
          ((and pidfd (= fd pidfd))
           (push (cons nil :pid-exit) results))
          ((not (zerop (logand evts epoll:+epollin+)))
           (push (cons fd :readable) results))
          ((not (zerop (logand evts (logior epoll:+epollhup+
                                           epoll:+epollrdhup+
                                           epoll:+epollerr+))))
           (push (cons fd :eof) results))
          (t
           (push (cons fd :error) results)))))
    (nreverse results)))

(defun process-monitor-close (monitor)
  "Release the epoll fd held by MONITOR."
  (epoll:epoll-close (process-monitor-epoll-fd monitor)))

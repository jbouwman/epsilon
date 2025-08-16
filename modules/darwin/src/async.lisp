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
   ;; Async operation types
   #:async-operation
   #:async-operation-p
   #:make-async-operation
   #:async-operation-fd
   #:async-operation-type
   #:async-operation-buffer
   #:async-operation-callback
   #:async-operation-error-callback
   
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
   #:set-nonblocking))

(in-package #:epsilon.async)

;;; ============================================================================
;;; Async Operation Types
;;; ============================================================================

(defstruct async-operation
  "Represents a pending async I/O operation"
  (fd nil :type (or null fixnum))
  (type nil :type keyword)
  (buffer nil)
  (callback nil :type (or null function))
  (error-callback nil :type (or null function)))

;;; ============================================================================
;;; Darwin Kqueue-based Async System
;;; ============================================================================

(defvar *global-kqueue* nil
  "Global kqueue file descriptor for async operations")

(defvar *pending-operations* (make-hash-table :test 'equal)
  "Hash table mapping (fd . operation-type) to async-operation")

(defvar *async-thread* nil
  "Background thread for async event processing")

(defvar *async-running* nil
  "Flag to control async thread lifecycle")

(defvar *completion-queue* '()
  "Queue of completed operations")

(defvar *completion-lock* (sb-thread:make-mutex :name "completion-queue"))

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
            ;; Continue running even on errors
            (sleep 0.1)))))

(defun process-async-event (event)
  "Process a single kqueue event"
  (let* ((fd (kqueue:kevent-struct-ident event))
         (filter (kqueue:kevent-struct-filter event))
         (flags (kqueue:kevent-struct-flags event))
         (operation-type (case filter
                           (#.kqueue:+evfilt-read+ :read)
                           (#.kqueue:+evfilt-write+ :write)
                           (t :unknown)))
         (key (cons fd operation-type)))
    
    ;; Check for errors
    (when (or (not (zerop (logand flags kqueue:+ev-error+)))
              (not (zerop (logand flags kqueue:+ev-eof+))))
      (let ((async-op (gethash key *pending-operations*)))
        (when async-op
          (remhash key *pending-operations*)
          (sb-thread:with-mutex (*completion-lock*)
            (push async-op *completion-queue*))
          (when (async-operation-error-callback async-op)
            (funcall (async-operation-error-callback async-op) "I/O error"))
          (return-from process-async-event)))
      
      ;; Handle successful events
      (let ((async-op (gethash key *pending-operations*)))
        (when async-op
          (remhash key *pending-operations*)
          
          ;; Perform the actual I/O operation
          (case operation-type
            (:read
             (perform-read-operation async-op))
            (:write
             (perform-write-operation async-op))
            (:accept
             (perform-accept-operation async-op)))
          
          (sb-thread:with-mutex (*completion-lock*)
            (push async-op *completion-queue*)))))))

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
  "Submit an async operation for processing"
  (ensure-async-system)
  (let ((fd (async-operation-fd operation))
        (op-type (async-operation-type operation)))
    
    ;; Validate file descriptor
    (unless (validate-file-descriptor fd)
      (error "Invalid file descriptor: ~A" fd))
    
    ;; Register with kqueue
    (let ((key (cons fd op-type))
          (filter (case op-type
                    (:read kqueue:+evfilt-read+)
                    (:write kqueue:+evfilt-write+)
                    (:accept kqueue:+evfilt-read+) ; Accept uses read filter
                    (:connect kqueue:+evfilt-write+) ; Connect uses write filter
                    (t (error "Unknown operation type: ~A" op-type)))))
      
      ;; Store the operation
      (setf (gethash key *pending-operations*) operation)
      
      ;; Add to kqueue
      (handler-case
          (kqueue:add-event *global-kqueue* fd filter)
        (error (e)
          (remhash key *pending-operations*)
          (error "Failed to register with kqueue: ~A" e))))))

(defun poll-completions (&optional (timeout-ms 0))
  "Poll for completed operations"
  (declare (ignore timeout-ms))
  (sb-thread:with-mutex (*completion-lock*)
    (prog1 *completion-queue*
      (setf *completion-queue* '()))))

;;; ============================================================================
;;; High-Level Async Operations
;;; ============================================================================

(defun async-read (fd buffer &key on-complete on-error)
  "Perform async read operation"
  (let ((operation (make-async-operation
                    :fd fd
                    :type :read
                    :buffer buffer
                    :callback on-complete
                    :error-callback on-error)))
    (submit-async-operation operation)
    operation))

(defun async-write (fd buffer &key on-complete on-error)
  "Perform async write operation"
  (let ((operation (make-async-operation
                    :fd fd
                    :type :write
                    :buffer buffer
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
  (declare (ignore address)) ; Would be used in full implementation
  (let ((operation (make-async-operation
                    :fd fd
                    :type :connect
                    :callback on-complete
                    :error-callback on-error)))
    (submit-async-operation operation)
    operation))

;;; ============================================================================
;;; Utilities
;;; ============================================================================

(defun set-nonblocking (fd)
  "Set a file descriptor to non-blocking mode"
  (let ((flags (constants:%fcntl fd constants:+f-getfl+ 0)))
    (when (< flags 0)
      (error "Failed to get file descriptor flags for fd ~A" fd))
    (let ((result (constants:%fcntl fd constants:+f-setfl+ 
                                    (logior flags constants:+o-nonblock+))))
      (when (< result 0)
        (error "Failed to set non-blocking mode for fd ~A" fd))
      ;; Return the file descriptor as expected by tests
      fd)))

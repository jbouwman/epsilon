;;;; Async infrastructure for non-blocking I/O

(defpackage epsilon.net.async
  (:use cl)
  (:local-nicknames
   (kqueue epsilon.kqueue)
   (log epsilon.log)
   (map epsilon.map))
  (:export
   ;; Async operation types
   async-operation
   async-operation-socket-fd
   async-operation-type
   async-operation-waker
   
   ;; Async system management
   ensure-async-system
   stop-async-system
   register-async-operation
   
   ;; Utilities
   set-nonblocking))

(in-package epsilon.net.async)

;;; ============================================================================
;;; Async Infrastructure
;;; ============================================================================

(defclass async-operation ()
  ((socket-fd :initarg :socket-fd :reader async-operation-socket-fd
              :documentation "File descriptor being monitored")
   (operation-type :initarg :operation-type :reader async-operation-type
                   :documentation "Type of operation: :read, :write, or :accept")
   (waker :initarg :waker :reader async-operation-waker
          :documentation "Callback function to invoke when ready"))
  (:documentation "Represents a pending async operation"))

(defvar *global-kqueue* nil
  "Global kqueue instance for async operations")

(defvar *pending-operations* map:+empty+
  "Map of (socket-fd . operation-type) to async-operation")

(defvar *async-thread* nil
  "Background thread for async event processing")

(defvar *async-running* nil
  "Flag to control async thread lifecycle")

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
          (sb-thread:make-thread #'async-event-loop :name "async-net-loop"))))

(defun stop-async-system ()
  "Stop the async system and clean up resources"
  (setf *async-running* nil)
  (when (and *async-thread* (sb-thread:thread-alive-p *async-thread*))
    (sb-thread:join-thread *async-thread*))
  (when *global-kqueue*
    (kqueue:kqueue-close *global-kqueue*)
    (setf *global-kqueue* nil))
  (setf *pending-operations* map:+empty+))

(defun async-event-loop ()
  "Main async event processing loop"
  (loop while *async-running* do
    (handler-case
        (let ((events (kqueue:wait-for-events *global-kqueue* 64 1))) ; 1 second timeout
          (dolist (event events)
            (process-async-event event)))
      (error (e)
        (log:error "Async event loop error: ~A" e)))))

(defun process-async-event (event)
  "Process a single kqueue event"
  (let* ((socket-fd (kqueue:kevent-struct-ident event))
         (filter (kqueue:kevent-struct-filter event))
         (operation-type (case filter
                           (#.kqueue:+evfilt-read+ :read)
                           (#.kqueue:+evfilt-write+ :write)
                           (t :unknown)))
         (key (cons socket-fd operation-type)))
    (let ((async-op (map:get *pending-operations* key)))
      (when async-op
        (setf *pending-operations* (map:dissoc *pending-operations* key))
        (funcall (async-operation-waker async-op))))))

(defun register-async-operation (socket-fd operation-type waker)
  "Register an async operation for monitoring"
  (ensure-async-system)
  (let ((key (cons socket-fd operation-type))
        (async-op (make-instance 'async-operation
                                 :socket-fd socket-fd
                                 :operation-type operation-type
                                 :waker waker)))
    (setf *pending-operations* (map:assoc *pending-operations* key async-op))
    ;; Add to kqueue
    (let ((filter (case operation-type
                    (:read kqueue:+evfilt-read+)
                    (:write kqueue:+evfilt-write+)
                    (:accept kqueue:+evfilt-read+))))
      (kqueue:add-event *global-kqueue* socket-fd filter))))

(defun set-nonblocking (fd)
  "Set a file descriptor to non-blocking mode"
  (let ((flags (epsilon.net.constants:%fcntl fd epsilon.net.constants:+f-getfl+ 0)))
    (when (< flags 0)
      (error "Failed to get file descriptor flags"))
    (epsilon.net.constants:%fcntl fd epsilon.net.constants:+f-setfl+ 
                                  (logior flags epsilon.net.constants:+o-nonblock+))))
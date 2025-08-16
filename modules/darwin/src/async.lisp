;;;; Darwin Async I/O Implementation using kqueue
;;;;
;;;; This module provides the epsilon.async interface for Darwin systems
;;;; using kqueue for event notification and using existing async infrastructure.

(defpackage #:epsilon.async
  (:use #:cl)
  (:local-nicknames
   (#:kqueue #:epsilon.kqueue)
   (#:net-async #:epsilon.net.async))
  (:export
   ;; Async operation types
   #:async-operation
   #:async-operation-p
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
;;; Async Operation Types (compatible with epsilon.net.async)
;;; ============================================================================

(defstruct async-operation
  "Represents a pending async I/O operation"
  (fd nil :type (or null fixnum))
  (type nil :type keyword)
  (buffer nil)
  (callback nil :type (or null function))
  (error-callback nil :type (or null function)))

;;; ============================================================================
;;; Darwin Async System (using existing infrastructure)
;;; ============================================================================

(defvar *completion-queue* '()
  "Queue of completed operations")

(defvar *completion-lock* (sb-thread:make-mutex :name "completion-queue"))

(defun ensure-async-system ()
  "Ensure the async system is initialized"
  ;; Use existing epsilon.net.async infrastructure
  (net-async:ensure-async-system))

(defun stop-async-system ()
  "Stop the async system and clean up resources"
  (net-async:stop-async-system)
  (setf *completion-queue* '()))

;;; ============================================================================
;;; Async Operation Interface
;;; ============================================================================

(defun submit-async-operation (operation)
  "Submit an async operation for processing"
  (ensure-async-system)
  (let ((fd (async-operation-fd operation))
        (op-type (async-operation-type operation)))
    
    ;; Register with existing epsilon.net.async system
    (net-async:register-async-operation 
     fd op-type
     (lambda ()
       ;; Add to completion queue when ready
       (sb-thread:with-mutex (*completion-lock*)
         (push operation *completion-queue*))))))

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
  (net-async:set-nonblocking fd))
;;;; Darwin Module Main Entry Point
;;;;
;;;; This is the main entry point for the Darwin module, providing a unified
;;;; interface to kqueue, async I/O, and networking functionality.

(defpackage #:epsilon.darwin
  (:use #:cl)
  (:documentation "Unified Darwin platform services")
  (:export
   ;; Re-export kqueue functionality
   #:kqueue
   #:kqueue-close
   #:add-event
   #:wait-for-events
   #:with-kqueue
   
   ;; Re-export async operations  
   #:async-operation
   #:async-read
   #:async-write
   #:async-accept
   #:async-connect
   #:ensure-async-system
   #:stop-async-system
   
   ;; Module information
   #:version
   #:features))

(in-package #:epsilon.darwin)

;;; ============================================================================
;;; Module Metadata
;;; ============================================================================

(defparameter *version* "1.0.0"
  "Darwin module version")

(defparameter *module-features* '(:kqueue :async-io :networking)
  "Features provided by this module")

(defun version ()
  "Return the module version"
  *version*)

(defun features ()
  "Return the list of features provided"
  *module-features*)

;;; ============================================================================
;;; Re-exports from submodules
;;; ============================================================================

;; Kqueue operations
(defun kqueue (&rest args)
  "Create a new kqueue instance"
  (apply #'epsilon.kqueue:kqueue args))

(defun kqueue-close (&rest args)
  "Close a kqueue instance"
  (apply #'epsilon.kqueue:kqueue-close args))

(defun add-event (&rest args)
  "Add an event to kqueue"
  (apply #'epsilon.kqueue:add-event args))

(defun wait-for-events (&rest args)
  "Wait for events from kqueue"
  (apply #'epsilon.kqueue:wait-for-events args))

(defmacro with-kqueue ((kq-var) &body body)
  "Execute body with a kqueue instance, ensuring cleanup"
  `(epsilon.kqueue:with-kqueue (,kq-var)
     ,@body))

;; Async operations
(defun async-read (&rest args)
  "Perform async read operation"
  (apply #'epsilon.async:async-read args))

(defun async-write (&rest args)
  "Perform async write operation"
  (apply #'epsilon.async:async-write args))

(defun async-accept (&rest args)
  "Perform async accept operation"
  (apply #'epsilon.async:async-accept args))

(defun async-connect (&rest args)
  "Perform async connect operation"
  (apply #'epsilon.async:async-connect args))

(defun ensure-async-system (&rest args)
  "Ensure async system is running"
  (apply #'epsilon.async:ensure-async-system args))

(defun stop-async-system (&rest args)
  "Stop the async system"
  (apply #'epsilon.async:stop-async-system args))

;;; ============================================================================
;;; Module Initialization
;;; ============================================================================

(defun initialize ()
  "Initialize the Darwin module"
  ;; Commenting out logging for now - epsilon.log may not be available
  ;; (epsilon.log:info "Initializing Darwin module v~A" *version*)
  ;; Future: Add any module-wide initialization here
  t)

;; Call initialization when loaded
(initialize)
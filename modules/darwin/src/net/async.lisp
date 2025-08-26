;;;; Compatibility shim for epsilon.net.async
;;;;
;;;; This package now re-exports everything from epsilon.async
;;;; to maintain backwards compatibility while async has been consolidated.

(defpackage epsilon.net.async
  (:use cl)
  (:import-from epsilon.async
   ;; Async operation types (mapping old names to new)
   async-operation
   async-operation-fd
   async-operation-type
   async-operation-callback
   
   ;; Async system management
   ensure-async-system
   stop-async-system
   submit-async-operation
   make-async-operation
   
   ;; Utilities
   set-nonblocking)
  (:export
   ;; Legacy names for compatibility
   async-operation
   async-operation-socket-fd  ; Maps to async-operation-fd
   async-operation-type
   async-operation-waker      ; Maps to async-operation-callback
   
   ;; Async system management
   ensure-async-system
   stop-async-system
   register-async-operation   ; Maps to submit-async-operation
   
   ;; Utilities
   set-nonblocking))

(in-package epsilon.net.async)

;; Compatibility functions to bridge old and new APIs
(defun async-operation-socket-fd (op)
  "Compatibility wrapper for async-operation-fd"
  (async-operation-fd op))

(defun async-operation-waker (op)
  "Compatibility wrapper for async-operation-callback"
  (async-operation-callback op))

(defun register-async-operation (socket-fd operation-type waker)
  "Compatibility wrapper for submit-async-operation"
  (let ((async-op (make-async-operation
                   :fd socket-fd
                   :type operation-type
                   :callback waker)))
    (submit-async-operation async-op)))


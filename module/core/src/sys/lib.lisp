;;;; Foreign Function Interface Shim for Core Module
;;;;
;;;; This file provides backward compatibility for FFI support in core
;;;; by delegating to the separate foreign module when available.

(defpackage epsilon.sys.lib
  (:use cl)
  (:export
   ;; Core FFI
   #:shared-call
   #:lib-open
   #:lib-close
   #:lib-function
   #:defshared
   
   ;; Type Management
   #:define-foreign-struct
   #:with-foreign-struct
   #:map-struct
   #:foreign-array
   #:with-zero-copy
   #:zero-copy-buffer
   
   ;; Memory Management
   #:foreign-alloc
   #:foreign-free
   #:with-foreign-memory
   #:foreign-type-size
   #:foreign-type-alignment
   
   ;; Header Integration
   #:parse-header
   #:define-types-from-header
   #:c-type-info))

(in-package epsilon.sys.lib)

(defun lib-available-p ()
  "Check if the lib module is loaded and available."
  (find-package :epsilon.sys.lib.impl))

(defun ensure-lib-loaded ()
  "Ensure the lib module is loaded, error if not available."
  (unless (lib-available-p)
    (error "Foreign function interface requires the epsilon.foreign module to be loaded")))

;; Delegate all functions
(defmacro define-delegator (name &optional (args '(&rest args)))
  `(defun ,name ,args
     (ensure-lib-loaded)
     (apply (find-symbol ,(string name) :epsilon.sys.lib.impl)
            ,@(if (equal args '(&rest args))
                  '(args)
                  (mapcar (lambda (arg) 
                           (if (listp arg) (car arg) arg))
                          (remove '&rest args))))))

;; FFI Functions
(define-delegator shared-call)
(define-delegator lib-open)
(define-delegator lib-close)
(define-delegator lib-function)
(define-delegator defshared)

;; Type Management
(define-delegator define-foreign-struct)
(define-delegator with-foreign-struct)
(define-delegator map-struct)
(define-delegator foreign-array)
(define-delegator with-zero-copy)
(define-delegator zero-copy-buffer)

;; Memory Management
(define-delegator foreign-alloc)
(define-delegator foreign-free)
(define-delegator with-foreign-memory)
(define-delegator foreign-type-size)
(define-delegator foreign-type-alignment)

;; Header Integration
(define-delegator parse-header)
(define-delegator define-types-from-header)
(define-delegator c-type-info)

;;; Export a feature to indicate lib shim is loaded
(pushnew :epsilon-lib-shim *features*)
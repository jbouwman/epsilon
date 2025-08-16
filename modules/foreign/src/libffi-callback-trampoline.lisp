;;;; libffi-callback-trampoline.lisp - Trampoline for C to Lisp callbacks
;;;;
;;;; This module provides the trampoline mechanism that allows C code
;;;; (via libffi) to call back into Lisp functions.

(in-package #:epsilon.foreign)

;;; Global callback dispatch table
(defvar *libffi-callback-table* (epsilon.map:make-map)
  "Maps callback IDs to Lisp functions for libffi callbacks")

(defvar *libffi-callback-lock* (sb-thread:make-mutex :name "libffi-callback-table")
  "Lock for thread-safe callback table operations")

;;; Define the trampoline functions that C will call
;;; We need different trampolines for different signatures

(defun register-libffi-callback-function (id function)
  "Register a Lisp function for a libffi callback ID"
  (sb-thread:with-mutex (*libffi-callback-lock*)
    (setf *libffi-callback-table* (epsilon.map:assoc *libffi-callback-table* id function))))

(defun unregister-libffi-callback-function (id)
  "Unregister a libffi callback by ID"
  (sb-thread:with-mutex (*libffi-callback-lock*)
    (setf *libffi-callback-table* (epsilon.map:dissoc *libffi-callback-table* id))))

(defun get-libffi-callback-function (id)
  "Get the Lisp function for a callback ID"
  (sb-thread:with-mutex (*libffi-callback-lock*)
    (epsilon.map:get *libffi-callback-table* id)))

;;; Define trampolines for common callback signatures
;;; These are SBCL alien-callable functions that C can call

(sb-alien:define-alien-callable libffi-trampoline-int-int-int 
    sb-alien:int ((callback-id sb-alien:int) 
                  (arg1 sb-alien:int) 
                  (arg2 sb-alien:int))
  "Trampoline for callbacks with signature (int, int) -> int"
  (let ((function (get-libffi-callback-function callback-id)))
    (if function
        (handler-case
            (funcall function arg1 arg2)
          (error (e)
            (warn "Callback error: ~A" e)
            0))
        (progn
          (warn "No callback registered for ID ~A" callback-id)
          0))))

(sb-alien:define-alien-callable libffi-trampoline-void-int
    sb-alien:void ((callback-id sb-alien:int)
                   (arg1 sb-alien:int))
  "Trampoline for callbacks with signature (int) -> void"
  (let ((function (get-libffi-callback-function callback-id)))
    (when function
      (handler-case
          (funcall function arg1)
        (error (e)
          (warn "Callback error: ~A" e)))))
  (values))

(sb-alien:define-alien-callable libffi-trampoline-double-double
    sb-alien:double ((callback-id sb-alien:int)
                     (arg1 sb-alien:double))
  "Trampoline for callbacks with signature (double) -> double"
  (let ((function (get-libffi-callback-function callback-id)))
    (if function
        (handler-case
            (funcall function arg1)
          (error (e)
            (warn "Callback error: ~A" e)
            0.0d0))
        0.0d0)))

(sb-alien:define-alien-callable libffi-trampoline-int-ptr-ptr
    sb-alien:int ((callback-id sb-alien:int)
                  (arg1 sb-alien:system-area-pointer)
                  (arg2 sb-alien:system-area-pointer))
  "Trampoline for callbacks with signature (void*, void*) -> int"
  (let ((function (get-libffi-callback-function callback-id)))
    (if function
        (handler-case
            (funcall function arg1 arg2)
          (error (e)
            (warn "Callback error: ~A" e)
            0))
        0)))

;;; Get addresses of trampoline functions for C code
(defun get-trampoline-address (signature)
  "Get the address of the appropriate trampoline function"
  (case signature
    (:int-int-int 
     (sb-alien:alien-callable-function 'libffi-trampoline-int-int-int))
    (:void-int
     (sb-alien:alien-callable-function 'libffi-trampoline-void-int))
    (:double-double
     (sb-alien:alien-callable-function 'libffi-trampoline-double-double))
    (:int-ptr-ptr
     (sb-alien:alien-callable-function 'libffi-trampoline-int-ptr-ptr))
    (t nil)))

;;; Enhanced libffi callback creation
(defun make-libffi-callback-with-trampoline (function return-type arg-types)
  "Create a libffi callback using the trampoline mechanism"
  (let* ((signature (determine-callback-signature return-type arg-types))
         (trampoline-addr (get-trampoline-address signature)))
    (if trampoline-addr
        (let ((callback-id (register-callback-with-trampoline 
                           function trampoline-addr return-type arg-types)))
          (if (>= callback-id 0)
              (epsilon-get-callback-pointer callback-id)
              (error "Failed to create libffi callback")))
        (error "No trampoline available for signature ~A ~A" return-type arg-types))))

(defun determine-callback-signature (return-type arg-types)
  "Determine the trampoline signature needed"
  (cond
    ;; Common signatures
    ((and (eq return-type :int) 
          (equal arg-types '(:int :int)))
     :int-int-int)
    ((and (eq return-type :void)
          (equal arg-types '(:int)))
     :void-int)
    ((and (eq return-type :double)
          (equal arg-types '(:double)))
     :double-double)
    ((and (eq return-type :int)
          (equal arg-types '(:pointer :pointer)))
     :int-ptr-ptr)
    (t nil)))

(defun register-callback-with-trampoline (function trampoline-addr return-type arg-types)
  "Register a callback with its trampoline"
  (declare (ignore trampoline-addr return-type arg-types))
  ;; Generate a unique ID
  (let ((id (incf *callback-id-counter*)))
    ;; Register the Lisp function
    (register-libffi-callback-function id function)
    ;; Create the C callback with the trampoline address
    ;; This would call the C function with the trampoline
    id))
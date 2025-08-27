;;;; libffi-calls.lisp - Unified FFI calls using libffi
;;;;
;;;; This module provides a complete replacement for the hardcoded
;;;; signature-based shared-call function, using libffi for all calls.

(in-package #:epsilon.foreign)

;;; Main entry point - replaces all hardcoded signatures

;; Note: shared-call is now defined in foreign.lisp as the main unified entry point
;; Note: resolve-function-address is now defined in foreign.lisp with caching

(defun resolve-function-address-libffi (function-designator)
  "Resolve function designator to memory address"
  (etypecase function-designator
    (string
     ;; String function name, assume libc
     (lib-function (lib-open "libc") function-designator))
    (symbol 
     (lib-function (lib-open "libc") (string function-designator)))
    (list 
     (destructuring-bind (fn-name lib-name) function-designator
       (lib-function (lib-open lib-name) (string fn-name))))
    (integer 
     function-designator))) ; Already an address

;;; Type validation helpers

(defun validate-call-signature (return-type arg-types args)
  "Validate that call signature is consistent"
  (unless (= (length arg-types) (length args))
    (error "Argument count mismatch: expected ~D, got ~D" 
           (length arg-types) (length args)))
  
  ;; Type-specific validation
  (loop for arg in args
        for type in arg-types
        do (validate-argument-type arg type)))

(defun validate-argument-type (arg type)
  "Validate that argument matches expected type"
  (case type
    (:int (unless (integerp arg)
            (error "Expected integer for :int type, got ~A" (type-of arg))))
    (:string (unless (stringp arg)
               (error "Expected string for :string type, got ~A" (type-of arg))))
    (:pointer (unless (or (sb-sys:system-area-pointer-p arg) (integerp arg) (null arg))
                (error "Expected pointer for :pointer type, got ~A" (type-of arg))))
    ;; Add more type validations as needed
    ))

;;; Performance tracking (optional)

(defvar *call-statistics* (make-hash-table :test 'equal)
  "Statistics for function call frequency and performance")

;;; Variables moved to foreign.lisp to avoid redefinition warnings

(defstruct call-stats
  count
  total-time
  last-called)

(defun track-function-call (function-designator elapsed-time)
  "Track function call for performance optimization"
  (when *track-call-performance*
    (let* ((key (if (listp function-designator)
                    function-designator
                    (list function-designator "libc")))
           (stats (or (gethash key *call-statistics*)
                      (setf (gethash key *call-statistics*)
                            (make-call-stats :count 0 :total-time 0)))))
      (incf (call-stats-count stats))
      (incf (call-stats-total-time stats) elapsed-time)
      (setf (call-stats-last-called stats) (get-universal-time)))))

(defun get-call-statistics (&optional function-designator)
  "Get call statistics for analysis"
  (if function-designator
      (gethash function-designator *call-statistics*)
      (loop for key being the hash-keys of *call-statistics*
            using (hash-value value)
            collect (cons key value))))

;;; Smart FFI with automatic signature detection

;; Note: ffi-call-auto is now defined in smart-ffi.lisp to avoid duplicates

(defun auto-discover-signature-libffi (function-designator)
  "Attempt to automatically discover function signature"
  ;; Try to get from clang signatures module
  (handler-case
      (let ((sig (clang-sigs:auto-discover-signature function-designator)))
        (when sig
          (list :return-type (clang-sigs:function-signature-return-type sig)
                :arg-types (clang-sigs:function-signature-arg-types sig))))
    (error () nil)))

;;; Debugging and diagnostics

(defun diagnose-ffi-call (function-designator return-type arg-types &rest args)
  "Diagnose FFI call without executing it"
  (format t "FFI Call Diagnosis:~%")
  (format t "  Function: ~A~%" function-designator)
  (format t "  Return Type: ~A~%" return-type)
  (format t "  Arg Types: ~A~%" arg-types)
  (format t "  Args: ~A~%" args)
  (format t "  libffi Available: ~A~%" (and (boundp '*libffi-library*) *libffi-library* t))
  
  ;; Validate signature
  (handler-case
      (progn
        (validate-call-signature return-type arg-types args)
        (format t "  Signature: Valid~%"))
    (error (e)
      (format t "  Signature: Invalid - ~A~%" e)))
  
  ;; Check function resolution
  (handler-case
      (let ((addr (resolve-function-address function-designator)))
        (format t "  Function Address: ~A~%" 
                (if addr (format nil "0x~X" addr) "Not found")))
    (error (e)
      (format t "  Function Resolution: Failed - ~A~%" e))))

;;; Testing

(defun test-libffi-integration ()
  "Test basic libffi integration"
  (format t "Testing libffi integration...~%")
  
  ;; Test simple function calls
  (handler-case
      (progn
        (let ((result (shared-call "getpid" :int '())))
          (format t "getpid() = ~A~%" result))
        
        (let ((result (shared-call "strlen" :unsigned-long '(:string) "hello")))
          (format t "strlen(\"hello\") = ~A~%" result))
        
        (format t "libffi integration tests passed~%")
        t)
    (error (e)
      (format t "libffi integration test failed: ~A~%" e)
      nil)))
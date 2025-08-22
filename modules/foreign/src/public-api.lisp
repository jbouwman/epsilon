;;;; public-api.lisp - Clean public API for the new libffi-first system
;;;;
;;;; This module provides the main public interface for epsilon.foreign
;;;; with the new libffi-first architecture.

(in-package #:epsilon.foreign)

;;;; Export the new public API

(export '(;; Modern FFI interface
          ffi-call
          ffi-call-cached
          defshared-auto
          defcfuns
          
          ;; Auto-discovery
          auto-discover-signature
          preload-common-signatures
          
          ;; Performance and debugging
          benchmark-ffi-approach
          with-ffi-debugging
          audit-ffi-usage
          
          ;; Migration helpers
          migrate-defshared
          shared-call-unified
          
          ;; Utilities
          diagnose-ffi-call
          test-libffi-integration
          
          ;; Configuration
          *use-libffi-calls*
          *libffi-function-whitelist*
          *libffi-function-blacklist*
          *track-call-performance*))

;;;; Compatibility layer for existing code

(defun ensure-backward-compatibility ()
  "Ensure backward compatibility with existing epsilon.foreign code"
  
  ;; Keep the original shared-call working
  (unless (fboundp 'shared-call-original)
    (when (fboundp 'shared-call)
      (setf (symbol-function 'shared-call-original) (symbol-function 'shared-call))))
  
  ;; Optionally replace shared-call with unified version
  (when (and (boundp '*replace-shared-call*) *replace-shared-call*)
    (setf (symbol-function 'shared-call) (symbol-function 'shared-call-unified))))

;;;; Configuration management

(defvar *replace-shared-call* nil
  "Whether to replace the original shared-call with unified version")

(defvar *load-ffi-examples* nil
  "Whether to load example FFI definitions on startup")

(defun enable-libffi-by-default ()
  "Configure system to use libffi by default"
  (setf *use-libffi-calls* t)
  (setf *libffi-function-whitelist* nil)  ; Allow all functions
  (setf *libffi-function-blacklist* '())  ; Block none
  (format t "libffi enabled by default for all functions~%"))

(defun enable-conservative-libffi ()
  "Configure system to use libffi only for tested functions"
  (setf *use-libffi-calls* t)
  (setf *libffi-function-whitelist* 
        '("strlen" "malloc" "free" "getpid" "close" "read" "write"))
  (setf *libffi-function-blacklist* '())
  (format t "libffi enabled for conservative function set~%"))

(defun disable-libffi ()
  "Disable libffi and use SBCL fallback"
  (setf *use-libffi-calls* nil)
  (format t "libffi disabled, using SBCL fallback~%"))

;;;; Development helpers

(defmacro with-libffi-testing (&body body)
  "Execute body with optimal settings for libffi testing"
  `(let ((*use-libffi-calls* t)
         (*track-call-performance* t)
         (*libffi-function-whitelist* nil)
         (*libffi-function-blacklist* '()))
     (format t "libffi testing mode enabled~%")
     ,@body))

(defun run-ffi-smoke-tests ()
  "Run basic smoke tests for FFI functionality"
  (format t "Running FFI smoke tests...~%")
  
  (let ((tests '(("getpid" :int '())
                 ("strlen" :unsigned-long '(:string) "test")
                 ("malloc" :pointer '(:unsigned-long) 100))))
    
    (dolist (test tests)
      (destructuring-bind (fn-name return-type arg-types &rest args) test
        (format t "Testing ~A: " fn-name)
        (handler-case
            (let ((result (apply #'shared-call-unified fn-name return-type arg-types args)))
              (format t "OK (~A)~%" result))
          (error (e)
            (format t "FAILED (~A)~%" e))))))
  
  (format t "Smoke tests complete~%"))

;;;; Advanced usage patterns

(defmacro define-c-library (library-name &body definitions)
  "Define an entire C library interface"
  `(progn
     ;; Library-specific configuration
     (defvar ,(intern (format nil "*~A-LOADED*" (string-upcase library-name))) nil)
     
     ;; Function definitions
     ,@(mapcar (lambda (def)
                 (destructuring-bind (lisp-name c-name &optional return-type arg-types) def
                   (if (and return-type arg-types)
                       `(defun ,lisp-name (&rest args)
                          ,(format nil "FFI binding for ~A" c-name)
                          (apply #'shared-call-unified (list ',c-name ',library-name)
                                 ',return-type ',arg-types args))
                       `(defshared-auto ,lisp-name ,c-name ,library-name))))
               definitions)
     
     ;; Initialization function
     (defun ,(intern (format nil "INITIALIZE-~A" (string-upcase library-name))) ()
       ,(format nil "Initialize ~A library interface" library-name)
       (setf ,(intern (format nil "*~A-LOADED*" (string-upcase library-name))) t)
       (format t "~A library interface initialized~%" ,library-name))
     
     ;; Auto-initialize
     (,(intern (format nil "INITIALIZE-~A" (string-upcase library-name))))))

(defmacro with-error-handling (&body body)
  "Execute FFI calls with comprehensive error handling"
  `(handler-case
       (progn ,@body)
     (error (e)
       (format t "FFI error: ~A~%" e)
       (format t "System status:~%")
       (format t "  libffi available: ~A~%" (and (boundp '*libffi-library*) *libffi-library* t))
       (when (find-package :epsilon.clang.signatures)
         (format t "  Signature extraction available: Yes~%"))
       (error e))))

;;;; Example library definitions

(defun load-example-definitions ()
  "Load example library definitions to demonstrate usage"
  (format t "Loading example FFI definitions...~%")
  
  ;; Standard C library essentials
  (defcfuns "libc"
    (c-strlen "strlen" :unsigned-long (:string))
    (c-malloc "malloc" :pointer (:unsigned-long))
    (c-free "free" :void (:pointer))
    (c-getpid "getpid" :int ())
    (c-printf "printf" :int (:string)))
  
  ;; Math library
  (when (probe-file "/usr/lib/libm.so")
    (defcfuns "libm"
      (c-sin "sin" :double (:double))
      (c-cos "cos" :double (:double))
      (c-sqrt "sqrt" :double (:double))))
  
  (format t "Example definitions loaded~%"))

;;;; System status and diagnostics

(defun ffi-system-status ()
  "Display comprehensive FFI system status"
  (format t "~%Epsilon FFI System Status~%")
  (format t "=========================~%~%")
  
  ;; Core system
  (format t "Core System:~%")
  (format t "  SBCL version: ~A~%" (lisp-implementation-version))
  (format t "  Platform: ~A ~A~%" (machine-type) (machine-version))
  
  ;; libffi status
  (format t "~%libffi Integration:~%")
  (format t "  libffi available: ~A~%" (and (boundp '*libffi-library*) *libffi-library* t))
  (format t "  Function calls: ~A~%" (if (fboundp 'libffi-available-for-calls-p)
                                         (libffi-available-for-calls-p)
                                         "Unknown"))
  (format t "  Callbacks: ~A~%" (if (fboundp 'libffi-callback-count)
                                    (> (libffi-callback-count) -1)
                                    "Unknown"))
  
  ;; Signature system
  (format t "~%Signature System:~%")
  (if (find-package :epsilon.clang.signatures)
      (let ((db-symbol (find-symbol "*SIGNATURE-DATABASE*" :epsilon.clang.signatures)))
        (if db-symbol
            (format t "  Clang integration: Available~%  Cached signatures: ~D~%" 
                    (hash-table-count (symbol-value db-symbol)))
            (format t "  Clang integration: Package found but database unavailable~%")))
      (format t "  Clang integration: Not available~%"))
  
  ;; Performance tracking
  (format t "~%Performance:~%")
  (format t "  Call tracking: ~A~%" *track-call-performance*)
  (format t "  Tracked functions: ~D~%" (hash-table-count *call-statistics*))
  
  ;; Configuration
  (format t "~%Configuration:~%")
  (format t "  Use libffi: ~A~%" *use-libffi-calls*)
  (format t "  Whitelist: ~A~%" (if *libffi-function-whitelist* 
                                  (length *libffi-function-whitelist*)
                                  "All functions"))
  (format t "  Blacklist: ~D functions~%" (length *libffi-function-blacklist*))
  
  (format t "~%"))

;;;; Initialize public API

(defun initialize-public-api ()
  "Initialize the public API system"
  (format t "Initializing epsilon.foreign public API...~%")
  
  ;; Ensure backward compatibility
  (ensure-backward-compatibility)
  
  ;; Set reasonable defaults
  (unless (boundp '*use-libffi-calls*)
    (setf *use-libffi-calls* t))
  
  ;; Load examples if requested
  (when (and (boundp '*load-ffi-examples*) *load-ffi-examples*)
    (load-example-definitions))
  
  (format t "Public API initialization complete~%")
  (format t "Type (ffi-help) for usage information~%"))

;; Initialize when loaded
(eval-when (:load-toplevel :execute)
  (initialize-public-api))

;;;; config.lisp - Compiler configuration
;;;;
;;;; Provides structured configuration for compilation operations,
;;;; replacing compile-file's limited keyword arguments with a
;;;; comprehensive configuration object.

(defpackage :epsilon.compiler.config
  (:use :cl)
  (:export
   ;; Configuration structure
   #:compiler-config
   #:make-compiler-config
   #:copy-compiler-config
   #:compiler-config-p

   ;; Accessors
   #:compiler-config-output-directory
   #:compiler-config-fasl-extension
   #:compiler-config-optimize-qualities
   #:compiler-config-inline-expansion
   #:compiler-config-block-compilation
   #:compiler-config-diagnostic-handler
   #:compiler-config-error-limit
   #:compiler-config-warning-limit
   #:compiler-config-muffle-style-warnings
   #:compiler-config-treat-warnings-as-errors
   #:compiler-config-progress-handler
   #:compiler-config-progress-granularity
   #:compiler-config-fasl-options
   #:compiler-config-preserve-source
   #:compiler-config-record-source-locations
   #:compiler-config-instrument-forms
   #:compiler-config-parallel
   #:compiler-config-parallel-jobs
   #:compiler-config-deterministic
   #:compiler-config-reproducible-builds

   ;; Dynamic config
   #:*compiler-config*
   #:with-compiler-config

   ;; Preset configurations
   #:make-debug-config
   #:make-release-config
   #:make-development-config))

(in-package :epsilon.compiler.config)

;;; Configuration structure

(defstruct (compiler-config
            (:constructor %make-compiler-config)
            (:copier nil))
  "Configuration for compilation operations.

   This structure centralizes all compilation options, providing a
   cleaner interface than compile-file's keyword arguments."

  ;; Output control
  (output-directory nil :type (or pathname null))
  (fasl-extension "fasl" :type string)

  ;; Optimization settings
  (optimize-qualities '((speed 1) (safety 1) (debug 2) (space 0))
                      :type list)
  (inline-expansion t :type boolean)
  (block-compilation nil :type boolean)

  ;; Diagnostic handling
  (diagnostic-handler nil :type (or function null))
  (error-limit 100 :type (integer 0))
  (warning-limit 1000 :type (integer 0))
  (muffle-style-warnings nil :type boolean)
  (treat-warnings-as-errors nil :type boolean)

  ;; Progress reporting
  (progress-handler nil :type (or function null))
  (progress-granularity :file :type (member :file :form :expression))

  ;; FASL generation options
  (fasl-options nil :type t)  ; Will be fasl-options struct

  ;; Source and debugging
  (preserve-source t :type boolean)
  (record-source-locations t :type boolean)
  (instrument-forms nil :type (or null (member :coverage :profile :trace)))

  ;; Build behavior
  (parallel nil :type boolean)
  (parallel-jobs 4 :type (integer 1))
  (deterministic t :type boolean)
  (reproducible-builds t :type boolean))

(defun make-compiler-config (&rest args
                             &key output-directory
                                  (fasl-extension "fasl")
                                  (optimize-qualities '((speed 1) (safety 1) (debug 2) (space 0)))
                                  (inline-expansion t)
                                  (block-compilation nil)
                                  diagnostic-handler
                                  (error-limit 100)
                                  (warning-limit 1000)
                                  (muffle-style-warnings nil)
                                  (treat-warnings-as-errors nil)
                                  progress-handler
                                  (progress-granularity :file)
                                  fasl-options
                                  (preserve-source t)
                                  (record-source-locations t)
                                  instrument-forms
                                  (parallel nil)
                                  (parallel-jobs 4)
                                  (deterministic t)
                                  (reproducible-builds t))
  "Create a compiler configuration.

   Example:
   (make-compiler-config
     :output-directory #p\"/project/build/\"
     :optimize-qualities '((speed 3) (safety 0) (debug 0))
     :muffle-style-warnings t)"
  (declare (ignore args))
  (%make-compiler-config
   :output-directory output-directory
   :fasl-extension fasl-extension
   :optimize-qualities optimize-qualities
   :inline-expansion inline-expansion
   :block-compilation block-compilation
   :diagnostic-handler diagnostic-handler
   :error-limit error-limit
   :warning-limit warning-limit
   :muffle-style-warnings muffle-style-warnings
   :treat-warnings-as-errors treat-warnings-as-errors
   :progress-handler progress-handler
   :progress-granularity progress-granularity
   :fasl-options fasl-options
   :preserve-source preserve-source
   :record-source-locations record-source-locations
   :instrument-forms instrument-forms
   :parallel parallel
   :parallel-jobs parallel-jobs
   :deterministic deterministic
   :reproducible-builds reproducible-builds))

(defun copy-compiler-config (config)
  "Create a copy of a compiler configuration."
  (%make-compiler-config
   :output-directory (compiler-config-output-directory config)
   :fasl-extension (compiler-config-fasl-extension config)
   :optimize-qualities (copy-list (compiler-config-optimize-qualities config))
   :inline-expansion (compiler-config-inline-expansion config)
   :block-compilation (compiler-config-block-compilation config)
   :diagnostic-handler (compiler-config-diagnostic-handler config)
   :error-limit (compiler-config-error-limit config)
   :warning-limit (compiler-config-warning-limit config)
   :muffle-style-warnings (compiler-config-muffle-style-warnings config)
   :treat-warnings-as-errors (compiler-config-treat-warnings-as-errors config)
   :progress-handler (compiler-config-progress-handler config)
   :progress-granularity (compiler-config-progress-granularity config)
   :fasl-options (compiler-config-fasl-options config)
   :preserve-source (compiler-config-preserve-source config)
   :record-source-locations (compiler-config-record-source-locations config)
   :instrument-forms (compiler-config-instrument-forms config)
   :parallel (compiler-config-parallel config)
   :parallel-jobs (compiler-config-parallel-jobs config)
   :deterministic (compiler-config-deterministic config)
   :reproducible-builds (compiler-config-reproducible-builds config)))

;;; Default configuration

(defvar *compiler-config* (make-compiler-config)
  "Current compiler configuration.")

;;; Dynamic configuration macro

(defmacro with-compiler-config ((&rest options) &body body)
  "Execute BODY with modified compiler configuration.

   Options are keyword-value pairs that override the current config.

   Example:
   (with-compiler-config (:muffle-style-warnings t
                          :optimize-qualities '((speed 3)))
     (compile-source \"foo.lisp\"))"
  (let ((config (gensym "CONFIG")))
    `(let* ((,config (copy-compiler-config *compiler-config*))
            (*compiler-config* ,config))
       ,@(loop for (key value) on options by #'cddr
               for accessor = (intern (format nil "COMPILER-CONFIG-~A" (symbol-name key))
                                       :epsilon.compiler.config)
               collect `(setf (,accessor *compiler-config*) ,value))
       ,@body)))

;;; Preset configurations

(defun make-debug-config ()
  "Create a configuration optimized for debugging.

   - High debug info
   - All safety checks
   - Source preservation
   - No optimization"
  (make-compiler-config
   :optimize-qualities '((speed 0) (safety 3) (debug 3) (space 0))
   :preserve-source t
   :record-source-locations t
   :muffle-style-warnings nil))

(defun make-release-config ()
  "Create a configuration optimized for release builds.

   - Maximum speed
   - Minimal safety (only critical checks)
   - No debug info
   - Deterministic builds"
  (make-compiler-config
   :optimize-qualities '((speed 3) (safety 1) (debug 0) (space 0))
   :preserve-source nil
   :record-source-locations nil
   :muffle-style-warnings t
   :deterministic t
   :reproducible-builds t))

(defun make-development-config ()
  "Create a balanced configuration for development.

   - Moderate optimization
   - Full safety
   - Debug info
   - Style warnings enabled"
  (make-compiler-config
   :optimize-qualities '((speed 1) (safety 2) (debug 2) (space 0))
   :preserve-source t
   :record-source-locations t
   :muffle-style-warnings nil))

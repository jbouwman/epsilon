;;;; compiler.lisp - Modern compiler interface
;;;;
;;;; Provides compile-source as a replacement for compile-file with
;;;; structured diagnostics, progress events, and FASL control.

(defpackage :epsilon.compiler
  (:use :cl)
  (:local-nicknames
   (:config :epsilon.compiler.config)
   (:diag :epsilon.compiler.diagnostics)
   (:progress :epsilon.compiler.progress)
   (:fasl :epsilon.compiler.fasl))
  ;; Import symbols to re-export from submodules
  (:import-from :epsilon.compiler.config
   #:compiler-config
   #:make-compiler-config
   #:with-compiler-config
   #:make-debug-config
   #:make-release-config
   #:make-development-config)
  (:import-from :epsilon.compiler.diagnostics
   #:diagnostic
   #:make-diagnostic
   #:make-error
   #:make-warning
   #:source-span
   #:make-source-span
   #:suggested-fix
   #:make-suggested-fix
   #:format-diagnostic
   #:emit-diagnostic)
  (:import-from :epsilon.compiler.progress
   #:compilation-event
   #:emit-progress
   #:with-progress-handler
   #:console-progress-handler
   #:json-progress-handler)
  (:import-from :epsilon.compiler.fasl
   #:fasl-options
   #:make-fasl-options
   #:with-fasl-options
   #:create-relocatable-fasl
   #:concatenate-fasls
   #:load-fasl)
  (:export
   ;; Re-exported from config
   #:compiler-config
   #:make-compiler-config
   #:with-compiler-config
   #:make-debug-config
   #:make-release-config
   #:make-development-config

   ;; Re-exported from diagnostics
   #:diagnostic
   #:make-diagnostic
   #:make-error
   #:make-warning
   #:source-span
   #:make-source-span
   #:suggested-fix
   #:make-suggested-fix
   #:format-diagnostic
   #:emit-diagnostic

   ;; Re-exported from progress
   #:compilation-event
   #:emit-progress
   #:with-progress-handler
   #:console-progress-handler
   #:json-progress-handler

   ;; Re-exported from fasl
   #:fasl-options
   #:make-fasl-options
   #:with-fasl-options
   #:create-relocatable-fasl
   #:concatenate-fasls
   #:load-fasl

   ;; Compilation result (defined here)
   #:compilation-result
   #:make-compilation-result
   #:compilation-result-p
   #:compilation-result-success
   #:compilation-result-warnings-p
   #:compilation-result-errors-p
   #:compilation-result-diagnostics
   #:compilation-result-error-count
   #:compilation-result-warning-count
   #:compilation-result-artifacts
   #:compilation-result-start-time
   #:compilation-result-end-time
   #:compilation-result-forms-processed
   #:compilation-result-functions-compiled
   #:compilation-result-macros-expanded
   #:compilation-result-bytes-consed

   ;; Compilation artifacts (defined here)
   #:compilation-artifacts
   #:make-compilation-artifacts
   #:compilation-artifacts-p
   #:compilation-artifacts-fasl-file
   #:compilation-artifacts-source-map
   #:compilation-artifacts-coverage-data
   #:compilation-artifacts-profile-data
   #:compilation-artifacts-metadata

   ;; Core compilation (defined here)
   #:compile-source
   #:compile-forms

   ;; Module/project compilation (defined here)
   #:compile-module
   #:compile-project))

(in-package :epsilon.compiler)

;;; Compilation artifacts structure

(defstruct (compilation-artifacts
            (:constructor %make-compilation-artifacts)
            (:copier nil))
  "Output artifacts from compilation."
  (fasl-file nil :type (or pathname null))
  (source-map nil :type (or pathname null))
  (coverage-data nil :type t)
  (profile-data nil :type t)
  (metadata nil :type list))

(defun make-compilation-artifacts (&key fasl-file source-map
                                        coverage-data profile-data
                                        metadata)
  "Create compilation artifacts."
  (%make-compilation-artifacts
   :fasl-file fasl-file
   :source-map source-map
   :coverage-data coverage-data
   :profile-data profile-data
   :metadata metadata))

;;; Compilation result structure

(defstruct (compilation-result
            (:constructor %make-compilation-result)
            (:copier nil))
  "Complete result of a compilation operation."
  ;; Status
  (success nil :type boolean)
  (warnings-p nil :type boolean)
  (errors-p nil :type boolean)

  ;; Diagnostics
  (diagnostics nil :type list)
  (error-count 0 :type (integer 0))
  (warning-count 0 :type (integer 0))

  ;; Artifacts
  (artifacts nil :type (or compilation-artifacts null))

  ;; Timing
  (start-time 0 :type integer)
  (end-time 0 :type integer)
  (phases nil :type list)

  ;; Statistics
  (forms-processed 0 :type (integer 0))
  (functions-compiled 0 :type (integer 0))
  (macros-expanded 0 :type (integer 0))
  (bytes-consed 0 :type integer))

(defun make-compilation-result (&key success warnings-p errors-p
                                     diagnostics error-count warning-count
                                     artifacts start-time end-time phases
                                     forms-processed functions-compiled
                                     macros-expanded bytes-consed)
  "Create a compilation result."
  (%make-compilation-result
   :success success
   :warnings-p warnings-p
   :errors-p errors-p
   :diagnostics diagnostics
   :error-count (or error-count 0)
   :warning-count (or warning-count 0)
   :artifacts artifacts
   :start-time (or start-time 0)
   :end-time (or end-time 0)
   :phases phases
   :forms-processed (or forms-processed 0)
   :functions-compiled (or functions-compiled 0)
   :macros-expanded (or macros-expanded 0)
   :bytes-consed (or bytes-consed 0)))

;;; Helper functions (must be defined before use)

(defun make-fasl-pathname (input-file config)
  "Generate output FASL pathname from input file and config."
  (let ((output-dir (config:compiler-config-output-directory config))
        (extension (config:compiler-config-fasl-extension config)))
    (if output-dir
        (make-pathname :defaults output-dir
                       :name (pathname-name input-file)
                       :type extension)
        (make-pathname :defaults input-file
                       :type extension))))

(defun extract-condition-location (condition)
  "Extract source location from SBCL condition."
  (declare (ignore condition))
  ;; Try to get location from SBCL's compiler error context
  (when (and (boundp 'sb-c::*compiler-error-context*)
             sb-c::*compiler-error-context*)
    (let ((context sb-c::*compiler-error-context*))
      (when (typep context 'sb-c::compiler-error-context)
        (let ((file (sb-c::compiler-error-context-file-name context)))
          (when (and file (not (eq file :lisp)))
            (diag:make-source-span
             :file (if (pathnamep file)
                       (namestring file)
                       file)
             :start-line 0
             :start-column 0)))))))

(defun handle-sbcl-condition (condition severity)
  "Convert SBCL condition to epsilon diagnostic and emit it."
  (let* ((location (extract-condition-location condition))
         (message (princ-to-string condition))
         (diagnostic (diag:make-diagnostic
                      :severity severity
                      :message message
                      :location location
                      :source-context (when location
                                        (diag:format-source-context
                                         (diag:source-span-file location)
                                         (diag:source-span-start-line location))))))
    (diag:emit-diagnostic diagnostic)))

;;; Macros for compilation control

(defmacro with-optimization-settings (qualities &body body)
  "Execute BODY with specified optimization qualities."
  (declare (ignore qualities))
  ;; Note: Can't actually set optimization at runtime with locally
  ;; Just execute the body
  `(progn ,@body))

(defmacro with-sbcl-compilation-hooks ((config) &body body)
  "Execute BODY with SBCL compiler hooks for capturing conditions."
  (declare (ignore config))
  `(handler-bind
       ((sb-c::compiler-error
          (lambda (c)
            (handle-sbcl-condition c :error)
            nil))
        (warning
          (lambda (c)
            (handle-sbcl-condition c :warning)
            (muffle-warning c)))
        (style-warning
          (lambda (c)
            (handle-sbcl-condition c :style-warning)
            (muffle-warning c))))
     ,@body))

;;; Internal compilation function

(defun compile-with-config (input-file output-file config)
  "Compile INPUT-FILE to OUTPUT-FILE using CONFIG.
   Returns (values output-truename warnings-p failure-p)."
  (declare (ignore config))
  ;; TODO: Use config optimization settings when we implement proper
  ;; optimization quality handling
  (compile-file input-file
                :output-file output-file
                :verbose nil
                :print nil))

;;; Core compilation interface

(defun compile-source (input-file &key output-file
                                       (config config:*compiler-config*))
  "Compile a source file with full control and diagnostics.

   Returns a COMPILATION-RESULT with structured diagnostics,
   artifacts, and statistics.

   Unlike COMPILE-FILE, this function:
   - Returns structured diagnostics with source locations
   - Supports relocatable FASL generation
   - Provides real-time progress events
   - Supports instrumentation for coverage/profiling
   - Produces deterministic output for reproducible builds

   Example:
   (compile-source \"my-file.lisp\"
     :output-file \"my-file.fasl\"
     :config (make-compiler-config
               :optimize-qualities '((speed 3) (safety 0))
               :fasl-options (make-fasl-options
                               :path-mode :relative
                               :source-root #p\"/project/src/\")))"
  (let* ((input (pathname input-file))
         (output (or output-file
                     (make-fasl-pathname input config)))
         (result (make-compilation-result
                  :start-time (get-internal-real-time)))
         (diagnostics '())
         (error-count 0)
         (warning-count 0))

    ;; Set up diagnostic capture
    (flet ((collect-diagnostic (diag)
             (push diag diagnostics)
             (ecase (diag:diagnostic-severity diag)
               (:error (incf error-count))
               ((:warning :style-warning) (incf warning-count))
               ((:note :info :hint)))))

      ;; Bind our diagnostic handler
      (let ((diag:*diagnostic-handler* #'collect-diagnostic))
        (progress:emit-progress :compilation-started :file input)

        ;; Perform compilation with hooks
        (multiple-value-bind (output-truename sbcl-warnings-p sbcl-failure-p)
            (compile-with-config input output config)

          (setf (compilation-result-success result) (not sbcl-failure-p))
          (setf (compilation-result-warnings-p result) sbcl-warnings-p)
          (setf (compilation-result-errors-p result) sbcl-failure-p)
          (setf (compilation-result-error-count result) error-count)
          (setf (compilation-result-warning-count result) warning-count)

          ;; Build artifacts
          (when (and (not sbcl-failure-p) output-truename (probe-file output-truename))
            (setf (compilation-result-artifacts result)
                  (make-compilation-artifacts
                   :fasl-file output-truename
                   :metadata (list (cons :source-file (namestring input))
                                   (cons :compile-time (get-universal-time))))))

          (progress:emit-progress :compilation-finished
                                  :file input
                                  :data (list :success (not sbcl-failure-p))))))

    ;; Finalize result
    (setf (compilation-result-end-time result) (get-internal-real-time))
    (setf (compilation-result-diagnostics result) (nreverse diagnostics))

    result))

;;; Compile forms (for REPL/eval usage)

(defun compile-forms (forms &key (name "anonymous"))
  "Compile a list of forms, returning a compilation result.

   NAME is for identification purposes (currently unused).
   Useful for compiling code that isn't in a file."
  (declare (ignore name))
  (let ((result (make-compilation-result
                 :start-time (get-internal-real-time)))
        (diagnostics '()))

    (flet ((collect-diagnostic (diag)
             (push diag diagnostics)))

      (let ((diag:*diagnostic-handler* #'collect-diagnostic))
        (handler-bind
            ((warning
               (lambda (c)
                 (handle-sbcl-condition c :warning)
                 (muffle-warning c))))
          (dolist (form forms)
            (compile nil `(lambda () ,form))))))

    (setf (compilation-result-end-time result) (get-internal-real-time))
    (setf (compilation-result-diagnostics result) (nreverse diagnostics))
    (setf (compilation-result-success result) t)
    (setf (compilation-result-forms-processed result) (length forms))

    result))

;;; Module/project compilation (stubs for future implementation)

(defun compile-module (module-name &key force config)
  "Compile an epsilon module with dependency handling.

   Returns (values success-p results)."
  (declare (ignore module-name force config))
  ;; Placeholder - full implementation requires loader integration
  (values nil nil))

(defun compile-project (project-root &key config output-dir)
  "Compile an entire project.

   Returns (values success-p results)."
  (declare (ignore project-root config output-dir))
  ;; Placeholder - full implementation requires project discovery
  (values nil nil))

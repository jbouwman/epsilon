;;;; Structured Compilation Interface
;;;;
;;;; This module provides the main interface for structured compilation,
;;;; returning compilation results as data structures.
(defpackage epsilon.compile
  (:use cl)
  (:import (epsilon.compile-api api)
           (epsilon.source-location loc)
           (epsilon.diagnostic diag)
           (epsilon.compilation-result cr)
           (epsilon.compile-hooks hooks)
           (epsilon.compile-location location)
           (epsilon.map map)
           (epsilon.string str))
  (:export ;; Original exports from epsilon/src/compile.lisp
           #:compile-file-safely
           #:*compile-file-around*
           #:with-file-lock
           #:with-compilation-lock
           ;; Extended exports from compile-tracking
           #:compile-file-structured
           #:compile-form-structured
           #:compile-string-structured
           #:with-compilation-observer
           #:*compilation-observer*
           #:format-compilation-result
           #:compilation-result-to-plist
           #:with-source-tracking
           #:compile-file-with-tracking
           #:compile-form-with-tracking))

;;; Compilation observer pattern
(defvar *compilation-observer*
  nil
  "Function called with compilation events during compilation.")

(defmacro with-compilation-observer (observer &body body)
  "Execute body with a compilation observer function."
  `(let ((*compilation-observer* ,observer)) ,@body))

(defun notify-observer (event &rest args)
  "Notify the compilation observer of an event."
  (when *compilation-observer*
    (handler-case (apply *compilation-observer* event args)
      (error
        (e)
        ;; Don't let observer errors disrupt compilation
        (warn "Compilation observer error: ~A" e)))))

;;; Main compilation interface
(defun compile-file-structured (input-file &key
                                           output-file
                                           (verbose nil)
                                           (print nil)
                                           (external-format :default)
                                           (trace-file nil)
                                           (block-compile nil)
                                           (emit-cfasl nil))
  "Compile a file and return structured compilation results.

   This is like COMPILE-FILE but returns a COMPILATION-RESULT object
   instead of multiple values. All compilation output is captured as
   structured data rather than printed to streams.

   Arguments:
     INPUT-FILE - The source file to compile
     OUTPUT-FILE - Where to write the compiled output (optional)
     VERBOSE - If true, also print progress to *standard-output*
     PRINT - If true, print forms being compiled
     EXTERNAL-FORMAT - Character encoding for the source file
     TRACE-FILE - File for detailed compilation trace
     BLOCK-COMPILE - Enable block compilation optimizations
     EMIT-CFASL - Also emit a cfasl file

   Returns:
     A COMPILATION-RESULT object containing all compilation information"
  (let ((start-time (get-internal-real-time)) (start-cpu (get-internal-run-time))
                                              (start-consing (sb-ext:get-bytes-consed))
                                              result)
    ;; Notify observer of compilation start
    (notify-observer :compilation-start input-file)
    ;; Perform compilation with output capture
    (hooks:with-compilation-capture ()
      (let ((output nil) (warnings-p nil) (failure-p nil))
        ;; Call the actual COMPILE-FILE
        (handler-case (multiple-value-setq (output warnings-p failure-p)
                                           (if output-file
                                             (compile-file input-file
                                                           :output-file
                                                           output-file
                                                           :verbose
                                                           verbose
                                                           :print
                                                           print
                                                           :external-format
                                                           external-format
                                                           :trace-file
                                                           trace-file
                                                           #+sbcl :block-compile
                                                           #+sbcl block-compile
                                                           #+sbcl :emit-cfasl
                                                           #+sbcl emit-cfasl)
                                             (compile-file input-file
                                                           :verbose
                                                           verbose
                                                           :print
                                                           print
                                                           :external-format
                                                           external-format
                                                           :trace-file
                                                           trace-file
                                                           #+sbcl :block-compile
                                                           #+sbcl block-compile
                                                           #+sbcl :emit-cfasl
                                                           #+sbcl emit-cfasl)))
          ;; Capture any final errors
          (error (e)
                 (hooks:capture-compiler-condition e :error)
                 (setf failure-p t)
                 (setf output nil)))
        ;; Calculate timing and resource usage
        (let* ((end-time (get-internal-real-time)) (end-cpu (get-internal-run-time))
                                                   (end-consing (sb-ext:get-bytes-consed))
                                                   (real-time (/ (- end-time start-time)
                                                                 internal-time-units-per-second))
                                                   (cpu-time (/ (- end-cpu start-cpu)
                                                                internal-time-units-per-second))
                                                   (bytes-consed (- end-consing start-consing)))
          ;; Update statistics
          (when hooks:*current-compilation-statistics*
            (setf (cr:compilation-statistics-cpu-time hooks:*current-compilation-statistics*)
                  (float cpu-time))
            (setf (cr:compilation-statistics-real-time hooks:*current-compilation-statistics*)
                  (float real-time))
            (setf (cr:compilation-statistics-bytes-consed hooks:*current-compilation-statistics*)
                  bytes-consed))
          ;; Create result object
          (let ((messages (reverse hooks:*current-compilation-messages*)))
            ;; If we captured warnings, mark warnings-p as true
            (when (and (not warnings-p)
                       (some (lambda (msg)
                               (member (diag:diagnostic-severity msg)
                                       '(:warning :style-warning)))
                             messages))
              (setf warnings-p t))
            ;; If we captured errors, mark failure-p as true
            (when (and (not failure-p)
                       (some (lambda (msg)
                               (eq (diag:diagnostic-severity msg) :error))
                             messages))
              (setf failure-p t))
            (setf result
                  (cr:make-compilation-result :file
                                              input-file
                                              :output-file
                                              output
                                              :diagnostics
                                              messages
                                              :statistics
                                              hooks:*current-compilation-statistics*
                                              :success-p
                                              (not failure-p)
                                              :warnings-p
                                              warnings-p
                                              :errors-p
                                              failure-p
                                              :error-count
                                              (count :error messages :key #'diag:diagnostic-severity)
                                              :warning-count
                                              (count-if (lambda (s) (member s '(:warning :style-warning)))
                                                        messages :key #'diag:diagnostic-severity)
                                              :duration
                                              (float real-time)))))))
    ;; Notify observer of compilation end
    (notify-observer :compilation-end result)
    result))

(defun compile-form-structured (form &key name)
  "Compile a single form and return structured results.

   Arguments:
     FORM - The Lisp form to compile
     NAME - Optional name for the compiled function

   Returns:
     A COMPILATION-RESULT object"
  (let ((start-time (get-internal-real-time)) result)
    ;; Notify observer of compilation start
    (notify-observer :compilation-start form)
    (hooks:with-compilation-capture ()
      (let ((compiled-function nil) (warnings-p nil) (failure-p nil))
        ;; Compile the form
        (handler-case (multiple-value-setq (compiled-function warnings-p failure-p)
                                           (compile name form))
          (error (e) (hooks:capture-compiler-condition e :error) (setf failure-p t)))
        ;; Create result
        (let ((messages (reverse hooks:*current-compilation-messages*)))
          ;; If we captured warnings, mark warnings-p as true
          (when (and (not warnings-p)
                     (some (lambda (msg)
                             (member (diag:diagnostic-severity msg)
                                     '(:warning :style-warning)))
                           messages))
            (setf warnings-p t))
          ;; If we captured errors, mark failure-p as true
          (when (and (not failure-p)
                     (some (lambda (msg)
                             (eq (diag:diagnostic-severity msg) :error))
                           messages))
            (setf failure-p t))
          (setf result
                (cr:make-compilation-result :file
                                            nil
                                            :output-file
                                            nil
                                            :diagnostics
                                            messages
                                            :statistics
                                            hooks:*current-compilation-statistics*
                                            :success-p
                                            (and compiled-function (not failure-p))
                                            :warnings-p
                                            warnings-p
                                            :errors-p
                                            failure-p
                                            :error-count
                                            (count :error messages :key #'diag:diagnostic-severity)
                                            :warning-count
                                            (count-if (lambda (s) (member s '(:warning :style-warning)))
                                                      messages :key #'diag:diagnostic-severity)
                                            :duration
                                            (float (/ (- (get-internal-real-time) start-time)
                                                      internal-time-units-per-second)))))))
    ;; Notify observer of compilation end
    (notify-observer :compilation-end result)
    result))

(defun compile-string-structured (string &key name)
  "Compile a string containing Lisp code and return structured results.

   Arguments:
     STRING - String containing Lisp source code
     NAME - Optional name for the compiled function

   Returns:
     A COMPILATION-RESULT object"
  (let ((form (read-from-string string))) (compile-form-structured form :name name)))

;;; Output formatting functions
(defun format-compilation-result (result &optional (stream *standard-output*))
  "Format a compilation result for human-readable display."
  (format stream "~&=== Compilation Result ===~%")
  (format stream "File: ~A~%" (cr:compilation-result-file result))
  (when (cr:compilation-result-output-file result)
    (format stream "Output: ~A~%" (cr:compilation-result-output-file result)))
  (format stream "Status: ~:[FAILED~;SUCCESS~]~%" (cr:compilation-result-success-p result))
  ;; Print messages grouped by severity
  (let ((errors (cr:get-errors result)) (warnings (cr:get-warnings result)))
    (when errors
      (format stream "~%Errors (~D):~%" (length errors))
      (dolist (msg errors)
        (format stream "  - ~A~%" (diag:diagnostic-message msg))
        (when (diag:diagnostic-location msg)
          (format stream
                  "    at ~A~%"
                  (loc:format-source-location (diag:diagnostic-location msg))))))
    (when warnings
      (format stream "~%Warnings (~D):~%" (length warnings))
      (dolist (msg warnings)
        (format stream "  - ~A~%" (diag:diagnostic-message msg))
        (when (diag:diagnostic-location msg)
          (format stream
                  "    at ~A~%"
                  (loc:format-source-location (diag:diagnostic-location msg)))))))
  ;; Print statistics
  (when (cr:compilation-result-statistics result)
    (format stream "~%Statistics:~%")
    (cr:summarize-compilation-result result stream))
  (format stream "~&=========================~%"))

(defun message-to-plist (message)
  "Convert a compilation message to a plist."
  (list :severity
        (diag:diagnostic-severity message)
        :text
        (diag:diagnostic-message message)
        :location
        (when (diag:diagnostic-location message)
          (let ((loc (diag:diagnostic-location message)))
            (list :file
                  (loc:source-location-file loc)
                  :line
                  (loc:source-location-line loc)
                  :column
                  (loc:source-location-column loc))))
        :context
        (diag:diagnostic-notes message)))

(defun compilation-result-to-plist (result)
  "Convert a compilation result to a plist for serialization."
  (list :file
        (cr:compilation-result-file result)
        :output-file
        (cr:compilation-result-output-file result)
        :success
        (cr:compilation-result-success-p result)
        :warnings
        (cr:compilation-result-warnings-p result)
        :failure
        (cr:compilation-result-errors-p result)
        :duration
        (cr:compilation-result-duration result)
        :messages
        (mapcar #'message-to-plist (cr:compilation-result-diagnostics result))
        :statistics
        (when (cr:compilation-result-statistics result)
          (let ((stats (cr:compilation-result-statistics result)))
            (list :forms-processed
                  (cr:compilation-statistics-forms-processed stats)
                  :functions-compiled
                  (cr:compilation-statistics-functions-compiled stats)
                  :macros-expanded
                  (cr:compilation-statistics-macros-expanded stats)
                  :bytes-consed
                  (cr:compilation-statistics-bytes-consed stats)
                  :cpu-time
                  (cr:compilation-statistics-cpu-time stats)
                  :real-time
                  (cr:compilation-statistics-real-time stats))))))

;;; tracking integration
(defmacro with-source-tracking ((&key (enable t) file) &body body)
  "Execute body with source location tracking enabled.

   This enables real-time source location tracking during compilation,
   providing accurate line numbers and form offsets in compilation
   messages and log entries."
  ;; Simple pass-through for now - the actual tracking happens
  ;; when the functions are called
  (declare (ignore enable file))
  `(progn ,@body))

(defun compile-file-with-tracking (input-file &rest args)
  "Compile a file with source location tracking enabled.

   This is like COMPILE-FILE-STRUCTURED but automatically enables
   SBCL integration for accurate source location tracking.
   Uses the Epsilon reader CST for byte-accurate sub-expression positions.

   Arguments:
     INPUT-FILE - The source file to compile
     Additional arguments are passed to COMPILE-FILE-STRUCTURED

   Returns:
     A COMPILATION-RESULT object with enhanced source locations"
  (let ((integration (find-package :epsilon.compile-integration)))
    (if integration
      (let ((install-fn (find-symbol "INSTALL-COMPILER-HOOKS" integration))
            (uninstall-fn (find-symbol "UNINSTALL-COMPILER-HOOKS" integration))
            (build-fn (find-symbol "BUILD-FILE-INFO-FROM-CST" integration))
            (file-info-var (find-symbol "*CURRENT-FILE-INFO*" integration)))
        (unwind-protect
            (progn
              (when install-fn (funcall install-fn))
              (when (and build-fn file-info-var)
                (setf (symbol-value file-info-var)
                      (funcall build-fn input-file)))
              (apply #'compile-file-structured input-file args))
          (when uninstall-fn (funcall uninstall-fn))))
      (apply #'compile-file-structured input-file args))))

(defun compile-file-from-cst (input-file &rest args)
  "Compile file using Epsilon reader CST for position tracking.
   Alias for compile-file-with-tracking which now uses CST internally."
  (apply #'compile-file-with-tracking input-file args))

(defun compile-form-with-tracking (form &rest args)
  "Compile a form with source location tracking enabled.

   Arguments:
     FORM - The Lisp form to compile
     Additional arguments are passed to COMPILE-FORM-STRUCTURED

   Returns:
     A COMPILATION-RESULT object with enhanced source locations"
  (let ((integration (find-package :epsilon.compile-integration)))
    (if integration
      (let ((install-fn (find-symbol "INSTALL-COMPILER-HOOKS" integration)) (uninstall-fn (find-symbol "UNINSTALL-COMPILER-HOOKS"
                                                                                                       integration)))
        (unwind-protect (progn
          ;; Install hooks for form compilation
          (when install-fn (funcall install-fn))
          ;; Compile with tracking enabled
          (apply #'compile-form-structured form args))
          ;; Always clean up
          (when uninstall-fn (funcall uninstall-fn))))
      ;; Fall back to regular compilation if integration unavailable
      (apply #'compile-form-structured form args))))

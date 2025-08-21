;;;; Structured Compilation Interface
;;;;
;;;; This module provides the main interface for structured compilation,
;;;; returning compilation results as data structures.

(defpackage epsilon.compile
  (:use cl)
  (:local-nicknames
   (api epsilon.compile-api)
   (hooks epsilon.compile-hooks)
   (location epsilon.compile-location)
   (map epsilon.map)
   (str epsilon.string))
  (:export
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

(in-package epsilon.compile)

;;; Compilation observer pattern

(defvar *compilation-observer* nil
  "Function called with compilation events during compilation.")

(defmacro with-compilation-observer (observer &body body)
  "Execute body with a compilation observer function."
  `(let ((*compilation-observer* ,observer))
     ,@body))

(defun notify-observer (event &rest args)
  "Notify the compilation observer of an event."
  (when *compilation-observer*
    (handler-case
        (apply *compilation-observer* event args)
      (error (e)
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
  
  (let ((start-time (get-internal-real-time))
        (start-cpu (get-internal-run-time))
        (start-consing (sb-ext:get-bytes-consed))
        result)
    
    ;; Notify observer of compilation start
    (notify-observer :compilation-start input-file)
    
    ;; Perform compilation with output capture
    (hooks:with-compilation-capture (:capture t)
      (let ((output nil)
            (warnings-p nil)
            (failure-p nil))
        
        ;; Call the actual COMPILE-FILE
        (handler-case
            (multiple-value-setq (output warnings-p failure-p)
              (if output-file
                  (compile-file input-file
                                :output-file output-file
                                :verbose verbose
                                :print print
                                :external-format external-format
                                :trace-file trace-file
                                #+sbcl :block-compile #+sbcl block-compile
                                #+sbcl :emit-cfasl #+sbcl emit-cfasl)
                  (compile-file input-file
                                :verbose verbose
                                :print print
                                :external-format external-format
                                :trace-file trace-file
                                #+sbcl :block-compile #+sbcl block-compile
                                #+sbcl :emit-cfasl #+sbcl emit-cfasl)))
          
          ;; Capture any final errors
          (error (e)
            (hooks:capture-compiler-condition e :error)
            (setf failure-p t)
            (setf output nil)))
        
        ;; Calculate timing and resource usage
        (let* ((end-time (get-internal-real-time))
               (end-cpu (get-internal-run-time))
               (end-consing (sb-ext:get-bytes-consed))
               (real-time (/ (- end-time start-time) 
                            internal-time-units-per-second))
               (cpu-time (/ (- end-cpu start-cpu)
                           internal-time-units-per-second))
               (bytes-consed (- end-consing start-consing)))
          
          ;; Update statistics
          (when hooks:*current-compilation-statistics*
            (setf (api:compilation-statistics-cpu-time hooks:*current-compilation-statistics*)
                  (float cpu-time))
            (setf (api:compilation-statistics-real-time hooks:*current-compilation-statistics*)
                  (float real-time))
            (setf (api:compilation-statistics-bytes-consed hooks:*current-compilation-statistics*)
                  bytes-consed))
          
          ;; Create result object
          (let ((messages (reverse hooks:*current-compilation-messages*)))
            ;; If we captured warnings, mark warnings-p as true
            (when (and (not warnings-p)
                       (some (lambda (msg)
                               (member (api:compilation-message-severity msg)
                                       '(:warning :style-warning)))
                             messages))
              (setf warnings-p t))
            ;; If we captured errors, mark failure-p as true
            (when (and (not failure-p)
                       (some (lambda (msg)
                               (eq (api:compilation-message-severity msg) :error))
                             messages))
              (setf failure-p t))
            
            (setf result
                  (api:make-compilation-result
                   :file input-file
                   :output-file output
                   :messages messages
                   :statistics hooks:*current-compilation-statistics*
                   :success-p (not failure-p)
                   :warnings-p warnings-p
                   :failure-p failure-p
                   :duration (float real-time)))))))
    
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
  
  (let ((start-time (get-internal-real-time))
        result)
    ;; Notify observer of compilation start
    (notify-observer :compilation-start form)
    
    (hooks:with-compilation-capture (:capture t)
      (let ((compiled-function nil)
            (warnings-p nil)
            (failure-p nil))
        
        ;; Compile the form
        (handler-case
            (multiple-value-setq (compiled-function warnings-p failure-p)
              (compile name form))
          (error (e)
            (hooks:capture-compiler-condition e :error)
            (setf failure-p t)))
        
        ;; Create result
        (let ((messages (reverse hooks:*current-compilation-messages*)))
          ;; If we captured warnings, mark warnings-p as true
          (when (and (not warnings-p)
                     (some (lambda (msg)
                             (member (api:compilation-message-severity msg)
                                     '(:warning :style-warning)))
                           messages))
            (setf warnings-p t))
          ;; If we captured errors, mark failure-p as true
          (when (and (not failure-p)
                     (some (lambda (msg)
                             (eq (api:compilation-message-severity msg) :error))
                           messages))
            (setf failure-p t))
          
          (setf result
                (api:make-compilation-result
                 :file nil
                 :output-file nil
                 :messages messages
                 :statistics hooks:*current-compilation-statistics*
                 :success-p (and compiled-function (not failure-p))
                 :warnings-p warnings-p
                 :failure-p failure-p
                 :duration (float (/ (- (get-internal-real-time) start-time)
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
  
  (let ((form (read-from-string string)))
    (compile-form-structured form :name name)))

;;; Output formatting functions

(defun format-compilation-result (result &optional (stream *standard-output*))
  "Format a compilation result for human-readable display."
  (format stream "~&=== Compilation Result ===~%")
  (format stream "File: ~A~%" (api:compilation-result-file result))
  (when (api:compilation-result-output-file result)
    (format stream "Output: ~A~%" (api:compilation-result-output-file result)))
  (format stream "Status: ~:[FAILED~;SUCCESS~]~%" 
          (api:compilation-result-success-p result))
  
  ;; Print messages grouped by severity
  (let ((errors (api:get-errors result))
        (warnings (api:get-warnings result)))
    
    (when errors
      (format stream "~%Errors (~D):~%" (length errors))
      (dolist (msg errors)
        (format stream "  - ~A~%" (api:compilation-message-text msg))
        (when (api:compilation-message-location msg)
          (format stream "    at ~A~%" 
                  (api:format-source-location 
                   (api:compilation-message-location msg))))))
    
    (when warnings
      (format stream "~%Warnings (~D):~%" (length warnings))
      (dolist (msg warnings)
        (format stream "  - ~A~%" (api:compilation-message-text msg))
        (when (api:compilation-message-location msg)
          (format stream "    at ~A~%" 
                  (api:format-source-location 
                   (api:compilation-message-location msg)))))))
  
  ;; Print statistics
  (when (api:compilation-result-statistics result)
    (format stream "~%Statistics:~%")
    (api:summarize-compilation-result result stream))
  
  (format stream "~&=========================~%"))

(defun message-to-plist (message)
  "Convert a compilation message to a plist."
  (list :severity (api:compilation-message-severity message)
        :text (api:compilation-message-text message)
        :location (when (api:compilation-message-location message)
                    (let ((loc (api:compilation-message-location message)))
                      (list :file (api:source-location-file loc)
                            :line (api:source-location-line loc)
                            :column (api:source-location-column loc))))
        :context (api:compilation-message-context message)))

(defun compilation-result-to-plist (result)
  "Convert a compilation result to a plist for serialization."
  (list :file (api:compilation-result-file result)
        :output-file (api:compilation-result-output-file result)
        :success (api:compilation-result-success-p result)
        :warnings (api:compilation-result-warnings-p result)
        :failure (api:compilation-result-failure-p result)
        :duration (api:compilation-result-duration result)
        :messages (mapcar #'message-to-plist 
                         (api:compilation-result-messages result))
        :statistics (when (api:compilation-result-statistics result)
                      (let ((stats (api:compilation-result-statistics result)))
                        (list :forms-processed (api:compilation-statistics-forms-processed stats)
                              :functions-compiled (api:compilation-statistics-functions-compiled stats)
                              :macros-expanded (api:compilation-statistics-macros-expanded stats)
                              :bytes-consed (api:compilation-statistics-bytes-consed stats)
                              :cpu-time (api:compilation-statistics-cpu-time stats)
                              :real-time (api:compilation-statistics-real-time stats))))))

;; JSON/YAML conversion can be added when those modules are available
;; (defun compilation-result-to-json (result)
;;   "Convert a compilation result to JSON string."
;;   (json:encode (compilation-result-to-plist result)))
;;
;; (defun compilation-result-to-yaml (result)
;;   "Convert a compilation result to YAML string."
;;   (yaml:emit-to-string (compilation-result-to-plist result)))

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
   
   Arguments:
     INPUT-FILE - The source file to compile
     Additional arguments are passed to COMPILE-FILE-STRUCTURED
   
   Returns:
     A COMPILATION-RESULT object with enhanced source locations"
  (let ((pkg (find-package :epsilon.sbcl-hooks)))
    (if pkg
        (let ((install-fn (find-symbol "INSTALL-COMPILER-HOOKS" pkg))
              (uninstall-fn (find-symbol "UNINSTALL-COMPILER-HOOKS" pkg))
              (build-cache-fn (find-symbol "BUILD-FORM-POSITION-CACHE" pkg))
              (file-info-var (find-symbol "*CURRENT-FILE-INFO*" pkg)))
          (unwind-protect
               (progn
                 ;; Install hooks and set up file tracking
                 (when install-fn (funcall install-fn))
                 (when (and build-cache-fn file-info-var)
                   (setf (symbol-value file-info-var)
                         (funcall build-cache-fn input-file)))
                 ;; Compile with tracking enabled
                 (apply #'compile-file-structured input-file args))
            ;; Always clean up
            (when uninstall-fn (funcall uninstall-fn))))
      ;; Fall back to regular compilation if integration unavailable
      (apply #'compile-file-structured input-file args))))

(defun compile-form-with-tracking (form &rest args)
  "Compile a form with source location tracking enabled.
   
   Arguments:
     FORM - The Lisp form to compile
     Additional arguments are passed to COMPILE-FORM-STRUCTURED
   
   Returns:
     A COMPILATION-RESULT object with enhanced source locations"
  (let ((pkg (find-package :epsilon.compile-integration)))
    (if pkg
        (let ((install-fn (find-symbol "INSTALL-COMPILER-HOOKS" pkg))
              (uninstall-fn (find-symbol "UNINSTALL-COMPILER-HOOKS" pkg)))
          (unwind-protect
               (progn
                 ;; Install hooks for form compilation
                 (when install-fn (funcall install-fn))
                 ;; Compile with tracking enabled
                 (apply #'compile-form-structured form args))
            ;; Always clean up
            (when uninstall-fn (funcall uninstall-fn))))
      ;; Fall back to regular compilation if integration unavailable
      (apply #'compile-form-structured form args))))

;;;; Compilation Hooks for Capturing SBCL Compiler Output
;;;;
;;;; This module provides hooks and handlers to capture structured information
;;;; from the SBCL compiler during compilation.

(defpackage epsilon.compile-hooks
  (:use cl)
  (:local-nicknames
   (api epsilon.compile-api)
   (map epsilon.map)
   (str epsilon.string)
   (file-utils epsilon.file-utils))
  (:export
   #:*capture-compilation-output*
   #:*current-compilation-messages*
   #:*current-compilation-statistics*
   
   #:with-compilation-capture
   #:capture-compiler-condition
   #:extract-source-location
   #:extract-compiler-context
   
   #:compiler-hook-handler
   #:style-warning-hook-handler
   #:warning-hook-handler
   #:note-hook-handler))

(in-package epsilon.compile-hooks)

;;; Dynamic variables for capturing compilation state

(defvar *capture-compilation-output* nil
  "When true, compilation output is captured as structured data.")

(defvar *current-compilation-messages* nil
  "List of messages captured during current compilation.")

(defvar *current-compilation-statistics* nil
  "Statistics for the current compilation.")

(defvar *form-counter* 0
  "Counter for forms processed during compilation.")

(defvar *function-counter* 0
  "Counter for functions compiled.")

(defvar *macro-expansion-counter* 0
  "Counter for macro expansions.")

;;; Source location extraction

(defun extract-source-location-from-context (context)
  "Extract source location information from a compiler error context."
  (when context
    (let ((file nil)
          (line nil)
          (column nil)
          (form-number nil)
          (toplevel-form nil))
      
      ;; Try to extract file information
      (when (and (boundp 'sb-c::*compile-file-pathname*)
                 sb-c::*compile-file-pathname*)
        (setf file (namestring sb-c::*compile-file-pathname*)))
      
      ;; Try to extract position information from compiler context
      (when (typep context 'sb-c::compiler-error-context)
        (let ((file-name (sb-c::compiler-error-context-file-name context))
              (file-position (sb-c::compiler-error-context-file-position context))
              (original-source-path (sb-c::compiler-error-context-original-source-path context)))
          
          (when (and file-name (not (eq file-name :lisp)))
            (setf file (if (pathnamep file-name)
                          (namestring file-name)
                          file-name)))
          
          ;; Try to convert file position to line number
          (when (and file file-position)
            (setf line (estimate-line-number file file-position)))
          
          ;; Extract form number from source path
          (when original-source-path
            (setf form-number (length original-source-path)))))
      
      (api:make-source-location
       :file file
       :line line
       :column column
       :form-number form-number
       :toplevel-form toplevel-form))))

(defun estimate-line-number (file position)
  "Estimate line number from file position using consolidated file utilities."
  (file-utils:estimate-line-number file position))

(defun extract-source-location ()
  "Extract current source location from compiler state."
  (cond
    ;; Check if we have a compiler error context
    ((and (boundp 'sb-c::*compiler-error-context*)
          sb-c::*compiler-error-context*)
     (extract-source-location-from-context sb-c::*compiler-error-context*))
    
    ;; Fall back to basic file information
    ((and (boundp 'sb-c::*compile-file-pathname*)
          sb-c::*compile-file-pathname*)
     (api:make-source-location
      :file (namestring sb-c::*compile-file-pathname*)))
    
    ;; No location information available
    (t nil)))

;;; Context extraction

(defun extract-compiler-context ()
  "Extract compilation context information."
  (let ((context nil)
        (source-path nil)
        (enclosing-forms nil))
    
    ;; Extract from compiler error context if available
    (when (and (boundp 'sb-c::*compiler-error-context*)
               sb-c::*compiler-error-context*
               (typep sb-c::*compiler-error-context* 'sb-c::compiler-error-context))
      (let ((error-context sb-c::*compiler-error-context*))
        (setf context (sb-c::compiler-error-context-context error-context))
        (setf enclosing-forms 
              (append (sb-c::compiler-error-context-enclosing-source error-context)
                      (sb-c::compiler-error-context-source error-context)))))
    
    ;; Extract current path if available
    (when (and (boundp 'sb-c::*current-path*)
               sb-c::*current-path*)
      (setf source-path sb-c::*current-path*))
    
    (values context source-path enclosing-forms)))

;;; Condition handlers

(defun capture-compiler-condition (condition severity)
  "Capture a compiler condition as a structured message."
  (when *capture-compilation-output*
    (let ((location (extract-source-location)))
      (multiple-value-bind (context source-path enclosing-forms)
          (extract-compiler-context)
        (let ((message (api:make-compilation-message
                        :severity severity
                        :text (princ-to-string condition)
                        :location location
                        :context context
                        :source-path source-path
                        :enclosing-forms enclosing-forms)))
          (push message *current-compilation-messages*))))))

(defun compiler-error-hook-handler (condition)
  "Handler for compiler errors."
  (capture-compiler-condition condition :error)
  ;; Let the error propagate
  nil)

(defun warning-hook-handler (condition)
  "Handler for warnings."
  (capture-compiler-condition condition :warning)
  ;; Muffle the warning since we've captured it
  (when *capture-compilation-output*
    (muffle-warning condition)))

(defun style-warning-hook-handler (condition)
  "Handler for style warnings."
  (capture-compiler-condition condition :style-warning)
  ;; Muffle the warning since we've captured it
  (when *capture-compilation-output*
    (muffle-warning condition)))

(defun compiler-note-hook-handler (condition)
  "Handler for compiler notes."
  (capture-compiler-condition condition :note)
  ;; Notes are typically already muffled
  nil)

;;; Statistics tracking

(defun update-compilation-statistics ()
  "Update compilation statistics counters."
  (when *current-compilation-statistics*
    (incf (api:compilation-statistics-forms-processed *current-compilation-statistics*))))

(defun track-function-compilation ()
  "Track that a function was compiled."
  (when *current-compilation-statistics*
    (incf (api:compilation-statistics-functions-compiled *current-compilation-statistics*))))

(defun track-macro-expansion ()
  "Track that a macro was expanded."
  (when *current-compilation-statistics*
    (incf (api:compilation-statistics-macros-expanded *current-compilation-statistics*))))

;;; Main capture macro

(defmacro with-compilation-capture ((&key (capture t)) &body body)
  "Execute body with compilation output capture enabled."
  `(let ((*capture-compilation-output* ,capture)
         (*current-compilation-messages* nil)
         (*current-compilation-statistics* (when ,capture
                                              (api:make-compilation-statistics)))
         (*form-counter* 0)
         (*function-counter* 0)
         (*macro-expansion-counter* 0))
     (if ,capture
         (handler-bind ((sb-c::compiler-error #'compiler-error-hook-handler)
                        (warning #'warning-hook-handler)
                        (style-warning #'style-warning-hook-handler))
           (prog1
               (progn ,@body)
             ;; Update final statistics
             (when *current-compilation-statistics*
               (setf (api:compilation-statistics-forms-processed *current-compilation-statistics*)
                     *form-counter*)
               (setf (api:compilation-statistics-functions-compiled *current-compilation-statistics*)
                     *function-counter*)
               (setf (api:compilation-statistics-macros-expanded *current-compilation-statistics*)
                     *macro-expansion-counter*))))
         (progn ,@body))))

;;; Hook installation utilities

(defun install-compilation-hooks ()
  "Install hooks for capturing compilation output."
  ;; This would hook into SBCL internals if needed
  ;; For now, we rely on handler-bind in with-compilation-capture
  nil)

(defun uninstall-compilation-hooks ()
  "Uninstall compilation capture hooks."
  ;; Cleanup if we installed any global hooks
  nil)
;;;; Enhanced Source Location Tracking for Compilation
;;;;
;;;; This module provides enhanced source location tracking that extracts
;;;; accurate line and column information from SBCL's compiler internals.

(defpackage epsilon.compile-location
  (:use cl)
  (:local-nicknames
   (api epsilon.compile-api)
   (map epsilon.map)
   (file-utils epsilon.file-utils))
  (:export
   #:track-source-location
   #:get-current-source-location
   #:enhance-location-with-line-info
   #:with-source-tracking))

(in-package epsilon.compile-location)

;;; File line caching is now handled by epsilon.file-utils

;;; Integration with SBCL compiler internals

(defun extract-sbcl-source-location ()
  "Extract source location from SBCL compiler state."
  (let ((file nil)
        (position nil)
        (toplevel-form nil)
        (form-number nil))
    
    ;; Get file from compiler state
    (when (and (boundp 'sb-c::*compile-file-pathname*)
               sb-c::*compile-file-pathname*)
      (setf file sb-c::*compile-file-pathname*))
    
    ;; Try to get position from compiler error context
    (when (and (boundp 'sb-c::*compiler-error-context*)
               sb-c::*compiler-error-context*)
      (typecase sb-c::*compiler-error-context*
        (sb-c::compiler-error-context
         (let ((context sb-c::*compiler-error-context*))
           (setf position (sb-c::compiler-error-context-file-position context))
           (setf toplevel-form (sb-c::compiler-error-context-original-form context))))))
    
    ;; Try to get form number from current path
    (when (and (boundp 'sb-c::*current-path*)
               sb-c::*current-path*)
      (let ((path sb-c::*current-path*))
        (when (consp path)
          (setf form-number (length path)))))
    
    (values file position toplevel-form form-number)))

(defun get-current-source-location ()
  "Get the current source location with line and column information."
  (multiple-value-bind (file position toplevel-form form-number)
      (extract-sbcl-source-location)
    
    (multiple-value-bind (line column)
        (when (and file position)
          (file-utils:file-position-to-line-column file position))
      
      (api:make-source-location
       :file (when file (namestring file))
       :line line
       :column column
       :form-number form-number
       :toplevel-form toplevel-form))))

(defun enhance-location-with-line-info (location)
  "Enhance a source location with line/column information if missing."
  (if (or (api:source-location-line location)
          (not (api:source-location-file location)))
      location
      ;; Try to extract line info from current compiler state
      (let ((current (get-current-source-location)))
        (if (and current
                 (equal (api:source-location-file current)
                        (api:source-location-file location)))
            (api:make-source-location
             :file (api:source-location-file location)
             :line (api:source-location-line current)
             :column (api:source-location-column current)
             :form-number (or (api:source-location-form-number location)
                             (api:source-location-form-number current))
             :toplevel-form (or (api:source-location-toplevel-form location)
                               (api:source-location-toplevel-form current)))
            location))))

;;; Source tracking context manager

(defvar *source-tracking-enabled* nil
  "When true, source location tracking is active.")

(defvar *current-tracked-location* nil
  "The currently tracked source location.")

(defmacro with-source-tracking ((&key (enable t)) &body body)
  "Execute body with source location tracking enabled."
  `(let ((*source-tracking-enabled* ,enable)
         (*current-tracked-location* nil))
     ,@body))

(defun track-source-location (form)
  "Track the source location of a form being compiled."
  (declare (ignore form))
  (when *source-tracking-enabled*
    (setf *current-tracked-location* (get-current-source-location))))

;;; Hook into SBCL's source path tracking

(defun install-source-tracking-hooks ()
  "Install hooks for enhanced source location tracking."
  ;; This would require deeper integration with SBCL internals
  ;; For now, we rely on extracting information when needed
  nil)

;;; Utility functions for working with source locations

(defun format-line-column (line column)
  "Format line and column for display."
  (format nil "~D:~D" line column))

(defun source-location-string (location)
  "Convert a source location to a string representation."
  (when location
    (format nil "~@[~A~]~@[:~A~]"
            (api:source-location-file location)
            (when (and (api:source-location-line location)
                       (api:source-location-column location))
              (format-line-column
               (api:source-location-line location)
               (api:source-location-column location))))))
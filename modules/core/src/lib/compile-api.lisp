;;;; Structured Compilation API
;;;;
;;;; This module provides a structured interface to the SBCL compiler that
;;;; captures compilation results as data structures rather than text output.

(defpackage epsilon.compile-api
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (str epsilon.string)
   (seq epsilon.sequence))
  (:export
   ;; Result structures
   #:compilation-result
   #:make-compilation-result
   #:compilation-result-p
   #:compilation-result-file
   #:compilation-result-output-file
   #:compilation-result-messages
   #:compilation-result-statistics
   #:compilation-result-success-p
   #:compilation-result-warnings-p
   #:compilation-result-failure-p
   #:compilation-result-duration
   
   ;; Message structures
   #:compilation-message
   #:make-compilation-message
   #:compilation-message-p
   #:compilation-message-severity
   #:compilation-message-text
   #:compilation-message-location
   #:compilation-message-context
   #:compilation-message-source-path
   #:compilation-message-enclosing-forms
   
   ;; Location structures
   #:source-location
   #:make-source-location
   #:source-location-p
   #:source-location-file
   #:source-location-line
   #:source-location-column
   #:source-location-form-number
   #:source-location-toplevel-form
   
   ;; Statistics
   #:compilation-statistics
   #:make-compilation-statistics
   #:compilation-statistics-p
   #:compilation-statistics-forms-processed
   #:compilation-statistics-functions-compiled
   #:compilation-statistics-macros-expanded
   #:compilation-statistics-bytes-consed
   #:compilation-statistics-cpu-time
   #:compilation-statistics-real-time
   
   ;; Severity levels
   #:severity-level
   #:error-severity
   #:warning-severity
   #:style-warning-severity
   #:note-severity
   #:info-severity
   
   ;; Utility functions
   #:count-messages-by-severity
   #:get-errors
   #:get-warnings
   #:format-source-location
   #:format-compilation-message
   #:summarize-compilation-result))

(in-package epsilon.compile-api)

;;; Severity levels for compilation messages
(deftype severity-level ()
  '(member :error :warning :style-warning :note :info))

(defconstant +error-severity+ :error)
(defconstant +warning-severity+ :warning)
(defconstant +style-warning-severity+ :style-warning)
(defconstant +note-severity+ :note)
(defconstant +info-severity+ :info)

;;; Source location information
(defstruct (source-location
            (:constructor %make-source-location)
            (:copier nil))
  "Represents a location in source code."
  (file nil :type (or string pathname null))
  (line nil :type (or fixnum null))
  (column nil :type (or fixnum null))
  (form-number nil :type (or fixnum null))
  (toplevel-form nil :type t))

(defun make-source-location (&key file line column form-number toplevel-form)
  "Create a source location structure."
  (%make-source-location
   :file file
   :line line
   :column column
   :form-number form-number
   :toplevel-form toplevel-form))

;;; Compilation message structure
(defstruct (compilation-message
            (:constructor %make-compilation-message)
            (:copier nil))
  "Represents a single compilation diagnostic message."
  (severity :info :type severity-level)
  (text "" :type string)
  (location nil :type (or source-location null))
  (context nil :type list)
  (source-path nil :type list)
  (enclosing-forms nil :type list))

(defun make-compilation-message (&key (severity :info) text location context source-path enclosing-forms)
  "Create a compilation message structure."
  (%make-compilation-message
   :severity severity
   :text (or text "")
   :location location
   :context context
   :source-path source-path
   :enclosing-forms enclosing-forms))

;;; Compilation statistics
(defstruct (compilation-statistics
            (:constructor %make-compilation-statistics)
            (:copier nil))
  "Statistics about a compilation run."
  (forms-processed 0 :type fixnum)
  (functions-compiled 0 :type fixnum)
  (macros-expanded 0 :type fixnum)
  (bytes-consed 0 :type unsigned-byte)
  (cpu-time 0.0 :type single-float)
  (real-time 0.0 :type single-float))

(defun make-compilation-statistics (&key (forms-processed 0)
                                          (functions-compiled 0)
                                          (macros-expanded 0)
                                          (bytes-consed 0)
                                          (cpu-time 0.0)
                                          (real-time 0.0))
  "Create a compilation statistics structure."
  (%make-compilation-statistics
   :forms-processed forms-processed
   :functions-compiled functions-compiled
   :macros-expanded macros-expanded
   :bytes-consed bytes-consed
   :cpu-time cpu-time
   :real-time real-time))

;;; Main compilation result structure
(defstruct (compilation-result
            (:constructor %make-compilation-result)
            (:copier nil))
  "Complete result of a file compilation."
  (file nil :type (or string pathname null))
  (output-file nil :type (or string pathname null))
  (messages nil :type list)
  (statistics nil :type (or compilation-statistics null))
  (success-p t :type boolean)
  (warnings-p nil :type boolean)
  (failure-p nil :type boolean)
  (duration 0.0 :type single-float))

(defun make-compilation-result (&key file output-file messages statistics 
                                      (success-p t) warnings-p failure-p
                                      (duration 0.0))
  "Create a compilation result structure."
  (%make-compilation-result
   :file file
   :output-file output-file
   :messages (or messages nil)
   :statistics (or statistics (make-compilation-statistics))
   :success-p success-p
   :warnings-p warnings-p
   :failure-p failure-p
   :duration duration))

;;; Utility functions for working with compilation results

(defun count-messages-by-severity (result severity)
  "Count messages of a specific severity in a compilation result."
  (count severity (compilation-result-messages result)
         :key #'compilation-message-severity))

(defun get-errors (result)
  "Get all error messages from a compilation result."
  (remove-if-not (lambda (msg)
                   (eq (compilation-message-severity msg) :error))
                 (compilation-result-messages result)))

(defun get-warnings (result)
  "Get all warning messages from a compilation result."
  (remove-if-not (lambda (msg)
                   (member (compilation-message-severity msg)
                           '(:warning :style-warning)))
                 (compilation-result-messages result)))

(defun format-source-location (location &optional (stream nil))
  "Format a source location for display."
  (when location
    (format stream "~@[~A~]~@[:~D~]~@[:~D~]"
            (source-location-file location)
            (source-location-line location)
            (source-location-column location))))

(defun format-compilation-message (message &optional (stream nil))
  "Format a compilation message for display."
  (format stream "~A: ~A~@[ at ~A~]"
          (compilation-message-severity message)
          (compilation-message-text message)
          (when (compilation-message-location message)
            (format-source-location (compilation-message-location message)))))

(defun summarize-compilation-result (result &optional (stream *standard-output*))
  "Print a summary of compilation results."
  (format stream "Compilation of ~A:~%" (compilation-result-file result))
  (format stream "  Success: ~A~%" (compilation-result-success-p result))
  (format stream "  Warnings: ~D~%" (count-messages-by-severity result :warning))
  (format stream "  Errors: ~D~%" (count-messages-by-severity result :error))
  (format stream "  Style warnings: ~D~%" (count-messages-by-severity result :style-warning))
  (format stream "  Notes: ~D~%" (count-messages-by-severity result :note))
  (when (compilation-result-statistics result)
    (let ((stats (compilation-result-statistics result)))
      (format stream "  Forms processed: ~D~%" (compilation-statistics-forms-processed stats))
      (format stream "  Functions compiled: ~D~%" (compilation-statistics-functions-compiled stats))
      (format stream "  Duration: ~,3F seconds~%" (compilation-result-duration result)))))
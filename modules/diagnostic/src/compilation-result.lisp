;;;; compilation-result.lisp - Canonical compilation result types
;;;;
;;;; Unified compilation-result, compilation-statistics, and
;;;; compilation-artifacts types used by both epsilon.compiler and
;;;; epsilon.compile-tracking.

(cl:defpackage :epsilon.compilation-result
  (:use :cl)
  (:local-nicknames
   (:diag :epsilon.diagnostic))
  (:export
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

   ;; Artifacts
   #:compilation-artifacts
   #:make-compilation-artifacts
   #:compilation-artifacts-p
   #:compilation-artifacts-fasl-file
   #:compilation-artifacts-source-map
   #:compilation-artifacts-coverage-data
   #:compilation-artifacts-profile-data
   #:compilation-artifacts-metadata

   ;; Result
   #:compilation-result
   #:make-compilation-result
   #:compilation-result-p
   #:compilation-result-success-p
   #:compilation-result-warnings-p
   #:compilation-result-errors-p
   #:compilation-result-diagnostics
   #:compilation-result-error-count
   #:compilation-result-warning-count
   #:compilation-result-artifacts
   #:compilation-result-statistics
   #:compilation-result-duration
   #:compilation-result-file
   #:compilation-result-output-file

   ;; Utilities
   #:count-diagnostics-by-severity
   #:get-errors
   #:get-warnings
   #:summarize-compilation-result))

(in-package :epsilon.compilation-result)

;;; ====================================================================
;;; Compilation Statistics
;;; ====================================================================

(defstruct (compilation-statistics
            (:constructor %make-compilation-statistics)
            (:copier nil))
  "Statistics about a compilation run."
  (forms-processed    0   :type fixnum)
  (functions-compiled 0   :type fixnum)
  (macros-expanded    0   :type fixnum)
  (bytes-consed       0   :type unsigned-byte)
  (cpu-time           0.0 :type single-float)
  (real-time          0.0 :type single-float))

(defun make-compilation-statistics (&key (forms-processed 0)
                                         (functions-compiled 0)
                                         (macros-expanded 0)
                                         (bytes-consed 0)
                                         (cpu-time 0.0)
                                         (real-time 0.0))
  "Create compilation statistics."
  (%make-compilation-statistics
   :forms-processed forms-processed
   :functions-compiled functions-compiled
   :macros-expanded macros-expanded
   :bytes-consed bytes-consed
   :cpu-time cpu-time
   :real-time real-time))

;;; ====================================================================
;;; Compilation Artifacts
;;; ====================================================================

(defstruct (compilation-artifacts
            (:constructor %make-compilation-artifacts)
            (:copier nil))
  "Output artifacts from compilation."
  (fasl-file     nil :type (or pathname null))
  (source-map    nil :type (or pathname null))
  (coverage-data nil :type t)
  (profile-data  nil :type t)
  (metadata      nil :type list))

(defun make-compilation-artifacts (&key fasl-file source-map coverage-data
                                        profile-data metadata)
  "Create compilation artifacts."
  (%make-compilation-artifacts
   :fasl-file fasl-file
   :source-map source-map
   :coverage-data coverage-data
   :profile-data profile-data
   :metadata metadata))

;;; ====================================================================
;;; Compilation Result
;;; ====================================================================

(defstruct (compilation-result
            (:constructor %make-compilation-result)
            (:copier nil))
  "Complete result of compiling one or more files/forms.

   Merges fields from the previous epsilon.compiler and
   epsilon.compile-tracking result types."
  ;; Status
  (success-p    nil :type boolean)
  (warnings-p   nil :type boolean)
  (errors-p     nil :type boolean)
  ;; Diagnostics (list of epsilon.diagnostic:diagnostic)
  (diagnostics  nil :type list)
  (error-count  0   :type (integer 0))
  (warning-count 0  :type (integer 0))
  ;; Artifacts
  (artifacts    nil :type (or compilation-artifacts null))
  ;; Statistics
  (statistics   nil :type (or compilation-statistics null))
  ;; Timing
  (duration     0.0 :type single-float)
  ;; Source
  (file         nil :type (or pathname string null))
  (output-file  nil :type (or pathname string null)))

(defun make-compilation-result (&key success-p warnings-p errors-p
                                     diagnostics error-count warning-count
                                     artifacts statistics (duration 0.0)
                                     file output-file)
  "Create a compilation result."
  (%make-compilation-result
   :success-p success-p
   :warnings-p warnings-p
   :errors-p errors-p
   :diagnostics diagnostics
   :error-count (or error-count 0)
   :warning-count (or warning-count 0)
   :artifacts artifacts
   :statistics (or statistics (make-compilation-statistics))
   :duration duration
   :file file
   :output-file output-file))

;;; ====================================================================
;;; Utilities
;;; ====================================================================

(defun count-diagnostics-by-severity (result severity)
  "Count diagnostics of a specific severity in a compilation result."
  (count severity (compilation-result-diagnostics result)
         :key #'diag:diagnostic-severity))

(defun get-errors (result)
  "Get all error diagnostics from a compilation result."
  (remove-if-not (lambda (d) (eq (diag:diagnostic-severity d) :error))
                 (compilation-result-diagnostics result)))

(defun get-warnings (result)
  "Get all warning diagnostics from a compilation result."
  (remove-if-not (lambda (d) (member (diag:diagnostic-severity d)
                                     '(:warning :style-warning)))
                 (compilation-result-diagnostics result)))

(defun summarize-compilation-result (result &optional (stream *standard-output*))
  "Print a summary of compilation results."
  (format stream "Compilation of ~A:~%" (compilation-result-file result))
  (format stream "  Success: ~A~%" (compilation-result-success-p result))
  (format stream "  Errors: ~D~%" (compilation-result-error-count result))
  (format stream "  Warnings: ~D~%" (compilation-result-warning-count result))
  (when (compilation-result-statistics result)
    (let ((stats (compilation-result-statistics result)))
      (format stream "  Forms: ~D~%" (compilation-statistics-forms-processed stats))
      (format stream "  Functions: ~D~%" (compilation-statistics-functions-compiled stats))
      (format stream "  Duration: ~,3Fs~%" (compilation-result-duration result)))))

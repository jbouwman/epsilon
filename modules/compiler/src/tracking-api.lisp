;;;; Structured Compilation API
;;;;
;;;; Re-exports core types (source-location, diagnostic, compilation-result)
;;;; and provides compile-tracking-specific utilities.

(defpackage epsilon.compile-api
  (:use cl)
  (:import (epsilon.source-location loc)
           (epsilon.diagnostic diag)
           (epsilon.compilation-result cr))
  (:export
    ;; Re-exported source-location (from core)
    #:source-location-p
    #:source-location-file
    #:source-location-line
    #:source-location-column
    #:format-source-location
    ;; Re-exported compilation-result (from core)
    #:compilation-result-p
    #:compilation-result-file
    #:compilation-result-output-file
    #:compilation-result-diagnostics
    #:compilation-result-statistics
    #:compilation-result-success-p
    #:compilation-result-warnings-p
    #:compilation-result-errors-p
    #:compilation-result-duration
    ;; Re-exported compilation-statistics (from core)
    #:compilation-statistics-p
    #:compilation-statistics-forms-processed
    #:compilation-statistics-functions-compiled
    #:compilation-statistics-macros-expanded
    #:compilation-statistics-bytes-consed
    #:compilation-statistics-cpu-time
    #:compilation-statistics-real-time
    ;; Re-exported diagnostic severity
    #:severity-level
    ;; Utility functions
    #:count-diagnostics-by-severity
    #:get-errors
    #:get-warnings
    #:summarize-compilation-result))

(in-package epsilon.compile-api)

;;; Severity type
(deftype severity-level ()
  '(member :error :warning :style-warning :note :info))

;;; Re-export core symbols by aliasing
;; Source location
(setf (symbol-function 'source-location-p) #'loc:source-location-p)
(setf (symbol-function 'source-location-file) #'loc:source-location-file)
(setf (symbol-function 'source-location-line) #'loc:source-location-line)
(setf (symbol-function 'source-location-column) #'loc:source-location-column)
(setf (symbol-function 'format-source-location) #'loc:format-source-location)

;; Compilation result
(setf (symbol-function 'compilation-result-p) #'cr:compilation-result-p)
(setf (symbol-function 'compilation-result-file) #'cr:compilation-result-file)
(setf (symbol-function 'compilation-result-output-file) #'cr:compilation-result-output-file)
(setf (symbol-function 'compilation-result-diagnostics) #'cr:compilation-result-diagnostics)
(setf (symbol-function 'compilation-result-statistics) #'cr:compilation-result-statistics)
(setf (symbol-function 'compilation-result-success-p) #'cr:compilation-result-success-p)
(setf (symbol-function 'compilation-result-warnings-p) #'cr:compilation-result-warnings-p)
(setf (symbol-function 'compilation-result-errors-p) #'cr:compilation-result-errors-p)
(setf (symbol-function 'compilation-result-duration) #'cr:compilation-result-duration)

;; Compilation statistics
(setf (symbol-function 'compilation-statistics-p) #'cr:compilation-statistics-p)
(setf (symbol-function 'compilation-statistics-forms-processed) #'cr:compilation-statistics-forms-processed)
(setf (symbol-function 'compilation-statistics-functions-compiled) #'cr:compilation-statistics-functions-compiled)
(setf (symbol-function 'compilation-statistics-macros-expanded) #'cr:compilation-statistics-macros-expanded)
(setf (symbol-function 'compilation-statistics-bytes-consed) #'cr:compilation-statistics-bytes-consed)
(setf (symbol-function 'compilation-statistics-cpu-time) #'cr:compilation-statistics-cpu-time)
(setf (symbol-function 'compilation-statistics-real-time) #'cr:compilation-statistics-real-time)

;;; Utility functions (delegate to core)
(setf (symbol-function 'count-diagnostics-by-severity) #'cr:count-diagnostics-by-severity)
(setf (symbol-function 'get-errors) #'cr:get-errors)
(setf (symbol-function 'get-warnings) #'cr:get-warnings)
(setf (symbol-function 'summarize-compilation-result) #'cr:summarize-compilation-result)

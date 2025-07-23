;;;; Hot Reload Stub
;;;;
;;;; Temporary stub for hot-reload functionality to allow the system to compile
;;;; while the real hot-reload implementation is being fixed.

(defpackage #:epsilon.tool.hot-reload
  (:use #:cl)
  (:export
   ;; Main API
   #:start-hot-reload
   #:stop-hot-reload
   #:reload-changed
   #:force-reload
   #:get-reload-statistics
   #:reload-session-active-p
   #:clear-reload-cache
   
   ;; Statistics structure
   #:reload-statistics
   #:reload-statistics-files-watched
   #:reload-statistics-changes-detected
   #:reload-statistics-reloads-performed
   #:reload-statistics-files-recompiled
   #:reload-statistics-files-fast-loaded
   #:reload-statistics-tests-rerun
   #:reload-statistics-total-reload-time
   #:reload-statistics-last-reload-time))

(in-package #:epsilon.tool.hot-reload)

;; Statistics structure
(defstruct reload-statistics
  (files-watched 0)
  (changes-detected 0)
  (reloads-performed 0)
  (files-recompiled 0)
  (files-fast-loaded 0)
  (tests-rerun 0)
  (total-reload-time 0.0)
  (last-reload-time 0.0))

;; Global state
(defparameter *dummy-stats* (make-reload-statistics))
(defparameter *session-active* nil)

;; Stub implementations
(defun start-hot-reload (&key directories files auto-reload reload-tests)
  "Stub implementation - does nothing"
  (declare (ignore directories files auto-reload reload-tests))
  (setf *session-active* t)
  t)

(defun stop-hot-reload ()
  "Stub implementation - does nothing"
  (setf *session-active* nil)
  nil)

(defun reload-changed ()
  "Stub implementation - does nothing"
  nil)

(defun force-reload ()
  "Stub implementation - does nothing"
  nil)

(defun get-reload-statistics ()
  "Return dummy statistics"
  *dummy-stats*)

(defun reload-session-active-p ()
  "Check if session is active"
  *session-active*)

(defun clear-reload-cache ()
  "Stub implementation - does nothing"
  nil)
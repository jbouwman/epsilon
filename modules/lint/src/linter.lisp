;;;; Main linter engine for Epsilon
;;;;
;;;; This module provides the main linting functionality and integrates
;;;; with the Epsilon build system.

(defpackage :epsilon.lint
  (:use :cl)
  (:local-nicknames
   (#:str #:epsilon.string)
   (#:seq #:epsilon.sequence)
   (#:path #:epsilon.path)
   (#:rules #:epsilon.lint.rules)
   (#:fs #:epsilon.sys.fs)
   (#:loader #:epsilon.loader))
  (:export
   #:lint-package
   #:lint-files
   #:format-issues))

(in-package :epsilon.lint)

;;; Package file discovery

(defun find-package-files (package-name build-environment)
  "Find all Lisp source files for a package using build environment"
  (error "fixme"))

;;; Issue formatting

(defun severity-symbol (severity)
  "Convert severity keyword to display symbol"
  (case severity
    (:error "error")
    (:warning "warning") 
    (:info "info")
    (:hint "hint")
    (otherwise "unknown")))

(defun format-issue (issue)
  "Format a single lint issue for display"
  (format nil "~A:~D:~D: ~A: ~A"
          (rules:issue-filename issue)
          (rules:issue-line issue)
          (rules:issue-column issue)
          (severity-symbol (rules:issue-severity issue))
          (rules:issue-message issue)))

(defun format-issues (issues)
  "Format list of issues for display"
  (if issues
      (format nil "~{~A~%~}" (mapcar #'format-issue issues))
      "No issues found."))

;;; Statistics and summary

(defun count-issues-by-severity (issues)
  "Count issues grouped by severity"
  (let ((counts (list :error 0 :warning 0 :info 0 :hint 0)))
    (dolist (issue issues)
      (let ((severity (rules:issue-severity issue)))
        (case severity
          (:error (incf (getf counts :error)))
          (:warning (incf (getf counts :warning)))
          (:info (incf (getf counts :info)))
          (:hint (incf (getf counts :hint))))))
    counts))

(defun format-summary (issues file-count)
  "Format summary statistics"
  (let ((counts (count-issues-by-severity issues))
        (total (length issues)))
    (format nil "~%Checked ~D file~:P, found ~D issue~:P (~D error~:P, ~D warning~:P, ~D info, ~D hint~:P)"
            file-count total
            (getf counts :error)
            (getf counts :warning)
            (getf counts :info)
            (getf counts :hint))))

;;; Main linting functions

(defun lint-files (filenames)
  "Lint a list of files and return all issues"
  (let ((all-issues '()))
    (dolist (filename filenames)
      (when (and (stringp filename) (fs:exists-p filename))
        (format t "Linting ~A...~%" filename)
        (let ((issues (rules:lint-file filename)))
          (setf all-issues (append issues all-issues)))))
    all-issues))

(defun lint-package (package-name build-environment)
  "Lint all files in a package"
  (format t "Linting package ~A...~%" package-name)
  (let* ((files (find-package-files package-name build-environment))
         (issues (lint-files files)))
    
    ;; Display results
    (format t "~A" (format-issues issues))
    (format t "~A~%" (format-summary issues (length files)))
    
    ;; Return issues for further processing
    issues))

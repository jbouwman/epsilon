;;; epsilon-xref.el --- Xref backend for Epsilon -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; Author: Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides Xref integration for Epsilon buffers.  It enables
;; go-to-definition (M-.), find-references (M-?), and apropos functionality
;; through the ELS protocol.

;;; Code:

(require 'xref)
(require 'cl-lib)
(require 'epsilon-client)

;;; Setup

(defun epsilon-xref-setup ()
  "Set up Xref for Epsilon buffers."
  (add-hook 'xref-backend-functions #'epsilon-xref-backend nil t))

(defun epsilon-xref-teardown ()
  "Tear down Xref for Epsilon buffers."
  (remove-hook 'xref-backend-functions #'epsilon-xref-backend t))

;;; Xref Backend

(defun epsilon-xref-backend ()
  "Return the Epsilon Xref backend."
  'epsilon)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql epsilon)))
  "Return identifier at point for Epsilon."
  (epsilon--symbol-at-point))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql epsilon)))
  "Return completion table for Epsilon identifiers."
  (epsilon-xref--completion-table))

(cl-defmethod xref-backend-definitions ((_backend (eql epsilon)) identifier)
  "Find definitions of IDENTIFIER."
  (epsilon-xref--find-definitions identifier))

(cl-defmethod xref-backend-references ((_backend (eql epsilon)) identifier)
  "Find references to IDENTIFIER."
  (epsilon-xref--find-references identifier))

(cl-defmethod xref-backend-apropos ((_backend (eql epsilon)) pattern)
  "Find symbols matching PATTERN."
  (epsilon-xref--apropos pattern))

;;; Implementation

(defun epsilon-xref--find-definitions (identifier)
  "Find definitions of IDENTIFIER via ELS."
  (condition-case nil
      (let ((response (epsilon-request-sync
                       "xref" "definitions"
                       `((symbol . ,identifier)
                         (file . ,(buffer-file-name))
                         (line . ,(line-number-at-pos))
                         (column . ,(current-column))
                         (package . ,(epsilon--current-package))))))
        (if (equal "ok" (alist-get 'status response))
            (epsilon-xref--convert-locations (alist-get 'definitions response))
          nil))
    (error nil)))

(defun epsilon-xref--find-references (identifier)
  "Find references to IDENTIFIER via ELS."
  (condition-case nil
      (let ((response (epsilon-request-sync
                       "xref" "references"
                       `((symbol . ,identifier)
                         (file . ,(buffer-file-name))
                         (line . ,(line-number-at-pos))
                         (column . ,(current-column))
                         (package . ,(epsilon--current-package))))))
        (if (equal "ok" (alist-get 'status response))
            (epsilon-xref--convert-locations (alist-get 'references response))
          nil))
    (error nil)))

(defun epsilon-xref--apropos (pattern)
  "Find symbols matching PATTERN via ELS."
  (condition-case nil
      (let ((response (epsilon-request-sync
                       "inspect" "apropos"
                       `((pattern . ,pattern)))))
        (if (equal "ok" (alist-get 'status response))
            (epsilon-xref--convert-apropos-matches
             (alist-get 'matches response))
          nil))
    (error nil)))

(defun epsilon-xref--completion-table ()
  "Return completion table for Epsilon symbols."
  (condition-case nil
      (let ((response (epsilon-request-sync
                       "complete" "symbols"
                       `((package . ,(epsilon--current-package))))))
        (if (equal "ok" (alist-get 'status response))
            (mapcar (lambda (s) (alist-get 'name s))
                    (alist-get 'symbols response))
          nil))
    (error nil)))

;;; Conversion Functions

(defun epsilon-xref--convert-locations (locations)
  "Convert ELS LOCATIONS to xref-item list."
  (when locations
    (mapcar #'epsilon-xref--convert-location locations)))

(defun epsilon-xref--convert-location (loc)
  "Convert single ELS location LOC to xref-item."
  (let* ((file (alist-get 'file loc))
         (line (or (alist-get 'line loc) 1))
         (column (or (alist-get 'column loc) 0))
         (name (or (alist-get 'name loc)
                   (alist-get 'symbol loc)
                   "definition"))
         (snippet (or (alist-get 'snippet loc)
                      (alist-get 'context loc)
                      name)))
    (xref-make snippet
               (xref-make-file-location file line column))))

(defun epsilon-xref--convert-apropos-matches (matches)
  "Convert ELS apropos MATCHES to xref-item list."
  (when matches
    (cl-remove-if
     #'null
     (mapcar
      (lambda (match)
        (let ((file (alist-get 'file match))
              (line (alist-get 'line match))
              (name (alist-get 'name match))
              (kind (alist-get 'kind match)))
          (when file
            (xref-make (format "%s [%s]" name (or kind "symbol"))
                       (xref-make-file-location file (or line 1) 0)))))
      matches))))

;;; Interactive Commands

(defun epsilon-find-definition ()
  "Find definition of symbol at point."
  (interactive)
  (xref-find-definitions (epsilon--symbol-at-point)))

(defun epsilon-find-references ()
  "Find references to symbol at point."
  (interactive)
  (xref-find-references (epsilon--symbol-at-point)))

(defun epsilon-xref-apropos (pattern)
  "Find symbols matching PATTERN."
  (interactive "sApropos pattern: ")
  (xref-find-apropos pattern))

;;; Peek Definition (without navigation)

(defun epsilon-peek-definition ()
  "Show definition of symbol at point in a popup."
  (interactive)
  (let ((identifier (epsilon--symbol-at-point)))
    (if identifier
        (let ((defs (epsilon-xref--find-definitions identifier)))
          (if defs
              (let ((first-def (car defs)))
                (epsilon-xref--show-peek first-def))
            (message "No definition found for: %s" identifier)))
      (message "No symbol at point"))))

(defun epsilon-xref--show-peek (xref-item)
  "Show XREF-ITEM in a peek popup."
  (let* ((location (xref-item-location xref-item))
         (file (xref-location-group location))
         (line (xref-file-location-line location)))
    (when (and file line)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- line))
          (let ((context-start (line-beginning-position -2))
                (context-end (line-end-position 3)))
            (message "%s:%d\n%s"
                     (file-name-nondirectory file)
                     line
                     (buffer-substring context-start context-end))))))))

;;; Source Location Tracking

(defvar epsilon-xref--source-locations (make-hash-table :test 'equal)
  "Cache of source locations for defined symbols.")

(defun epsilon-xref-record-definition (name file line column)
  "Record that NAME is defined at FILE:LINE:COLUMN."
  (puthash name
           `((file . ,file)
             (line . ,line)
             (column . ,column))
           epsilon-xref--source-locations))

(defun epsilon-xref-lookup-cached (name)
  "Look up cached location for NAME."
  (gethash name epsilon-xref--source-locations))

(provide 'epsilon-xref)

;;; epsilon-xref.el ends here

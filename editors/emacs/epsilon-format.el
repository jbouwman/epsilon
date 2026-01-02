;;; epsilon-format.el --- Code formatting for Epsilon -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; Author: Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides code formatting integration with the ELS server.
;; It sends code to the server for formatting and applies the result.

;;; Code:

(require 'epsilon-client)

;;; Customization

(defgroup epsilon-format nil
  "Epsilon code formatting settings."
  :group 'epsilon
  :prefix "epsilon-format-")

(defcustom epsilon-format-indent-width 2
  "Number of spaces per indentation level."
  :type 'integer
  :group 'epsilon-format)

(defcustom epsilon-format-max-width 80
  "Target maximum line width for formatting."
  :type 'integer
  :group 'epsilon-format)

(defcustom epsilon-format-on-save nil
  "When non-nil, format buffer on save."
  :type 'boolean
  :group 'epsilon-format)

;;; Formatting Functions

(defun epsilon-format-region (start end)
  "Format the region between START and END using ELS.
If connected to ELS, sends the code for server-side formatting.
Otherwise, falls back to local indentation."
  (interactive "r")
  (if (epsilon-connected-p)
      (epsilon--format-region-via-els start end)
    (epsilon--format-region-local start end)))

(defun epsilon-format-buffer ()
  "Format the entire buffer using ELS."
  (interactive)
  (epsilon-format-region (point-min) (point-max)))

(defun epsilon-format-defun ()
  "Format the current defun/top-level form using ELS."
  (interactive)
  (save-excursion
    (let ((start (progn (beginning-of-defun) (point)))
          (end (progn (end-of-defun) (point))))
      (epsilon-format-region start end))))

(defun epsilon-format-last-sexp ()
  "Format the sexp before point using ELS."
  (interactive)
  (let ((end (point))
        (start (save-excursion (backward-sexp) (point))))
    (epsilon-format-region start end)))

;;; ELS Integration

(defun epsilon--format-region-via-els (start end)
  "Format region between START and END via ELS server."
  (let* ((code (buffer-substring-no-properties start end))
         (options `((indent-width . ,epsilon-format-indent-width)
                    (max-width . ,epsilon-format-max-width))))
    (epsilon-request
     "format" "format"
     `((code . ,code)
       (options . ,options))
     (lambda (response)
       (if (equal "ok" (alist-get 'status response))
           (let ((formatted (alist-get 'formatted response)))
             (when formatted
               (epsilon--apply-formatting start end formatted)))
         (message "Formatting failed: %s"
                  (alist-get 'message (alist-get 'error response))))))))

(defun epsilon--apply-formatting (start end formatted)
  "Apply FORMATTED text to region between START and END.
Preserves point position relative to content when possible."
  (let* ((point-offset (- (point) start))
         (original-length (- end start))
         (new-length (length formatted)))
    ;; Only update if there's a change
    (unless (string= (buffer-substring-no-properties start end) formatted)
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (insert formatted))
      ;; Adjust point
      (let ((new-offset (min point-offset new-length)))
        (goto-char (+ start new-offset)))
      (message "Formatted %d characters" new-length))))

;;; Local Fallback

(defun epsilon--format-region-local (start end)
  "Format region between START and END using local indentation."
  (indent-region start end)
  (message "Formatted locally (ELS not connected)"))

;;; Format on Save

(defun epsilon-format--maybe-format-on-save ()
  "Format buffer on save if `epsilon-format-on-save' is enabled."
  (when epsilon-format-on-save
    (epsilon-format-buffer)))

;;; Synchronous Formatting

(defun epsilon-format-string (code)
  "Format CODE string synchronously via ELS.
Returns formatted string or original on error."
  (if (epsilon-connected-p)
      (let* ((options `((indent-width . ,epsilon-format-indent-width)
                        (max-width . ,epsilon-format-max-width)))
             (response (epsilon-request-sync
                        "format" "format"
                        `((code . ,code)
                          (options . ,options)))))
        (if (equal "ok" (alist-get 'status response))
            (or (alist-get 'formatted response) code)
          code))
    code))

;;; Setup

(defun epsilon-format-setup ()
  "Set up formatting for the current buffer."
  ;; Add format-on-save hook
  (add-hook 'before-save-hook #'epsilon-format--maybe-format-on-save nil t))

(provide 'epsilon-format)

;;; epsilon-format.el ends here

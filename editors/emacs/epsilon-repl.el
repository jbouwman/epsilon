;;; epsilon-repl.el --- Epsilon REPL buffer -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; Author: Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides a comint-based REPL for interactive Epsilon
;; evaluation.  It integrates with the ELS protocol for evaluation,
;; I/O multiplexing, and session management.

;;; Code:

(require 'comint)
(require 'epsilon-client)

;;; Customization

(defgroup epsilon-repl nil
  "Epsilon REPL settings."
  :group 'epsilon
  :prefix "epsilon-repl-")

(defcustom epsilon-repl-prompt "epsilon> "
  "Prompt string for the Epsilon REPL."
  :type 'string
  :group 'epsilon-repl)

(defcustom epsilon-repl-history-file
  (expand-file-name "epsilon-history" user-emacs-directory)
  "File to store REPL history."
  :type 'file
  :group 'epsilon-repl)

(defcustom epsilon-repl-history-size 500
  "Maximum number of history entries to save."
  :type 'integer
  :group 'epsilon-repl)

;;; Faces

(defface epsilon-repl-prompt-face
  '((t :inherit comint-highlight-prompt))
  "Face for Epsilon REPL prompt."
  :group 'epsilon-repl)

(defface epsilon-repl-output-face
  '((t :inherit default))
  "Face for Epsilon REPL stdout output."
  :group 'epsilon-repl)

(defface epsilon-repl-result-face
  '((t :inherit font-lock-constant-face))
  "Face for Epsilon REPL evaluation results."
  :group 'epsilon-repl)

(defface epsilon-repl-error-face
  '((t :inherit error))
  "Face for Epsilon REPL errors."
  :group 'epsilon-repl)

(defface epsilon-repl-warning-face
  '((t :inherit warning))
  "Face for Epsilon REPL warnings."
  :group 'epsilon-repl)

;;; Mode Definition

(defvar epsilon-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'epsilon-repl-interrupt)
    (define-key map (kbd "C-c C-z") #'epsilon-repl-switch-to-source)
    (define-key map (kbd "C-c C-p") #'epsilon-repl-set-package)
    (define-key map (kbd "C-c C-o") #'epsilon-repl-clear-output)
    (define-key map (kbd "C-c M-o") #'epsilon-repl-clear-buffer)
    (define-key map (kbd "M-.") #'xref-find-definitions)
    (define-key map (kbd "M-,") #'xref-go-back)
    (define-key map (kbd "TAB") #'completion-at-point)
    (define-key map (kbd "C-c C-d d") #'epsilon-describe-symbol)
    (define-key map (kbd "C-c C-d a") #'epsilon-apropos)
    map)
  "Keymap for `epsilon-repl-mode'.")

(defvar-local epsilon-repl--session-id nil
  "Session ID for this REPL buffer.")

(defvar-local epsilon-repl--current-package "epsilon.user"
  "Current package for this REPL buffer.")

(defvar-local epsilon-repl--pending-input nil
  "Pending input awaiting response.")

(defvar-local epsilon-repl--output-marker nil
  "Marker for output insertion point.")

(define-derived-mode epsilon-repl-mode comint-mode "Epsilon REPL"
  "Major mode for Epsilon REPL interaction.

Commands:
\\{epsilon-repl-mode-map}"
  :group 'epsilon-repl
  ;; Comint settings
  (setq-local comint-prompt-regexp (concat "^" (regexp-quote epsilon-repl-prompt)))
  (setq-local comint-input-sender #'epsilon-repl-input-sender)
  (setq-local comint-process-echoes nil)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-input-ring-file-name epsilon-repl-history-file)
  (setq-local comint-input-ring-size epsilon-repl-history-size)
  ;; Read history
  (comint-read-input-ring t)
  ;; Output marker
  (setq epsilon-repl--output-marker (make-marker))
  ;; Register I/O notification handlers
  (epsilon-repl--setup-io-handlers)
  ;; Syntax
  (set-syntax-table lisp-mode-syntax-table)
  ;; Font-lock
  (setq-local font-lock-defaults '(lisp-font-lock-keywords)))

(defun epsilon-repl--setup-io-handlers ()
  "Set up I/O notification handlers for the REPL."
  (epsilon-register-notification-handler
   "io" "stdout"
   (lambda (payload)
     (epsilon-repl-insert-output (alist-get 'text payload))))
  (epsilon-register-notification-handler
   "io" "stderr"
   (lambda (payload)
     (epsilon-repl-insert-error (alist-get 'text payload))))
  (epsilon-register-notification-handler
   "debug" "break"
   (lambda (payload)
     (epsilon-repl-insert-error
      (format "Debugger break: %s\n" (alist-get 'message payload))))))

;;; REPL Buffer Management

(defun epsilon-repl ()
  "Start or switch to Epsilon REPL."
  (interactive)
  (let ((buffer (get-buffer-create "*epsilon-repl*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'epsilon-repl-mode)
        (epsilon-repl-mode)
        (epsilon-repl--initialize)))
    (pop-to-buffer buffer)))

(defun epsilon-repl--initialize ()
  "Initialize the REPL buffer."
  (epsilon-ensure-connected)
  (setq epsilon-repl--session-id (epsilon-session-id))
  ;; Start a fake process to make comint happy
  (let ((proc (start-process "epsilon-repl" (current-buffer) nil)))
    (set-process-query-on-exit-flag proc nil))
  ;; Initial prompt
  (epsilon-repl-insert-prompt)
  (set-marker epsilon-repl--output-marker (point-max)))

(defun epsilon-repl-create-session ()
  "Create a new ELS session for this REPL."
  (let ((response (epsilon-request-sync
                   "control" "create-session"
                   `((name . ,(buffer-name))))))
    (when (equal "ok" (alist-get 'status response))
      (setq epsilon-repl--session-id (alist-get 'session-id response)))))

;;; Input Handling

(defun epsilon-repl-input-sender (_proc input)
  "Send INPUT to ELS for evaluation."
  (let ((trimmed (string-trim input)))
    (unless (string-empty-p trimmed)
      (setq epsilon-repl--pending-input trimmed)
      (set-marker epsilon-repl--output-marker (point-max))
      (epsilon-request
       "eval" "eval"
       `((code . ,trimmed)
         (package . ,epsilon-repl--current-package))
       #'epsilon-repl-handle-eval-response)
      ;; Save history
      (comint-add-to-input-history trimmed))))

(defun epsilon-repl-handle-eval-response (response)
  "Handle evaluation RESPONSE from ELS."
  (let ((status (alist-get 'status response)))
    (cond
     ((equal status "ok")
      (let ((values (alist-get 'values response)))
        (dolist (val values)
          (epsilon-repl-insert-result (alist-get 'printed val)))))
     ((equal status "error")
      (let ((error-info (alist-get 'error response)))
        (epsilon-repl-insert-error
         (format "Error: %s\n" (alist-get 'message error-info)))))
     (t
      (epsilon-repl-insert-error
       (format "Unknown response status: %s\n" status)))))
  (setq epsilon-repl--pending-input nil)
  (epsilon-repl-insert-prompt))

;;; Output Insertion

(defun epsilon-repl-insert-output (text)
  "Insert stdout TEXT in REPL buffer."
  (when-let ((buffer (get-buffer "*epsilon-repl*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char epsilon-repl--output-marker)
          (insert (propertize text 'face 'epsilon-repl-output-face
                              'rear-nonsticky t))
          (set-marker epsilon-repl--output-marker (point)))))))

(defun epsilon-repl-insert-result (text)
  "Insert evaluation result TEXT in REPL buffer."
  (when-let ((buffer (get-buffer "*epsilon-repl*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (concat "=> " text "\n")
                            'face 'epsilon-repl-result-face
                            'rear-nonsticky t))))))

(defun epsilon-repl-insert-error (text)
  "Insert error TEXT in REPL buffer."
  (when-let ((buffer (get-buffer "*epsilon-repl*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize text 'face 'epsilon-repl-error-face
                            'rear-nonsticky t))))))

(defun epsilon-repl-insert-warning (text)
  "Insert warning TEXT in REPL buffer."
  (when-let ((buffer (get-buffer "*epsilon-repl*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize text 'face 'epsilon-repl-warning-face
                            'rear-nonsticky t))))))

(defun epsilon-repl-insert-prompt ()
  "Insert REPL prompt."
  (when-let ((buffer (get-buffer "*epsilon-repl*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (let ((prompt (format "%s%s"
                              (if (not (equal epsilon-repl--current-package
                                              "epsilon.user"))
                                  (concat epsilon-repl--current-package "> ")
                                "")
                              epsilon-repl-prompt)))
          (insert (propertize prompt
                              'face 'epsilon-repl-prompt-face
                              'read-only t
                              'rear-nonsticky t
                              'front-sticky t)))
        (set-marker epsilon-repl--output-marker (point))))))

;;; Evaluation Commands

(defun epsilon-eval-last-sexp ()
  "Evaluate the sexp before point."
  (interactive)
  (let ((sexp (epsilon--sexp-before-point)))
    (epsilon-request
     "eval" "eval"
     `((code . ,sexp)
       (package . ,(epsilon--current-package)))
     (lambda (response)
       (if (equal "ok" (alist-get 'status response))
           (message "=> %s"
                    (mapconcat (lambda (v) (alist-get 'printed v))
                               (alist-get 'values response)
                               ", "))
         (message "Error: %s"
                  (alist-get 'message (alist-get 'error response))))))))

(defun epsilon-eval-region (start end)
  "Evaluate region from START to END."
  (interactive "r")
  (let ((code (buffer-substring-no-properties start end)))
    (epsilon-request
     "eval" "eval-region"
     `((code . ,code)
       (file . ,(buffer-file-name))
       (package . ,(epsilon--current-package))
       (start-line . ,(line-number-at-pos start))
       (start-column . ,(save-excursion
                          (goto-char start)
                          (current-column))))
     (lambda (response)
       (if (equal "ok" (alist-get 'status response))
           (let ((vals (mapconcat (lambda (v) (alist-get 'printed v))
                                  (alist-get 'values response) ", ")))
             (message "=> %s" vals)
             (epsilon-repl-insert-result vals))
         (let ((err (alist-get 'message (alist-get 'error response))))
           (message "Error: %s" err)
           (epsilon-repl-insert-error (concat "Error: " err "\n"))))))))

(defun epsilon-eval-buffer ()
  "Evaluate entire buffer."
  (interactive)
  (epsilon-eval-region (point-min) (point-max)))

(defun epsilon-eval-defun ()
  "Evaluate the top-level form at point."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (epsilon-eval-region (point) end))))

(defun epsilon-eval-and-print-last-sexp ()
  "Evaluate sexp before point and print result in buffer."
  (interactive)
  (let ((sexp (epsilon--sexp-before-point)))
    (epsilon-request
     "eval" "eval"
     `((code . ,sexp)
       (package . ,(epsilon--current-package)))
     (lambda (response)
       (if (equal "ok" (alist-get 'status response))
           (let ((result (mapconcat (lambda (v) (alist-get 'printed v))
                                    (alist-get 'values response) ", ")))
             (save-excursion
               (insert " ;; => " result)))
         (message "Error: %s"
                  (alist-get 'message (alist-get 'error response))))))))

;;; REPL Commands

(defun epsilon-repl-interrupt ()
  "Interrupt current evaluation."
  (interactive)
  (epsilon-request
   "control" "interrupt"
   nil
   (lambda (response)
     (if (equal "ok" (alist-get 'status response))
         (message "Interrupted")
       (message "Interrupt failed")))))

(defun epsilon-repl-switch-to-source ()
  "Switch to the most recent Epsilon source buffer."
  (interactive)
  (let ((source-buffer
         (cl-find-if (lambda (buf)
                       (with-current-buffer buf
                         (derived-mode-p 'epsilon-mode)))
                     (buffer-list))))
    (if source-buffer
        (pop-to-buffer source-buffer)
      (message "No Epsilon source buffer found"))))

(defun epsilon-repl-set-package (package)
  "Set the current PACKAGE for the REPL."
  (interactive
   (list (completing-read "Package: "
                          (epsilon-repl--list-packages)
                          nil nil nil nil
                          epsilon-repl--current-package)))
  (setq epsilon-repl--current-package package)
  (message "Package set to: %s" package))

(defun epsilon-repl--list-packages ()
  "Return list of available packages."
  (let ((response (epsilon-request-sync "module" "list-packages")))
    (when (equal "ok" (alist-get 'status response))
      (alist-get 'packages response))))

(defun epsilon-repl-clear-output ()
  "Clear output from last command."
  (interactive)
  (when-let ((buffer (get-buffer "*epsilon-repl*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char epsilon-repl--output-marker)
          (delete-region (point) (point-max)))))))

(defun epsilon-repl-clear-buffer ()
  "Clear entire REPL buffer."
  (interactive)
  (when-let ((buffer (get-buffer "*epsilon-repl*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (epsilon-repl-insert-prompt)))))

;;; Documentation Commands

(defun epsilon-describe-symbol (symbol)
  "Describe SYMBOL."
  (interactive
   (list (or (epsilon--symbol-at-point)
             (read-string "Symbol: "))))
  (epsilon-request
   "inspect" "describe"
   `((symbol . ,symbol)
     (package . ,(epsilon--current-package)))
   (lambda (response)
     (if (equal "ok" (alist-get 'status response))
         (with-help-window "*epsilon-help*"
           (princ (alist-get 'documentation response)))
       (message "No documentation for: %s" symbol)))))

(defun epsilon-apropos (pattern)
  "Show all symbols matching PATTERN."
  (interactive "sApropos pattern: ")
  (epsilon-request
   "inspect" "apropos"
   `((pattern . ,pattern))
   (lambda (response)
     (if (equal "ok" (alist-get 'status response))
         (let ((matches (alist-get 'matches response)))
           (if matches
               (with-help-window "*epsilon-apropos*"
                 (dolist (match matches)
                   (princ (format "%s: %s\n"
                                  (alist-get 'name match)
                                  (alist-get 'kind match)))))
             (message "No matches for: %s" pattern)))
       (message "Apropos failed")))))

;;; Macroexpansion

(defun epsilon-macroexpand ()
  "Macroexpand the form at point."
  (interactive)
  (let ((sexp (epsilon--sexp-before-point)))
    (epsilon-request
     "eval" "macroexpand"
     `((code . ,sexp)
       (package . ,(epsilon--current-package)))
     (lambda (response)
       (if (equal "ok" (alist-get 'status response))
           (with-help-window "*epsilon-macroexpand*"
             (let ((expanded (alist-get 'expansion response)))
               (princ expanded)
               (with-current-buffer "*epsilon-macroexpand*"
                 (lisp-mode))))
         (message "Macroexpand failed: %s"
                  (alist-get 'message (alist-get 'error response))))))))

(defun epsilon-macroexpand-all ()
  "Fully macroexpand the form at point."
  (interactive)
  (let ((sexp (epsilon--sexp-before-point)))
    (epsilon-request
     "eval" "macroexpand-all"
     `((code . ,sexp)
       (package . ,(epsilon--current-package)))
     (lambda (response)
       (if (equal "ok" (alist-get 'status response))
           (with-help-window "*epsilon-macroexpand*"
             (let ((expanded (alist-get 'expansion response)))
               (princ expanded)
               (with-current-buffer "*epsilon-macroexpand*"
                 (lisp-mode))))
         (message "Macroexpand-all failed: %s"
                  (alist-get 'message (alist-get 'error response))))))))

;;; Switch to REPL

(defun epsilon-switch-to-repl ()
  "Switch to REPL buffer, creating if needed."
  (interactive)
  (epsilon-repl))

(provide 'epsilon-repl)

;;; epsilon-repl.el ends here

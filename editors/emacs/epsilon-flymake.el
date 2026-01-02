;;; epsilon-flymake.el --- Flymake backend for Epsilon -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; Author: Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides Flymake integration for Epsilon buffers.  It supports
;; both request-based checking and push diagnostics from the ELS server.

;;; Code:

(require 'flymake)
(require 'epsilon-client)

;;; Customization

(defgroup epsilon-flymake nil
  "Epsilon Flymake settings."
  :group 'epsilon
  :prefix "epsilon-flymake-")

(defcustom epsilon-flymake-check-on-save t
  "Check buffer when saved."
  :type 'boolean
  :group 'epsilon-flymake)

(defcustom epsilon-flymake-check-on-change t
  "Check buffer on change (with debouncing)."
  :type 'boolean
  :group 'epsilon-flymake)

(defcustom epsilon-flymake-check-delay 0.5
  "Delay in seconds before checking after a change."
  :type 'number
  :group 'epsilon-flymake)

;;; State Variables

(defvar-local epsilon-flymake--report-fn nil
  "Flymake report function for current buffer.")

(defvar-local epsilon-flymake--check-timer nil
  "Timer for delayed checking.")

(defvar-local epsilon-flymake--pending-request nil
  "ID of pending check request.")

;;; Setup

(defun epsilon-flymake-setup ()
  "Set up Flymake for Epsilon buffers."
  ;; Add our checker to Flymake
  (add-hook 'flymake-diagnostic-functions #'epsilon-flymake-checker nil t)
  ;; Register for push diagnostics
  (epsilon-register-notification-handler
   "diagnostics" "publish-diagnostics"
   #'epsilon-flymake--handle-push-diagnostics)
  ;; Enable Flymake
  (flymake-mode 1)
  ;; Optional: check on save
  (when epsilon-flymake-check-on-save
    (add-hook 'after-save-hook #'epsilon-flymake--check-buffer nil t)))

(defun epsilon-flymake-teardown ()
  "Tear down Flymake for Epsilon buffers."
  (remove-hook 'flymake-diagnostic-functions #'epsilon-flymake-checker t)
  (remove-hook 'after-save-hook #'epsilon-flymake--check-buffer t)
  (epsilon-unregister-notification-handler "diagnostics" "publish-diagnostics")
  (when epsilon-flymake--check-timer
    (cancel-timer epsilon-flymake--check-timer)
    (setq epsilon-flymake--check-timer nil)))

;;; Flymake Checker

(defun epsilon-flymake-checker (report-fn &rest _args)
  "Flymake checker for Epsilon.
REPORT-FN is called with diagnostics."
  (setq epsilon-flymake--report-fn report-fn)
  ;; Cancel any pending request
  (when epsilon-flymake--pending-request
    (remhash epsilon-flymake--pending-request epsilon--pending-requests)
    (setq epsilon-flymake--pending-request nil))
  ;; Check if we're connected
  (unless (epsilon-ready-p)
    ;; Not connected - try to connect in background, report nothing for now
    (epsilon-ensure-connected)
    (funcall report-fn nil)
    (cl-return-from epsilon-flymake-checker nil))
  ;; Request diagnostics for current buffer
  (condition-case err
      (setq epsilon-flymake--pending-request
            (epsilon-request
             "diagnostics" "analyze"
             `((file . ,(buffer-file-name))
               (contents . ,(buffer-substring-no-properties
                             (point-min) (point-max))))
             #'epsilon-flymake--handle-response))
    (error
     ;; Report the error as a diagnostic
     (funcall report-fn
              (list (flymake-make-diagnostic
                     (current-buffer)
                     (point-min) (point-max)
                     :error
                     (format "Epsilon check failed: %s"
                             (error-message-string err))))))))

(defun epsilon-flymake--check-buffer ()
  "Trigger a Flymake check of the current buffer."
  (when (and (derived-mode-p 'epsilon-mode)
             (bound-and-true-p flymake-mode))
    (flymake-start)))

(defun epsilon-flymake--schedule-check ()
  "Schedule a delayed check of the current buffer."
  (when (and epsilon-flymake-check-on-change
             (derived-mode-p 'epsilon-mode)
             (bound-and-true-p flymake-mode))
    (when epsilon-flymake--check-timer
      (cancel-timer epsilon-flymake--check-timer))
    (setq epsilon-flymake--check-timer
          (run-with-timer epsilon-flymake-check-delay nil
                          #'epsilon-flymake--check-buffer))))

;;; Response Handling

(defun epsilon-flymake--handle-response (response)
  "Handle diagnostics RESPONSE from ELS."
  (setq epsilon-flymake--pending-request nil)
  (when epsilon-flymake--report-fn
    (cond
     ;; No response (connection lost or timeout)
     ((null response)
      (funcall epsilon-flymake--report-fn nil))
     ;; Success
     ((equal "ok" (alist-get 'status response))
      (let ((diagnostics (epsilon-flymake--convert-diagnostics
                          (alist-get 'diagnostics response))))
        (funcall epsilon-flymake--report-fn diagnostics)))
     ;; Error response
     (t
      (funcall epsilon-flymake--report-fn
               (list (flymake-make-diagnostic
                      (current-buffer)
                      (point-min) (point-max)
                      :error
                      (format "Check failed: %s"
                              (alist-get 'message
                                         (alist-get 'error response))))))))))

(defun epsilon-flymake--handle-push-diagnostics (payload)
  "Handle push diagnostics notification PAYLOAD."
  (let ((file (alist-get 'file payload)))
    (when-let ((buffer (find-buffer-visiting file)))
      (with-current-buffer buffer
        (when (and epsilon-flymake--report-fn
                   (bound-and-true-p flymake-mode))
          (let ((diagnostics (epsilon-flymake--convert-diagnostics
                              (alist-get 'diagnostics payload))))
            (funcall epsilon-flymake--report-fn diagnostics)))))))

;;; Diagnostic Conversion

(defun epsilon-flymake--convert-diagnostics (els-diagnostics)
  "Convert ELS-DIAGNOSTICS to Flymake format."
  (mapcar #'epsilon-flymake--convert-diagnostic els-diagnostics))

(defun epsilon-flymake--convert-diagnostic (diag)
  "Convert a single ELS diagnostic DIAG to Flymake format."
  (let* ((severity (alist-get 'severity diag))
         (type (epsilon-flymake--severity-to-type severity))
         (start-line (alist-get 'start-line diag))
         (start-col (or (alist-get 'start-column diag) 1))
         (end-line (or (alist-get 'end-line diag) start-line))
         (end-col (or (alist-get 'end-column diag) (1+ start-col)))
         (message (alist-get 'message diag))
         (code (alist-get 'code diag)))
    (flymake-make-diagnostic
     (current-buffer)
     (epsilon-flymake--line-col-to-pos start-line start-col)
     (epsilon-flymake--line-col-to-pos end-line end-col)
     type
     (if code
         (format "[%s] %s" code message)
       message))))

(defun epsilon-flymake--severity-to-type (severity)
  "Convert ELS SEVERITY string to Flymake type."
  (pcase severity
    ("error" :error)
    ("warning" :warning)
    ("note" :note)
    ("hint" :note)
    ("info" :note)
    (_ :note)))

(defun epsilon-flymake--line-col-to-pos (line col)
  "Convert LINE and COL to buffer position."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- (max 1 line)))
      (let ((line-end (line-end-position)))
        (forward-char (min (1- (max 1 col))
                           (- line-end (point)))))
      (point))))

;;; Interactive Commands

(defun epsilon-flymake-show-diagnostics ()
  "Show all diagnostics in current buffer."
  (interactive)
  (if (bound-and-true-p flymake-mode)
      (flymake-show-buffer-diagnostics)
    (message "Flymake is not enabled in this buffer")))

(defun epsilon-flymake-goto-next-error ()
  "Go to next Flymake error."
  (interactive)
  (flymake-goto-next-error))

(defun epsilon-flymake-goto-prev-error ()
  "Go to previous Flymake error."
  (interactive)
  (flymake-goto-prev-error))

;;; Diagnostic Display

(defun epsilon-flymake--format-diagnostic (diag)
  "Format DIAG for display in echo area."
  (let ((text (flymake-diagnostic-text diag))
        (type (flymake-diagnostic-type diag)))
    (format "%s: %s"
            (pcase type
              (:error "Error")
              (:warning "Warning")
              (:note "Note")
              (_ "Info"))
            text)))

(defun epsilon-flymake-show-diagnostic-at-point ()
  "Show diagnostic at point in echo area."
  (interactive)
  (let ((diags (flymake-diagnostics (point))))
    (if diags
        (message "%s" (mapconcat #'epsilon-flymake--format-diagnostic
                                 diags "\n"))
      (message "No diagnostics at point"))))

(provide 'epsilon-flymake)

;;; epsilon-flymake.el ends here

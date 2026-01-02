;;; epsilon-eldoc.el --- Eldoc backend for Epsilon -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; Author: Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides Eldoc integration for Epsilon buffers.  It displays
;; function signatures and documentation in the echo area as you type.

;;; Code:

(require 'eldoc)
(require 'epsilon-client)

;;; Customization

(defgroup epsilon-eldoc nil
  "Epsilon Eldoc settings."
  :group 'epsilon
  :prefix "epsilon-eldoc-")

(defcustom epsilon-eldoc-show-types t
  "Show type information in eldoc."
  :type 'boolean
  :group 'epsilon-eldoc)

(defcustom epsilon-eldoc-show-docstring nil
  "Show docstring excerpt in eldoc."
  :type 'boolean
  :group 'epsilon-eldoc)

;;; Faces

(defface epsilon-eldoc-function-face
  '((t :inherit font-lock-function-name-face))
  "Face for function names in eldoc."
  :group 'epsilon-eldoc)

(defface epsilon-eldoc-type-face
  '((t :inherit font-lock-type-face))
  "Face for types in eldoc."
  :group 'epsilon-eldoc)

(defface epsilon-eldoc-active-param-face
  '((t :inherit eldoc-highlight-function-argument))
  "Face for the active parameter in eldoc."
  :group 'epsilon-eldoc)

(defface epsilon-eldoc-keyword-face
  '((t :inherit font-lock-builtin-face))
  "Face for keyword arguments in eldoc."
  :group 'epsilon-eldoc)

;;; State Variables

(defvar-local epsilon-eldoc--last-result nil
  "Cache of last eldoc result.")

(defvar-local epsilon-eldoc--last-point nil
  "Point position of last eldoc request.")

(defvar-local epsilon-eldoc--pending-request nil
  "ID of pending eldoc request.")

;;; Setup

(defun epsilon-eldoc-setup ()
  "Set up Eldoc for Epsilon buffers."
  (setq-local eldoc-documentation-strategy #'eldoc-documentation-compose)
  (add-hook 'eldoc-documentation-functions #'epsilon-eldoc-function nil t)
  (eldoc-mode 1))

(defun epsilon-eldoc-teardown ()
  "Tear down Eldoc for Epsilon buffers."
  (remove-hook 'eldoc-documentation-functions #'epsilon-eldoc-function t))

;;; Eldoc Function

(defun epsilon-eldoc-function (callback &rest _)
  "Eldoc function for Epsilon.
CALLBACK is called with the documentation string."
  ;; Don't try if not connected - prevents blocking
  (when (epsilon-ready-p)
    ;; Check if we're in a function call
    (when-let ((context (epsilon-eldoc--get-context)))
      (let ((symbol (car context))
            (arg-index (cdr context)))
        ;; Check cache
        (if (and epsilon-eldoc--last-result
                 (equal epsilon-eldoc--last-point (point))
                 (equal (car epsilon-eldoc--last-result) symbol))
            ;; Use cached result
            (let ((doc (epsilon-eldoc--format-cached symbol arg-index)))
              (when doc
                (funcall callback doc)))
          ;; Make new request (async, won't block)
          (when epsilon-eldoc--pending-request
            (remhash epsilon-eldoc--pending-request epsilon--pending-requests))
          (setq epsilon-eldoc--pending-request
                (epsilon-request
                 "complete" "eldoc"
                 `((symbol . ,symbol)
                   (file . ,(buffer-file-name))
                   (line . ,(line-number-at-pos))
                   (column . ,(current-column))
                   (package . ,(epsilon--current-package)))
                 (lambda (response)
                   (setq epsilon-eldoc--pending-request nil)
                   (when (and response (equal "ok" (alist-get 'status response)))
                     (setq epsilon-eldoc--last-result (cons symbol response)
                           epsilon-eldoc--last-point (point))
                     (let ((doc (epsilon-eldoc--format-response
                                 response arg-index)))
                       (when doc
                         (funcall callback doc)))))))
          ;; Return cached value immediately if available
          (when epsilon-eldoc--last-result
            (let ((doc (epsilon-eldoc--format-cached symbol arg-index)))
              doc)))))))

;;; Context Detection

(defun epsilon-eldoc--get-context ()
  "Get the current function call context.
Returns (FUNCTION-NAME . ARG-INDEX) or nil."
  (save-excursion
    (let ((arg-index 0)
          (start-point (point)))
      ;; Find enclosing paren
      (when-let ((paren-pos (nth 1 (syntax-ppss))))
        (goto-char paren-pos)
        ;; Count arguments
        (save-excursion
          (forward-char 1)
          (while (< (point) start-point)
            (skip-chars-forward " \t\n")
            (when (< (point) start-point)
              (condition-case nil
                  (progn
                    (forward-sexp 1)
                    (when (<= (point) start-point)
                      (cl-incf arg-index)))
                (scan-error nil)))))
        ;; Get function name
        (forward-char 1)
        (when-let ((sym (epsilon--symbol-at-point)))
          (cons sym (1- arg-index)))))))

;;; Formatting

(defun epsilon-eldoc--format-response (response arg-index)
  "Format eldoc RESPONSE for display.
ARG-INDEX is the index of the current argument."
  (let* ((symbol (alist-get 'symbol response))
         (arglist (alist-get 'arglist response))
         (type (alist-get 'type response))
         (doc (alist-get 'documentation response)))
    (concat
     (propertize symbol 'face 'epsilon-eldoc-function-face)
     " "
     (epsilon-eldoc--format-arglist arglist arg-index)
     (when (and epsilon-eldoc-show-types type)
       (concat " -> " (propertize type 'face 'epsilon-eldoc-type-face)))
     (when (and epsilon-eldoc-show-docstring doc)
       (concat "\n" (epsilon-eldoc--truncate-docstring doc))))))

(defun epsilon-eldoc--format-cached (symbol arg-index)
  "Format cached result for SYMBOL with ARG-INDEX."
  (when (and epsilon-eldoc--last-result
             (equal (car epsilon-eldoc--last-result) symbol))
    (epsilon-eldoc--format-response
     (cdr epsilon-eldoc--last-result) arg-index)))

(defun epsilon-eldoc--format-arglist (arglist arg-index)
  "Format ARGLIST with ARG-INDEX highlighted."
  (if (null arglist)
      "()"
    (concat
     "("
     (mapconcat
      (lambda (arg)
        (let* ((idx (cl-position arg arglist :test #'equal))
               (is-keyword (and (stringp arg) (string-prefix-p "&" arg)))
               (is-active (and (not is-keyword) (eql idx arg-index))))
          (cond
           (is-active
            (propertize arg 'face 'epsilon-eldoc-active-param-face))
           (is-keyword
            (propertize arg 'face 'epsilon-eldoc-keyword-face))
           (t arg))))
      arglist " ")
     ")")))

(defun epsilon-eldoc--truncate-docstring (doc)
  "Truncate DOC to a single line for display."
  (let ((first-line (car (split-string doc "\n" t))))
    (if (> (length first-line) 80)
        (concat (substring first-line 0 77) "...")
      first-line)))

;;; Interactive Commands

(defun epsilon-eldoc-refresh ()
  "Force refresh of eldoc display."
  (interactive)
  (setq epsilon-eldoc--last-result nil
        epsilon-eldoc--last-point nil)
  (eldoc-print-current-symbol-info))

(provide 'epsilon-eldoc)

;;; epsilon-eldoc.el ends here

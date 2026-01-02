;;; epsilon-complete.el --- Completion backend for Epsilon -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; Author: Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides completion integration for Epsilon buffers.
;; It supports both Company mode and the built-in completion-at-point
;; mechanism (used by Corfu, Vertico, etc.).

;;; Code:

(require 'epsilon-client)

;;; Customization

(defgroup epsilon-complete nil
  "Epsilon completion settings."
  :group 'epsilon
  :prefix "epsilon-complete-")

(defcustom epsilon-complete-annotation-style 'short
  "Style for completion annotations.
`short' shows just the type, `full' shows type and module."
  :type '(choice (const :tag "Short (type only)" short)
                 (const :tag "Full (type and module)" full))
  :group 'epsilon-complete)

(defcustom epsilon-complete-sort-by-relevance t
  "Sort completions by relevance rather than alphabetically."
  :type 'boolean
  :group 'epsilon-complete)

;;; State Variables

(defvar-local epsilon-complete--cached-completions nil
  "Cached completion candidates.")

(defvar-local epsilon-complete--cached-prefix nil
  "Prefix used for cached completions.")

;;; Setup

(defun epsilon-complete-setup ()
  "Set up completion for Epsilon buffers."
  (add-hook 'completion-at-point-functions
            #'epsilon-complete-at-point nil t))

(defun epsilon-complete-teardown ()
  "Tear down completion for Epsilon buffers."
  (remove-hook 'completion-at-point-functions
               #'epsilon-complete-at-point t))

;;; Completion at Point

(defun epsilon-complete-at-point ()
  "Completion-at-point function for Epsilon."
  (when (derived-mode-p 'epsilon-mode 'epsilon-repl-mode)
    (when-let ((bounds (epsilon-complete--bounds)))
      (let ((start (car bounds))
            (end (cdr bounds)))
        (list start end
              (completion-table-dynamic
               #'epsilon-complete--candidates)
              :annotation-function #'epsilon-complete--annotation
              :company-docsig #'epsilon-complete--signature
              :company-doc-buffer #'epsilon-complete--doc-buffer
              :company-location #'epsilon-complete--location
              :exit-function #'epsilon-complete--exit-function)))))

(defun epsilon-complete--bounds ()
  "Return bounds of the symbol at point as (START . END)."
  (let ((sym (bounds-of-thing-at-point 'symbol)))
    (if sym
        sym
      ;; No symbol at point, use point as both start and end
      (cons (point) (point)))))

;;; Candidate Retrieval

(defun epsilon-complete--candidates (prefix)
  "Get completion candidates for PREFIX."
  ;; Check cache
  (if (and epsilon-complete--cached-completions
           epsilon-complete--cached-prefix
           (string-prefix-p epsilon-complete--cached-prefix prefix))
      ;; Filter cached results
      (cl-remove-if-not
       (lambda (c) (string-prefix-p prefix c))
       epsilon-complete--cached-completions)
    ;; Fetch new completions (use fast timeout to avoid blocking)
    (when (epsilon-ready-p)
      (condition-case nil
          (let ((response (epsilon-request-sync-fast
                           "complete" "complete"
                           `((prefix . ,prefix)
                             (package . ,(epsilon--current-package))
                             (file . ,(buffer-file-name))
                             (line . ,(line-number-at-pos))
                             (column . ,(current-column))
                             (context . ,(epsilon-complete--context))))))
            (when (and response (equal "ok" (alist-get 'status response)))
              (let ((completions (alist-get 'completions response)))
                ;; Cache results
                (setq epsilon-complete--cached-prefix prefix
                      epsilon-complete--cached-completions
                      (mapcar (lambda (c)
                                (let ((candidate (alist-get 'candidate c)))
                                  ;; Store metadata as text properties
                                  (propertize candidate
                                              'epsilon-meta c)))
                              completions))
                epsilon-complete--cached-completions)))
        (error nil)))))

(defun epsilon-complete--context ()
  "Get completion context (e.g., :function, :variable, :type)."
  (save-excursion
    (let ((ppss (syntax-ppss)))
      (cond
       ;; Inside string
       ((nth 3 ppss) "string")
       ;; Inside comment
       ((nth 4 ppss) "comment")
       ;; At function position (first element of list)
       ((and (nth 1 ppss)
             (save-excursion
               (goto-char (nth 1 ppss))
               (forward-char 1)
               (skip-chars-forward " \t\n")
               (= (point) (car (epsilon-complete--bounds)))))
        "function")
       ;; Default
       (t "symbol")))))

;;; Metadata Accessors

(defun epsilon-complete--get-meta (candidate key)
  "Get metadata KEY for CANDIDATE."
  (when-let ((meta (get-text-property 0 'epsilon-meta candidate)))
    (alist-get key meta)))

;;; Annotation and Documentation

(defun epsilon-complete--annotation (candidate)
  "Return annotation for CANDIDATE."
  (let ((type (epsilon-complete--get-meta candidate 'type))
        (kind (epsilon-complete--get-meta candidate 'kind))
        (module (epsilon-complete--get-meta candidate 'module)))
    (cond
     ((eq epsilon-complete-annotation-style 'full)
      (concat (when module (format " [%s]" module))
              (when kind (format " <%s>" kind))
              (when type (format " : %s" type))))
     (t ; short
      (cond
       (type (format " : %s" type))
       (kind (format " <%s>" kind))
       (t ""))))))

(defun epsilon-complete--signature (candidate)
  "Return signature for CANDIDATE."
  (or (epsilon-complete--get-meta candidate 'signature)
      (epsilon-complete--get-meta candidate 'arglist)))

(defun epsilon-complete--doc-buffer (candidate)
  "Return documentation buffer for CANDIDATE."
  (when-let ((doc (epsilon-complete--get-meta candidate 'documentation)))
    (with-current-buffer (get-buffer-create " *epsilon-doc*")
      (erase-buffer)
      (insert doc)
      (goto-char (point-min))
      (current-buffer))))

(defun epsilon-complete--location (candidate)
  "Return source location for CANDIDATE."
  (let ((file (epsilon-complete--get-meta candidate 'file))
        (line (epsilon-complete--get-meta candidate 'line)))
    (when (and file line)
      (cons file line))))

;;; Post-Completion

(defun epsilon-complete--exit-function (candidate status)
  "Handle completion of CANDIDATE with STATUS."
  (when (eq status 'finished)
    ;; Insert snippet if available
    (when-let ((snippet (epsilon-complete--get-meta candidate 'snippet)))
      (delete-char (- (length candidate)))
      (epsilon-complete--insert-snippet snippet))))

(defun epsilon-complete--insert-snippet (snippet)
  "Insert SNIPPET with placeholder handling."
  ;; Simple placeholder handling: $1, $2, etc.
  (let ((start (point))
        (placeholders '()))
    (insert snippet)
    ;; Find placeholders
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\$\\([0-9]+\\)" nil t)
        (let ((num (string-to-number (match-string 1)))
              (pos (match-beginning 0)))
          (push (cons num pos) placeholders)
          (replace-match ""))))
    ;; Move to first placeholder
    (when placeholders
      (goto-char (cdr (assq 1 (sort placeholders
                                     (lambda (a b) (< (car a) (car b))))))))))

;;; Company Backend (optional)

(defun epsilon-company-backend (command &optional arg &rest _)
  "Company backend for Epsilon completion."
  (interactive (list 'interactive))
  (pcase command
    ('interactive (company-begin-backend 'epsilon-company-backend))
    ('prefix (epsilon-complete--company-prefix))
    ('candidates (epsilon-complete--candidates arg))
    ('annotation (epsilon-complete--annotation arg))
    ('meta (epsilon-complete--signature arg))
    ('doc-buffer (epsilon-complete--doc-buffer arg))
    ('location (epsilon-complete--location arg))
    ('post-completion (epsilon-complete--exit-function arg 'finished))
    ('sorted epsilon-complete-sort-by-relevance)
    ('no-cache t)))

(defun epsilon-complete--company-prefix ()
  "Return completion prefix for Company."
  (when (derived-mode-p 'epsilon-mode 'epsilon-repl-mode)
    (let ((sym (epsilon--symbol-at-point)))
      (when sym
        (cons sym t)))))

;;; Cape Integration (for Corfu users)

(defun epsilon-cape-symbol ()
  "Cape completion function for Epsilon symbols."
  (epsilon-complete-at-point))

;;; Clear Cache

(defun epsilon-complete-clear-cache ()
  "Clear completion cache."
  (interactive)
  (setq epsilon-complete--cached-completions nil
        epsilon-complete--cached-prefix nil))

(provide 'epsilon-complete)

;;; epsilon-complete.el ends here

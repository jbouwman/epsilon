;;; epsilon-semantic.el --- Semantic font-locking for Epsilon -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; Author: Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides semantic syntax highlighting for Epsilon Lisp
;; by requesting token information from the ELS server.  This enables
;; accurate highlighting of functions, macros, special forms, and
;; other constructs based on their actual definitions rather than
;; simple pattern matching.

;;; Code:

(require 'epsilon-client)
(require 'jit-lock)

;;; Customization

(defgroup epsilon-semantic nil
  "Semantic highlighting settings for Epsilon."
  :group 'epsilon
  :prefix "epsilon-semantic-")

(defcustom epsilon-semantic-enable t
  "When non-nil, enable semantic highlighting via ELS."
  :type 'boolean
  :group 'epsilon-semantic)

(defcustom epsilon-semantic-delay 0.3
  "Delay in seconds before requesting semantic tokens after changes."
  :type 'number
  :group 'epsilon-semantic)

(defcustom epsilon-semantic-chunk-size 4096
  "Maximum region size to tokenize at once."
  :type 'integer
  :group 'epsilon-semantic)

;;; Face Definitions
;;; These faces are used for semantic highlighting and can be customized.

(defface epsilon-semantic-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for special forms (defun, let, if, etc.)."
  :group 'epsilon-semantic)

(defface epsilon-semantic-function-face
  '((t :inherit font-lock-function-name-face))
  "Face for function names."
  :group 'epsilon-semantic)

(defface epsilon-semantic-variable-face
  '((t :inherit font-lock-variable-name-face))
  "Face for variable references."
  :group 'epsilon-semantic)

(defface epsilon-semantic-type-face
  '((t :inherit font-lock-type-face))
  "Face for type names."
  :group 'epsilon-semantic)

(defface epsilon-semantic-string-face
  '((t :inherit font-lock-string-face))
  "Face for string literals."
  :group 'epsilon-semantic)

(defface epsilon-semantic-number-face
  '((t :inherit font-lock-constant-face))
  "Face for numeric literals."
  :group 'epsilon-semantic)

(defface epsilon-semantic-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comments."
  :group 'epsilon-semantic)

(defface epsilon-semantic-macro-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for macro names."
  :group 'epsilon-semantic)

(defface epsilon-semantic-constant-face
  '((t :inherit font-lock-constant-face))
  "Face for constants (keywords, +foo+)."
  :group 'epsilon-semantic)

(defface epsilon-semantic-special-face
  '((t :inherit font-lock-variable-name-face :slant italic))
  "Face for special variables (*foo*)."
  :group 'epsilon-semantic)

(defface epsilon-semantic-reader-macro-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for reader macros (#'foo, etc.)."
  :group 'epsilon-semantic)

(defface epsilon-semantic-parameter-face
  '((t :inherit font-lock-variable-name-face :weight bold))
  "Face for function parameters."
  :group 'epsilon-semantic)

;;; Token Type to Face Mapping

(defvar epsilon-semantic--type-face-alist
  '(("KEYWORD" . epsilon-semantic-keyword-face)
    ("FUNCTION" . epsilon-semantic-function-face)
    ("VARIABLE" . epsilon-semantic-variable-face)
    ("PARAMETER" . epsilon-semantic-parameter-face)
    ("TYPE" . epsilon-semantic-type-face)
    ("STRING" . epsilon-semantic-string-face)
    ("NUMBER" . epsilon-semantic-number-face)
    ("COMMENT" . epsilon-semantic-comment-face)
    ("MACRO" . epsilon-semantic-macro-face)
    ("CONSTANT" . epsilon-semantic-constant-face)
    ("SPECIAL" . epsilon-semantic-special-face)
    ("READER-MACRO" . epsilon-semantic-reader-macro-face)
    ("PACKAGE" . font-lock-type-face)
    ("SLOT" . font-lock-variable-name-face)
    ("OPERATOR" . font-lock-builtin-face))
  "Mapping from token type strings to faces.")

(defun epsilon-semantic--type-to-face (type-name)
  "Convert TYPE-NAME to the corresponding face."
  (or (cdr (assoc type-name epsilon-semantic--type-face-alist))
      'font-lock-variable-name-face))

;;; State Variables

(defvar-local epsilon-semantic--pending-request nil
  "ID of pending semantic tokens request.")

(defvar-local epsilon-semantic--timer nil
  "Timer for delayed semantic token requests.")

(defvar-local epsilon-semantic--tokens nil
  "Cached semantic tokens for the buffer.")

(defvar-local epsilon-semantic--buffer-tick nil
  "Buffer modification tick when tokens were last fetched.")

;;; Token Application

(defun epsilon-semantic--apply-tokens (tokens)
  "Apply TOKENS to the current buffer for font-locking.
TOKENS is a vector of [start length type] arrays."
  (with-silent-modifications
    ;; Clear existing semantic highlights
    (remove-text-properties (point-min) (point-max)
                            '(epsilon-semantic nil face nil))
    ;; Apply new tokens
    (dolist (token (append tokens nil))
      (when (and (vectorp token) (= (length token) 3))
        (let* ((start (1+ (aref token 0)))  ; Convert 0-indexed to 1-indexed
               (len (aref token 1))
               (type-name (aref token 2))
               (end (+ start len))
               (face (epsilon-semantic--type-to-face type-name)))
          (when (and (<= start (point-max))
                     (<= end (point-max))
                     (> len 0))
            (put-text-property start end 'epsilon-semantic t)
            (put-text-property start end 'face face)))))))

;;; ELS Communication

(defun epsilon-semantic--request-tokens (&optional start end)
  "Request semantic tokens for region from START to END.
If START and END are nil, tokenize visible region."
  (when (and epsilon-semantic-enable
             (epsilon-connected-p))
    ;; Cancel any pending request
    (when epsilon-semantic--pending-request
      (remhash epsilon-semantic--pending-request epsilon--pending-requests)
      (setq epsilon-semantic--pending-request nil))
    ;; Determine region to tokenize
    (let* ((beg (or start (window-start)))
           (finish (or end (min (+ beg epsilon-semantic-chunk-size)
                                (point-max))))
           (code (buffer-substring-no-properties beg finish))
           (offset (1- beg)))  ; For adjusting token positions
      (setq epsilon-semantic--pending-request
            (epsilon-request
             "format" "semantic-tokens"
             `((code . ,code))
             (lambda (response)
               (setq epsilon-semantic--pending-request nil)
               (when (equal "ok" (alist-get 'status response))
                 (let ((tokens (alist-get 'tokens response)))
                   (when tokens
                     ;; Adjust token positions for offset
                     (let ((adjusted-tokens
                            (mapcar (lambda (tok)
                                      (when (vectorp tok)
                                        (vector (+ (aref tok 0) offset)
                                                (aref tok 1)
                                                (aref tok 2))))
                                    (append tokens nil))))
                       (epsilon-semantic--apply-tokens
                        (vconcat adjusted-tokens))))))))))))

(defun epsilon-semantic--request-tokens-debounced ()
  "Request tokens with debouncing to avoid excessive requests."
  (when epsilon-semantic--timer
    (cancel-timer epsilon-semantic--timer))
  (setq epsilon-semantic--timer
        (run-with-idle-timer epsilon-semantic-delay nil
                             #'epsilon-semantic--request-for-buffer
                             (current-buffer))))

(defun epsilon-semantic--request-for-buffer (buffer)
  "Request semantic tokens for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq epsilon-semantic--timer nil)
      (epsilon-semantic--request-tokens))))

;;; JIT-Lock Integration

(defun epsilon-semantic-fontify-region (start end &optional _loudly)
  "Fontify region between START and END with semantic tokens.
This is called by jit-lock when a region needs fontifying."
  ;; First apply basic font-lock
  (font-lock-default-fontify-region start end _loudly)
  ;; Then request semantic tokens asynchronously
  (when (and epsilon-semantic-enable (epsilon-connected-p))
    (epsilon-semantic--request-tokens start end))
  ;; Return the jit-lock expected value
  nil)

;;; Change Detection

(defun epsilon-semantic--after-change (_beg _end _len)
  "Handle buffer changes for semantic highlighting."
  (when epsilon-semantic-enable
    (epsilon-semantic--request-tokens-debounced)))

;;; Refresh Command

(defun epsilon-semantic-refresh ()
  "Refresh semantic highlighting for the entire buffer."
  (interactive)
  (if (epsilon-connected-p)
      (let ((code (buffer-substring-no-properties (point-min) (point-max))))
        (epsilon-request
         "format" "semantic-tokens"
         `((code . ,code))
         (lambda (response)
           (if (equal "ok" (alist-get 'status response))
               (let ((tokens (alist-get 'tokens response)))
                 (epsilon-semantic--apply-tokens tokens)
                 (message "Applied %d semantic tokens" (length tokens)))
             (message "Semantic tokens request failed")))))
    (message "Not connected to ELS")))

;;; Mode Integration

(defun epsilon-semantic-setup ()
  "Set up semantic highlighting for the current buffer."
  (when epsilon-semantic-enable
    ;; Register for change notifications
    (add-hook 'after-change-functions #'epsilon-semantic--after-change nil t)
    ;; Register for window scroll to update visible tokens
    (add-hook 'window-scroll-functions
              (lambda (_win _start)
                (epsilon-semantic--request-tokens-debounced))
              nil t)
    ;; Initial token request
    (when (epsilon-connected-p)
      (run-with-idle-timer 0.5 nil
                           #'epsilon-semantic--request-for-buffer
                           (current-buffer)))))

(defun epsilon-semantic-teardown ()
  "Remove semantic highlighting from the current buffer."
  (when epsilon-semantic--timer
    (cancel-timer epsilon-semantic--timer))
  (remove-hook 'after-change-functions #'epsilon-semantic--after-change t)
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max)
                            '(epsilon-semantic nil face nil))))

;;; Minor Mode

(define-minor-mode epsilon-semantic-mode
  "Minor mode for semantic syntax highlighting via ELS."
  :lighter " Sem"
  :group 'epsilon-semantic
  (if epsilon-semantic-mode
      (epsilon-semantic-setup)
    (epsilon-semantic-teardown)))

(provide 'epsilon-semantic)

;;; epsilon-semantic.el ends here

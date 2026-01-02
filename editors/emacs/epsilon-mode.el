;;; epsilon-mode.el --- Major mode for Epsilon Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; Author: Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides a major mode for editing Epsilon Lisp source files.
;; It integrates with the various Epsilon subsystems (Flymake, Xref, Eldoc,
;; completion, etc.) to provide full IDE features.

;;; Code:

(require 'lisp-mode)
(require 'epsilon-client)

;; Optional requires - loaded when available
(declare-function epsilon-format-setup "epsilon-format")
(declare-function epsilon-semantic-setup "epsilon-semantic")
(declare-function epsilon-format-region "epsilon-format")
(declare-function epsilon-format-buffer "epsilon-format")
(declare-function epsilon-format-defun "epsilon-format")
(declare-function epsilon-semantic-refresh "epsilon-semantic")

;;; Customization

(defgroup epsilon-mode nil
  "Epsilon source mode settings."
  :group 'epsilon
  :prefix "epsilon-mode-")

(defcustom epsilon-mode-hook nil
  "Hook run after entering Epsilon mode."
  :type 'hook
  :group 'epsilon-mode)

;;; Font Lock

(defvar epsilon-mode-font-lock-keywords
  (let ((special-forms
         '("defun" "defmacro" "defvar" "defparameter" "defconstant"
           "defgeneric" "defmethod" "defclass" "defstruct" "deftype"
           "defprotocol" "defrecord" "defmodule" "defsystem"
           "lambda" "let" "let*" "flet" "labels" "macrolet"
           "if" "when" "unless" "cond" "case" "typecase" "ecase" "etypecase"
           "progn" "prog1" "prog2" "block" "return-from" "return"
           "tagbody" "go" "catch" "throw" "unwind-protect"
           "loop" "do" "do*" "dolist" "dotimes" "while"
           "handler-case" "handler-bind" "restart-case" "restart-bind"
           "with-open-file" "with-output-to-string" "with-input-from-string"
           "multiple-value-bind" "multiple-value-call" "multiple-value-prog1"
           "destructuring-bind" "setq" "setf" "psetq" "psetf"
           "and" "or" "not" "declare" "the" "locally"
           "eval-when" "load-time-value" "quote" "function"
           "in-package" "use-package" "import" "export" "require"))
        (builtin-functions
         '("car" "cdr" "cons" "list" "list*" "append" "reverse" "nreverse"
           "length" "nth" "nthcdr" "first" "second" "third" "fourth" "fifth"
           "rest" "last" "butlast" "member" "assoc" "rassoc"
           "mapcar" "maplist" "mapc" "mapl" "mapcan" "mapcon"
           "reduce" "remove" "remove-if" "remove-if-not"
           "find" "find-if" "find-if-not" "position" "position-if"
           "count" "count-if" "count-if-not" "substitute" "substitute-if"
           "sort" "stable-sort" "merge" "concatenate" "copy-seq" "subseq"
           "make-array" "aref" "array-dimensions" "array-rank"
           "gethash" "remhash" "maphash" "make-hash-table" "hash-table-count"
           "format" "princ" "prin1" "print" "pprint" "write"
           "read" "read-from-string" "read-line"
           "apply" "funcall" "eval" "compile" "load"
           "eq" "eql" "equal" "equalp" "=" "/=" "<" ">" "<=" ">="
           "+" "-" "*" "/" "mod" "rem" "floor" "ceiling" "round" "truncate"
           "sqrt" "expt" "log" "exp" "sin" "cos" "tan" "abs" "max" "min"
           "string" "string=" "string-equal" "string<" "string>"
           "symbol-name" "symbol-value" "symbol-function" "symbol-plist"
           "intern" "make-symbol" "gensym" "get-setf-expansion"
           "type-of" "typep" "subtypep" "coerce"
           "error" "warn" "signal" "cerror" "assert" "check-type"
           "slot-value" "make-instance" "initialize-instance"
           "values" "values-list" "nth-value")))
    `((,(concat "(" (regexp-opt special-forms 'words))
       (1 font-lock-keyword-face))
      (,(regexp-opt builtin-functions 'words)
       . font-lock-builtin-face)
      (,(rx (group (or "defun" "defmacro" "defgeneric" "defmethod"))
            (+ space)
            (group (+ (or word (syntax symbol)))))
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face))
      (,(rx (group (or "defvar" "defparameter" "defconstant"))
            (+ space)
            (group (+ (or word (syntax symbol)))))
       (1 font-lock-keyword-face)
       (2 font-lock-variable-name-face))
      (,(rx (group (or "defclass" "defstruct" "deftype" "defprotocol" "defrecord"))
            (+ space)
            (group (+ (or word (syntax symbol)))))
       (1 font-lock-keyword-face)
       (2 font-lock-type-face))
      ;; Keywords (:foo)
      (,(rx symbol-start ":" (+ (or word (syntax symbol))))
       . font-lock-constant-face)
      ;; Reader macros (#foo)
      (,(rx "#" (or "'" "`" "," "." "+" "-" "p" "P" "c" "C"
                    (seq (opt (+ digit)) (or "a" "A" "r" "R"))))
       . font-lock-preprocessor-face)
      ;; Special variables (*foo*)
      (,(rx symbol-start "*" (+ (or word (syntax symbol))) "*" symbol-end)
       . font-lock-variable-name-face)
      ;; Constants (+foo+)
      (,(rx symbol-start "+" (+ (or word (syntax symbol))) "+" symbol-end)
       . font-lock-constant-face)))
  "Font-lock keywords for Epsilon mode.")

;;; Syntax Table

(defvar epsilon-mode-syntax-table
  (let ((table (make-syntax-table lisp-mode-syntax-table)))
    ;; Epsilon-specific syntax adjustments
    (modify-syntax-entry ?# "' 14" table)
    (modify-syntax-entry ?\| "\" 23bn" table)
    table)
  "Syntax table for Epsilon mode.")

;;; Keymap

(defvar epsilon-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Evaluation
    (define-key map (kbd "C-x C-e") #'epsilon-eval-last-sexp)
    (define-key map (kbd "C-M-x") #'epsilon-eval-defun)
    (define-key map (kbd "C-c C-r") #'epsilon-eval-region)
    (define-key map (kbd "C-c C-k") #'epsilon-compile-file)
    (define-key map (kbd "C-c C-l") #'epsilon-load-file)
    (define-key map (kbd "C-c C-e") #'epsilon-eval-and-print-last-sexp)
    ;; REPL
    (define-key map (kbd "C-c C-z") #'epsilon-switch-to-repl)
    (define-key map (kbd "C-c C-c") #'epsilon-interrupt)
    ;; Navigation
    (define-key map (kbd "M-.") #'xref-find-definitions)
    (define-key map (kbd "M-,") #'xref-go-back)
    (define-key map (kbd "M-?") #'xref-find-references)
    ;; Documentation
    (define-key map (kbd "C-c C-d d") #'epsilon-describe-symbol)
    (define-key map (kbd "C-c C-d a") #'epsilon-apropos)
    (define-key map (kbd "C-c C-d h") #'epsilon-hyperspec-lookup)
    ;; Macroexpand
    (define-key map (kbd "C-c C-m") #'epsilon-macroexpand)
    (define-key map (kbd "C-c M-m") #'epsilon-macroexpand-all)
    ;; Inspector
    (define-key map (kbd "C-c C-i") #'epsilon-inspect)
    ;; Tests
    (define-key map (kbd "C-c t t") #'epsilon-run-test-at-point)
    (define-key map (kbd "C-c t f") #'epsilon-run-tests-in-file)
    (define-key map (kbd "C-c t m") #'epsilon-run-tests-in-module)
    ;; Completion
    (define-key map (kbd "TAB") #'completion-at-point)
    (define-key map (kbd "C-M-i") #'completion-at-point)
    ;; Formatting
    (define-key map (kbd "C-c C-f") #'epsilon-format-defun)
    (define-key map (kbd "C-c M-f") #'epsilon-format-buffer)
    ;; Semantic highlighting
    (define-key map (kbd "C-c C-s") #'epsilon-semantic-refresh)
    ;; Workspace commands
    (define-key map (kbd "C-c C-w i") #'epsilon-workspace-info)
    (define-key map (kbd "C-c C-w r") #'epsilon-reload-workspace)
    (define-key map (kbd "C-c C-w s") #'epsilon-switch-workspace)
    map)
  "Keymap for `epsilon-mode'.")

;;; Mode Definition

;;;###autoload
(define-derived-mode epsilon-mode lisp-mode "Epsilon"
  "Major mode for editing Epsilon Lisp code.

Commands:
\\{epsilon-mode-map}"
  :group 'epsilon-mode
  :syntax-table epsilon-mode-syntax-table
  ;; Font-lock
  (setq-local font-lock-defaults
              '(epsilon-mode-font-lock-keywords
                nil nil nil nil
                (font-lock-mark-block-function . mark-defun)))
  ;; Comments
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local comment-start-skip ";+ *")
  ;; Indentation
  (setq-local indent-line-function #'lisp-indent-line)
  (setq-local lisp-indent-function #'epsilon-indent-function)
  ;; Sexp navigation
  (setq-local forward-sexp-function nil)
  (setq-local parse-sexp-ignore-comments t)
  ;; Imenu
  (setq-local imenu-generic-expression lisp-imenu-generic-expression)
  ;; Enable subsystems (conditionally loaded)
  (when (featurep 'epsilon-eldoc)
    (epsilon-eldoc-setup))
  (when (featurep 'epsilon-flymake)
    (epsilon-flymake-setup))
  (when (featurep 'epsilon-xref)
    (epsilon-xref-setup))
  (when (featurep 'epsilon-imenu)
    (epsilon-imenu-setup))
  (when (featurep 'epsilon-complete)
    (epsilon-complete-setup))
  (when (featurep 'epsilon-format)
    (epsilon-format-setup))
  (when (featurep 'epsilon-semantic)
    (epsilon-semantic-setup))
  ;; Mode-line status indicator
  (epsilon-mode-line-setup)
  ;; Connection handling
  (if epsilon-connect-on-open
      ;; Connect immediately when opening file
      (epsilon-connect-with-workspace)
    ;; Fallback: connect on first edit
    (add-hook 'first-change-hook #'epsilon-ensure-connected nil t)))

;;; Indentation

(defun epsilon-indent-function (indent-point state)
  "Epsilon indentation function.
INDENT-POINT is the position being indented.
STATE is the `parse-partial-sexp' state at that position."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (nth 1 state)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\s(")))
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
                                         (progn (forward-sexp 1) (point))))
            method)
        (setq method (epsilon--get-indent-function function))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state indent-point normal-indent))
              (method
               (funcall method indent-point state))
              (t normal-indent))))))

(defun epsilon--get-indent-function (name)
  "Get indentation method for function NAME."
  (let ((sym (intern-soft name)))
    (or (and sym (get sym 'epsilon-indent-function))
        (and sym (get sym 'lisp-indent-function)))))

;; Set up indentation for common forms
(dolist (form '(defun defmacro defmethod defgeneric
                lambda let let* flet labels macrolet
                if when unless cond case typecase
                block return-from catch throw unwind-protect
                handler-case handler-bind restart-case
                with-open-file with-output-to-string
                multiple-value-bind destructuring-bind
                loop do do* dolist dotimes while))
  (put form 'epsilon-indent-function 'defun))

(dolist (form '(if progn prog1 prog2))
  (put form 'epsilon-indent-function 1))

;;; File Operations

(defun epsilon-compile-file (&optional file)
  "Compile FILE (default: current buffer) without loading."
  (interactive)
  (let ((file (or file (buffer-file-name))))
    (unless file
      (error "Buffer is not visiting a file"))
    (when (buffer-modified-p)
      (save-buffer))
    (epsilon-request
     "module" "compile-file"
     `((file . ,file)
       (load . :false))
     (lambda (response)
       (if (equal "ok" (alist-get 'status response))
           (let ((warnings (alist-get 'warnings response)))
             (if warnings
                 (progn
                   (message "Compiled with %d warnings" (length warnings))
                   (epsilon--show-compilation-results warnings))
               (message "Compiled successfully")))
         (message "Compilation failed: %s"
                  (alist-get 'message (alist-get 'error response))))))))

(defun epsilon-load-file (&optional file)
  "Load FILE (default: current buffer)."
  (interactive)
  (let ((file (or file (buffer-file-name))))
    (unless file
      (error "Buffer is not visiting a file"))
    (when (buffer-modified-p)
      (save-buffer))
    (epsilon-request
     "eval" "load-file"
     `((file . ,file))
     (lambda (response)
       (if (equal "ok" (alist-get 'status response))
           (message "Loaded %s" (file-name-nondirectory file))
         (message "Load failed: %s"
                  (alist-get 'message (alist-get 'error response))))))))

(defun epsilon--show-compilation-results (warnings)
  "Show WARNINGS in a compilation buffer."
  (with-current-buffer (get-buffer-create "*epsilon-compilation*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (compilation-mode)
      (insert "Epsilon Compilation Results\n\n")
      (dolist (warning warnings)
        (let ((file (alist-get 'file warning))
              (line (alist-get 'line warning))
              (message (alist-get 'message warning)))
          (insert (format "%s:%d: warning: %s\n" file line message)))))
    (display-buffer (current-buffer))))

;;; Module Operations

(defun epsilon-load-module (module)
  "Load Epsilon MODULE with dependencies."
  (interactive
   (list (completing-read "Module: " (epsilon--list-modules))))
  (epsilon-request
   "module" "load-module"
   `((module . ,module))
   (lambda (response)
     (if (equal "ok" (alist-get 'status response))
         (message "Loaded: %s"
                  (mapconcat #'identity (alist-get 'loaded response) ", "))
       (message "Failed: %s"
                (alist-get 'message (alist-get 'error response)))))))

(defun epsilon-compile-module (module)
  "Compile Epsilon MODULE."
  (interactive
   (list (completing-read "Module: " (epsilon--list-modules))))
  (epsilon-request
   "module" "compile-module"
   `((module . ,module))
   (lambda (response)
     (let ((warnings (alist-get 'warnings response)))
       (if warnings
           (message "Compiled with %d warnings" (length warnings))
         (message "Compiled successfully"))))))

(defun epsilon--list-modules ()
  "Return list of available modules."
  (let ((response (epsilon-request-sync "module" "list-modules")))
    (when (equal "ok" (alist-get 'status response))
      (mapcar (lambda (m) (alist-get 'name m))
              (alist-get 'modules response)))))

;;; Documentation

(defun epsilon-hyperspec-lookup (symbol)
  "Look up SYMBOL in the Common Lisp HyperSpec."
  (interactive
   (list (or (epsilon--symbol-at-point)
             (read-string "Symbol: "))))
  (let ((url (format "http://www.lispworks.com/documentation/HyperSpec/Body/f_%s.htm"
                     (downcase symbol))))
    (browse-url url)))

;;; Workspace Commands

(defun epsilon-workspace-info ()
  "Display information about the current workspace."
  (interactive)
  (if-let ((workspace (epsilon--active-workspace)))
      (epsilon-request
       "module" "workspace-info"
       `((path . ,workspace))
       #'epsilon--display-workspace-info)
    (message "Not in an Epsilon workspace")))

(defun epsilon--display-workspace-info (response)
  "Display workspace info from RESPONSE."
  (if (equal "ok" (alist-get 'status response))
      (with-help-window "*epsilon-workspace*"
        (let ((name (alist-get 'name response))
              (modules (alist-get 'modules response))
              (module-count (alist-get 'module-count response)))
          (princ (format "Workspace: %s\n" name))
          (when module-count
            (princ (format "Modules: %d\n\n" module-count)))
          (when modules
            (dolist (module modules)
              (let ((mod-name (alist-get 'name module))
                    (version (alist-get 'version module)))
                (princ (format "  %s" mod-name))
                (when version
                  (princ (format " (%s)" version)))
                (princ "\n"))))))
    (message "Failed to get workspace info: %s"
             (alist-get 'message (alist-get 'error response)))))

(defun epsilon-reload-workspace ()
  "Reload the current workspace modules."
  (interactive)
  (if-let ((workspace (epsilon--active-workspace)))
      (epsilon-request
       "module" "load-workspace"
       `((path . ,workspace))
       (lambda (response)
         (if (equal "ok" (alist-get 'status response))
             (let ((count (alist-get 'module-count response)))
               (setq epsilon--current-workspace workspace)
               (epsilon--update-mode-line)
               (message "Reloaded workspace: %d modules" (or count 0)))
           (message "Failed to reload workspace: %s"
                    (alist-get 'message (alist-get 'error response))))))
    (message "Not in an Epsilon workspace")))

(defun epsilon-switch-workspace (path)
  "Switch to workspace at PATH."
  (interactive "DWorkspace directory: ")
  (let ((expanded-path (expand-file-name path)))
    (epsilon-request
     "module" "load-workspace"
     `((path . ,expanded-path))
     (lambda (response)
       (if (equal "ok" (alist-get 'status response))
           (progn
             (setq epsilon--current-workspace expanded-path)
             (epsilon--update-mode-line)
             (message "Switched to workspace: %s"
                      (file-name-nondirectory
                       (directory-file-name expanded-path))))
         (message "Failed to switch workspace: %s"
                  (alist-get 'message (alist-get 'error response))))))))

;;; Auto-mode Setup

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.e\\'" . epsilon-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lisp\\'" . epsilon-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("module\\.lisp\\'" . epsilon-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("workspace\\.lisp\\'" . epsilon-mode))

(provide 'epsilon-mode)

;;; epsilon-mode.el ends here

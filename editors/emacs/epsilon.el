;;; epsilon.el --- Epsilon Language Server integration -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; Author: Jesse Bouwman
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: languages lisp tools
;; URL: https://github.com/jbouwman/epsilon

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This package provides full IDE integration for Epsilon Lisp via the
;; Epsilon Language Server (ELS).  It includes:
;;
;; - `epsilon-mode': Major mode for Epsilon source files
;; - `epsilon-repl': Interactive REPL with comint integration
;; - Flymake: Live diagnostics
;; - Xref: Go-to-definition and find-references
;; - Eldoc: Inline function signatures
;; - Completion: Symbol completion (Company/Corfu/Cape)
;; - Debugger: Full debugger with restart handling
;; - Project.el: Workspace/module integration
;; - Imenu: Buffer outline
;; - Inspector: Object inspection
;; - Test Runner: Test discovery and execution
;; - Formatting: Code formatting via ELS (C-c C-f / C-c M-f)
;; - Semantic: Server-side syntax highlighting (C-c C-s to refresh)
;;
;; Quick start:
;;
;;   (require 'epsilon)
;;   (epsilon-mode)  ;; for .lisp files
;;   M-x epsilon-repl  ;; start REPL
;;
;; Or with use-package:
;;
;;   (use-package epsilon
;;     :mode ("\\.lisp\\'" . epsilon-mode)
;;     :commands (epsilon-repl epsilon-connect))

;;; Code:

;; Core modules
(require 'epsilon-client)
(require 'epsilon-mode)
(require 'epsilon-repl)

;; Subsystems (loaded on demand)
(require 'epsilon-flymake)
(require 'epsilon-xref)
(require 'epsilon-eldoc)
(require 'epsilon-complete)
(require 'epsilon-debug)
(require 'epsilon-project)
(require 'epsilon-imenu)
(require 'epsilon-inspector)
(require 'epsilon-test)
(require 'epsilon-format)
(require 'epsilon-semantic)

;;; Customization

(defgroup epsilon nil
  "Epsilon Language Server integration."
  :group 'languages
  :prefix "epsilon-")

;;; Initialization

(defun epsilon-setup ()
  "Set up Epsilon integration for all buffers."
  (interactive)
  ;; Register project backend
  (epsilon-project-setup)
  ;; Set up debugger handler
  (epsilon-debug-setup)
  (message "Epsilon integration activated"))

(defun epsilon-teardown ()
  "Remove Epsilon integration."
  (interactive)
  (epsilon-project-teardown)
  (epsilon-disconnect)
  (message "Epsilon integration deactivated"))

;;; Main Entry Points

;;;###autoload
(defun epsilon ()
  "Start Epsilon REPL and connect to server."
  (interactive)
  (epsilon-repl))

;;;###autoload
(defun epsilon-connect-and-start ()
  "Connect to ELS server and start REPL."
  (interactive)
  (epsilon-connect)
  (epsilon-repl))

;;; Keymap

(defvar epsilon-prefix-map
  (let ((map (make-sparse-keymap)))
    ;; Connection
    (define-key map (kbd "c") #'epsilon-connect)
    (define-key map (kbd "d") #'epsilon-disconnect)
    (define-key map (kbd "r") #'epsilon-reconnect)
    ;; REPL
    (define-key map (kbd "z") #'epsilon-repl)
    ;; Modules
    (define-key map (kbd "l") #'epsilon-load-module)
    (define-key map (kbd "k") #'epsilon-compile-module)
    ;; Testing
    (define-key map (kbd "t") #'epsilon-run-tests)
    ;; Inspector
    (define-key map (kbd "i") #'epsilon-inspect)
    ;; Documentation
    (define-key map (kbd "?") #'epsilon-describe-symbol)
    ;; Formatting
    (define-key map (kbd "f") #'epsilon-format-buffer)
    ;; Semantic
    (define-key map (kbd "s") #'epsilon-semantic-refresh)
    map)
  "Prefix keymap for Epsilon commands.")

;;;###autoload
(defvar-keymap epsilon-mode-prefix-map
  :doc "Prefix map for Epsilon mode."
  "c" #'epsilon-connect
  "d" #'epsilon-disconnect
  "z" #'epsilon-repl
  "l" #'epsilon-load-module
  "t" #'epsilon-run-tests
  "i" #'epsilon-inspect
  "f" #'epsilon-format-buffer
  "s" #'epsilon-semantic-refresh)

;;; Auto-mode Setup

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lisp\\'" . epsilon-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("module\\.lisp\\'" . epsilon-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("workspace\\.lisp\\'" . epsilon-mode))

;;; Info for package.el

;;;###autoload
(defconst epsilon-pkg
  '(epsilon
    :version "0.1.0"
    :requires ((emacs "28.1"))
    :keywords ("languages" "lisp" "tools")
    :url "https://github.com/jbouwman/epsilon")
  "Package descriptor for epsilon.")

(provide 'epsilon)

;;; epsilon.el ends here

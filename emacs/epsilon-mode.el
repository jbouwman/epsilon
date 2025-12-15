;;; epsilon-mode.el --- Epsilon Lisp development mode for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Jesse Bouwman

;; Author: Jesse Bouwman <jesse@bouwman.org>
;; Maintainer: Jesse Bouwman <jesse@bouwman.org>
;; URL: https://github.com/jbouwman/epsilon
;; Homepage: https://github.com/jbouwman/epsilon
;; Keywords: languages, lisp, tools
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Epsilon-mode provides comprehensive development support for the Epsilon
;; Lisp framework.  It integrates with SLIME/SWANK and provides:
;;
;; - Project management with module.lisp detection
;; - Module search path configuration
;; - Test execution at point with rich results display
;; - Module loading and dependency management
;; - Flymake integration for real-time linting
;; - Compilation with error navigation
;; - Imenu support for test/function navigation
;; - xref integration for symbol lookup
;;
;; Quick Start:
;;   M-x epsilon-mode        - Enable in current buffer
;;   M-x epsilon-start-repl  - Start an Epsilon REPL
;;   C-c C-e p               - Select project
;;   C-c C-e t               - Run test at point
;;   C-c C-e c               - Compile current module

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'compile)
(require 'project)

;; Optional dependencies - load if available
(when (locate-library "slime")
  (require 'slime))
(when (locate-library "flymake")
  (require 'flymake))

;; Declare SLIME functions to avoid compiler warnings
(declare-function slime-connected-p "slime")
(declare-function slime-eval-async "slime")
(declare-function slime-start "slime")
(declare-function slime-output-buffer "slime")

;;; Customization

(defgroup epsilon nil
  "Epsilon Lisp development support."
  :group 'lisp
  :prefix "epsilon-")

(defcustom epsilon-executable nil
  "Path to the epsilon executable.
If nil, searches PATH and common locations."
  :type '(choice (const :tag "Auto-detect" nil)
                 (file :tag "Path"))
  :group 'epsilon)

(defcustom epsilon-root nil
  "Root directory of the Epsilon installation.
If nil, auto-detected from file locations."
  :type '(choice (const :tag "Auto-detect" nil)
                 (directory :tag "Path"))
  :group 'epsilon)

(defcustom epsilon-default-module-paths '("./modules" "../modules")
  "Default module search paths relative to project root."
  :type '(repeat string)
  :group 'epsilon)

(defcustom epsilon-test-buffer-name "*Epsilon Tests*"
  "Name of buffer for test output."
  :type 'string
  :group 'epsilon)

(defcustom epsilon-lint-buffer-name "*Epsilon Lint*"
  "Name of buffer for lint output."
  :type 'string
  :group 'epsilon)

(defcustom epsilon-repl-buffer-name "*Epsilon REPL*"
  "Name of buffer for Epsilon REPL."
  :type 'string
  :group 'epsilon)

(defcustom epsilon-use-flymake t
  "Whether to enable Flymake for real-time linting."
  :type 'boolean
  :group 'epsilon)

(defcustom epsilon-lint-delay 0.5
  "Delay in seconds before running lint after changes."
  :type 'number
  :group 'epsilon)

(defcustom epsilon-compilation-scroll-output t
  "Whether to scroll compilation output."
  :type 'boolean
  :group 'epsilon)

;;; Variables

(defvar epsilon-current-project nil
  "Currently selected Epsilon project.")

(defvar epsilon-project-list nil
  "List of discovered Epsilon projects.")

(defvar epsilon-module-search-path nil
  "Current module search path.")

(defvar epsilon-last-compilation-buffer nil
  "Buffer from last compilation command.")

(defvar-local epsilon-project-root nil
  "Buffer-local project root directory.")

;;; Keymap

(defvar epsilon-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Project management
    (define-key map (kbd "C-c C-e p") #'epsilon-select-project)
    (define-key map (kbd "C-c C-e r") #'epsilon-refresh-projects)
    (define-key map (kbd "C-c C-e s") #'epsilon-configure-search-path)
    ;; Testing
    (define-key map (kbd "C-c C-e t") #'epsilon-run-test-at-point)
    (define-key map (kbd "C-c C-e T") #'epsilon-run-module-tests)
    (define-key map (kbd "C-c C-e j") #'epsilon-jump-to-test)
    ;; Compilation and loading
    (define-key map (kbd "C-c C-e c") #'epsilon-compile-module)
    (define-key map (kbd "C-c C-e l") #'epsilon-load-current-module)
    (define-key map (kbd "C-c C-e b") #'epsilon-build-all)
    ;; Linting
    (define-key map (kbd "C-c C-e L") #'epsilon-lint-module)
    (define-key map (kbd "C-c C-e f") #'epsilon-lint-file)
    ;; REPL
    (define-key map (kbd "C-c C-e R") #'epsilon-start-repl)
    (define-key map (kbd "C-c C-e e") #'epsilon-eval-expression)
    (define-key map (kbd "C-c C-e r") #'epsilon-send-region-to-repl)
    (define-key map (kbd "C-c C-e d") #'epsilon-send-defun-to-repl)
    ;; Navigation
    (define-key map (kbd "C-c C-e .") #'epsilon-find-definition)
    (define-key map (kbd "C-c C-e ?") #'epsilon-describe-symbol)
    map)
  "Keymap for Epsilon mode.")

;;; Finding Epsilon Executable

(defconst epsilon-common-install-locations
  '("~/.local/bin/epsilon"
    "~/.epsilon/bin/epsilon"
    "/usr/local/bin/epsilon"
    "/opt/epsilon/bin/epsilon")
  "Common installation locations for the epsilon executable.")

(defun epsilon-find-executable ()
  "Find the epsilon executable.
Search order:
1. User-configured `epsilon-executable'
2. EPSILON_HOME/bin/epsilon
3. Project-local epsilon script
4. PATH lookup
5. Common installation locations"
  (or epsilon-executable
      ;; Check EPSILON_HOME first
      (let ((home (getenv "EPSILON_HOME")))
        (when home
          (let ((exe (expand-file-name "bin/epsilon" home)))
            (when (file-executable-p exe) exe))))
      ;; Check project root
      (let ((root (epsilon-find-root)))
        (when root
          (let ((exe (expand-file-name "epsilon" root)))
            (when (file-executable-p exe) exe))))
      ;; PATH lookup
      (executable-find "epsilon")
      ;; Common installation locations
      (cl-find-if #'file-executable-p
                  (mapcar #'expand-file-name epsilon-common-install-locations))))

(defun epsilon-find-root ()
  "Find the Epsilon root directory.
Search order:
1. User-configured `epsilon-root'
2. Locate upward from current buffer for epsilon project
3. EPSILON_HOME environment variable
4. ~/.epsilon (default installation location)"
  (or epsilon-root
      ;; Look for project root from current file
      (when buffer-file-name
        (let ((dir (locate-dominating-file buffer-file-name "module.lisp")))
          (when dir
            ;; Walk up to find the epsilon root (contains modules directory)
            (let ((parent (locate-dominating-file dir "modules")))
              (when (and parent (file-exists-p (expand-file-name "modules" parent)))
                parent)))))
      ;; Check EPSILON_HOME
      (let ((home (getenv "EPSILON_HOME")))
        (when (and home (file-directory-p home))
          home))
      ;; Default location
      (let ((default-loc (expand-file-name "~/.epsilon")))
        (when (file-directory-p default-loc)
          default-loc))))

(defun epsilon-installation-status ()
  "Check and report the status of Epsilon installation."
  (interactive)
  (let ((exe (epsilon-find-executable))
        (root (epsilon-find-root)))
    (message "Epsilon status:\n  Executable: %s\n  Root: %s"
             (or exe "NOT FOUND")
             (or root "NOT FOUND"))))

;;; Project Management

(defun epsilon-find-projects (&optional directory)
  "Find all Epsilon projects starting from DIRECTORY.
An Epsilon project is identified by the presence of a module.lisp file."
  (let ((search-dir (or directory
                        (epsilon-find-root)
                        default-directory))
        (projects '()))
    (when search-dir
      (dolist (file (directory-files-recursively search-dir "module\\.lisp$" nil))
        (let* ((project-dir (file-name-directory file))
               (module-info (epsilon-parse-module-file file)))
          (when module-info
            (push (list :name (plist-get module-info :name)
                        :path project-dir
                        :module-file file
                        :info module-info)
                  projects)))))
    (nreverse projects)))

(defun epsilon-parse-module-file (file)
  "Parse an Epsilon module.lisp FILE and extract metadata."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (let ((info '())
                (eof (gensym)))
            (cl-loop for form = (read (current-buffer) nil eof)
                     until (eq form eof)
                     do (when (keywordp form)
                          (let ((value (read (current-buffer) nil eof)))
                            (unless (eq value eof)
                              (setq info (plist-put info form value))))))
            info)
        (error nil)))))

(defun epsilon-select-project ()
  "Interactively select an Epsilon project."
  (interactive)
  (let ((projects (or epsilon-project-list (epsilon-refresh-projects))))
    (if projects
        (let* ((project-names (mapcar (lambda (p)
                                        (format "%s (%s)"
                                                (plist-get p :name)
                                                (abbreviate-file-name (plist-get p :path))))
                                      projects))
               (selection (completing-read "Select Epsilon project: " project-names nil t)))
          (when selection
            (let ((index (cl-position selection project-names :test #'string=)))
              (setq epsilon-current-project (nth index projects))
              (epsilon-configure-project epsilon-current-project)
              (message "Selected project: %s"
                       (plist-get epsilon-current-project :name)))))
      (message "No Epsilon projects found"))))

(defun epsilon-refresh-projects ()
  "Refresh the list of discovered Epsilon projects."
  (interactive)
  (setq epsilon-project-list (epsilon-find-projects))
  (message "Found %d Epsilon projects" (length epsilon-project-list))
  epsilon-project-list)

(defun epsilon-configure-project (project)
  "Configure Emacs for the selected Epsilon PROJECT."
  (when project
    (let ((project-dir (plist-get project :path)))
      (setq epsilon-module-search-path
            (mapcar (lambda (path)
                      (expand-file-name path project-dir))
                    epsilon-default-module-paths))
      (when (and (fboundp 'slime-connected-p) (slime-connected-p))
        (epsilon-setup-swank-environment project)))))

;;; Compilation

(defvar epsilon-compilation-error-regexp-alist
  '((epsilon-error
     "^\\(?:ERROR\\|Error\\).*at \\([^:]+\\):\\([0-9]+\\)" 1 2 nil 2)
    (epsilon-warning
     "^\\(?:WARNING\\|Warning\\).*at \\([^:]+\\):\\([0-9]+\\)" 1 2 nil 1)
    (epsilon-note
     "^\\(?:NOTE\\|Note\\).*at \\([^:]+\\):\\([0-9]+\\)" 1 2 nil 0)
    ;; SBCL style errors
    (sbcl-error
     "^; +file: \\([^ \n]+\\)$" 1 nil nil 0)
    (sbcl-position
     "^;[ ]*\\(?:in\\|at\\): \\([^ \n]+\\), line \\([0-9]+\\)" 1 2))
  "Compilation error patterns for Epsilon.")

(defun epsilon-compile-module (&optional module-name)
  "Compile MODULE-NAME or the current module."
  (interactive)
  (let* ((module (or module-name (epsilon-current-module-name)))
         (exe (epsilon-find-executable))
         (root (epsilon-find-root)))
    (unless exe
      (user-error "Cannot find epsilon executable"))
    (unless module
      (user-error "Cannot determine current module"))
    (let ((default-directory root)
          (compilation-scroll-output epsilon-compilation-scroll-output))
      (compilation-start
       (format "%s --module %s --eval t" exe module)
       'epsilon-compilation-mode
       (lambda (_) (format "*Epsilon Compile: %s*" module))))))

(defun epsilon-build-all ()
  "Build all Epsilon modules."
  (interactive)
  (let ((exe (epsilon-find-executable))
        (root (epsilon-find-root)))
    (unless exe
      (user-error "Cannot find epsilon executable"))
    (let ((default-directory root)
          (compilation-scroll-output epsilon-compilation-scroll-output))
      (compilation-start
       (format "%s --exec epsilon.release:build" exe)
       'epsilon-compilation-mode
       (lambda (_) "*Epsilon Build*")))))

(define-compilation-mode epsilon-compilation-mode "Epsilon"
  "Compilation mode for Epsilon builds."
  (setq-local compilation-error-regexp-alist
              (append '(epsilon-error epsilon-warning epsilon-note sbcl-error sbcl-position)
                      compilation-error-regexp-alist))
  (setq-local compilation-error-regexp-alist-alist
              (append epsilon-compilation-error-regexp-alist
                      compilation-error-regexp-alist-alist)))

;;; Test Management

(defun epsilon-run-test-at-point ()
  "Run the Epsilon test at point."
  (interactive)
  (let ((test-name (epsilon-find-test-at-point)))
    (if test-name
        (epsilon-execute-test test-name)
      (message "No test found at point"))))

(defun epsilon-find-test-at-point ()
  "Find the name of the test function at point."
  (save-excursion
    (beginning-of-defun)
    (when (looking-at "(deftest\\s-+\\([[:alnum:]-_]+\\)")
      (match-string-no-properties 1))))

(defun epsilon-run-module-tests (&optional module-name)
  "Run all tests for MODULE-NAME or current module."
  (interactive)
  (let ((module (or module-name (epsilon-current-module-name))))
    (if module
        (epsilon-execute-module-tests module)
      (message "Could not determine current module"))))

(defun epsilon-current-module-name ()
  "Determine the current module name from the file path or package."
  (cond
   ;; Try to extract from in-package form
   ((save-excursion
      (goto-char (point-min))
      (re-search-forward "(in-package[[:space:]]+\\([^)]+\\))" nil t))
    (let ((package-name (string-trim (match-string-no-properties 1))))
      ;; Convert package name to module name (take first two parts)
      (when (string-match "^\\([^.]+\\.[^.]+\\)" package-name)
        (match-string 1 package-name))))
   ;; Try defpackage
   ((save-excursion
      (goto-char (point-min))
      (re-search-forward "(defpackage[[:space:]]+\\([^[:space:])]+\\)" nil t))
    (let ((package-name (match-string-no-properties 1)))
      (when (string-match "^\\([^.]+\\.[^.]+\\)" package-name)
        (match-string 1 package-name))))
   ;; Try to extract from file path
   ((and buffer-file-name (epsilon-find-root))
    (let* ((root (epsilon-find-root))
           (relative (file-relative-name buffer-file-name root)))
      (when (string-match "modules/\\([^/]+\\)/" relative)
        (concat "epsilon." (match-string 1 relative)))))
   (t nil)))

(defun epsilon-execute-test (test-name)
  "Execute a single test TEST-NAME."
  (let ((module (epsilon-current-module-name))
        (exe (epsilon-find-executable))
        (root (epsilon-find-root)))
    (unless exe
      (user-error "Cannot find epsilon executable"))
    (let ((default-directory root))
      (compilation-start
       (format "%s --test %s:%s --verbose"
               exe (or module "") test-name)
       'epsilon-test-mode
       (lambda (_) (format "*Epsilon Test: %s*" test-name))))))

(defun epsilon-execute-module-tests (module-name)
  "Execute all tests for MODULE-NAME."
  (let ((exe (epsilon-find-executable))
        (root (epsilon-find-root)))
    (unless exe
      (user-error "Cannot find epsilon executable"))
    (let ((default-directory root))
      (compilation-start
       (format "%s --test %s --verbose" exe module-name)
       'epsilon-test-mode
       (lambda (_) (format "*Epsilon Tests: %s*" module-name))))))

(define-compilation-mode epsilon-test-mode "Epsilon-Test"
  "Compilation mode for Epsilon test results."
  (setq-local compilation-error-regexp-alist
              '((epsilon-test-fail "^FAIL.*at \\([^:]+\\):\\([0-9]+\\)" 1 2 nil 2)
                (epsilon-test-error "^ERROR.*at \\([^:]+\\):\\([0-9]+\\)" 1 2 nil 2)))
  (setq-local compilation-error-regexp-alist-alist
              '((epsilon-test-fail "^FAIL.*at \\([^:]+\\):\\([0-9]+\\)" 1 2 nil 2)
                (epsilon-test-error "^ERROR.*at \\([^:]+\\):\\([0-9]+\\)" 1 2 nil 2))))

;;; Linting

(defun epsilon-lint-module (&optional module-name)
  "Lint MODULE-NAME or the current module."
  (interactive)
  (let ((module (or module-name (epsilon-current-module-name)))
        (exe (epsilon-find-executable))
        (root (epsilon-find-root)))
    (unless exe
      (user-error "Cannot find epsilon executable"))
    (unless module
      (user-error "Cannot determine current module"))
    (let ((default-directory root))
      (compilation-start
       (format "%s --exec \"(epsilon.lint:lint-module \\\"%s\\\")\"" exe module)
       'epsilon-lint-mode
       (lambda (_) (format "*Epsilon Lint: %s*" module))))))

(defun epsilon-lint-file (&optional file)
  "Lint FILE or the current file."
  (interactive)
  (let ((file-path (or file buffer-file-name))
        (exe (epsilon-find-executable))
        (root (epsilon-find-root)))
    (unless exe
      (user-error "Cannot find epsilon executable"))
    (unless file-path
      (user-error "No file to lint"))
    (let ((default-directory root))
      (compilation-start
       (format "%s --exec \"(epsilon.lint:lint-file \\\"%s\\\")\"" exe file-path)
       'epsilon-lint-mode
       (lambda (_) (format "*Epsilon Lint: %s*" (file-name-nondirectory file-path)))))))

(define-compilation-mode epsilon-lint-mode "Epsilon-Lint"
  "Compilation mode for Epsilon lint results."
  (setq-local compilation-error-regexp-alist
              '((epsilon-lint-error "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): error:" 1 2 3 2)
                (epsilon-lint-warning "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): warning:" 1 2 3 1)
                (epsilon-lint-info "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): info:" 1 2 3 0)))
  (setq-local compilation-error-regexp-alist-alist
              '((epsilon-lint-error "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): error:" 1 2 3 2)
                (epsilon-lint-warning "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): warning:" 1 2 3 1)
                (epsilon-lint-info "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): info:" 1 2 3 0))))

;;; Flymake Integration

(defvar-local epsilon--flymake-proc nil
  "Flymake process for current buffer.")

(defun epsilon-flymake-backend (report-fn &rest _args)
  "Flymake backend for Epsilon.  REPORT-FN is the callback."
  (unless (executable-find "epsilon")
    (error "Cannot find epsilon executable for Flymake"))
  ;; Kill any existing process
  (when (process-live-p epsilon--flymake-proc)
    (kill-process epsilon--flymake-proc))
  (let* ((source (current-buffer))
         (filename buffer-file-name))
    (when filename
      (save-restriction
        (widen)
        (setq epsilon--flymake-proc
              (make-process
               :name "epsilon-flymake"
               :noquery t
               :connection-type 'pipe
               :buffer (generate-new-buffer " *epsilon-flymake*")
               :command (list (epsilon-find-executable)
                              "--exec"
                              (format "(epsilon.lint:lint-file \"%s\" :format :emacs)"
                                      filename))
               :sentinel
               (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (unwind-protect
                       (if (with-current-buffer source
                             (eq proc epsilon--flymake-proc))
                           (with-current-buffer (process-buffer proc)
                             (goto-char (point-min))
                             (let ((diags nil))
                               (while (not (eobp))
                                 (when (looking-at "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(\\w+\\): \\(.+\\)$")
                                   (let* ((line (string-to-number (match-string 2)))
                                          (col (string-to-number (match-string 3)))
                                          (type (match-string 4))
                                          (msg (match-string 5))
                                          (severity (cond
                                                     ((string= type "error") :error)
                                                     ((string= type "warning") :warning)
                                                     (t :note))))
                                     (push (flymake-make-diagnostic
                                            source
                                            (cons line col)
                                            (cons line (1+ col))
                                            severity
                                            msg)
                                           diags)))
                                 (forward-line 1))
                               (funcall report-fn (nreverse diags))))
                         (flymake-log :warning "Canceling obsolete check %s" proc))
                     (kill-buffer (process-buffer proc)))))))))))

(defun epsilon-setup-flymake ()
  "Set up Flymake for Epsilon buffers."
  (when (and epsilon-use-flymake
             (fboundp 'flymake-mode))
    (add-hook 'flymake-diagnostic-functions #'epsilon-flymake-backend nil t)
    (flymake-mode 1)))

;;; REPL Integration

(defvar epsilon-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "C-c C-c") #'epsilon-repl-interrupt)
    (define-key map (kbd "C-c C-q") #'epsilon-repl-quit)
    (define-key map (kbd "C-c C-l") #'epsilon-repl-load-file)
    (define-key map (kbd "TAB") #'completion-at-point)
    map)
  "Keymap for Epsilon REPL mode.")

(defvar epsilon-repl-prompt-regexp "^[^> \n]*> *"
  "Regexp matching the Epsilon REPL prompt.")

(defvar epsilon-repl-history-file
  (expand-file-name "epsilon-history" user-emacs-directory)
  "File for saving Epsilon REPL history.")

(define-derived-mode epsilon-repl-mode comint-mode "Epsilon-REPL"
  "Major mode for the Epsilon REPL.

\\{epsilon-repl-mode-map}"
  :group 'epsilon
  (setq comint-prompt-regexp epsilon-repl-prompt-regexp)
  (setq comint-prompt-read-only t)
  (setq comint-input-ring-file-name epsilon-repl-history-file)
  (setq comint-input-ignoredups t)
  (setq-local comment-start ";")
  (setq-local comment-end "")
  ;; Enable completion
  (add-hook 'completion-at-point-functions #'epsilon-completion-at-point nil t)
  ;; Load history
  (comint-read-input-ring t)
  ;; Save history on exit
  (add-hook 'kill-buffer-hook #'comint-write-input-ring nil t)
  ;; Syntax highlighting
  (setq font-lock-defaults '(lisp-font-lock-keywords)))

(defun epsilon-start-repl ()
  "Start an Epsilon REPL."
  (interactive)
  (let ((exe (epsilon-find-executable))
        (root (epsilon-find-root)))
    (unless exe
      (user-error "Cannot find epsilon executable"))
    (if (and (fboundp 'slime) (fboundp 'slime-connected-p))
        ;; Use SLIME if available
        (progn
          (unless (slime-connected-p)
            (let ((default-directory (or root default-directory)))
              (slime)))
          (when (slime-connected-p)
            (epsilon-setup)))
      ;; Use enhanced comint-based REPL
      (let* ((default-directory (or root default-directory))
             (buffer (get-buffer-create epsilon-repl-buffer-name)))
        (unless (comint-check-proc buffer)
          (with-current-buffer buffer
            (make-comint-in-buffer "Epsilon" buffer exe)
            (epsilon-repl-mode)))
        (pop-to-buffer buffer)))))

(defun epsilon-repl-interrupt ()
  "Interrupt the current REPL operation."
  (interactive)
  (comint-interrupt-subjob))

(defun epsilon-repl-quit ()
  "Quit the Epsilon REPL."
  (interactive)
  (comint-write-input-ring)
  (comint-send-string (get-buffer-process (current-buffer)) "(quit)\n"))

(defun epsilon-repl-load-file (file)
  "Load FILE in the Epsilon REPL."
  (interactive "fLoad file: ")
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (comint-send-string proc (format "(load \"%s\")\n" (expand-file-name file))))))

(defun epsilon-send-region-to-repl (start end)
  "Send region from START to END to the Epsilon REPL."
  (interactive "r")
  (let ((buffer (get-buffer epsilon-repl-buffer-name)))
    (if (and buffer (get-buffer-process buffer))
        (let ((code (buffer-substring-no-properties start end)))
          (with-current-buffer buffer
            (goto-char (point-max))
            (comint-send-string (get-buffer-process buffer)
                               (concat code "\n")))
          (display-buffer buffer))
      (user-error "No Epsilon REPL running. Start one with `epsilon-start-repl'"))))

(defun epsilon-send-defun-to-repl ()
  "Send the current defun to the Epsilon REPL."
  (interactive)
  (save-excursion
    (let ((start (progn (beginning-of-defun) (point)))
          (end (progn (end-of-defun) (point))))
      (epsilon-send-region-to-repl start end))))

(defun epsilon-eval-expression (expr)
  "Evaluate EXPR in the Epsilon environment."
  (interactive "sEpsilon eval: ")
  (let ((exe (epsilon-find-executable))
        (root (epsilon-find-root)))
    (unless exe
      (user-error "Cannot find epsilon executable"))
    (let ((default-directory (or root default-directory)))
      (shell-command
       (format "%s --quiet --eval \"%s\"" exe (replace-regexp-in-string "\"" "\\\\\"" expr))))))

;;; Eldoc Support

(defvar epsilon-eldoc-cache (make-hash-table :test 'equal)
  "Cache for eldoc documentation.")

(defconst epsilon-builtin-functions
  '(;; Core functions with signatures
    ("defun" . "(defun NAME LAMBDA-LIST [DOCSTRING] BODY...)")
    ("defmacro" . "(defmacro NAME LAMBDA-LIST [DOCSTRING] BODY...)")
    ("defvar" . "(defvar NAME [INITIAL-VALUE [DOCSTRING]])")
    ("defparameter" . "(defparameter NAME INITIAL-VALUE [DOCSTRING])")
    ("defconstant" . "(defconstant NAME VALUE [DOCSTRING])")
    ("defclass" . "(defclass NAME (SUPERCLASSES...) (SLOTS...) OPTIONS...)")
    ("defgeneric" . "(defgeneric NAME LAMBDA-LIST OPTIONS...)")
    ("defmethod" . "(defmethod NAME QUALIFIERS SPECIALIZED-LAMBDA-LIST BODY...)")
    ("defpackage" . "(defpackage NAME OPTIONS...)")
    ("lambda" . "(lambda LAMBDA-LIST BODY...)")
    ("let" . "(let ((VAR VALUE)...) BODY...)")
    ("let*" . "(let* ((VAR VALUE)...) BODY...)")
    ("if" . "(if TEST THEN [ELSE])")
    ("when" . "(when TEST BODY...)")
    ("unless" . "(unless TEST BODY...)")
    ("cond" . "(cond (TEST BODY...)...)")
    ("case" . "(case KEYFORM (KEYS BODY...)...)")
    ("loop" . "(loop CLAUSE...)")
    ("dolist" . "(dolist (VAR LIST [RESULT]) BODY...)")
    ("dotimes" . "(dotimes (VAR COUNT [RESULT]) BODY...)")
    ("mapcar" . "(mapcar FUNCTION LIST &REST MORE-LISTS)")
    ("reduce" . "(reduce FUNCTION SEQUENCE &KEY :KEY :FROM-END :START :END :INITIAL-VALUE)")
    ("format" . "(format DESTINATION CONTROL-STRING &REST ARGS)")
    ("funcall" . "(funcall FUNCTION &REST ARGS)")
    ("apply" . "(apply FUNCTION &REST ARGS)")
    ;; Epsilon test framework
    ("deftest" . "(deftest NAME &KEY :DESCRIPTION :SUITE BODY...)")
    ("defsuite" . "(defsuite NAME &KEY :DESCRIPTION)")
    ("is" . "(is FORM [MESSAGE])")
    ("is-true" . "(is-true FORM [MESSAGE])")
    ("is-false" . "(is-false FORM [MESSAGE])")
    ("signals" . "(signals CONDITION-TYPE FORM)")
    ;; Epsilon map functions
    ("map:make-map" . "(map:make-map &REST KEY-VALUE-PAIRS)")
    ("map:get" . "(map:get MAP KEY [DEFAULT])")
    ("map:assoc" . "(map:assoc MAP KEY VALUE)")
    ("map:dissoc" . "(map:dissoc MAP KEY)")
    ("map:keys" . "(map:keys MAP)")
    ("map:vals" . "(map:vals MAP)")
    ;; Epsilon sequence functions
    ("seq:map" . "(seq:map FUNCTION SEQUENCE)")
    ("seq:filter" . "(seq:filter PREDICATE SEQUENCE)")
    ("seq:reduce" . "(seq:reduce FUNCTION SEQUENCE &KEY :INITIAL-VALUE)")
    ("seq:take" . "(seq:take N SEQUENCE)")
    ("seq:drop" . "(seq:drop N SEQUENCE)"))
  "Built-in function signatures for eldoc.")

(defun epsilon-eldoc-function ()
  "Return eldoc documentation for symbol at point."
  (let ((sym (thing-at-point 'symbol t)))
    (when sym
      (let ((doc (or (gethash sym epsilon-eldoc-cache)
                     (cdr (assoc sym epsilon-builtin-functions)))))
        (when doc
          (format "%s" doc))))))

(defun epsilon-setup-eldoc ()
  "Set up eldoc for Epsilon buffers."
  (when (fboundp 'eldoc-mode)
    (setq-local eldoc-documentation-function #'epsilon-eldoc-function)
    (eldoc-mode 1)))

;;; Company-mode Backend

(defvar epsilon-completion-keywords
  '("defun" "defmacro" "defvar" "defparameter" "defconstant"
    "defclass" "defgeneric" "defmethod" "defpackage" "deftest" "defsuite"
    "lambda" "let" "let*" "flet" "labels" "macrolet" "symbol-macrolet"
    "if" "when" "unless" "cond" "case" "ecase" "typecase" "etypecase"
    "loop" "do" "do*" "dolist" "dotimes" "prog" "prog*"
    "block" "return" "return-from" "tagbody" "go"
    "catch" "throw" "unwind-protect"
    "handler-case" "handler-bind" "restart-case" "restart-bind"
    "multiple-value-bind" "multiple-value-call" "multiple-value-prog1"
    "progn" "prog1" "prog2"
    "setq" "setf" "psetq" "psetf"
    "and" "or" "not"
    "eq" "eql" "equal" "equalp"
    "car" "cdr" "cons" "list" "list*" "append"
    "first" "second" "third" "rest" "nth" "nthcdr"
    "mapcar" "mapc" "maplist" "mapl" "mapcan" "mapcon"
    "reduce" "remove" "remove-if" "remove-if-not"
    "find" "find-if" "find-if-not" "position" "count"
    "sort" "stable-sort" "merge"
    "format" "print" "princ" "prin1" "write"
    "read" "read-line" "read-char"
    "funcall" "apply" "values" "values-list"
    ;; Epsilon specific
    "is" "is-true" "is-false" "signals" "with-fixture")
  "Keywords for Epsilon completion.")

(defvar epsilon-module-symbols-cache nil
  "Cache for module symbols.")

(defun epsilon-get-module-symbols ()
  "Get exported symbols from loaded Epsilon modules."
  (or epsilon-module-symbols-cache
      (setq epsilon-module-symbols-cache
            (append epsilon-completion-keywords
                    ;; Add common epsilon.* symbols
                    '("epsilon.map:make-map" "epsilon.map:get" "epsilon.map:assoc"
                      "epsilon.map:dissoc" "epsilon.map:keys" "epsilon.map:vals"
                      "epsilon.seq:map" "epsilon.seq:filter" "epsilon.seq:reduce"
                      "epsilon.str:split" "epsilon.str:join" "epsilon.str:trim"
                      "epsilon.json:parse" "epsilon.json:encode"
                      "epsilon.http:http-get" "epsilon.http:http-post"
                      "epsilon.test:deftest" "epsilon.test:defsuite"
                      "epsilon.test:is" "epsilon.test:is-true")))))

(defun epsilon-completion-at-point ()
  "Completion at point function for Epsilon."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            (epsilon-get-module-symbols)
            :exclusive 'no
            :company-docsig #'epsilon-eldoc-function))))

(defun epsilon-company-backend (command &optional arg &rest _ignored)
  "Company backend for Epsilon completion.
COMMAND is the company command, ARG is the prefix."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'epsilon-company-backend))
    (prefix (and (derived-mode-p 'lisp-mode 'epsilon-mode 'epsilon-repl-mode)
                 (company-grab-symbol)))
    (candidates
     (let ((symbols (epsilon-get-module-symbols)))
       (cl-remove-if-not
        (lambda (c) (string-prefix-p arg c))
        symbols)))
    (annotation
     (let ((doc (cdr (assoc arg epsilon-builtin-functions))))
       (when doc (concat " " doc))))
    (meta
     (cdr (assoc arg epsilon-builtin-functions)))
    (sorted t)))

(defun epsilon-setup-company ()
  "Set up company-mode for Epsilon buffers."
  (when (fboundp 'company-mode)
    (setq-local company-backends
                (cons 'epsilon-company-backend
                      (default-value 'company-backends)))
    (company-mode 1)))

;;; Module Loading

(defun epsilon-load-current-module ()
  "Load the current module via the command line."
  (interactive)
  (let ((module (epsilon-current-module-name))
        (exe (epsilon-find-executable))
        (root (epsilon-find-root)))
    (unless exe
      (user-error "Cannot find epsilon executable"))
    (unless module
      (user-error "Cannot determine current module"))
    (let ((default-directory root))
      (shell-command
       (format "%s --module %s --eval t" exe module))
      (message "Module %s loaded" module))))

;;; SWANK Integration

(defun epsilon-setup-swank-environment (project)
  "Set up SWANK environment for Epsilon PROJECT."
  (when (and (fboundp 'slime-connected-p) (slime-connected-p))
    (slime-eval-async
     `(epsilon-swank:configure-environment
       '(:project-path ,(plist-get project :path)
         :module-paths ,epsilon-module-search-path
         :project-name ,(plist-get project :name)))
     (lambda (result)
       (message "Epsilon environment configured: %s" result)))))

(defun epsilon-configure-search-path ()
  "Interactively configure the module search path."
  (interactive)
  (let ((current-path (mapconcat #'identity epsilon-module-search-path ":")))
    (let ((new-path (read-string "Module search path (colon-separated): " current-path)))
      (setq epsilon-module-search-path (split-string new-path ":" t))
      (when epsilon-current-project
        (epsilon-configure-project epsilon-current-project))
      (message "Module search path updated"))))

;;; Navigation

(defun epsilon-jump-to-test ()
  "Jump to the test definition at point."
  (interactive)
  (let ((test-name (epsilon-find-test-at-point)))
    (when test-name
      (message "Test: %s" test-name))))

(defun epsilon-find-definition ()
  "Find definition of symbol at point."
  (interactive)
  (if (and (fboundp 'slime-connected-p) (slime-connected-p))
      (call-interactively #'slime-edit-definition)
    (message "SLIME not connected - connect for symbol navigation")))

(defun epsilon-describe-symbol ()
  "Describe the symbol at point."
  (interactive)
  (if (and (fboundp 'slime-connected-p) (slime-connected-p))
      (call-interactively #'slime-describe-symbol)
    (message "SLIME not connected - connect for symbol lookup")))

;;; Imenu Support

(defvar epsilon-imenu-generic-expression
  '(("Tests" "^(deftest\\s-+\\([[:alnum:]-_]+\\)" 1)
    ("Functions" "^(defun\\s-+\\([[:alnum:]-_]+\\)" 1)
    ("Macros" "^(defmacro\\s-+\\([[:alnum:]-_]+\\)" 1)
    ("Variables" "^(defvar\\s-+\\([[:alnum:]-_*]+\\)" 1)
    ("Parameters" "^(defparameter\\s-+\\([[:alnum:]-_*]+\\)" 1)
    ("Constants" "^(defconstant\\s-+\\([[:alnum:]-_+]+\\)" 1)
    ("Classes" "^(defclass\\s-+\\([[:alnum:]-_]+\\)" 1)
    ("Generics" "^(defgeneric\\s-+\\([[:alnum:]-_]+\\)" 1)
    ("Methods" "^(defmethod\\s-+\\([[:alnum:]-_]+\\)" 1)
    ("Packages" "^(defpackage\\s-+\\([[:alnum:].-]+\\)" 1))
  "Imenu expression for Epsilon Lisp files.")

(defun epsilon-setup-imenu ()
  "Set up Imenu for Epsilon buffers."
  (setq-local imenu-generic-expression epsilon-imenu-generic-expression)
  (setq-local imenu-case-fold-search nil))

;;; Test Results Mode

(defvar epsilon-test-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    (define-key map "r" #'epsilon-rerun-tests)
    (define-key map "g" #'recompile)
    map)
  "Keymap for Epsilon test results mode.")

(define-derived-mode epsilon-test-results-mode special-mode "Epsilon-Tests"
  "Major mode for displaying Epsilon test results."
  (setq buffer-read-only t))

(defun epsilon-rerun-tests ()
  "Rerun the last test execution."
  (interactive)
  (when epsilon-last-compilation-buffer
    (with-current-buffer epsilon-last-compilation-buffer
      (recompile))))

;;; Minor Mode Definition

;;;###autoload
(define-minor-mode epsilon-mode
  "Minor mode for Epsilon Lisp development.

Features:
- Project management with module.lisp detection
- Module search path configuration
- Test execution at point with rich results display
- Flymake integration for real-time linting
- Eldoc support for function signatures
- Company-mode completion backend
- Enhanced REPL with history and completion

Key bindings:
\\{epsilon-mode-map}"
  :lighter " E"
  :keymap epsilon-mode-map
  :group 'epsilon
  (if epsilon-mode
      (progn
        (epsilon-setup-imenu)
        (epsilon-setup-eldoc)
        (epsilon-setup-company)
        (when epsilon-use-flymake
          (epsilon-setup-flymake))
        ;; Set up completion at point
        (add-hook 'completion-at-point-functions #'epsilon-completion-at-point nil t)
        (when (called-interactively-p 'interactive)
          (epsilon-refresh-projects)
          (when (and (not epsilon-current-project)
                     epsilon-project-list)
            (epsilon-select-project))))
    ;; Cleanup when disabled
    (remove-hook 'completion-at-point-functions #'epsilon-completion-at-point t)
    (when (and epsilon-use-flymake (fboundp 'flymake-mode))
      (flymake-mode -1))
    (when (fboundp 'eldoc-mode)
      (eldoc-mode -1))))

;;;###autoload
(defun epsilon-setup ()
  "Set up Epsilon mode and load SWANK extensions."
  (interactive)
  (when (and (fboundp 'slime-connected-p) (slime-connected-p))
    (slime-eval-async
     `(progn
        (load ,(expand-file-name "epsilon-swank.lisp"
                                 (file-name-directory
                                  (or load-file-name buffer-file-name))))
        t)
     (lambda (_result)
       (message "Epsilon SWANK extensions loaded")))))

;;;###autoload
(defun epsilon-mode-maybe ()
  "Enable `epsilon-mode' if in an Epsilon project."
  (when (and buffer-file-name
             (string-match-p "\\.lisp\\'" buffer-file-name)
             (epsilon-find-root))
    (epsilon-mode 1)))

(provide 'epsilon-mode)

;;; epsilon-mode.el ends here

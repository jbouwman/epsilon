;;; epsilon-mode.el --- Epsilon Lisp development mode for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jesse Bouwman

;; Author: Jesse Bouwman
;; Keywords: lisp, epsilon, development
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (slime "2.27"))

;;; Commentary:

;; Epsilon-mode provides development support for the Epsilon Lisp framework.
;; It integrates with SLIME/SWANK to provide:
;; - Project management with module.lisp detection
;; - Module search path configuration  
;; - Test execution at point
;; - Module loading and dependency management

;;; Code:

(require 'cl-lib)
(require 'cl-macs)

;; Optional SLIME integration - only require if available
(when (locate-library "slime")
  (require 'slime))

;; Declare SLIME functions to avoid compiler warnings
(declare-function slime-connected-p "slime")
(declare-function slime-eval-async "slime")

;;; Customization

(defgroup epsilon nil
  "Epsilon Lisp development support."
  :group 'lisp
  :prefix "epsilon-")

(defcustom epsilon-executable "epsilon"
  "Path to the epsilon executable."
  :type 'string
  :group 'epsilon)

(defcustom epsilon-default-module-paths '("./modules" "../modules")
  "Default module search paths relative to project root."
  :type '(repeat string)
  :group 'epsilon)

(defcustom epsilon-test-buffer-name "*Epsilon Tests*"
  "Name of buffer for test output."
  :type 'string
  :group 'epsilon)

;;; Variables

(defvar epsilon-current-project nil
  "Currently selected Epsilon project.")

(defvar epsilon-project-list nil
  "List of discovered Epsilon projects.")

(defvar epsilon-module-search-path nil
  "Current module search path.")

(defvar epsilon-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e p") #'epsilon-select-project)
    (define-key map (kbd "C-c C-e t") #'epsilon-run-test-at-point)
    (define-key map (kbd "C-c C-e T") #'epsilon-run-module-tests)
    (define-key map (kbd "C-c C-e l") #'epsilon-load-current-module)
    (define-key map (kbd "C-c C-e s") #'epsilon-configure-search-path)
    (define-key map (kbd "C-c C-e j") #'epsilon-jump-to-test)
    (define-key map (kbd "C-c C-e r") #'epsilon-refresh-projects)
    map)
  "Keymap for Epsilon mode.")

;;; Project Management

(defun epsilon-find-projects (&optional directory)
  "Find all Epsilon projects starting from DIRECTORY.
An Epsilon project is identified by the presence of a module.lisp file."
  (let ((search-dir (or directory default-directory))
        (projects '()))
    (dolist (file (directory-files-recursively search-dir "module\\.lisp$"))
      (let* ((project-dir (file-name-directory file))
             (module-info (epsilon-parse-module-file file)))
        (when module-info
          (push (list :name (plist-get module-info :name)
                     :path project-dir
                     :module-file file
                     :info module-info)
                projects))))
    projects))

(defun epsilon-parse-module-file (file)
  "Parse an Epsilon module.lisp file and extract metadata."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (let ((info '())
                (eof (gensym)))
            (loop for form = (read (current-buffer) nil eof)
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
                                              (plist-get p :path)))
                                     projects))
               (selection (completing-read "Select Epsilon project: " project-names)))
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
      ;; Set up module search path
      (setq epsilon-module-search-path
            (mapcar (lambda (path)
                     (expand-file-name path project-dir))
                   epsilon-default-module-paths))
      ;; Configure SLIME if connected
      (when (slime-connected-p)
        (epsilon-setup-swank-environment project)))))

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
    (when (looking-at "(deftest\\s-+\\(\\sw+\\)")
      (match-string 1))))

(defun epsilon-run-module-tests ()
  "Run all tests for the current module."
  (interactive)
  (let ((module-name (epsilon-current-module-name)))
    (if module-name
        (epsilon-execute-module-tests module-name)
      (message "Could not determine current module"))))

(defun epsilon-current-module-name ()
  "Determine the current module name from the file path or package."
  (cond
   ;; Try to extract from current package
   ((and (eq major-mode 'lisp-mode)
         buffer-file-name
         (save-excursion
           (goto-char (point-min))
           (re-search-forward "(defpackage\\s-+\\(\\S-+\\)" nil t)))
    (let ((package-name (match-string 1)))
      ;; Convert package name to module name
      (when (string-match "^\\([^.]+\\.[^.]+\\)" package-name)
        (match-string 1 package-name))))
   ;; Try to extract from file path
   ((and epsilon-current-project buffer-file-name)
    (let* ((project-path (plist-get epsilon-current-project :path))
           (relative-path (file-relative-name buffer-file-name project-path)))
      (when (string-match "^modules/\\([^/]+\\)/" relative-path)
        (concat (plist-get epsilon-current-project :name) "." (match-string 1 relative-path)))))
   (t nil)))

(defun epsilon-execute-test (test-name)
  "Execute a single test TEST-NAME via SWANK."
  (if (and (fboundp 'slime-connected-p) (slime-connected-p))
      (slime-eval-async
       `(epsilon-swank:run-test ,test-name)
       (lambda (result)
         (epsilon-display-test-results result)))
    (message "SLIME not connected")))

(defun epsilon-execute-module-tests (module-name)
  "Execute all tests for MODULE-NAME via SWANK."
  (if (and (fboundp 'slime-connected-p) (slime-connected-p))
      (slime-eval-async
       `(epsilon-swank:run-module-tests ,module-name)
       (lambda (result)
         (epsilon-display-test-results result)))
    (message "SLIME not connected")))

(defun epsilon-display-test-results (results)
  "Display test RESULTS in a dedicated buffer."
  (let ((buffer (get-buffer-create epsilon-test-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Epsilon Test Results - %s\n\n" 
                       (format-time-string "%Y-%m-%d %H:%M:%S")))
        (epsilon-format-test-results results)
        (goto-char (point-min))
        (epsilon-test-results-mode)))
    (display-buffer buffer)))

(defun epsilon-format-test-results (results)
  "Format test RESULTS for display."
  (dolist (result results)
    (let ((name (plist-get result :name))
          (status (plist-get result :status))
          (message (plist-get result :message))
          (time (plist-get result :time)))
      (insert (format "%s %s (%.3fs)\n" 
                     (if (eq status :pass) "✓" "✗")
                     name
                     (or time 0.0)))
      (when message
        (insert (format "  %s\n" message)))
      (insert "\n"))))

;;; Module Loading

(defun epsilon-load-current-module ()
  "Load the current module via SWANK."
  (interactive)
  (let ((module-name (epsilon-current-module-name)))
    (if module-name
        (if (and (fboundp 'slime-connected-p) (slime-connected-p))
            (slime-eval-async
             `(epsilon-swank:load-module ,module-name)
             (lambda (result)
               (message "Module %s loaded: %s" module-name result)))
          (message "SLIME not connected"))
      (message "Could not determine current module"))))

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

;;; Test Results Mode

(defvar epsilon-test-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    (define-key map "r" #'epsilon-rerun-tests)
    (define-key map "j" #'epsilon-jump-to-test-from-results)
    map)
  "Keymap for Epsilon test results mode.")

(define-derived-mode epsilon-test-results-mode special-mode "Epsilon-Tests"
  "Major mode for displaying Epsilon test results."
  (setq buffer-read-only t))

(defun epsilon-rerun-tests ()
  "Rerun the last test execution."
  (interactive)
  (message "Rerunning tests..."))

(defun epsilon-jump-to-test ()
  "Jump to the test definition at point."
  (interactive)
  (let ((test-name (epsilon-find-test-at-point)))
    (when test-name
      (message "Jumping to test: %s" test-name))))

(defun epsilon-jump-to-test-from-results ()
  "Jump to test definition from results buffer."
  (interactive)
  (message "Jump to test from results"))

;;; Minor Mode Definition

;;;###autoload
(define-minor-mode epsilon-mode
  "Minor mode for Epsilon Lisp development."
  :lighter " ε"
  :keymap epsilon-mode-map
  :group 'epsilon
  (when epsilon-mode
    (epsilon-refresh-projects)
    (when (and (not epsilon-current-project) 
               epsilon-project-list
               (called-interactively-p 'interactive))
      (epsilon-select-project))))

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

(provide 'epsilon-mode)

;;; epsilon-mode.el ends here
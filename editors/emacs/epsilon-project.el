;;; epsilon-project.el --- Project.el integration for Epsilon -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; Author: Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides Project.el integration for Epsilon workspaces.
;; It enables project-aware operations like finding files, running tests,
;; and managing modules.

;;; Code:

(require 'project)
(require 'cl-lib)
(require 'epsilon-client)

;;; Customization

(defgroup epsilon-project nil
  "Epsilon project settings."
  :group 'epsilon
  :prefix "epsilon-project-")

(defcustom epsilon-project-root-markers
  '("workspace.lisp" "module.lisp" ".epsilon-workspace")
  "Files that identify an Epsilon project root."
  :type '(repeat string)
  :group 'epsilon-project)

;;; Project Backend

(defun epsilon-project-find-functions (dir)
  "Project.el backend for Epsilon workspaces.
Return project instance if DIR is in an Epsilon workspace."
  (when-let ((root (epsilon-project--find-workspace-root dir)))
    (cons 'epsilon-project root)))

(cl-defmethod project-root ((project (head epsilon-project)))
  "Return root of Epsilon PROJECT."
  (cdr project))

(cl-defmethod project-name ((project (head epsilon-project)))
  "Return name of Epsilon PROJECT."
  (let ((root (project-root project)))
    (file-name-nondirectory (directory-file-name root))))

(cl-defmethod project-files ((project (head epsilon-project)) &optional _dirs)
  "Return files in Epsilon PROJECT."
  (let ((root (project-root project)))
    (epsilon-project--collect-files root)))

(cl-defmethod project-ignores ((project (head epsilon-project)) _dir)
  "Return ignored patterns for Epsilon PROJECT."
  (list ".git" ".hg" ".svn" "*.fasl" "*.lx64fsl" "*.dx64fsl"
        "__pycache__" "node_modules" "target" "build" "dist"))

(cl-defmethod project-buffers ((project (head epsilon-project)))
  "Return buffers belonging to Epsilon PROJECT."
  (let ((root (file-truename (project-root project))))
    (cl-remove-if-not
     (lambda (buf)
       (when-let ((file (buffer-file-name buf)))
         (string-prefix-p root (file-truename file))))
     (buffer-list))))

;;; Workspace Detection

(defun epsilon-project--find-workspace-root (dir)
  "Find Epsilon workspace root containing DIR."
  (let ((dir (file-truename dir)))
    (cl-some
     (lambda (marker)
       (locate-dominating-file dir marker))
     epsilon-project-root-markers)))

(defun epsilon-project--collect-files (root)
  "Collect all source files under ROOT."
  (let ((files '()))
    ;; Try to get file list from ELS
    (condition-case nil
        (let ((response (epsilon-request-sync
                         "module" "workspace-files"
                         `((path . ,root)))))
          (when (equal "ok" (alist-get 'status response))
            (setq files (alist-get 'files response))))
      (error
       ;; Fallback to filesystem scan
       (setq files (epsilon-project--scan-files root))))
    files))

(defun epsilon-project--scan-files (root)
  "Scan ROOT for Lisp source files."
  (let ((files '()))
    (dolist (ext '("lisp" "lsp" "cl" "asd"))
      (setq files
            (append files
                    (directory-files-recursively
                     root (format "\\.%s\\'" ext)))))
    files))

;;; Workspace Info

(defun epsilon-project-workspace-info ()
  "Display information about the current workspace."
  (interactive)
  (if-let ((project (project-current)))
      (let ((root (project-root project)))
        (epsilon-request
         "module" "workspace-info"
         `((path . ,root))
         (lambda (response)
           (if (equal "ok" (alist-get 'status response))
               (epsilon-project--show-workspace-info response)
             (message "Failed to get workspace info")))))
    (message "Not in an Epsilon project")))

(defun epsilon-project--show-workspace-info (response)
  "Show workspace info from RESPONSE."
  (with-help-window "*epsilon-workspace*"
    (let ((name (alist-get 'name response))
          (modules (alist-get 'modules response)))
      (princ (format "Workspace: %s\n\n" name))
      (princ "Modules:\n")
      (dolist (module modules)
        (let ((mod-name (alist-get 'name module))
              (version (alist-get 'version module))
              (path (alist-get 'path module)))
          (princ (format "  %s (%s)\n" mod-name version))
          (princ (format "    Path: %s\n" path)))))))

;;; Module Commands

(defun epsilon-project--list-modules ()
  "Return list of modules in current workspace."
  (when-let ((project (project-current)))
    (let ((root (project-root project)))
      (condition-case nil
          (let ((response (epsilon-request-sync
                           "module" "list-modules"
                           `((workspace . ,root)))))
            (when (equal "ok" (alist-get 'status response))
              (mapcar (lambda (m) (alist-get 'name m))
                      (alist-get 'modules response))))
        (error nil)))))

(defun epsilon-load-module (module)
  "Load Epsilon MODULE with dependencies."
  (interactive
   (list (completing-read "Module: "
                          (epsilon-project--list-modules)
                          nil t)))
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
   (list (completing-read "Module: "
                          (epsilon-project--list-modules)
                          nil t)))
  (epsilon-request
   "module" "compile-module"
   `((module . ,module))
   (lambda (response)
     (if (equal "ok" (alist-get 'status response))
         (let ((warnings (alist-get 'warnings response)))
           (if warnings
               (message "Compiled with %d warnings" (length warnings))
             (message "Compiled successfully")))
       (message "Failed: %s"
                (alist-get 'message (alist-get 'error response)))))))

(defun epsilon-reload-module (module)
  "Reload Epsilon MODULE."
  (interactive
   (list (completing-read "Module: "
                          (epsilon-project--list-modules)
                          nil t)))
  (epsilon-request
   "module" "reload-module"
   `((module . ,module))
   (lambda (response)
     (if (equal "ok" (alist-get 'status response))
         (message "Reloaded: %s" module)
       (message "Failed: %s"
                (alist-get 'message (alist-get 'error response)))))))

;;; Package Commands

(defun epsilon-project--list-packages ()
  "Return list of packages in current session."
  (condition-case nil
      (let ((response (epsilon-request-sync "module" "list-packages")))
        (when (equal "ok" (alist-get 'status response))
          (alist-get 'packages response)))
    (error nil)))

(defun epsilon-in-package (package)
  "Switch to PACKAGE."
  (interactive
   (list (completing-read "Package: "
                          (epsilon-project--list-packages)
                          nil t)))
  (epsilon-request
   "eval" "in-package"
   `((package . ,package))
   (lambda (response)
     (if (equal "ok" (alist-get 'status response))
         (message "Now in package: %s" package)
       (message "Failed: %s"
                (alist-get 'message (alist-get 'error response)))))))

;;; Register with Project.el

;;;###autoload
(defun epsilon-project-setup ()
  "Set up Epsilon project integration."
  (add-hook 'project-find-functions #'epsilon-project-find-functions))

;;;###autoload
(defun epsilon-project-teardown ()
  "Remove Epsilon project integration."
  (remove-hook 'project-find-functions #'epsilon-project-find-functions))

;; Auto-register
(epsilon-project-setup)

(provide 'epsilon-project)

;;; epsilon-project.el ends here

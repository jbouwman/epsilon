;;; epsilon-mode-test.el --- Tests for epsilon-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Automated tests for epsilon-mode functionality

;;; Code:

(require 'ert)

;; Mock SLIME functions if not available
(unless (fboundp 'slime-connected-p)
  (defun slime-connected-p () nil)
  (defun slime-eval-async (form callback) 
    (funcall callback '((:name "mock-test" :status :pass :time 0.001)))))

(require 'epsilon-mode)

;;; Test Configuration

(defvar epsilon-test-project-dir
  (expand-file-name "test-project" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing test project.")

(defvar epsilon-test-data-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing test data.")

;;; Helper Functions

(defmacro epsilon-test-with-temp-buffer (content &rest body)
  "Execute BODY in a temporary buffer containing CONTENT."
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     (lisp-mode)
     ;; Set up epsilon-mode manually to avoid interactive prompts
     (setq epsilon-mode t)
     ,@body))

(defmacro epsilon-test-with-project (&rest body)
  "Execute BODY with epsilon-test-project-dir as the current project."
  `(let ((epsilon-current-project 
          (list :name "test-project"
                :path epsilon-test-project-dir
                :module-file (expand-file-name "module.lisp" epsilon-test-project-dir))))
     ,@body))

;;; Project Management Tests

(ert-deftest epsilon-test-parse-module-file ()
  "Test parsing of module.lisp files."
  (let ((module-file (expand-file-name "module.lisp" epsilon-test-project-dir)))
    (should (file-exists-p module-file))
    (let ((module-info (epsilon-parse-module-file module-file)))
      (should module-info)
      (should (string= (plist-get module-info :name) "test-project"))
      (should (string= (plist-get module-info :version) "0.1.0")))))

(ert-deftest epsilon-test-find-projects ()
  "Test project discovery."
  (let ((projects (epsilon-find-projects epsilon-test-data-dir)))
    (should (>= (length projects) 1))
    (let ((test-project (cl-find-if (lambda (p) 
                                     (string= (plist-get p :name) "test-project"))
                                   projects)))
      (should test-project)
      (should (string= (plist-get test-project :path) 
                      (file-name-as-directory epsilon-test-project-dir))))))

;;; Test Discovery Tests

(ert-deftest epsilon-test-find-test-at-point ()
  "Test finding test function at point."
  (eval (epsilon-test-with-temp-buffer
         "(deftest my-test-function
  \"Test docstring\"
  (is (= 1 1)))"
         (goto-char (point-min))
         (forward-line 0)
         (should (string= (epsilon-find-test-at-point) "my-test-function")))))

(ert-deftest epsilon-test-find-test-at-point-no-test ()
  "Test finding test when not at a test function."
  (eval (epsilon-test-with-temp-buffer
         "(defun regular-function ()
  \"Not a test\"
  42)"
         (goto-char (point-min))
         (should (null (epsilon-find-test-at-point))))))

;;; Module Name Detection Tests

(ert-deftest epsilon-test-current-module-name-from-package ()
  "Test module name detection from package declaration."
  (eval (epsilon-test-with-temp-buffer
         "(defpackage epsilon.test.example
  (:use cl))"
         (should (string= (epsilon-current-module-name) "epsilon.test")))))

(ert-deftest epsilon-test-current-module-name-from-project ()
  "Test module name detection from project path."
  (epsilon-test-with-project
   (with-temp-buffer
     (setq buffer-file-name (expand-file-name "tests/test-tests.lisp" epsilon-test-project-dir))
     (lisp-mode)
     (epsilon-mode 1)
     ;; Should detect module name from file path
     (let ((module-name (epsilon-current-module-name)))
       (should (string-match "test-project" (or module-name "")))))))

;;; Configuration Tests

(ert-deftest epsilon-test-configure-project ()
  "Test project configuration."
  (let ((test-project (list :name "test-project"
                           :path epsilon-test-project-dir)))
    (epsilon-configure-project test-project)
    (should epsilon-module-search-path)
    (should (cl-some (lambda (path) 
                      (string-match-p "modules" path))
                    epsilon-module-search-path))))

;;; SWANK Integration Tests (Mock)

(ert-deftest epsilon-test-swank-not-connected ()
  "Test behavior when SWANK is not connected."
  ;; Mock slime-connected-p to return nil
  (cl-letf (((symbol-function 'slime-connected-p) (lambda () nil)))
    (should-not (slime-connected-p))
    ;; Test commands that require SWANK
    (epsilon-test-with-project
     (let ((module-name "test-project"))
       ;; These should not error, just show messages
       (epsilon-execute-test "test-add-numbers")
       (epsilon-execute-module-tests module-name)
       (epsilon-load-current-module)))))

;;; Integration Tests

(ert-deftest epsilon-test-mode-activation ()
  "Test epsilon-mode activation."
  (with-temp-buffer
    (lisp-mode)
    (epsilon-mode 1)
    (should epsilon-mode)
    (should (keymapp epsilon-mode-map))
    ;; Test key bindings exist
    (should (lookup-key epsilon-mode-map (kbd "C-c C-e p")))
    (should (lookup-key epsilon-mode-map (kbd "C-c C-e t")))
    (should (lookup-key epsilon-mode-map (kbd "C-c C-e T")))))

(ert-deftest epsilon-test-refresh-projects ()
  "Test project refresh functionality."
  ;; Clear project list
  (setq epsilon-project-list nil)
  ;; Refresh projects
  (let ((projects (epsilon-refresh-projects)))
    (should (listp projects))
    (should epsilon-project-list)
    (should (>= (length epsilon-project-list) 1))))

;;; Test Results Formatting Tests

(ert-deftest epsilon-test-format-test-results ()
  "Test formatting of test results."
  (let ((results (list (list :name "test-pass" :status :pass :time 0.001)
                      (list :name "test-fail" :status :fail :time 0.002 
                           :message "Test failed"))))
    (with-temp-buffer
      (epsilon-format-test-results results)
      (let ((content (buffer-string)))
        (should (string-match-p "✓ test-pass" content))
        (should (string-match-p "✗ test-fail" content))
        (should (string-match-p "Test failed" content))
        (should (string-match-p "0.001s" content))))))

;;; Error Handling Tests

(ert-deftest epsilon-test-parse-invalid-module-file ()
  "Test handling of invalid module.lisp files."
  (let ((temp-file (make-temp-file "epsilon-test-" nil ".lisp")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "invalid lisp content ("))
          (should (null (epsilon-parse-module-file temp-file))))
      (delete-file temp-file))))

(ert-deftest epsilon-test-nonexistent-module-file ()
  "Test handling of nonexistent module files."
  (should (null (epsilon-parse-module-file "/nonexistent/path/module.lisp"))))

;;; Test Runner

(defun epsilon-run-tests ()
  "Run all epsilon-mode tests."
  (interactive)
  (ert-run-tests-batch "epsilon-test-*"))

(defun epsilon-run-tests-interactively ()
  "Run epsilon-mode tests interactively."
  (interactive)
  (ert "epsilon-test-*"))

;;; Test Utilities for Manual Testing

(defun epsilon-test-setup-mock-project ()
  "Set up a mock project for manual testing."
  (interactive)
  (setq epsilon-current-project 
        (list :name "test-project"
              :path epsilon-test-project-dir
              :module-file (expand-file-name "module.lisp" epsilon-test-project-dir)))
  (epsilon-configure-project epsilon-current-project)
  (message "Mock project configured: %s" epsilon-test-project-dir))

(provide 'epsilon-mode-test)

;;; epsilon-mode-test.el ends here
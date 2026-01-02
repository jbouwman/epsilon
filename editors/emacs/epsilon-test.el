;;; epsilon-test.el --- Test runner for Epsilon -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; Author: Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides test runner integration for Epsilon.  It supports
;; discovering and running tests at various scopes (module, file, test),
;; and displays results in a compilation-like buffer.

;;; Code:

(require 'epsilon-client)
(require 'compile)
(require 'ansi-color)

;;; Customization

(defgroup epsilon-test nil
  "Epsilon test runner settings."
  :group 'epsilon
  :prefix "epsilon-test-")

(defcustom epsilon-test-verbose t
  "Show verbose test output."
  :type 'boolean
  :group 'epsilon-test)

(defcustom epsilon-test-show-passed nil
  "Show passed tests in results (can be verbose)."
  :type 'boolean
  :group 'epsilon-test)

;;; Faces

(defface epsilon-test-pass-face
  '((t :inherit success))
  "Face for passed tests."
  :group 'epsilon-test)

(defface epsilon-test-fail-face
  '((t :inherit error))
  "Face for failed tests."
  :group 'epsilon-test)

(defface epsilon-test-error-face
  '((t :inherit error :weight bold))
  "Face for test errors."
  :group 'epsilon-test)

(defface epsilon-test-skip-face
  '((t :inherit warning))
  "Face for skipped tests."
  :group 'epsilon-test)

;;; Mode Definition

(defvar epsilon-test-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'epsilon-test-rerun)
    (define-key map (kbd "n") #'epsilon-test-next-failure)
    (define-key map (kbd "p") #'epsilon-test-prev-failure)
    (define-key map (kbd "RET") #'epsilon-test-goto-failure)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `epsilon-test-mode'.")

(define-derived-mode epsilon-test-mode compilation-mode "Epsilon Tests"
  "Major mode for Epsilon test results.

Commands:
\\{epsilon-test-mode-map}"
  :group 'epsilon-test
  (setq-local compilation-error-regexp-alist
              '(epsilon-test-error))
  (add-to-list 'compilation-error-regexp-alist-alist
               '(epsilon-test-error
                 "^\\s-+at \\([^:]+\\):\\([0-9]+\\)"
                 1 2 nil 2)))

;;; State Variables

(defvar epsilon-test--last-scope nil
  "Last test scope.")

(defvar epsilon-test--last-target nil
  "Last test target.")

;;; Test Commands

(defun epsilon-run-tests (scope target)
  "Run tests for SCOPE (module, file, or test) targeting TARGET."
  (interactive
   (let* ((scope (completing-read "Scope: " '("module" "file" "test" "all")))
          (target (pcase scope
                    ("module" (completing-read "Module: "
                                               (epsilon-project--list-modules)))
                    ("file" (buffer-file-name))
                    ("test" (epsilon-test--test-at-point))
                    ("all" nil))))
     (list scope target)))
  (setq epsilon-test--last-scope scope
        epsilon-test--last-target target)
  (epsilon-test--run scope target))

(defun epsilon-run-test-at-point ()
  "Run the test at point."
  (interactive)
  (if-let ((test (epsilon-test--test-at-point)))
      (epsilon-test--run "test" test)
    (message "No test at point")))

(defun epsilon-run-tests-in-file ()
  "Run all tests in current file."
  (interactive)
  (if (buffer-file-name)
      (epsilon-test--run "file" (buffer-file-name))
    (message "Buffer is not visiting a file")))

(defun epsilon-run-tests-in-module ()
  "Run all tests in a module."
  (interactive)
  (let ((module (completing-read "Module: "
                                 (epsilon-project--list-modules))))
    (epsilon-test--run "module" module)))

(defun epsilon-run-all-tests ()
  "Run all tests in workspace."
  (interactive)
  (epsilon-test--run "all" nil))

(defun epsilon-test-rerun ()
  "Rerun the last test run."
  (interactive)
  (if epsilon-test--last-scope
      (epsilon-test--run epsilon-test--last-scope epsilon-test--last-target)
    (message "No previous test run")))

;;; Test Execution

(defun epsilon-test--run (scope target)
  "Run tests with SCOPE and TARGET."
  (let ((buffer (epsilon-test--get-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Running %s tests%s...\n\n"
                        scope
                        (if target (format " for %s" target) "")))
        (goto-char (point-max))))
    (display-buffer buffer)
    (epsilon-request
     "test" "run-tests"
     `((scope . ,scope)
       ,@(when target `((target . ,target)))
       (verbose . ,(if epsilon-test-verbose t :false)))
     (lambda (response)
       (epsilon-test--display-results buffer response)))))

(defun epsilon-test--get-buffer ()
  "Get or create the test results buffer."
  (let ((buffer (get-buffer-create "*epsilon-tests*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'epsilon-test-mode)
        (epsilon-test-mode)))
    buffer))

;;; Results Display

(defun epsilon-test--display-results (buffer response)
  "Display test RESPONSE in BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (if (equal "ok" (alist-get 'status response))
          (progn
            ;; Test results
            (dolist (result (alist-get 'results response))
              (epsilon-test--render-result result))
            ;; Summary
            (insert "\n")
            (epsilon-test--render-summary (alist-get 'summary response)))
        ;; Error running tests
        (insert (propertize
                 (format "Error: %s\n"
                         (alist-get 'message (alist-get 'error response)))
                 'face 'epsilon-test-error-face)))
      (goto-char (point-min)))))

(defun epsilon-test--render-result (result)
  "Render a single test RESULT."
  (let ((name (alist-get 'name result))
        (status (alist-get 'status result))
        (message (alist-get 'message result))
        (file (alist-get 'file result))
        (line (alist-get 'line result))
        (duration (alist-get 'duration result)))
    (pcase status
      ("pass"
       (when epsilon-test-show-passed
         (insert (propertize (format "  PASS " ) 'face 'epsilon-test-pass-face))
         (insert (format "%s" name))
         (when duration
           (insert (format " (%.3fs)" duration)))
         (insert "\n")))
      ("fail"
       (insert (propertize "  FAIL " 'face 'epsilon-test-fail-face))
       (insert (format "%s\n" name))
       (when message
         (insert (format "       %s\n" message)))
       (when (and file line)
         (insert (format "       at %s:%d\n" file line))))
      ("error"
       (insert (propertize " ERROR " 'face 'epsilon-test-error-face))
       (insert (format "%s\n" name))
       (when message
         (insert (format "       %s\n" message)))
       (when (and file line)
         (insert (format "       at %s:%d\n" file line))))
      ("skip"
       (insert (propertize "  SKIP " 'face 'epsilon-test-skip-face))
       (insert (format "%s" name))
       (when message
         (insert (format " (%s)" message)))
       (insert "\n")))))

(defun epsilon-test--render-summary (summary)
  "Render test SUMMARY."
  (let ((total (alist-get 'total summary))
        (passed (alist-get 'passed summary))
        (failed (alist-get 'failed summary))
        (errors (alist-get 'errors summary))
        (skipped (alist-get 'skipped summary))
        (duration (alist-get 'duration summary)))
    (insert (make-string 40 ?=) "\n")
    (insert (format "Total:   %d tests" total))
    (when duration
      (insert (format " in %.3fs" duration)))
    (insert "\n")
    (insert (propertize (format "Passed:  %d\n" passed)
                        'face 'epsilon-test-pass-face))
    (when (and failed (> failed 0))
      (insert (propertize (format "Failed:  %d\n" failed)
                          'face 'epsilon-test-fail-face)))
    (when (and errors (> errors 0))
      (insert (propertize (format "Errors:  %d\n" errors)
                          'face 'epsilon-test-error-face)))
    (when (and skipped (> skipped 0))
      (insert (propertize (format "Skipped: %d\n" skipped)
                          'face 'epsilon-test-skip-face)))
    (insert (make-string 40 ?=) "\n")
    ;; Final status line
    (if (and (or (null failed) (= failed 0))
             (or (null errors) (= errors 0)))
        (insert (propertize "All tests passed!\n" 'face 'epsilon-test-pass-face))
      (insert (propertize "Some tests failed.\n" 'face 'epsilon-test-fail-face)))))

;;; Navigation

(defun epsilon-test-next-failure ()
  "Move to next test failure."
  (interactive)
  (compilation-next-error 1))

(defun epsilon-test-prev-failure ()
  "Move to previous test failure."
  (interactive)
  (compilation-previous-error 1))

(defun epsilon-test-goto-failure ()
  "Go to the source of the failure at point."
  (interactive)
  (compile-goto-error))

;;; Test Detection

(defun epsilon-test--test-at-point ()
  "Get the test name at point."
  (save-excursion
    ;; Look for enclosing deftest form
    (when (re-search-backward "(def\\(?:test\\|check\\)\\s-+\\(\\sw+\\)" nil t)
      (match-string-no-properties 1))))

(defun epsilon-test--tests-in-buffer ()
  "Get list of test names in current buffer."
  (let ((tests '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "(def\\(?:test\\|check\\)\\s-+\\(\\sw+\\)" nil t)
        (push (match-string-no-properties 1) tests)))
    (nreverse tests)))

;;; Watch Mode (future enhancement)

(defvar epsilon-test--watch-timer nil
  "Timer for watch mode.")

(defun epsilon-test-watch ()
  "Toggle watch mode for tests."
  (interactive)
  (if epsilon-test--watch-timer
      (progn
        (cancel-timer epsilon-test--watch-timer)
        (setq epsilon-test--watch-timer nil)
        (message "Test watch mode disabled"))
    (setq epsilon-test--watch-timer
          (run-with-idle-timer 2 t #'epsilon-test--maybe-rerun))
    (message "Test watch mode enabled")))

(defun epsilon-test--maybe-rerun ()
  "Rerun tests if buffer has been saved since last run."
  ;; TODO: Track file modification times
  nil)

(provide 'epsilon-test)

;;; epsilon-test.el ends here

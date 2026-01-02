;;; epsilon-flymake-tests.el --- Tests for epsilon-flymake -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the Epsilon Flymake backend.

;;; Code:

(require 'ert)
(require 'epsilon-flymake)

;;; Diagnostic Conversion Tests

(ert-deftest epsilon-test-severity-to-type ()
  "Test severity to Flymake type conversion."
  (should (eq (epsilon-flymake--severity-to-type "error") :error))
  (should (eq (epsilon-flymake--severity-to-type "warning") :warning))
  (should (eq (epsilon-flymake--severity-to-type "note") :note))
  (should (eq (epsilon-flymake--severity-to-type "hint") :note))
  (should (eq (epsilon-flymake--severity-to-type "info") :note))
  (should (eq (epsilon-flymake--severity-to-type "unknown") :note)))

(ert-deftest epsilon-test-line-col-to-pos ()
  "Test line/column to position conversion."
  (with-temp-buffer
    (insert "line one\nline two\nline three")
    ;; Line 1, column 1 (0-indexed columns)
    (should (= (epsilon-flymake--line-col-to-pos 1 1) 1))
    ;; Line 2, column 1
    (should (= (epsilon-flymake--line-col-to-pos 2 1) 10))
    ;; Line 2, column 5
    (should (= (epsilon-flymake--line-col-to-pos 2 5) 14))))

(ert-deftest epsilon-test-convert-diagnostic ()
  "Test converting a single ELS diagnostic."
  (with-temp-buffer
    (insert "line one\nline two")
    (let* ((diag '((severity . "error")
                   (message . "Test error")
                   (start-line . 1)
                   (start-column . 1)
                   (end-line . 1)
                   (end-column . 5)))
           (flymake-diag (epsilon-flymake--convert-diagnostic diag)))
      (should flymake-diag)
      (should (eq (flymake-diagnostic-type flymake-diag) :error))
      (should (string= (flymake-diagnostic-text flymake-diag) "Test error")))))

(ert-deftest epsilon-test-convert-diagnostic-with-code ()
  "Test converting diagnostic with error code."
  (with-temp-buffer
    (insert "test content")
    (let* ((diag '((severity . "warning")
                   (message . "Unused variable")
                   (code . "W001")
                   (start-line . 1)
                   (start-column . 1)))
           (flymake-diag (epsilon-flymake--convert-diagnostic diag)))
      (should (string-match-p "\\[W001\\]"
                              (flymake-diagnostic-text flymake-diag))))))

;;; Setup Tests

(ert-deftest epsilon-test-flymake-setup ()
  "Test Flymake setup adds diagnostic function."
  (with-temp-buffer
    (epsilon-mode)
    (epsilon-flymake-setup)
    (should (memq 'epsilon-flymake-checker flymake-diagnostic-functions))))

(provide 'epsilon-flymake-tests)

;;; epsilon-flymake-tests.el ends here

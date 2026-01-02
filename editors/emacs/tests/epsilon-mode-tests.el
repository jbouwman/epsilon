;;; epsilon-mode-tests.el --- Tests for epsilon-mode -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the Epsilon major mode.

;;; Code:

(require 'ert)
(require 'epsilon-mode)

;;; Font Lock Tests

(ert-deftest epsilon-test-font-lock-defun ()
  "Test font-locking of defun."
  (with-temp-buffer
    (epsilon-mode)
    (insert "(defun foo (x) x)")
    (font-lock-ensure)
    (goto-char 2)
    (should (eq (face-at-point) 'font-lock-keyword-face))
    (goto-char 8)
    (should (eq (face-at-point) 'font-lock-function-name-face))))

(ert-deftest epsilon-test-font-lock-defvar ()
  "Test font-locking of defvar."
  (with-temp-buffer
    (epsilon-mode)
    (insert "(defvar *foo* 42)")
    (font-lock-ensure)
    (goto-char 2)
    (should (eq (face-at-point) 'font-lock-keyword-face))))

(ert-deftest epsilon-test-font-lock-keyword ()
  "Test font-locking of keywords."
  (with-temp-buffer
    (epsilon-mode)
    (insert "(list :key 'value)")
    (font-lock-ensure)
    (goto-char 7)
    (should (eq (face-at-point) 'font-lock-constant-face))))

;;; Syntax Table Tests

(ert-deftest epsilon-test-comment-syntax ()
  "Test comment syntax handling."
  (with-temp-buffer
    (epsilon-mode)
    (insert "; this is a comment\n(defun foo () nil)")
    (goto-char 5)
    (should (nth 4 (syntax-ppss)))))

(ert-deftest epsilon-test-string-syntax ()
  "Test string syntax handling."
  (with-temp-buffer
    (epsilon-mode)
    (insert "(princ \"hello world\")")
    (goto-char 10)
    (should (nth 3 (syntax-ppss)))))

;;; Indentation Tests

(ert-deftest epsilon-test-indent-defun ()
  "Test indentation of defun body."
  (with-temp-buffer
    (epsilon-mode)
    (insert "(defun foo (x)\nx)")
    (goto-char (point-max))
    (indent-for-tab-command)
    (should (= (current-indentation) 2))))

(ert-deftest epsilon-test-indent-let ()
  "Test indentation of let body."
  (with-temp-buffer
    (epsilon-mode)
    (insert "(let ((x 1))\nx)")
    (goto-char (point-max))
    (indent-for-tab-command)
    (should (= (current-indentation) 2))))

(ert-deftest epsilon-test-indent-if ()
  "Test indentation of if branches."
  (with-temp-buffer
    (epsilon-mode)
    (insert "(if condition\nthen\nelse)")
    (goto-char 20)
    (indent-for-tab-command)
    (should (= (current-indentation) 4))))

;;; Mode Activation Tests

(ert-deftest epsilon-test-mode-activation ()
  "Test that epsilon-mode activates correctly."
  (with-temp-buffer
    (epsilon-mode)
    (should (derived-mode-p 'epsilon-mode))
    (should (eq major-mode 'epsilon-mode))))

(ert-deftest epsilon-test-auto-mode-alist ()
  "Test auto-mode-alist for .lisp files."
  (should (rassq 'epsilon-mode auto-mode-alist)))

;;; Keymap Tests

(ert-deftest epsilon-test-keymap-exists ()
  "Test that epsilon-mode-map exists and is a keymap."
  (should (keymapp epsilon-mode-map)))

(ert-deftest epsilon-test-eval-keybindings ()
  "Test eval keybindings are set."
  (should (eq (lookup-key epsilon-mode-map (kbd "C-x C-e"))
              'epsilon-eval-last-sexp))
  (should (eq (lookup-key epsilon-mode-map (kbd "C-M-x"))
              'epsilon-eval-defun)))

;;; Buffer-local Variables

(ert-deftest epsilon-test-buffer-local-vars ()
  "Test that buffer-local variables are set correctly."
  (with-temp-buffer
    (epsilon-mode)
    (should (string-prefix-p ";" comment-start))))

(provide 'epsilon-mode-tests)

;;; epsilon-mode-tests.el ends here

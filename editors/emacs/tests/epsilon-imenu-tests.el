;;; epsilon-imenu-tests.el --- Tests for epsilon-imenu -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the Epsilon Imenu backend.

;;; Code:

(require 'ert)
(require 'epsilon-imenu)

;;; Category Tests

(ert-deftest epsilon-test-get-category-function ()
  "Test category lookup for functions."
  (should (equal (epsilon-imenu--get-category "function") "Functions"))
  (should (equal (epsilon-imenu--get-category "defun") "Functions"))
  (should (equal (epsilon-imenu--get-category "defmethod") "Functions")))

(ert-deftest epsilon-test-get-category-macro ()
  "Test category lookup for macros."
  (should (equal (epsilon-imenu--get-category "macro") "Macros"))
  (should (equal (epsilon-imenu--get-category "defmacro") "Macros")))

(ert-deftest epsilon-test-get-category-variable ()
  "Test category lookup for variables."
  (should (equal (epsilon-imenu--get-category "variable") "Variables"))
  (should (equal (epsilon-imenu--get-category "defvar") "Variables")))

(ert-deftest epsilon-test-get-category-unknown ()
  "Test category lookup for unknown kinds."
  (should (equal (epsilon-imenu--get-category "something-else") "Other")))

;;; Line to Position Tests

(ert-deftest epsilon-test-line-to-pos ()
  "Test line number to position conversion."
  (with-temp-buffer
    (insert "line 1\nline 2\nline 3")
    (should (= (epsilon-imenu--line-to-pos 1) 1))
    (should (= (epsilon-imenu--line-to-pos 2) 8))
    (should (= (epsilon-imenu--line-to-pos 3) 15))))

(ert-deftest epsilon-test-line-to-pos-nil ()
  "Test that nil line returns nil."
  (should-not (epsilon-imenu--line-to-pos nil)))

;;; Local Index Creation Tests

(ert-deftest epsilon-test-create-index-local ()
  "Test local index creation."
  (with-temp-buffer
    (insert "(defun foo () nil)\n")
    (insert "(defvar *bar* 42)\n")
    (insert "(defmacro baz () nil)")
    (let ((index (epsilon-imenu--create-index-local)))
      (should index)
      ;; Should have entries
      (should (or (assoc "Functions" index)
                  (assoc "foo" index))))))

;;; Flatten Index Tests

(ert-deftest epsilon-test-flatten-index-flat ()
  "Test flattening an already flat index."
  (let ((index '(("foo" . 1) ("bar" . 10))))
    (should (equal (epsilon-imenu--flatten-index index) index))))

(ert-deftest epsilon-test-flatten-index-nested ()
  "Test flattening a nested index."
  (let ((index '(("Functions" . (("foo" . 1) ("bar" . 10)))
                 ("Variables" . (("baz" . 20))))))
    (let ((flat (epsilon-imenu--flatten-index index)))
      (should (= (length flat) 3))
      (should (assoc "Functions: foo" flat))
      (should (assoc "Variables: baz" flat)))))

(provide 'epsilon-imenu-tests)

;;; epsilon-imenu-tests.el ends here

;;; epsilon-xref-tests.el --- Tests for epsilon-xref -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ERT tests for the Epsilon Xref backend.

;;; Code:

(require 'ert)
(require 'epsilon-xref)

;;; Location Conversion Tests

(ert-deftest epsilon-test-convert-location ()
  "Test converting an ELS location to xref-item."
  (let* ((loc '((file . "/path/to/file.lisp")
                (line . 42)
                (column . 10)
                (name . "test-function")
                (snippet . "(defun test-function ...)")))
         (xref-item (epsilon-xref--convert-location loc)))
    (should xref-item)
    (should (xref-item-p xref-item))
    (let ((location (xref-item-location xref-item)))
      (should (equal (xref-file-location-file location) "/path/to/file.lisp"))
      (should (equal (xref-file-location-line location) 42))
      (should (equal (xref-file-location-column location) 10)))))

(ert-deftest epsilon-test-convert-location-minimal ()
  "Test converting a minimal ELS location."
  (let* ((loc '((file . "/path/to/file.lisp")
                (name . "foo")))
         (xref-item (epsilon-xref--convert-location loc)))
    (should xref-item)
    (let ((location (xref-item-location xref-item)))
      (should (equal (xref-file-location-line location) 1))
      (should (equal (xref-file-location-column location) 0)))))

(ert-deftest epsilon-test-convert-locations-list ()
  "Test converting a list of ELS locations."
  (let* ((locs '(((file . "/a.lisp") (line . 1) (name . "a"))
                 ((file . "/b.lisp") (line . 2) (name . "b"))))
         (items (epsilon-xref--convert-locations locs)))
    (should (= (length items) 2))
    (should (equal (xref-file-location-file
                    (xref-item-location (car items)))
                   "/a.lisp"))))

(ert-deftest epsilon-test-convert-locations-nil ()
  "Test that nil locations returns nil."
  (should-not (epsilon-xref--convert-locations nil)))

;;; Xref Backend Tests

(ert-deftest epsilon-test-xref-backend ()
  "Test that xref backend returns epsilon symbol."
  (should (eq (epsilon-xref-backend) 'epsilon)))

(ert-deftest epsilon-test-xref-identifier-at-point ()
  "Test identifier extraction for xref."
  (with-temp-buffer
    (insert "(defun foo (x) x)")
    (goto-char 8) ; on "foo"
    (should (equal (xref-backend-identifier-at-point 'epsilon) "foo"))))

;;; Source Location Cache Tests

(ert-deftest epsilon-test-record-definition ()
  "Test recording a definition location."
  (let ((epsilon-xref--source-locations (make-hash-table :test 'equal)))
    (epsilon-xref-record-definition "test-fn" "/test.lisp" 10 5)
    (let ((loc (epsilon-xref-lookup-cached "test-fn")))
      (should loc)
      (should (equal (alist-get 'file loc) "/test.lisp"))
      (should (equal (alist-get 'line loc) 10)))))

(provide 'epsilon-xref-tests)

;;; epsilon-xref-tests.el ends here

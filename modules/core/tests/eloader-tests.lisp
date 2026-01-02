;;;; Tests for epsilon.eloader
;;;;
;;;; Tests the package-per-file loader functionality.

(defpackage :epsilon.eloader.tests
  (:use :cl :epsilon.test)
  (:local-nicknames (:eloader :epsilon.eloader)
                    (:fs :epsilon.sys.fs))
  (:export #:run-eloader-tests))

(in-package :epsilon.eloader.tests)

;;; ---------------------------------------------------------------------------
;;; File Detection Tests
;;; ---------------------------------------------------------------------------

(deftest test-e-file-p-detects-e-files
  "e-file-p returns T for .e files"
  (is (eloader:e-file-p "foo.e"))
  (is (eloader:e-file-p "/path/to/bar.e"))
  (is (eloader:e-file-p "src/epsilon/http/client.e")))

(deftest test-e-file-p-rejects-non-e-files
  "e-file-p returns NIL for non-.e files"
  (is (not (eloader:e-file-p "foo.lisp")))
  (is (not (eloader:e-file-p "foo.el")))
  (is (not (eloader:e-file-p "foo.e.bak")))
  (is (not (eloader:e-file-p "foo"))))

;;; ---------------------------------------------------------------------------
;;; Path Derivation Tests
;;; ---------------------------------------------------------------------------

(deftest test-derive-package-simple
  "derive-package-from-path works for simple paths"
  (is (equal "epsilon.http.client"
             (eloader:derive-package-from-path
              "src/epsilon/http/client.e"
              "src/"))))

(deftest test-derive-package-nested
  "derive-package-from-path works for deeply nested paths"
  (is (equal "epsilon.http.request.parser"
             (eloader:derive-package-from-path
              "/proj/src/epsilon/http/request/parser.e"
              "/proj/src/"))))

(deftest test-derive-package-single-level
  "derive-package-from-path works for single-level package"
  (is (equal "epsilon"
             (eloader:derive-package-from-path
              "src/epsilon.e"
              "src/"))))

(deftest test-derive-package-no-trailing-slash
  "derive-package-from-path handles src-root without trailing slash"
  (is (equal "epsilon.http"
             (eloader:derive-package-from-path
              "src/epsilon/http.e"
              "src"))))

;;; ---------------------------------------------------------------------------
;;; Package to Path Tests
;;; ---------------------------------------------------------------------------

(deftest test-package-to-path-simple
  "package-to-path converts dotted name to path"
  (is (equal "epsilon/http/client.e"
             (eloader:package-to-path "epsilon.http.client"))))

(deftest test-package-to-path-single
  "package-to-path works for single-component package"
  (is (equal "epsilon.e"
             (eloader:package-to-path "epsilon"))))

(deftest test-package-to-path-symbol
  "package-to-path works with symbol input"
  (is (equal "epsilon/http.e"
             (eloader:package-to-path 'epsilon.http))))

;;; ---------------------------------------------------------------------------
;;; Validation Tests
;;; ---------------------------------------------------------------------------

(deftest test-validate-matching-package
  "validate-package-path returns T for matching package"
  (multiple-value-bind (valid expected)
      (eloader:validate-package-path "epsilon.http.client"
                                     "src/epsilon/http/client.e"
                                     "src/")
    (is valid)
    (is (equal "epsilon.http.client" expected))))

(deftest test-validate-mismatched-package
  "validate-package-path returns NIL for mismatched package"
  (multiple-value-bind (valid expected)
      (eloader:validate-package-path "epsilon.wrong"
                                     "src/epsilon/http/client.e"
                                     "src/")
    (is (not valid))
    (is (equal "epsilon.http.client" expected))))

;;; ---------------------------------------------------------------------------
;;; Form Analysis Tests
;;; ---------------------------------------------------------------------------

(deftest test-find-package-form-defpackage
  "find-package-form finds defpackage"
  (let ((forms '((defpackage :foo (:use :cl)) (defun bar () 42))))
    (is (equal '(defpackage :foo (:use :cl))
               (eloader:find-package-form forms)))))

(deftest test-find-package-form-package
  "find-package-form finds package macro"
  (let ((forms '((package :foo (:use :cl)) (defun bar () 42))))
    (is (equal '(package :foo (:use :cl))
               (eloader:find-package-form forms)))))

(deftest test-find-package-form-none
  "find-package-form returns NIL when no package form"
  (let ((forms '((defun bar () 42) (defvar *x* 1))))
    (is (null (eloader:find-package-form forms)))))

(deftest test-extract-package-name
  "extract-package-name gets name from package form"
  (is (equal "FOO" (eloader:extract-package-name '(defpackage :foo (:use :cl)))))
  (is (equal "BAR" (eloader:extract-package-name '(package bar (:use :cl))))))

;;; ---------------------------------------------------------------------------
;;; Symbol Visibility Tests
;;; ---------------------------------------------------------------------------

(deftest test-public-symbol-p-normal
  "public-symbol-p returns T for normal symbols"
  (is (eloader:public-symbol-p 'foo))
  (is (eloader:public-symbol-p 'bar))
  (is (eloader:public-symbol-p 'my-function)))

(deftest test-public-symbol-p-percent-prefix
  "public-symbol-p returns NIL for %-prefixed symbols"
  (is (not (eloader:public-symbol-p '%internal)))
  (is (not (eloader:public-symbol-p '%helper)))
  (is (not (eloader:public-symbol-p '%foo-bar))))

(deftest test-public-symbol-p-dash-prefix
  "public-symbol-p returns NIL for dash-prefixed symbols"
  (is (not (eloader:public-symbol-p '-private)))
  (is (not (eloader:public-symbol-p '-helper)))
  (is (not (eloader:public-symbol-p '-internal-fn))))

;;; ---------------------------------------------------------------------------
;;; Implicit Package Tests
;;; ---------------------------------------------------------------------------

(deftest test-extract-imports
  "extract-imports parses import forms correctly"
  (let ((forms '((import (epsilon.http http) (epsilon.json json))
                 (defun foo () nil))))
    (let ((imports (eloader:extract-imports forms)))
      (is (= 2 (length imports)))
      (is (equal 'http (caar imports)))
      (is (equal 'epsilon.http (cdar imports))))))

(deftest test-extract-exports
  "extract-exports parses export forms correctly"
  (let ((forms '((export foo bar baz)
                 (defun foo () nil))))
    (let ((exports (eloader:extract-exports forms)))
      (is (= 3 (length exports)))
      (is (member 'foo exports))
      (is (member 'bar exports))
      (is (member 'baz exports)))))

(deftest test-has-package-form-p-true
  "has-package-form-p returns T when package form present"
  (is (eloader:has-package-form-p '((defpackage :foo (:use :cl)) (defun bar () nil))))
  (is (eloader:has-package-form-p '((package :foo (:use :cl)) (defun bar () nil)))))

(deftest test-has-package-form-p-false
  "has-package-form-p returns NIL when no package form"
  (is (not (eloader:has-package-form-p '((defun bar () nil) (defvar *x* 1))))))

;;; ---------------------------------------------------------------------------
;;; Module Integration Tests
;;; ---------------------------------------------------------------------------

(deftest test-e-module-p-true
  "e-module-p returns T for :source-type :e"
  (is (eloader:e-module-p '(:name "test" :source-type :e))))

(deftest test-e-module-p-false
  "e-module-p returns NIL for other source types"
  (is (not (eloader:e-module-p '(:name "test"))))
  (is (not (eloader:e-module-p '(:name "test" :source-type :lisp)))))

(deftest test-module-source-extension-e
  "module-source-extension returns .e for e modules"
  (is (equal ".e" (eloader:module-source-extension '(:name "test" :source-type :e)))))

(deftest test-module-source-extension-lisp
  "module-source-extension returns .lisp for lisp modules"
  (is (equal ".lisp" (eloader:module-source-extension '(:name "test"))))
  (is (equal ".lisp" (eloader:module-source-extension '(:name "test" :source-type :lisp)))))

;;; ---------------------------------------------------------------------------
;;; Test Runner
;;; ---------------------------------------------------------------------------

(defun run-eloader-tests ()
  "Run all eloader tests."
  (format t "~%Running epsilon.eloader tests...~%")
  (run-tests :package :epsilon.eloader.tests))

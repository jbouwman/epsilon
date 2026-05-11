;;;; Tests for epsilon.doc.coverage -- documentation coverage metrics

(defpackage epsilon.doc.coverage-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.doc.coverage coverage)))

;;; Test fixtures: a package with mixed documentation coverage

(defpackage epsilon.doc.coverage-tests.fixtures
  (:use :cl)
  (:export #:documented-fn
           #:undocumented-fn
           #:partial-fn
           #:documented-var))

(in-package :epsilon.doc.coverage-tests.fixtures)

(defun documented-fn (x y)
  "Add X and Y.

   :param x -- The first number.
   :param y -- The second number.
   :returns -- The sum.
   :see IMPL-297"
  (+ x y))

(defun undocumented-fn (a)
  (* a 2))

(defun partial-fn (n)
  "Compute something with N."
  (1+ n))

(defvar documented-var 42
  "A documented variable.")

(in-package :epsilon.doc.coverage-tests)

;;;; package-coverage tests

(deftest test-package-coverage-symbol-coverage
  "package-coverage computes percentage of exported symbols with docstrings."
  (let ((result (coverage:package-coverage "EPSILON.DOC.COVERAGE-TESTS.FIXTURES")))
    (assert-not-null result)
    ;; 3 of 4 exports have docstrings (documented-fn, partial-fn, documented-var)
    (let ((sym-cov (getf result :symbol-coverage)))
      (assert-true (> sym-cov 0.5))
      (assert-true (<= sym-cov 1.0)))))

(deftest test-package-coverage-param-coverage
  "package-coverage computes percentage of params with :param docs."
  (let ((result (coverage:package-coverage "EPSILON.DOC.COVERAGE-TESTS.FIXTURES")))
    (let ((total-params (getf result :total-params))
          (doc-params (getf result :documented-params))
          (param-cov (getf result :parameter-coverage)))
      ;; documented-fn has 2/2 params documented
      (assert-true (> doc-params 0))
      ;; At least some undocumented params exist
      (assert-true (> total-params doc-params))
      ;; Coverage between 0 and 1 exclusive
      (assert-true (> param-cov 0.0))
      (assert-true (< param-cov 1.0)))))

(deftest test-package-coverage-xref-coverage
  "package-coverage computes percentage of symbols with :see refs."
  (let ((result (coverage:package-coverage "EPSILON.DOC.COVERAGE-TESTS.FIXTURES")))
    (let ((xref-cov (getf result :xref-coverage)))
      ;; Only documented-fn has :see
      (assert-true (> xref-cov 0.0))
      (assert-true (< xref-cov 1.0)))))

(deftest test-package-coverage-counts
  "package-coverage includes raw counts."
  (let ((result (coverage:package-coverage "EPSILON.DOC.COVERAGE-TESTS.FIXTURES")))
    (assert-equal 4 (getf result :total-symbols))
    (assert-equal 3 (getf result :documented-symbols))))

(deftest test-package-coverage-nonexistent
  "package-coverage returns NIL for unknown package."
  (assert-nil (coverage:package-coverage "DOES-NOT-EXIST-PKG-9999")))

;;;; module-coverage tests

(deftest test-module-coverage-basic
  "module-coverage returns coverage for the module."
  (let ((result (coverage:module-coverage "epsilon.doc")))
    (assert-not-null result)
    ;; Should have symbol-coverage between 0 and 1
    (assert-true (<= 0.0 (getf result :symbol-coverage) 1.0))))

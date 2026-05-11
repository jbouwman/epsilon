;;;; Tests for epsilon.doc.extract -- symbol and module documentation extraction

(defpackage epsilon.doc.extract-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.doc.extract extract)
           (epsilon.doc.parse parse)
           (epsilon.annotate ann)))

;;; Test fixtures: define some functions with various documentation styles

(defpackage epsilon.doc.extract-tests.fixtures
  (:use :cl)
  (:export #:well-documented-fn
           #:plain-docstring-fn
           #:no-docstring-fn
           #:documented-var
           #:documented-constant
           #:documented-macro))

(in-package :epsilon.doc.extract-tests.fixtures)

(defun well-documented-fn (x y &key direction)
  "Compute something with X and Y.

   Uses a sophisticated algorithm.

   :param x         -- The first operand.
   :param y         -- The second operand.
   :param direction -- :up or :down.
   :returns         -- The computed result as an integer.
   :signals         -- `bad-input` if X is negative.
   :see             -- IMPL-297
   :since 1.0.0"
  (declare (ignore direction))
  (+ x y))

(defun plain-docstring-fn (a b)
  "Add A and B together."
  (+ a b))

(defun no-docstring-fn (n)
  (* n 2))

(defvar documented-var 42
  "A test variable with documentation.")

(defconstant documented-constant 3.14
  "Pi approximation.")

(defmacro documented-macro (form)
  "Execute FORM with tracing."
  `(progn ,form))

(in-package :epsilon.doc.extract-tests)

;;;; describe-symbol tests

(deftest test-describe-symbol-function
  "describe-symbol extracts structured docs for a well-documented function."
  (let ((result (extract:describe-symbol "WELL-DOCUMENTED-FN"
                  :package "EPSILON.DOC.EXTRACT-TESTS.FIXTURES")))
    (assert-not-null result)
    (assert-equal "WELL-DOCUMENTED-FN" (getf result :name))
    (assert-equal "EPSILON.DOC.EXTRACT-TESTS.FIXTURES" (getf result :package))
    (assert-equal :function (getf result :type))
    ;; Lambda list
    (assert-not-null (getf result :lambda-list))
    ;; Parsed docstring sections
    (assert-true (search "Compute something" (getf result :summary)))
    (assert-equal 3 (length (getf result :params)))
    (assert-true (search "computed result" (getf result :returns)))
    (assert-true (search "bad-input" (getf result :signals)))
    (assert-true (member "IMPL-297" (getf result :see) :test #'string=))
    (assert-equal "1.0.0" (getf result :since))))

(deftest test-describe-symbol-source-location
  "describe-symbol includes source location for well-known functions."
  ;; Use a CL function that has a known definition
  (let ((result (extract:describe-symbol "PARSE-DOCSTRING"
                  :package "EPSILON.DOC.PARSE")))
    (assert-not-null result)
    ;; Should have a :source key with at least :file
    (let ((source (getf result :source)))
      (when source
        (assert-true (stringp (getf source :file)))
        ;; File should contain "parse.lisp"
        (assert-true (search "parse" (getf source :file)))))))

(deftest test-describe-symbol-plain-docstring
  "describe-symbol works with an unstructured docstring."
  (let ((result (extract:describe-symbol "PLAIN-DOCSTRING-FN"
                  :package "EPSILON.DOC.EXTRACT-TESTS.FIXTURES")))
    (assert-not-null result)
    (assert-equal :function (getf result :type))
    (assert-equal "Add A and B together." (getf result :summary))
    (assert-nil (getf result :params))))

(deftest test-describe-symbol-no-docstring
  "describe-symbol works when there is no docstring."
  (let ((result (extract:describe-symbol "NO-DOCSTRING-FN"
                  :package "EPSILON.DOC.EXTRACT-TESTS.FIXTURES")))
    (assert-not-null result)
    (assert-equal :function (getf result :type))
    (assert-equal "" (getf result :summary))))

(deftest test-describe-symbol-variable
  "describe-symbol handles defvar."
  (let ((result (extract:describe-symbol "DOCUMENTED-VAR"
                  :package "EPSILON.DOC.EXTRACT-TESTS.FIXTURES")))
    (assert-not-null result)
    (assert-equal :variable (getf result :type))
    (assert-true (search "test variable" (getf result :summary)))))

(deftest test-describe-symbol-constant
  "describe-symbol handles defconstant."
  (let ((result (extract:describe-symbol "DOCUMENTED-CONSTANT"
                  :package "EPSILON.DOC.EXTRACT-TESTS.FIXTURES")))
    (assert-not-null result)
    (assert-equal :constant (getf result :type))
    (assert-true (search "Pi approximation" (getf result :summary)))))

(deftest test-describe-symbol-macro
  "describe-symbol identifies macros."
  (let ((result (extract:describe-symbol "DOCUMENTED-MACRO"
                  :package "EPSILON.DOC.EXTRACT-TESTS.FIXTURES")))
    (assert-not-null result)
    (assert-equal :macro (getf result :type))
    (assert-true (search "Execute FORM" (getf result :summary)))))

(deftest test-describe-symbol-nonexistent
  "describe-symbol returns NIL for unknown symbols."
  (let ((result (extract:describe-symbol "DOES-NOT-EXIST-ANYWHERE-12345"
                  :package "CL")))
    (assert-nil result)))

(deftest test-describe-symbol-with-annotations
  "describe-symbol merges #@ annotations with docstring."
  (let ((sym (intern "ANNOTATED-TEST-FN" (find-package :epsilon.doc.extract-tests.fixtures))))
    ;; Set up a function with annotations
    (setf (symbol-function sym)
          (lambda (x) (1+ x)))
    (setf (documentation sym 'function) "Increment X.")
    (ann:set-annotations sym '((:see . "IMPL-100") (:stability . :stable)))
    (let ((result (extract:describe-symbol (symbol-name sym)
                    :package "EPSILON.DOC.EXTRACT-TESTS.FIXTURES")))
      (assert-not-null result)
      (assert-equal "Increment X." (getf result :summary))
      (assert-true (member "IMPL-100" (getf result :see) :test #'string=))
      (assert-equal :stable (getf result :stability)))
    ;; Cleanup
    (remprop sym 'ann::annotations)))

;;;; describe-package tests

(deftest test-describe-package-returns-symbol-docs
  "describe-package returns docs for all exported symbols."
  (let ((result (extract:describe-package "EPSILON.DOC.PARSE")))
    (assert-not-null result)
    (assert-true (listp result))
    ;; Should contain parse-docstring and merge-annotations
    (assert-true (find "PARSE-DOCSTRING" result
                       :test #'string=
                       :key (lambda (p) (getf p :name))))
    (assert-true (find "MERGE-ANNOTATIONS" result
                       :test #'string=
                       :key (lambda (p) (getf p :name))))))

(deftest test-describe-package-nonexistent
  "describe-package returns NIL for unknown package."
  (assert-nil (extract:describe-package "DOES-NOT-EXIST-PKG-12345")))

;;;; describe-module tests

(deftest test-describe-module-basic
  "describe-module returns module metadata."
  (let ((result (extract:describe-module "epsilon.doc")))
    (assert-not-null result)
    (assert-equal "epsilon.doc" (getf result :name))
    ;; Should have a description from module.sexp
    (assert-true (> (length (getf result :description)) 0))
    ;; Should find packages
    (assert-not-null (getf result :packages))
    ;; Should find exported symbols
    (assert-not-null (getf result :symbols))))

(deftest test-describe-module-has-symbols
  "describe-module includes documented symbols from the module."
  (let* ((result (extract:describe-module "epsilon.doc"))
         (symbols (getf result :symbols)))
    ;; parse-docstring should be among the documented symbols
    (assert-true (find "PARSE-DOCSTRING" symbols
                       :test #'string=
                       :key (lambda (p) (getf p :name))))))

(deftest test-describe-module-nonexistent
  "describe-module gracefully handles unknown modules."
  (let ((result (extract:describe-module "does.not.exist.module.12345")))
    (assert-not-null result)
    (assert-equal "does.not.exist.module.12345" (getf result :name))
    (assert-nil (getf result :symbols))))

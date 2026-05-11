;;;; Tests for module schema validation

(defpackage epsilon.module-schema-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:import (epsilon.module-schema schema)))

(deftest test-valid-module-metadata
  ;; Valid metadata will fail only on filesystem checks (dirs don't exist)
  (let ((metadata '(:name "test-module"
                    :description "A test module"
                    :sources ("src")
                    :tests ("tests"))))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.sexp")
      (assert-true (not valid-p))
      (assert-true (not (null errors)))))
  t)

(deftest test-invalid-structure
  (let ((metadata '(:name)))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.sexp")
      (assert-true (not valid-p))
      (assert-true (not (null errors)))
      (assert-true (search "property list" (first errors)))))
  t)

(deftest test-missing-name
  (let ((metadata '(:description "test")))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.sexp")
      (assert-true (not valid-p))
      (assert-true (not (null errors)))
      (assert-true (search "required" (first errors)))))
  t)

(deftest test-unknown-keys
  (let ((metadata '(:name "test" :unknown-key "value")))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.sexp")
      (assert-true (not valid-p))
      (assert-true (not (null errors)))
      (assert-true (some (lambda (err) (search "Unknown key" err)) errors))))
  t)

(deftest test-type-errors
  ;; :name must be string, :sources must be list
  (let ((metadata '(:name 42 :sources "src")))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.sexp")
      (assert-true (not valid-p))
      (assert-true (>= (length errors) 2))))
  t)

(deftest test-sources-tests-overlap
  (let ((metadata '(:name "test"
                    :sources ("src" "common")
                    :tests ("tests" "common"))))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.sexp")
      (assert-true (not valid-p))
      (assert-true (some (lambda (err) (search "overlap" err)) errors))))
  t)

(deftest test-empty-name
  (let ((metadata '(:name "")))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.sexp")
      (assert-true (not valid-p))
      (assert-true (some (lambda (err) (search "empty" err)) errors))))
  t)

(deftest test-whitespace-in-name
  (let ((metadata '(:name "bad name")))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.sexp")
      (assert-true (not valid-p))
      (assert-true (some (lambda (err) (search "whitespace" err)) errors))))
  t)

(deftest test-minimal-valid-metadata
  ;; Only :name is required; with no dirs, filesystem checks are skipped
  (let ((metadata '(:name "test-module")))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.sexp")
      (assert-true valid-p)
      (assert-true (null errors))))
  t)

(deftest test-duplicate-requires
  (let ((metadata '(:name "test" :requires ("foo" "bar" "foo"))))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.sexp")
      (assert-true (not valid-p))
      (assert-true (some (lambda (err) (search "unique" err)) errors))))
  t)

(deftest test-cl-systems-strings
  (let ((metadata '(:name "test"
                    :cl-systems ("alexandria" "cl-ppcre"))))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.sexp")
      (assert-true valid-p)
      (assert-true (null errors))))
  t)

(deftest test-cl-systems-mixed-form
  (let ((metadata '(:name "test"
                    :cl-systems ("alexandria"
                                 ("babel" :version ">= 1.0")))))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.sexp")
      (assert-true valid-p)
      (assert-true (null errors))))
  t)

(deftest test-cl-systems-not-list
  (let ((metadata '(:name "test" :cl-systems "alexandria")))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.sexp")
      (assert-true (not valid-p))
      (assert-true (some (lambda (err) (search "must be a list" err)) errors))))
  t)

(deftest test-cl-systems-empty-name
  (let ((metadata '(:name "test" :cl-systems (""))))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.sexp")
      (assert-true (not valid-p))
      (assert-true (some (lambda (err) (search "non-empty string" err)) errors))))
  t)

(deftest test-cl-systems-duplicate
  (let ((metadata '(:name "test"
                    :cl-systems ("alexandria"
                                 ("alexandria" :version "1.0")))))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.sexp")
      (assert-true (not valid-p))
      (assert-true (some (lambda (err) (search "unique" err)) errors))))
  t)

(deftest test-cl-systems-bad-spec-shape
  (let ((metadata '(:name "test" :cl-systems ((:something-else)))))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.sexp")
      (assert-true (not valid-p))
      (assert-true (some (lambda (err) (search "non-empty string" err)) errors))))
  t)

(deftest test-stability-accepts-known-tiers
  "Each of the three documented tiers is accepted by the schema."
  (dolist (tier '(:stable :experimental :internal))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata
         (list :name "t" :sources '("src") :stability tier)
         "/nonexistent/module.sexp")
      (declare (ignore valid-p))
      (assert-true (notany (lambda (e) (search "stability" e :test #'char-equal)) errors)))))

(deftest test-stability-rejects-unknown-tier
  "An unknown :stability value produces an error mentioning :stable / :experimental / :internal."
  (multiple-value-bind (valid-p errors)
      (schema:validate-module-metadata
       '(:name "t" :sources ("src") :stability :provisional)
       "/nonexistent/module.sexp")
    (assert-true (not valid-p))
    (assert-true (some (lambda (e) (search "stability" e :test #'char-equal)) errors))
    (assert-true (some (lambda (e) (search "STABLE" e)) errors))))

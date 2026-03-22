;;;; Tests for module schema validation

(defpackage epsilon.module-schema-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.module-schema schema))
  (:enter t))

(deftest test-valid-module-metadata
  ;; Valid metadata will fail only on filesystem checks (dirs don't exist)
  (let ((metadata '(:name "test-module"
                    :description "A test module"
                    :sources ("src")
                    :tests ("tests"))))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.plist")
      (assert-true (not valid-p))
      (assert-true (not (null errors)))))
  t)

(deftest test-invalid-structure
  (let ((metadata '(:name)))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.plist")
      (assert-true (not valid-p))
      (assert-true (not (null errors)))
      (assert-true (search "property list" (first errors)))))
  t)

(deftest test-missing-name
  (let ((metadata '(:description "test")))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.plist")
      (assert-true (not valid-p))
      (assert-true (not (null errors)))
      (assert-true (search "required" (first errors)))))
  t)

(deftest test-unknown-keys
  (let ((metadata '(:name "test" :unknown-key "value")))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.plist")
      (assert-true (not valid-p))
      (assert-true (not (null errors)))
      (assert-true (some (lambda (err) (search "Unknown key" err)) errors))))
  t)

(deftest test-type-errors
  ;; :name must be string, :sources must be list
  (let ((metadata '(:name 42 :sources "src")))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.plist")
      (assert-true (not valid-p))
      (assert-true (>= (length errors) 2))))
  t)

(deftest test-sources-tests-overlap
  (let ((metadata '(:name "test"
                    :sources ("src" "common")
                    :tests ("tests" "common"))))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.plist")
      (assert-true (not valid-p))
      (assert-true (some (lambda (err) (search "overlap" err)) errors))))
  t)

(deftest test-empty-name
  (let ((metadata '(:name "")))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.plist")
      (assert-true (not valid-p))
      (assert-true (some (lambda (err) (search "empty" err)) errors))))
  t)

(deftest test-whitespace-in-name
  (let ((metadata '(:name "bad name")))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.plist")
      (assert-true (not valid-p))
      (assert-true (some (lambda (err) (search "whitespace" err)) errors))))
  t)

(deftest test-minimal-valid-metadata
  ;; Only :name is required; with no dirs, filesystem checks are skipped
  (let ((metadata '(:name "test-module")))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.plist")
      (assert-true valid-p)
      (assert-true (null errors))))
  t)

(deftest test-duplicate-requires
  (let ((metadata '(:name "test" :requires ("foo" "bar" "foo"))))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata metadata "/nonexistent/module.plist")
      (assert-true (not valid-p))
      (assert-true (some (lambda (err) (search "unique" err)) errors))))
  t)

;;;; epsilon.json.cli tests
;;;;
;;;; The handler shells out via sb-ext:exit on every termination path,
;;;; which the test runner can't catch without a subprocess.  These
;;;; tests cover the in-process pieces -- option parsing and the
;;;; output produced by valid in-memory input -- and rely on the
;;;; smoke tests in CI for the exit-code behavior.

(defpackage epsilon.json.cli-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.json json)
            (epsilon.json.cli cli)))

(in-package :epsilon.json.cli-tests)

(deftest cli-parse-options-defaults
  "With no flags, file is NIL (stdin) and every option flag is NIL."
  (multiple-value-bind (file check compact sort-keys canonical)
      (cli::%parse-options '())
    (assert-equal file nil)
    (assert-equal check nil)
    (assert-equal compact nil)
    (assert-equal sort-keys nil)
    (assert-equal canonical nil)))

(deftest cli-parse-options-positional-file
  "A bare argument becomes the input file."
  (multiple-value-bind (file check) (cli::%parse-options '("foo.json"))
    (assert-equal file "foo.json")
    (assert-equal check nil)))

(deftest cli-parse-options-flags
  "--check, --compact, --sort-keys propagate into the right slots."
  (multiple-value-bind (file check compact sort-keys canonical)
      (cli::%parse-options '("--check" "--compact" "--sort-keys" "x.json"))
    (assert-equal file "x.json")
    (assert-equal check t)
    (assert-equal compact t)
    (assert-equal sort-keys t)
    (assert-equal canonical nil)))

(deftest cli-parse-options-canonical-implies-compact-and-sort
  "--canonical forces compact + sort-keys regardless of explicit flags."
  (multiple-value-bind (file check compact sort-keys canonical)
      (cli::%parse-options '("--canonical"))
    (declare (ignore file check))
    (assert-equal compact t)
    (assert-equal sort-keys t)
    (assert-equal canonical t)))

(deftest cli-pretty-output-shape
  "The pretty printer (json:encode :pretty t) produces multi-line
   output with two-space indentation -- the format the CLI emits in
   its default mode."
  (let ((s (json:encode-to-string (json:parse "{\"a\":1}") :pretty t)))
    (assert-true (search (string #\Newline) s))
    (assert-true (search "  \"a\": 1" s))))

(deftest cli-canonical-output-shape
  "Canonical output (the form --canonical emits) has no whitespace
   and sorts object keys."
  (let ((s (json:encode-jcs (json:parse "{\"b\":2,\"a\":1}"))))
    (assert-equal s "{\"a\":1,\"b\":2}")))

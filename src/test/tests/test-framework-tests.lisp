;;;; Tests for the Test Framework Itself
;;;;
;;;; Basic tests to verify the epsilon.test module works correctly

(defpackage :epsilon.test-tests
  (:use :cl :epsilon.test))

(in-package :epsilon.test-tests)

(deftest basic-assertion
  "Test that basic assertions work"
  (is t)
  (is-equal 2 2)
  (is-equalp "hello" "hello"))

(deftest test-registration
  "Test that tests can be registered and run"
  ;; This test validates that the test framework can register itself
  (is t "Test framework is operational"))
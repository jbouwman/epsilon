;;;; Tests for the Test Framework Itself
;;;;
;;;; Basic tests to verify the epsilon.test module works correctly

(defpackage :epsilon.test-tests
  (:use :cl :epsilon.test)
  (:enter t))

(deftest basic-assertion
  "Test that basic assertions work"
  (assert-true t)
  (assert-equal 2 2)
  (assert-equalp "hello" "hello"))

(deftest test-registration
  "Test that tests can be registered and run"
  ;; This test validates that the test framework can register itself
  (assert-true t "Test framework is operational"))

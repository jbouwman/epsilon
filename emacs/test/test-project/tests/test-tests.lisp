;;;; Tests for test-project

(defpackage test-project.test-tests
  (:use cl)
  (:local-nicknames
   (test epsilon.test)
   (src test-project.test)))

(in-package test-project.test-tests)

(deftest test-add-numbers
  "Test the add-numbers function."
  (test:is (= (src:add-numbers 2 3) 5))
  (test:is (= (src:add-numbers 0 0) 0))
  (test:is (= (src:add-numbers -1 1) 0)))

(deftest test-multiply-numbers
  "Test the multiply-numbers function."
  (test:is (= (src:multiply-numbers 2 3) 6))
  (test:is (= (src:multiply-numbers 0 5) 0))
  (test:is (= (src:multiply-numbers -2 3) -6)))

(deftest test-example-function
  "Test the example-function."
  (test:is (= (src:example-function 5) 10))
  (test:is (= (src:example-function 0) 0))
  (test:is (null (src:example-function "not-a-number"))))

(deftest test-that-fails
  "A test that should fail for testing purposes."
  (test:is (= 1 2) "This test should fail"))

(deftest test-with-error
  "A test that throws an error."
  (error "This is a test error"))
(defpackage #:epsilon.lib.diff.tests
  (:use #:cl #:epsilon.test)
  (:local-nicknames (#:diff #:epsilon.lib.diff)))

(in-package #:epsilon.lib.diff.tests)

(defun diff-path (expected actual)
  "Helper to extract path from diff result, or NIL if no diff"
  (let ((result (diff:diff expected actual)))
    (when result
      (diff:diff-result-path result))))

(deftest test-identical-atoms ()
  (is-equal nil (diff:diff 42 42))
  (is-equal nil (diff:diff "hello" "hello"))
  (is-equal nil (diff:diff 'foo 'foo))
  (is-equal nil (diff:diff #\a #\a)))

(deftest test-different-atoms ()
  (let ((result1 (diff:diff 42 43))
        (result2 (diff:diff "hello" "world"))
        (result3 (diff:diff 'foo 'bar)))
    (is (diff:diff-result-p result1))
    (is-equal '() (diff:diff-result-path result1))
    (is-equal 42 (diff:diff-result-expected result1))
    (is-equal 43 (diff:diff-result-actual result1))
    (is-equal :value-mismatch (diff:diff-result-type result1))
    
    (is (diff:diff-result-p result2))
    (is-equal '() (diff:diff-result-path result2))
    (is-equal "hello" (diff:diff-result-expected result2))
    (is-equal "world" (diff:diff-result-actual result2))
    
    (is (diff:diff-result-p result3))
    (is-equal '() (diff:diff-result-path result3))
    (is-equal 'foo (diff:diff-result-expected result3))
    (is-equal 'bar (diff:diff-result-actual result3))))

(deftest test-cons-cells ()
  (is-equal nil (diff-path '(1 2 3) '(1 2 3)))
  (is-equal '(car) (diff-path '(1 2 3) '(2 2 3)))
  (is-equal '(cdr car) (diff-path '(1 2 3) '(1 3 3)))
  (is-equal '(cdr cdr car) (diff-path '(1 2 3) '(1 2 4))))

(deftest test-nested-lists ()
  (is-equal nil (diff-path '((1 2) (3 4)) '((1 2) (3 4))))
  (is-equal '(car car) (diff-path '((1 2) (3 4)) '((2 2) (3 4))))
  (is-equal '(cdr car cdr car) (diff-path '((1 2) (3 4)) '((1 2) (3 5)))))

(deftest test-1d-arrays ()
  (let ((arr1 (make-array 3 :initial-contents '(1 2 3)))
        (arr2 (make-array 3 :initial-contents '(1 2 3)))
        (arr3 (make-array 3 :initial-contents '(1 3 3)))
        (arr4 (make-array 4 :initial-contents '(1 2 3 4))))
    (is-equal nil (diff-path arr1 arr2))
    (is-equal '(1) (diff-path arr1 arr3))
    (is-equal '() (diff-path arr1 arr4))))

(deftest test-2d-arrays ()
  (let ((arr1 (make-array '(2 2) :initial-contents '((1 2) (3 4))))
        (arr2 (make-array '(2 2) :initial-contents '((1 2) (3 4))))
        (arr3 (make-array '(2 2) :initial-contents '((1 2) (3 5)))))
    (is-equal nil (diff-path arr1 arr2))
    (is-equal '((1 1)) (diff-path arr1 arr3))))

(deftest test-mixed-structures ()
  (let ((s1 '((1 2) #(3 4) 5))
        (s2 '((1 2) #(3 4) 5))
        (s3 '((1 2) #(3 5) 5)))
    (is-equal nil (diff-path s1 s2))
    (is-equal '(cdr car 1) (diff-path s1 s3))))

(deftest test-string-arrays ()
  (let ((arr1 (make-array 2 :initial-contents '("hello" "world")))
        (arr2 (make-array 2 :initial-contents '("hello" "world")))
        (arr3 (make-array 2 :initial-contents '("hello" "earth"))))
    (is-equal nil (diff-path arr1 arr2))
    (is-equal '(1) (diff-path arr1 arr3))))

(deftest test-type-mismatches ()
  (let ((result1 (diff:diff '(1 2 3) #(1 2 3)))
        (result2 (diff:diff #(1 2 3) '(1 2 3)))
        (result3 (diff:diff "hello" 'hello)))
    (is (diff:diff-result-p result1))
    (is-equal '() (diff:diff-result-path result1))
    (is-equal :type-mismatch (diff:diff-result-type result1))
    
    (is (diff:diff-result-p result2))
    (is-equal '() (diff:diff-result-path result2))
    (is-equal :type-mismatch (diff:diff-result-type result2))
    
    (is (diff:diff-result-p result3))
    (is-equal '() (diff:diff-result-path result3))
    (is-equal :type-mismatch (diff:diff-result-type result3))))

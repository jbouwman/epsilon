;;;; Tests for epsilon.mutable-map

(defpackage epsilon.mutable-map-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.mutable-map mmap))
  (:enter t))

;;; Construction tests

(deftest test-make-map
  "make-map creates an empty hash-table"
  (let ((m (mmap:make-map)))
    (assert-true (hash-table-p m))
    (assert-true (zerop (mmap:size m)))))

(deftest test-make-map-with-test
  "make-map accepts test argument"
  (let ((m (mmap:make-map :test 'eq)))
    (assert-true (eq 'eq (hash-table-test m)))))

;;; Access tests

(deftest test-get
  "get retrieves values by key"
  (let ((m (mmap:make-map)))
    (mmap:put! m :x 10)
    (assert-true (= 10 (mmap:get m :x)))
    (assert-true (null (mmap:get m :missing)))
    (assert-true (eq :default (mmap:get m :missing :default)))))

(deftest test-contains?
  "contains? checks key presence"
  (let ((m (mmap:make-map)))
    (mmap:put! m :present nil)
    (assert-true (mmap:contains? m :present))
    (assert-true (not (mmap:contains? m :absent)))))

(deftest test-size
  "size returns entry count"
  (let ((m (mmap:make-map)))
    (assert-true (= 0 (mmap:size m)))
    (mmap:put! m :a 1)
    (mmap:put! m :b 2)
    (mmap:put! m :c 3)
    (assert-true (= 3 (mmap:size m)))))

;;; Mutation tests

(deftest test-put!
  "put! sets key to value and returns map"
  (let ((m (mmap:make-map)))
    (assert-true (eq m (mmap:put! m :key "value")))
    (assert-true (equal "value" (mmap:get m :key)))))

(deftest test-put!-overwrite
  "put! overwrites existing values"
  (let ((m (mmap:make-map)))
    (mmap:put! m :key "old")
    (mmap:put! m :key "new")
    (assert-true (equal "new" (mmap:get m :key)))))

(deftest test-update!
  "update! transforms value with function"
  (let ((m (mmap:make-map)))
    (mmap:put! m :count 5)
    (mmap:update! m :count #'1+)
    (assert-true (= 6 (mmap:get m :count)))))

(deftest test-update!-with-default
  "update! uses default for missing keys"
  (let ((m (mmap:make-map)))
    (mmap:update! m :count #'1+ 0)
    (assert-true (= 1 (mmap:get m :count)))))

;;; Iteration tests

(deftest test-for-each
  "for-each calls function on each entry"
  (let ((m (mmap:make-map))
        (sum 0))
    (mmap:put! m :a 1)
    (mmap:put! m :b 2)
    (mmap:for-each m (lambda (k v)
                       (declare (ignore k))
                       (incf sum v)))
    (assert-true (= 3 sum))))

;;; Chaining tests

(deftest test-method-chaining
  "mutating methods return map for chaining"
  (let ((m (mmap:make-map)))
    (mmap:put! (mmap:put! (mmap:put! m :a 1) :b 2) :c 3)
    (assert-true (= 3 (mmap:size m)))
    (assert-true (= 1 (mmap:get m :a)))
    (assert-true (= 2 (mmap:get m :b)))
    (assert-true (= 3 (mmap:get m :c)))))

;;; Counting pattern (common use case)

(deftest test-counting-pattern
  "update! works for counting pattern"
  (let ((counts (mmap:make-map)))
    (dolist (item '(:a :b :a :c :a :b))
      (mmap:update! counts item #'1+ 0))
    (assert-true (= 3 (mmap:get counts :a)))
    (assert-true (= 2 (mmap:get counts :b)))
    (assert-true (= 1 (mmap:get counts :c)))))

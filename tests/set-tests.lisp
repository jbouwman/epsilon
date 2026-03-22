(defpackage epsilon.set-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.set set))
  (:enter t))

(deftest test-empty-set
  (assert-= (set:size set:+empty+) 0)
  (assert-true (not (set:contains-p set:+empty+ 'foo)))
  (assert-true (null (set:seq set:+empty+))))

(deftest test-basic-operations
  ;; Test add
  (let ((s1 (set:add set:+empty+ 'a)))
    (assert-true (= (set:size s1) 1))
    (assert-true (set:contains-p s1 'a))
    (assert-true (not (set:contains-p s1 'b))))

  ;; Test multiple adds
  (let ((s2 (-> set:+empty+
                (set:add 'a)
                (set:add 'b)
                (set:add 'c))))
    (assert-true (= (set:size s2) 3))
    (assert-true (set:contains-p s2 'a))
    (assert-true (set:contains-p s2 'b))
    (assert-true (set:contains-p s2 'c))
    (assert-true (not (set:contains-p s2 'd))))

  ;; Test duplicate add
  (let ((s3 (-> set:+empty+
                (set:add 'a)
                (set:add 'a))))
    (assert-true (= (set:size s3) 1))
    (assert-true (set:contains-p s3 'a))))

(deftest test-remove-operations
  (let ((s1 (-> set:+empty+
                (set:add 'a)
                (set:add 'b)
                (set:add 'c))))
    ;; Test disj
    (let ((s2 (set:disj s1 'b)))
      (assert-true (= (set:size s2) 2))
      (assert-true (set:contains-p s2 'a))
      (assert-true (not (set:contains-p s2 'b)))
      (assert-true (set:contains-p s2 'c)))

    ;; Test remove (alias for disj)
    (let ((s3 (set:remove s1 'a)))
      (assert-true (= (set:size s3) 2))
      (assert-true (not (set:contains-p s3 'a)))
      (assert-true (set:contains-p s3 'b))
      (assert-true (set:contains-p s3 'c)))

    ;; Test removing non-existent element
    (let ((s4 (set:disj s1 'x)))
      (assert-true (= (set:size s4) 3))
      (assert-true (set:set= s4 s1)))))

(deftest test-set-operations
  (let ((s1 (set:make-set 'a 'b 'c))
        (s2 (set:make-set 'b 'c 'd))
        (s3 (set:make-set 'a 'b)))

    ;; Test union
    (let ((u (set:union s1 s2)))
      (assert-= (set:size u) 4)
      (assert-true (set:contains-p u 'a))
      (assert-true (set:contains-p u 'b))
      (assert-true (set:contains-p u 'c))
      (assert-true (set:contains-p u 'd)))

    ;; Test intersection
    (let ((i (set:intersection s1 s2)))
      (assert-= (set:size i) 2)
      (assert-true (set:contains-p i 'b))
      (assert-true (set:contains-p i 'c))
      (assert-true (not (set:contains-p i 'a)))
      (assert-true (not (set:contains-p i 'd))))

    ;; Test difference
    (let ((d (set:difference s1 s2)))
      (assert-true (= (set:size d) 1))
      (assert-true (set:contains-p d 'a))
      (assert-true (not (set:contains-p d 'b)))
      (assert-true (not (set:contains-p d 'c))))

    ;; Test subset-p
    (assert-true (set:subset-p s3 s1))
    (assert-true (not (set:subset-p s1 s3)))
    (assert-true (set:subset-p s1 s1))))

(deftest test-higher-order-functions
  (let ((s (set:make-set 1 2 3 4 5)))

    ;; Test map
    (let ((doubled (set:map s (lambda (x) (* x 2)))))
      (assert-true (= (set:size doubled) 5))
      (assert-true (set:contains-p doubled 2))
      (assert-true (set:contains-p doubled 4))
      (assert-true (set:contains-p doubled 6))
      (assert-true (set:contains-p doubled 8))
      (assert-true (set:contains-p doubled 10)))

    ;; Test filter
    (let ((evens (set:filter (lambda (x) (evenp x)) s)))
      (assert-true (= (set:size evens) 2))
      (assert-true (set:contains-p evens 2))
      (assert-true (set:contains-p evens 4))
      (assert-true (not (set:contains-p evens 1))))

    ;; Test reduce
    (let ((sum (set:reduce #'+ s 0)))
      (assert-true (= sum 15)))))

(deftest test-equality
  (let ((s1 (set:make-set 'a 'b 'c))
        (s2 (set:make-set 'c 'b 'a))  ; different order
        (s3 (set:make-set 'a 'b)))

    (assert-true (set:set= s1 s2))
    (assert-true (not (set:set= s1 s3)))
    (assert-true (set:set= set:+empty+ set:+empty+))))

(deftest test-seq
  (let ((s (set:make-set 'a 'b 'c)))
    (let ((values (set:seq s)))
      (assert-true (= (length values) 3))
      (assert-true (member 'a values))
      (assert-true (member 'b values))
      (assert-true (member 'c values)))))

(deftest test-make-set
  (let ((s (set:make-set 1 2 3 2 1)))  ; duplicates
    (assert-= (set:size s) 3)
    (assert-true (set:contains-p s 1))
    (assert-true (set:contains-p s 2))
    (assert-true (set:contains-p s 3))))

(deftest test-predicates
  (assert-true (set:set-p (set:make-set 1 2 3)))
  (assert-true (not (set:set-p '(1 2 3))))
  (assert-true (not (set:set-p "not a set"))))

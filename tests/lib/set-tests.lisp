(defpackage epsilon.lib.set.tests
  (:use
   :cl
   :epsilon.lib.syntax
   :epsilon.tool.test)
  (:local-nicknames
   (set epsilon.lib.set)))

(in-package epsilon.lib.set.tests)

(deftest test-empty-set
  (is-= (set:size set:+empty+) 0)
  (is (not (set:contains-p set:+empty+ 'foo)))
  (is (null (set:seq set:+empty+))))

(deftest test-basic-operations
  ;; Test add
  (let ((s1 (set:add +empty+ 'a)))
    (is (= (set:size s1) 1))
    (is (set:contains-p s1 'a))
    (is (not (set:contains-p s1 'b))))
  
  ;; Test multiple adds
  (let ((s2 (-> +empty+
                (set:add 'a)
                (set:add 'b)
                (set:add 'c))))
    (is (= (set:size s2) 3))
    (is (set:contains-p s2 'a))
    (is (set:contains-p s2 'b))
    (is (set:contains-p s2 'c))
    (is (not (set:contains-p s2 'd))))
  
  ;; Test duplicate add
  (let ((s3 (-> set:+empty+
                (set:add 'a)
                (set:add 'a))))
    (is (= (set:size s3) 1))
    (is (set:contains-p s3 'a))))

(deftest test-remove-operations
  (let ((s1 (-> set:+empty+
                (set:add 'a)
                (set:add 'b)
                (set:add 'c))))
    ;; Test disj
    (let ((s2 (set:disj s1 'b)))
      (is (= (set:size s2) 2))
      (is (set:contains-p s2 'a))
      (is (not (set:contains-p s2 'b)))
      (is (set:contains-p s2 'c)))
    
    ;; Test remove (alias for disj)
    (let ((s3 (set:remove s1 'a)))
      (is (= (set:size s3) 2))
      (is (not (set:contains-p s3 'a)))
      (is (set:contains-p s3 'b))
      (is (set:contains-p s3 'c)))
    
    ;; Test removing non-existent element
    (let ((s4 (set:disj s1 'x)))
      (is (= (set:size s4) 3))
      (is (set:set= s4 s1)))))

(deftest test-set-operations
  (let ((s1 (set:make-set 'a 'b 'c))
        (s2 (set:make-set 'b 'c 'd))
        (s3 (set:make-set 'a 'b)))
    
    ;; Test union
    (let ((u (set:union s1 s2)))
      (is-= (set:size u) 4)
      (is (set:contains-p u 'a))
      (is (set:contains-p u 'b))
      (is (set:contains-p u 'c))
      (is (set:contains-p u 'd)))
    
    ;; Test intersection
    (let ((i (set:intersection s1 s2)))
      (is-= (set:size i) 2)
      (is (set:contains-p i 'b))
      (is (set:contains-p i 'c))
      (is (not (set:contains-p i 'a)))
      (is (not (set:contains-p i 'd))))
    
    ;; Test difference
    (let ((d (set:difference s1 s2)))
      (is (= (set:size d) 1))
      (is (set:contains-p d 'a))
      (is (not (set:contains-p d 'b)))
      (is (not (set:contains-p d 'c))))
    
    ;; Test subset-p
    (is (set:subset-p s3 s1))
    (is (not (set:subset-p s1 s3)))
    (is (set:subset-p s1 s1))))

(deftest test-higher-order-functions
  (let ((s (set:make-set 1 2 3 4 5)))
    
    ;; Test map
    (let ((doubled (set:map s (lambda (x) (* x 2)))))
      (is (= (set:size doubled) 5))
      (is (set:contains-p doubled 2))
      (is (set:contains-p doubled 4))
      (is (set:contains-p doubled 6))
      (is (set:contains-p doubled 8))
      (is (set:contains-p doubled 10)))
    
    ;; Test filter
    (let ((evens (set:filter (lambda (x) (evenp x)) s)))
      (is (= (set:size evens) 2))
      (is (set:contains-p evens 2))
      (is (set:contains-p evens 4))
      (is (not (set:contains-p evens 1))))
    
    ;; Test reduce
    (let ((sum (set:reduce #'+ s 0)))
      (is (= sum 15)))))

(deftest test-equality
  (let ((s1 (set:make-set 'a 'b 'c))
        (s2 (set:make-set 'c 'b 'a))  ; different order
        (s3 (set:make-set 'a 'b)))
    
    (is (set:set= s1 s2))
    (is (not (set:set= s1 s3)))
    (is (set:set= set:+empty+ set:+empty+))))

(deftest test-seq
  (let ((s (set:make-set 'a 'b 'c)))
    (let ((values (set:seq s)))
      (is (= (length values) 3))
      (is (member 'a values))
      (is (member 'b values))
      (is (member 'c values)))))

(deftest test-make-set
  (let ((s (set:make-set 1 2 3 2 1)))  ; duplicates
    (is-= (set:size s) 3)
    (is (set:contains-p s 1))
    (is (set:contains-p s 2))
    (is (set:contains-p s 3))))

(deftest test-predicates
  (is (set:set-p (set:make-set 1 2 3)))
  (is (not (set:set-p '(1 2 3))))
  (is (not (set:set-p "not a set"))))

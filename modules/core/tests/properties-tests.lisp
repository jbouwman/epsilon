(defpackage #:epsilon.properties.tests
  (:use
   #:cl
   #:epsilon.test)
  (:local-nicknames
   (#:prop #:epsilon.properties)))

(in-package #:epsilon.properties.tests)

;;; Generator tests

(deftest test-gen-return
  (let ((values (prop:sample (prop:gen-return 42) :count 5)))
    (is (every (lambda (v) (= v 42)) values))))

(deftest test-gen-integer
  (let ((values (prop:sample (prop:gen-integer :min 0 :max 100) :count 100)))
    (is (every (lambda (v) (and (integerp v) (<= 0 v 100))) values))))

(deftest test-gen-integer-range
  (let ((values (prop:sample (prop:gen-integer :min -10 :max 10) :count 100)))
    (is (every (lambda (v) (<= -10 v 10)) values))))

(deftest test-gen-float
  (let ((values (prop:sample (prop:gen-float :min 0.0 :max 1.0) :count 100)))
    (is (every (lambda (v) (and (floatp v) (<= 0.0 v 1.0))) values))))

(deftest test-gen-boolean
  (let ((values (prop:sample (prop:gen-boolean) :count 100)))
    (is (every (lambda (v) (member v '(t nil))) values))
    ;; Should have both true and false
    (is (member t values))
    (is (member nil values))))

(deftest test-gen-char
  (let ((values (prop:sample (prop:gen-char :min #\a :max #\z) :count 100)))
    (is (every (lambda (c) (and (characterp c)
                                 (char<= #\a c #\z)))
               values))))

(deftest test-gen-string
  (let ((values (prop:sample (prop:gen-string :max-length 20) :count 50)))
    (is (every #'stringp values))
    (is (every (lambda (s) (<= (length s) 20)) values))))

(deftest test-gen-list
  (let ((values (prop:sample (prop:gen-list (prop:gen-integer :min 0 :max 10)
                                            :max-length 10)
                             :count 50)))
    (is (every #'listp values))
    (is (every (lambda (lst) (<= (length lst) 10)) values))
    (is (every (lambda (lst) (every (lambda (n) (<= 0 n 10)) lst)) values))))

(deftest test-gen-vector
  (let ((values (prop:sample (prop:gen-vector (prop:gen-integer :min 0 :max 10)
                                              :max-length 10)
                             :count 50)))
    (is (every #'vectorp values))))

(deftest test-gen-one-of
  (let ((values (prop:sample (prop:gen-one-of (prop:gen-return :a)
                                              (prop:gen-return :b)
                                              (prop:gen-return :c))
                             :count 100)))
    (is (every (lambda (v) (member v '(:a :b :c))) values))))

(deftest test-gen-elements
  (let ((values (prop:sample (prop:gen-elements :red :green :blue) :count 100)))
    (is (every (lambda (v) (member v '(:red :green :blue))) values))))

(deftest test-gen-such-that
  (let ((values (prop:sample (prop:gen-such-that #'evenp
                                                 (prop:gen-integer :min 0 :max 100))
                             :count 50)))
    (is (every #'evenp values))))

(deftest test-gen-fmap
  (let ((values (prop:sample (prop:gen-fmap #'1+ (prop:gen-integer :min 0 :max 10))
                             :count 50)))
    (is (every (lambda (v) (<= 1 v 11)) values))))

(deftest test-gen-tuple
  (let ((values (prop:sample (prop:gen-tuple (prop:gen-integer :min 0 :max 10)
                                             (prop:gen-boolean))
                             :count 50)))
    (is (every (lambda (v)
                 (and (listp v)
                      (= (length v) 2)
                      (integerp (first v))
                      (member (second v) '(t nil))))
               values))))

;;; Shrinking tests

(deftest test-shrink-integer
  (let ((shrinks (prop:shrink-integer 100)))
    (is (member 0 shrinks))
    (is (member 99 shrinks))
    (is (member 50 shrinks))))

(deftest test-shrink-integer-min
  (let ((shrinks (prop:shrink-integer 0)))
    (is (null shrinks))))

(deftest test-shrink-list
  (let ((shrinks (prop:shrink-list '(1 2 3) #'prop:shrink-integer)))
    ;; Should include sublists
    (is (member '(2 3) shrinks :test #'equal))
    (is (member '(1 3) shrinks :test #'equal))
    (is (member '(1 2) shrinks :test #'equal))))

(deftest test-shrink-string
  (let ((shrinks (prop:shrink-string "abc")))
    (is (member "" shrinks :test #'equal))
    (is (member "bc" shrinks :test #'equal))
    (is (member "ac" shrinks :test #'equal))
    (is (member "ab" shrinks :test #'equal))))

;;; Property checking tests

(deftest test-property-passes
  (let ((result (prop:for-all ((x (prop:gen-integer :min 0 :max 100))
                               (y (prop:gen-integer :min 0 :max 100)))
                  (= (+ x y) (+ y x)))))
    (is (prop:property-result-passed-p result))))

(deftest test-property-fails
  (let ((result (prop:for-all ((x (prop:gen-integer :min 1 :max 100)))
                  (< x 50))))  ; Will fail for x >= 50
    (is (not (prop:property-result-passed-p result)))
    (is (>= (first (prop:property-result-failing-input result)) 50))))

(deftest test-implication
  ;; Using ==> to filter test cases
  (let ((result (prop:for-all ((x (prop:gen-integer :min -100 :max 100)))
                  (prop:==> (> x 0)
                            (> (* x x) 0)))))
    (is (prop:property-result-passed-p result))))

(deftest test-property-with-exception
  ;; Use gen-elements to ensure 0 is tested (higher probability)
  (let ((result (prop:for-all ((x (prop:gen-elements 0 1 2 3 0 0)))  ; 0 appears frequently
                  (/ 1 x))))  ; Will error when x = 0
    (is (not (prop:property-result-passed-p result)))
    (is (prop:property-result-exception result))))

;;; Property examples

(deftest test-reverse-reverse
  "Reversing a list twice gives the original list"
  (let ((result (prop:for-all ((lst (prop:gen-list (prop:gen-integer :min 0 :max 100))))
                  (equal (reverse (reverse lst)) lst))))
    (is (prop:property-result-passed-p result))))

(deftest test-append-length
  "Length of appended lists equals sum of lengths"
  (let ((result (prop:for-all ((a (prop:gen-list (prop:gen-integer)))
                               (b (prop:gen-list (prop:gen-integer))))
                  (= (length (append a b))
                     (+ (length a) (length b))))))
    (is (prop:property-result-passed-p result))))

(deftest test-sort-preserves-length
  "Sorting preserves list length"
  (let ((result (prop:for-all ((lst (prop:gen-list (prop:gen-integer :min 0 :max 1000))))
                  (= (length (sort (copy-list lst) #'<))
                     (length lst)))))
    (is (prop:property-result-passed-p result))))

(deftest test-sort-is-sorted
  "Sorted list is sorted"
  (let ((result (prop:for-all ((lst (prop:gen-list (prop:gen-integer :min 0 :max 1000))))
                  (let ((sorted (sort (copy-list lst) #'<)))
                    (or (null sorted)
                        (loop for (a b) on sorted
                              while b
                              always (<= a b)))))))
    (is (prop:property-result-passed-p result))))

;;; defproptest macro

(prop:defproptest addition-is-commutative
    ((x (prop:gen-integer :min -1000 :max 1000))
     (y (prop:gen-integer :min -1000 :max 1000)))
  (= (+ x y) (+ y x)))

(prop:defproptest multiplication-is-associative
    ((x (prop:gen-integer :min -100 :max 100))
     (y (prop:gen-integer :min -100 :max 100))
     (z (prop:gen-integer :min -100 :max 100)))
  (= (* x (* y z)) (* (* x y) z)))

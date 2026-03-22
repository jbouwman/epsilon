;;;; literal-syntax-tests.lisp - Tests for vector, map, set, and fn literal syntax
;;;;
;;;; Tests the Epsilon reader syntax extensions:
;;;; - [1 2 3]      -> vector literal
;;;; - {:a 1 :b 2}  -> HAMT map literal
;;;; - #{1 2 3}     -> HAMT set literal
;;;; - #f(+ % 1)    -> anonymous function shorthand

(defpackage epsilon.reader.literal-syntax-tests
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.reader reader)
            (epsilon.map map)
            (epsilon.set set))
  (:enter t))

;;;; Helper to read with epsilon syntax

(defun read-literal (string)
  "Read a string using epsilon reader syntax"
  (reader:with-epsilon-syntax
    (read-from-string string)))

;;;; ==========================================================================
;;;; Vector Literal Tests
;;;; ==========================================================================

(deftest test-vector-literal-empty
  (let ((v (read-literal "[]")))
    (assert-true (vectorp v))
    (assert-true (= 0 (length v)))))

(deftest test-vector-literal-with-values
  (let ((v (read-literal "[1 2 3]")))
    (assert-true (vectorp v))
    (assert-true (= 3 (length v)))
    (assert-true (= 1 (aref v 0)))
    (assert-true (= 2 (aref v 1)))
    (assert-true (= 3 (aref v 2)))))

(deftest test-vector-literal-with-mixed-types
  (let ((v (read-literal "[1 :a \"hello\" 3.14]")))
    (assert-true (vectorp v))
    (assert-true (= 4 (length v)))
    (assert-true (= 1 (aref v 0)))
    (assert-true (eq :a (aref v 1)))
    (assert-true (equal "hello" (aref v 2)))
    (assert-true (= 3.14 (aref v 3)))))

(deftest test-vector-literal-nested
  (let ((v (read-literal "[[1 2] [3 4]]")))
    (assert-true (vectorp v))
    (assert-true (= 2 (length v)))
    (assert-true (vectorp (aref v 0)))
    (assert-true (vectorp (aref v 1)))
    (assert-true (equalp #(1 2) (aref v 0)))
    (assert-true (equalp #(3 4) (aref v 1)))))

(deftest test-vector-literal-deeply-nested
  (let ((v (read-literal "[[[1]]]")))
    (assert-true (vectorp v))
    (assert-true (= 1 (aref (aref (aref v 0) 0) 0)))))

;;;; ==========================================================================
;;;; Map Literal Tests
;;;; ==========================================================================

(deftest test-map-literal-empty
  (let ((m (read-literal "{}")))
    (assert-true (typep m 'map:hamt))
    (assert-true (= 0 (map:count m)))))

(deftest test-map-literal-with-values
  (let ((m (read-literal "{:a 1 :b 2}")))
    (assert-true (typep m 'map:hamt))
    (assert-true (= 2 (map:count m)))
    (assert-true (= 1 (map:get m :a)))
    (assert-true (= 2 (map:get m :b)))))

(deftest test-map-literal-with-string-keys
  (let ((m (read-literal "{\"name\" \"Alice\" \"age\" 30}")))
    (assert-true (typep m 'map:hamt))
    (assert-true (equal "Alice" (map:get m "name")))
    (assert-true (= 30 (map:get m "age")))))

(deftest test-map-literal-nested
  (let ((m (read-literal "{:outer {:inner 42}}")))
    (assert-true (typep m 'map:hamt))
    (assert-true (typep (map:get m :outer) 'map:hamt))
    (assert-true (= 42 (map:get (map:get m :outer) :inner)))))

(deftest test-map-literal-with-vector-values
  (let ((m (read-literal "{:items [1 2 3]}")))
    (assert-true (typep m 'map:hamt))
    (assert-true (vectorp (map:get m :items)))
    (assert-true (equalp #(1 2 3) (map:get m :items)))))

;;;; ==========================================================================
;;;; Set Literal Tests
;;;; ==========================================================================

(deftest test-set-literal-empty
  (let ((s (read-literal "#{}")))
    (assert-true (typep s 'set:hamt-set))
    (assert-true (= 0 (set:count s)))))

(deftest test-set-literal-with-values
  (let ((s (read-literal "#{1 2 3}")))
    (assert-true (typep s 'set:hamt-set))
    (assert-true (= 3 (set:count s)))
    (assert-true (set:contains-p s 1))
    (assert-true (set:contains-p s 2))
    (assert-true (set:contains-p s 3))
    (assert-true (not (set:contains-p s 4)))))

(deftest test-set-literal-with-keywords
  (let ((s (read-literal "#{:a :b :c}")))
    (assert-true (typep s 'set:hamt-set))
    (assert-true (set:contains-p s :a))
    (assert-true (set:contains-p s :b))
    (assert-true (set:contains-p s :c))))

(deftest test-set-literal-deduplication
  (let ((s (read-literal "#{1 1 2 2 3 3}")))
    (assert-true (typep s 'set:hamt-set))
    ;; Sets should deduplicate
    (assert-true (= 3 (set:count s)))))

;;;; ==========================================================================
;;;; Anonymous Function Literal Tests
;;;; ==========================================================================

(deftest test-anon-fn-nullary
  (let ((fn (eval (read-literal "#f(+ 1 2)"))))
    (assert-true (functionp fn))
    (assert-true (= 3 (funcall fn)))))

(deftest test-anon-fn-single-arg
  (let ((fn (eval (read-literal "#f(+ % 1)"))))
    (assert-true (functionp fn))
    (assert-true (= 2 (funcall fn 1)))
    (assert-true (= 11 (funcall fn 10)))))

(deftest test-anon-fn-two-args
  (let ((fn (eval (read-literal "#f(+ %1 %2)"))))
    (assert-true (functionp fn))
    (assert-true (= 3 (funcall fn 1 2)))
    (assert-true (= 15 (funcall fn 10 5)))))

(deftest test-anon-fn-three-args
  (let ((fn (eval (read-literal "#f(list %1 %2 %3)"))))
    (assert-true (functionp fn))
    (assert-true (equal '(a b c) (funcall fn 'a 'b 'c)))))

(deftest test-anon-fn-with-nested-expr
  (let ((fn (eval (read-literal "#f(string-upcase (first %))"))))
    (assert-true (functionp fn))
    (assert-true (equal "HELLO" (funcall fn '("hello" "world"))))))

(deftest test-anon-fn-with-conditional
  (let ((fn (eval (read-literal "#f(if (> % 0) :positive :non-positive)"))))
    (assert-true (functionp fn))
    (assert-true (eq :positive (funcall fn 5)))
    (assert-true (eq :non-positive (funcall fn -5)))
    (assert-true (eq :non-positive (funcall fn 0)))))

;;;; ==========================================================================
;;;; Mixed Literal Tests
;;;; ==========================================================================

(deftest test-mixed-map-with-vector-and-set
  (let ((data (read-literal "{:items [1 2 3] :tags #{:a :b}}")))
    (assert-true (typep data 'map:hamt))
    (assert-true (vectorp (map:get data :items)))
    (assert-true (= 3 (length (map:get data :items))))
    (assert-true (typep (map:get data :tags) 'set:hamt-set))
    (assert-true (set:contains-p (map:get data :tags) :a))))

(deftest test-vector-of-maps
  (let ((data (read-literal "[{:a 1} {:b 2}]")))
    (assert-true (vectorp data))
    (assert-true (= 2 (length data)))
    (assert-true (typep (aref data 0) 'map:hamt))
    (assert-true (typep (aref data 1) 'map:hamt))
    (assert-true (= 1 (map:get (aref data 0) :a)))
    (assert-true (= 2 (map:get (aref data 1) :b)))))

(deftest test-set-of-vectors
  (let ((data (read-literal "#{[1 2] [3 4]}")))
    (assert-true (typep data 'set:hamt-set))
    (assert-true (= 2 (set:count data)))))

(deftest test-deeply-nested-mixed
  (let ((data (read-literal "{:level1 {:level2 {:level3 [1 2 #{:a :b}]}}}")))
    (assert-true (typep data 'map:hamt))
    (let* ((level1 (map:get data :level1))
           (level2 (map:get level1 :level2))
           (level3 (map:get level2 :level3)))
      (assert-true (vectorp level3))
      (assert-true (= 3 (length level3)))
      (assert-true (typep (aref level3 2) 'set:hamt-set)))))

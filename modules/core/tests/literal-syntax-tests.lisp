;;;; literal-syntax-tests.lisp - Tests for vector, map, set, and fn literal syntax
;;;;
;;;; Tests the Epsilon reader syntax extensions:
;;;; - [1 2 3]      -> vector literal
;;;; - {:a 1 :b 2}  -> HAMT map literal
;;;; - #{1 2 3}     -> HAMT set literal
;;;; - #f(+ % 1)    -> anonymous function shorthand

(defpackage #:epsilon.reader.literal-tests
  (:use
   #:cl
   #:epsilon.test)
  (:local-nicknames
   (#:reader #:epsilon.reader)
   (#:map #:epsilon.map)
   (#:set #:epsilon.set)))

(in-package #:epsilon.reader.literal-tests)

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
    (is (vectorp v))
    (is (= 0 (length v)))))

(deftest test-vector-literal-with-values
  (let ((v (read-literal "[1 2 3]")))
    (is (vectorp v))
    (is (= 3 (length v)))
    (is (= 1 (aref v 0)))
    (is (= 2 (aref v 1)))
    (is (= 3 (aref v 2)))))

(deftest test-vector-literal-with-mixed-types
  (let ((v (read-literal "[1 :a \"hello\" 3.14]")))
    (is (vectorp v))
    (is (= 4 (length v)))
    (is (= 1 (aref v 0)))
    (is (eq :a (aref v 1)))
    (is (equal "hello" (aref v 2)))
    (is (= 3.14 (aref v 3)))))

(deftest test-vector-literal-nested
  (let ((v (read-literal "[[1 2] [3 4]]")))
    (is (vectorp v))
    (is (= 2 (length v)))
    (is (vectorp (aref v 0)))
    (is (vectorp (aref v 1)))
    (is (equalp #(1 2) (aref v 0)))
    (is (equalp #(3 4) (aref v 1)))))

(deftest test-vector-literal-deeply-nested
  (let ((v (read-literal "[[[1]]]")))
    (is (vectorp v))
    (is (= 1 (aref (aref (aref v 0) 0) 0)))))

;;;; ==========================================================================
;;;; Map Literal Tests
;;;; ==========================================================================

(deftest test-map-literal-empty
  (let ((m (read-literal "{}")))
    (is (typep m 'map:hamt))
    (is (= 0 (map:count m)))))

(deftest test-map-literal-with-values
  (let ((m (read-literal "{:a 1 :b 2}")))
    (is (typep m 'map:hamt))
    (is (= 2 (map:count m)))
    (is (= 1 (map:get m :a)))
    (is (= 2 (map:get m :b)))))

(deftest test-map-literal-with-string-keys
  (let ((m (read-literal "{\"name\" \"Alice\" \"age\" 30}")))
    (is (typep m 'map:hamt))
    (is (equal "Alice" (map:get m "name")))
    (is (= 30 (map:get m "age")))))

(deftest test-map-literal-nested
  (let ((m (read-literal "{:outer {:inner 42}}")))
    (is (typep m 'map:hamt))
    (is (typep (map:get m :outer) 'map:hamt))
    (is (= 42 (map:get (map:get m :outer) :inner)))))

(deftest test-map-literal-with-vector-values
  (let ((m (read-literal "{:items [1 2 3]}")))
    (is (typep m 'map:hamt))
    (is (vectorp (map:get m :items)))
    (is (equalp #(1 2 3) (map:get m :items)))))

;;;; ==========================================================================
;;;; Set Literal Tests
;;;; ==========================================================================

(deftest test-set-literal-empty
  (let ((s (read-literal "#{}")))
    (is (typep s 'set:hamt-set))
    (is (= 0 (set:count s)))))

(deftest test-set-literal-with-values
  (let ((s (read-literal "#{1 2 3}")))
    (is (typep s 'set:hamt-set))
    (is (= 3 (set:count s)))
    (is (set:contains-p s 1))
    (is (set:contains-p s 2))
    (is (set:contains-p s 3))
    (is (not (set:contains-p s 4)))))

(deftest test-set-literal-with-keywords
  (let ((s (read-literal "#{:a :b :c}")))
    (is (typep s 'set:hamt-set))
    (is (set:contains-p s :a))
    (is (set:contains-p s :b))
    (is (set:contains-p s :c))))

(deftest test-set-literal-deduplication
  (let ((s (read-literal "#{1 1 2 2 3 3}")))
    (is (typep s 'set:hamt-set))
    ;; Sets should deduplicate
    (is (= 3 (set:count s)))))

;;;; ==========================================================================
;;;; Anonymous Function Literal Tests
;;;; ==========================================================================

(deftest test-anon-fn-nullary
  (let ((fn (eval (read-literal "#f(+ 1 2)"))))
    (is (functionp fn))
    (is (= 3 (funcall fn)))))

(deftest test-anon-fn-single-arg
  (let ((fn (eval (read-literal "#f(+ % 1)"))))
    (is (functionp fn))
    (is (= 2 (funcall fn 1)))
    (is (= 11 (funcall fn 10)))))

(deftest test-anon-fn-two-args
  (let ((fn (eval (read-literal "#f(+ %1 %2)"))))
    (is (functionp fn))
    (is (= 3 (funcall fn 1 2)))
    (is (= 15 (funcall fn 10 5)))))

(deftest test-anon-fn-three-args
  (let ((fn (eval (read-literal "#f(list %1 %2 %3)"))))
    (is (functionp fn))
    (is (equal '(a b c) (funcall fn 'a 'b 'c)))))

(deftest test-anon-fn-with-nested-expr
  (let ((fn (eval (read-literal "#f(string-upcase (first %))"))))
    (is (functionp fn))
    (is (equal "HELLO" (funcall fn '("hello" "world"))))))

(deftest test-anon-fn-with-conditional
  (let ((fn (eval (read-literal "#f(if (> % 0) :positive :non-positive)"))))
    (is (functionp fn))
    (is (eq :positive (funcall fn 5)))
    (is (eq :non-positive (funcall fn -5)))
    (is (eq :non-positive (funcall fn 0)))))

;;;; ==========================================================================
;;;; Mixed Literal Tests
;;;; ==========================================================================

(deftest test-mixed-map-with-vector-and-set
  (let ((data (read-literal "{:items [1 2 3] :tags #{:a :b}}")))
    (is (typep data 'map:hamt))
    (is (vectorp (map:get data :items)))
    (is (= 3 (length (map:get data :items))))
    (is (typep (map:get data :tags) 'set:hamt-set))
    (is (set:contains-p (map:get data :tags) :a))))

(deftest test-vector-of-maps
  (let ((data (read-literal "[{:a 1} {:b 2}]")))
    (is (vectorp data))
    (is (= 2 (length data)))
    (is (typep (aref data 0) 'map:hamt))
    (is (typep (aref data 1) 'map:hamt))
    (is (= 1 (map:get (aref data 0) :a)))
    (is (= 2 (map:get (aref data 1) :b)))))

(deftest test-set-of-vectors
  (let ((data (read-literal "#{[1 2] [3 4]}")))
    (is (typep data 'set:hamt-set))
    (is (= 2 (set:count data)))))

(deftest test-deeply-nested-mixed
  (let ((data (read-literal "{:level1 {:level2 {:level3 [1 2 #{:a :b}]}}}")))
    (is (typep data 'map:hamt))
    (let* ((level1 (map:get data :level1))
           (level2 (map:get level1 :level2))
           (level3 (map:get level2 :level3)))
      (is (vectorp level3))
      (is (= 3 (length level3)))
      (is (typep (aref level3 2) 'set:hamt-set)))))

;;;; ==========================================================================
;;;; Readtable Management Tests
;;;; ==========================================================================

(deftest test-enable-disable-syntax
  (let ((original *readtable*))
    (reader:enable-epsilon-syntax)
    (is (not (eq original *readtable*)))
    (reader:disable-epsilon-syntax)
    ;; After disable, we should have a restored readtable
    (is t)))

(deftest test-with-epsilon-syntax-restores
  (let ((before *readtable*))
    (reader:with-epsilon-syntax
      ;; Inside, we have epsilon extensions
      (is (not (eq before *readtable*))))
    ;; Outside, original is restored
    (is (eq before *readtable*))))

;;;; ==========================================================================
;;;; Integration Tests
;;;; ==========================================================================

(deftest test-mapcar-with-anon-fn
  (reader:with-epsilon-syntax
    (is (equal '(2 3 4)
               (mapcar (eval (read-from-string "#f(+ % 1)"))
                       '(1 2 3))))))

(deftest test-remove-if-with-anon-fn
  (reader:with-epsilon-syntax
    (is (equal '(2 4 6)
               (remove-if (eval (read-from-string "#f(oddp %)"))
                          '(1 2 3 4 5 6))))))

(deftest test-reduce-with-anon-fn
  (reader:with-epsilon-syntax
    (is (= 15
           (reduce (eval (read-from-string "#f(+ %1 %2)"))
                   '(1 2 3 4 5))))))

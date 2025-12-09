(defpackage #:epsilon.match.tests
  (:use
   #:cl
   #:epsilon.test)
  (:local-nicknames
   (#:m #:epsilon.match)
   (#:opt #:epsilon.option)
   (#:res #:epsilon.result)
   (#:map #:epsilon.map)))

(in-package #:epsilon.match.tests)

;;; Basic pattern matching tests

(deftest test-wildcard-pattern
  (is (= 42 (m:match 42
              (_ 42)))))

(deftest test-variable-binding
  (is (= 10 (m:match 5
              (x (* x 2))))))

(deftest test-literal-number
  (is (eq :one (m:match 1
                 (1 :one)
                 (2 :two)
                 (_ :other)))))

(deftest test-literal-string
  (is (eq :hello (m:match "hello"
                   ("hello" :hello)
                   (_ :other)))))

(deftest test-literal-keyword
  (is (eq :found (m:match :foo
                   (:foo :found)
                   (_ :not-found)))))

(deftest test-quoted-literal
  (is (eq :matched (m:match 'foo
                     ('foo :matched)
                     (_ :not-matched)))))

(deftest test-nil-literal
  (is (eq :nil (m:match nil
                 (nil :nil)
                 (_ :other)))))

(deftest test-t-literal
  (is (eq :true (m:match t
                  (t :true)
                  (_ :false)))))

;;; Predicate patterns

(deftest test-predicate-pattern
  (is (eq :number (m:match 42
                    ((? numberp) :number)
                    (_ :other)))))

(deftest test-predicate-with-binding
  (is (= 84 (m:match 42
              ((? numberp n) (* n 2))
              (_ 0)))))

(deftest test-predicate-failure
  (is (eq :other (m:match "hello"
                   ((? numberp n) n)
                   (_ :other)))))

;;; List patterns

(deftest test-empty-list-pattern
  (is (eq :empty (m:match '()
                   ((list) :empty)
                   (_ :not-empty)))))

(deftest test-single-element-list
  (is (= 5 (m:match '(5)
             ((list x) x)
             (_ 0)))))

(deftest test-two-element-list
  (is (= 7 (m:match '(3 4)
             ((list a b) (+ a b))
             (_ 0)))))

(deftest test-list-length-mismatch
  (is (eq :other (m:match '(1 2 3)
                   ((list a b) (+ a b))
                   (_ :other)))))

(deftest test-nested-list-pattern
  (is (equal '(1 2 3 4)
             (m:match '((1 2) (3 4))
               ((list (list a b) (list c d)) (list a b c d))
               (_ nil)))))

;;; List* patterns

(deftest test-list*-first-rest
  (is (= 1 (m:match '(1 2 3)
             ((list* first rest) first)
             (_ 0)))))

(deftest test-list*-rest
  (is (equal '(2 3)
             (m:match '(1 2 3)
               ((list* first rest) rest)
               (_ nil)))))

(deftest test-list*-two-heads
  (is (= 3 (m:match '(1 2 3 4)
             ((list* a b rest) (+ a b))
             (_ 0)))))

(deftest test-list*-empty-rest
  (is (equal nil
             (m:match '(1)
               ((list* x rest) rest)
               (_ :fail)))))

;;; Cons patterns

(deftest test-cons-pattern
  (is (= 1 (m:match '(1 . 2)
             ((cons h tl) h)
             (_ 0)))))

(deftest test-cons-tail
  (is (= 2 (m:match '(1 . 2)
             ((cons h tl) tl)
             (_ 0)))))

(deftest test-cons-on-list
  (is (equal '(2 3) (m:match '(1 2 3)
                      ((cons h tl) tl)
                      (_ nil)))))

;;; Vector patterns

(deftest test-vector-pattern
  (is (= 6 (m:match #(1 2 3)
             ((vector a b c) (+ a b c))
             (_ 0)))))

(deftest test-vector-length-mismatch
  (is (eq :other (m:match #(1 2)
                   ((vector a b c) (+ a b c))
                   (_ :other)))))

;;; Map patterns

(deftest test-map-single-key
  (let ((m (map:assoc map:+empty+ :name "Alice")))
    (is (equal "Alice"
               (m:match m
                 ((map :name n) n)
                 (_ nil))))))

(deftest test-map-multiple-keys
  (let ((m (map:assoc (map:assoc map:+empty+ :x 10) :y 20)))
    (is (= 30 (m:match m
                ((map :x x :y y) (+ x y))
                (_ 0))))))

(deftest test-map-missing-key
  (let ((m (map:assoc map:+empty+ :name "Alice")))
    (is (eq :missing (m:match m
                       ((map :age a) a)
                       (_ :missing))))))

;;; Or patterns

(deftest test-or-first-matches
  (is (eq :matched (m:match 1
                     ((or 1 2 3) :matched)
                     (_ :other)))))

(deftest test-or-second-matches
  (is (eq :matched (m:match 2
                     ((or 1 2 3) :matched)
                     (_ :other)))))

(deftest test-or-none-match
  (is (eq :other (m:match 5
                   ((or 1 2 3) :matched)
                   (_ :other)))))

;;; And patterns

(deftest test-and-pattern
  (is (= 10 (m:match 5
              ((and (? numberp) x) (* x 2))
              (_ 0)))))

(deftest test-and-pattern-failure
  (is (eq :fail (m:match "hello"
                  ((and (? numberp) x) x)
                  (_ :fail)))))

;;; Guard patterns

(deftest test-guard-success
  (is (eq :positive (m:match 5
                      ((guard n (> n 0)) :positive)
                      (_ :not-positive)))))

(deftest test-guard-failure
  (is (eq :not-positive (m:match -5
                          ((guard n (> n 0)) :positive)
                          (_ :not-positive)))))

(deftest test-guard-with-binding
  (is (= 10 (m:match 5
              ((guard n (evenp n)) (* n 3))
              ((guard n (oddp n)) (* n 2))
              (_ 0)))))

;;; Option patterns

(deftest test-some-pattern
  (is (= 42 (m:match (opt:some 42)
              ((some x) x)
              (none 0)))))

(deftest test-none-pattern
  (is (= 0 (m:match (opt:none)
             ((some x) x)
             (none 0)))))

(deftest test-nested-some
  (is (= 5 (m:match (opt:some (opt:some 5))
             ((some (some x)) x)
             (_ 0)))))

;;; Result patterns

(deftest test-ok-pattern
  (is (= 42 (m:match (res:ok 42)
              ((ok x) x)
              ((err e) 0)))))

(deftest test-err-pattern
  (is (equal "error" (m:match (res:err "error")
                       ((ok x) x)
                       ((err e) e)))))

(deftest test-ok-with-processing
  (is (= 84 (m:match (res:ok 42)
              ((ok n) (* n 2))
              ((err _) 0)))))

;;; if-match tests

(deftest test-if-match-success
  (is (= 10 (m:if-match (list a b) '(3 7)
              (+ a b)
              0))))

(deftest test-if-match-failure
  (is (= 0 (m:if-match (list a b) '(1 2 3)
             (+ a b)
             0))))

;;; when-match tests

(deftest test-when-match-success
  (is (= 6 (m:when-match (list a b c) '(1 2 3)
             (+ a b c)))))

(deftest test-when-match-failure
  (is (null (m:when-match (list a b) '(1 2 3)
              (+ a b)))))

;;; match-let tests

(deftest test-match-let-single
  (is (= 15 (m:match-let (((list x y) '(5 10)))
              (+ x y)))))

(deftest test-match-let-multiple
  (is (= 77 (m:match-let (((list a b) '(3 4))
                          ((list c d) '(5 6)))
              (* (+ a b) (+ c d))))))

;;; match-lambda tests

(deftest test-match-lambda
  (let ((f (m:match-lambda
             ((list a b) (+ a b))
             ((? numberp n) (* n 2))
             (_ 0))))
    (is (= 7 (funcall f '(3 4))))
    (is (= 10 (funcall f 5)))
    (is (= 0 (funcall f "hello")))))

;;; Complex patterns

(deftest test-deeply-nested-pattern
  (is (= 15 (m:match '((1 2) ((3 4) 5))
              ((list (list a b) (list (list c d) e))
               (+ a b c d e))
              (_ 0)))))

(deftest test-mixed-pattern-types
  (let ((data (map:assoc (map:assoc map:+empty+ :values '(1 2 3)) :name "test")))
    (is (= 6 (m:match data
               ((map :values (list* a rest) :name n)
                (+ a (reduce #'+ rest)))
               (_ 0))))))

;;; Error handling

(deftest test-no-match-error
  (is-thrown (error)
    (m:match 42
      ((? stringp s) s))))

(deftest test-match-let-error-on-failure
  (is-thrown (error)
    (m:match-let (((list a b) '(1 2 3)))
      (+ a b))))

(defpackage #:epsilon.do.tests
  (:use
   #:cl
   #:epsilon.test)
  (:local-nicknames
   (#:do #:epsilon.do)
   (#:opt #:epsilon.option)
   (#:res #:epsilon.result)))

(in-package #:epsilon.do.tests)

;;; m-bind tests for Option

(deftest test-m-bind-some
  (let ((result (do:m-bind (opt:some 5)
                           (lambda (x) (opt:some (* x 2))))))
    (is (opt:some-p result))
    (is (= 10 (opt:unwrap result)))))

(deftest test-m-bind-none
  (let ((result (do:m-bind (opt:none)
                           (lambda (x) (opt:some (* x 2))))))
    (is (opt:none-p result))))

(deftest test-m-bind-some-to-none
  (let ((result (do:m-bind (opt:some 5)
                           (lambda (x)
                             (declare (ignore x))
                             (opt:none)))))
    (is (opt:none-p result))))

;;; m-bind tests for Result

(deftest test-m-bind-ok
  (let ((result (do:m-bind (res:ok 10)
                           (lambda (x) (res:ok (* x 3))))))
    (is (res:ok-p result))
    (is (= 30 (res:unwrap result)))))

(deftest test-m-bind-err
  (let ((result (do:m-bind (res:err "failed")
                           (lambda (x) (res:ok (* x 2))))))
    (is (res:err-p result))
    (is (equal "failed" (res:unwrap-err result)))))

(deftest test-m-bind-ok-to-err
  (let ((result (do:m-bind (res:ok 5)
                           (lambda (x)
                             (declare (ignore x))
                             (res:err "new error")))))
    (is (res:err-p result))
    (is (equal "new error" (res:unwrap-err result)))))

;;; m-bind tests for List

(deftest test-m-bind-list
  (let ((result (do:m-bind '(1 2 3)
                           (lambda (x) (list x x)))))
    (is (equal '(1 1 2 2 3 3) result))))

(deftest test-m-bind-empty-list
  (let ((result (do:m-bind nil
                           (lambda (x) (list x x)))))
    (is (null result))))

(deftest test-m-bind-list-cartesian
  (let ((result (do:m-bind '(1 2)
                           (lambda (x)
                             (do:m-bind '(a b)
                                        (lambda (y)
                                          (list (list x y))))))))
    (is (equal '((1 a) (1 b) (2 a) (2 b)) result))))

;;; m-return tests

(deftest test-m-return-option
  (let ((result (do:m-return 42 :option)))
    (is (opt:some-p result))
    (is (= 42 (opt:unwrap result)))))

(deftest test-m-return-result
  (let ((result (do:m-return "hello" :result)))
    (is (res:ok-p result))
    (is (equal "hello" (res:unwrap result)))))

(deftest test-m-return-list
  (let ((result (do:m-return 'x :list)))
    (is (equal '(x) result))))

;;; do-m macro tests for Option

(deftest test-do-option-simple
  (let ((result (do:do-option
                  (x <- (opt:some 5))
                  (do:m-return (* x 2) :option))))
    (is (opt:some-p result))
    (is (= 10 (opt:unwrap result)))))

(deftest test-do-option-chain
  (let ((result (do:do-option
                  (x <- (opt:some 3))
                  (y <- (opt:some 4))
                  (do:m-return (+ x y) :option))))
    (is (opt:some-p result))
    (is (= 7 (opt:unwrap result)))))

(deftest test-do-option-short-circuit
  (let ((result (do:do-option
                  (x <- (opt:some 5))
                  (y <- (opt:none))
                  (do:m-return (+ x y) :option))))
    (is (opt:none-p result))))

(deftest test-do-option-with-let
  (let ((result (do:do-option
                  (x <- (opt:some 10))
                  (let doubled (* x 2))
                  (do:m-return doubled :option))))
    (is (opt:some-p result))
    (is (= 20 (opt:unwrap result)))))

(deftest test-do-option-guard-success
  (let ((result (do:do-option
                  (x <- (opt:some 10))
                  (guard (> x 5))
                  (do:m-return x :option))))
    (is (opt:some-p result))
    (is (= 10 (opt:unwrap result)))))

(deftest test-do-option-guard-failure
  (let ((result (do:do-option
                  (x <- (opt:some 3))
                  (guard (> x 5))
                  (do:m-return x :option))))
    (is (opt:none-p result))))

;;; do-m macro tests for Result

(deftest test-do-result-simple
  (let ((result (do:do-result
                  (x <- (res:ok 7))
                  (do:m-return (* x 3) :result))))
    (is (res:ok-p result))
    (is (= 21 (res:unwrap result)))))

(deftest test-do-result-chain
  (let ((result (do:do-result
                  (a <- (res:ok 2))
                  (b <- (res:ok 3))
                  (c <- (res:ok 4))
                  (do:m-return (* a b c) :result))))
    (is (res:ok-p result))
    (is (= 24 (res:unwrap result)))))

(deftest test-do-result-error-propagation
  (let ((result (do:do-result
                  (x <- (res:ok 5))
                  (y <- (res:err "oops"))
                  (do:m-return (+ x y) :result))))
    (is (res:err-p result))
    (is (equal "oops" (res:unwrap-err result)))))

(deftest test-do-result-with-let
  (let ((result (do:do-result
                  (x <- (res:ok 5))
                  (let square (* x x))
                  (do:m-return square :result))))
    (is (res:ok-p result))
    (is (= 25 (res:unwrap result)))))

;;; do-m macro tests for List

(deftest test-do-list-simple
  (let ((result (do:do-list
                  (x <- '(1 2 3))
                  (do:m-return (* x x) :list))))
    (is (equal '(1 4 9) result))))

(deftest test-do-list-comprehension
  (let ((result (do:do-list
                  (x <- '(1 2))
                  (y <- '(a b))
                  (do:m-return (list x y) :list))))
    (is (equal '((1 a) (1 b) (2 a) (2 b)) result))))

(deftest test-do-list-with-guard
  (let ((result (do:do-list
                  (x <- '(1 2 3 4 5))
                  (guard (evenp x))
                  (do:m-return x :list))))
    (is (equal '(2 4) result))))

(deftest test-do-list-empty
  (let ((result (do:do-list
                  (x <- nil)
                  (do:m-return (* x 2) :list))))
    (is (null result))))

(deftest test-do-list-pythagorean-triples
  ;; Classic list comprehension example
  (let ((result (do:do-list
                  (a <- '(1 2 3 4 5))
                  (b <- '(1 2 3 4 5))
                  (c <- '(1 2 3 4 5))
                  (guard (and (<= a b)
                              (= (+ (* a a) (* b b)) (* c c))))
                  (do:m-return (list a b c) :list))))
    (is (equal '((3 4 5)) result))))

;;; let-m tests

(deftest test-let-m-simple
  (let ((result (do:let-m ((x (opt:some 5)))
                  (do:m-return (* x 2) :option))))
    (is (opt:some-p result))
    (is (= 10 (opt:unwrap result)))))

(deftest test-let-m-chain
  (let ((result (do:let-m ((x (res:ok 3))
                           (y (res:ok 4)))
                  (do:m-return (+ x y) :result))))
    (is (res:ok-p result))
    (is (= 7 (res:unwrap result)))))

(deftest test-let-m-dependent
  (let ((result (do:let-m ((x (opt:some 5))
                           (y (opt:some (* x 2))))
                  (do:m-return (+ x y) :option))))
    (is (opt:some-p result))
    (is (= 15 (opt:unwrap result)))))

(deftest test-let-m-short-circuit
  (let ((result (do:let-m ((x (res:ok 10))
                           (y (res:err "error"))
                           (z (res:ok 20)))
                  (do:m-return (+ x y z) :result))))
    (is (res:err-p result))
    (is (equal "error" (res:unwrap-err result)))))

;;; Effects and sequencing

(deftest test-do-sequence-effects
  (let ((calls nil))
    (flet ((track (x)
             (push x calls)
             (opt:some x)))
      (do:do-option
        (track 1)
        (track 2)
        (track 3)
        (opt:some :done)))
    (is (equal '(3 2 1) calls))))

;;; Edge cases

(deftest test-do-single-expression
  (let ((result (do:do-option (opt:some 42))))
    (is (opt:some-p result))
    (is (= 42 (opt:unwrap result)))))

(deftest test-nested-do
  (let ((result (do:do-option
                  (x <- (opt:some 5))
                  (y <- (do:do-option
                          (a <- (opt:some 3))
                          (do:m-return (* a 2) :option)))
                  (do:m-return (+ x y) :option))))
    (is (opt:some-p result))
    (is (= 11 (opt:unwrap result)))))

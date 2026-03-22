(defpackage epsilon.sequence-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.sequence seq)
            (epsilon.string str))
  (:enter t))

(deftest test-sequence-creation
  (assert-true (seq:sequence-p (seq:seq '(1 2 3))))
  (assert-true (seq:sequence-p (seq:seq #(1 2 3))))
  (assert-true (seq:sequence-p (seq:seq nil)))
  (assert-true (seq:empty-p (seq:seq nil)))
  (assert-true (not (seq:empty-p (seq:seq '(1))))))

(deftest test-sequence-realization
  (let ((s (seq:seq '(1 2 3))))
    (assert-true (= 1 (seq:first s)))
    (assert-true (= 2 (seq:first (seq:rest s))))
    (assert-true (= 3 (seq:first (seq:rest (seq:rest s)))))
    (assert-true (seq:empty-p (seq:rest (seq:rest (seq:rest s)))))))

(deftest test-realize
  (assert-true (equal '(1 2 3) (seq:realize (seq:seq '(1 2 3)))))
  (assert-true (equal '() (seq:realize (seq:seq nil)))))

(deftest test-map
  (assert-true (equal '(2 3 4) (seq:realize (seq:map (lambda (x) (1+ x)) (seq:seq '(1 2 3))))))
  (assert-true (equal '(5 7 9) (seq:realize (seq:map #'+ (seq:seq '(1 2 3)) (seq:seq '(4 5 6))))))
  (assert-true (seq:empty-p (seq:map #'+ (seq:seq '()) (seq:seq '(1 2 3))))))

(deftest test-drop
  (assert-true (equal '(3) (seq:realize (seq:drop 2 (seq:seq '(1 2 3))))))
  (assert-true (equal '() (seq:realize (seq:drop 4 (seq:seq '(1 2 3))))))
  (assert-true (seq:empty-p (seq:drop 1 (seq:seq '(1))))))

(deftest test-filter
  (assert-true (equal '(1 3 5) (seq:realize (seq:filter #'oddp (seq:seq '(1 2 3 4 5 6))))))
  (assert-true (equal '(2 4 6) (seq:realize (seq:filter #'evenp (seq:seq '(1 2 3 4 5 6))))))
  (assert-true (seq:empty-p (seq:filter (constantly nil) (seq:seq '(1 2 3))))))

(deftest test-split
  (assert-true (equal '("hello" "world") (seq:realize (str:split #\Space "hello world"))))
  (assert-true (equal '("a" "b" "c") (seq:realize (str:split #\, "a,b,c"))))
  (assert-true (equal '("abc") (seq:realize (str:split #\, "abc"))))
  (assert-true (equal '("" "") (seq:realize (str:split #\a "a"))))
  (assert-true (equal '("") (seq:realize (str:split #\a "")))))

(deftest test-every-p
  ;; Test with all elements satisfying predicate
  (assert-true (seq:every-p #'evenp (seq:seq '(2 4 6 8))))
  (assert-true (seq:every-p #'oddp (seq:seq '(1 3 5 7))))

  ;; Test with some elements not satisfying predicate
  (assert-true (not (seq:every-p #'evenp (seq:seq '(2 4 5 8)))))
  (assert-true (not (seq:every-p #'oddp (seq:seq '(1 3 4 7)))))

  ;; Test with empty sequence (should return true)
  (assert-true (seq:every-p #'evenp seq:*empty*))
  (assert-true (seq:every-p (constantly nil) seq:*empty*))

  ;; Test with single element
  (assert-true (seq:every-p #'evenp (seq:seq '(2))))
  (assert-true (not (seq:every-p #'evenp (seq:seq '(3))))))

(deftest test-not-empty-p
  ;; Test with non-empty sequences
  (assert-true (seq:not-empty-p (seq:seq '(1 2 3))))
  (assert-true (seq:not-empty-p (seq:seq '(1))))

  ;; Test with empty sequence
  (assert-true (not (seq:not-empty-p seq:*empty*)))
  (assert-true (not (seq:not-empty-p (seq:seq '()))))
  (assert-true (not (seq:not-empty-p (seq:seq nil)))))

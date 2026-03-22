(defpackage epsilon.function-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.function fn))
  (:enter t))

;;; Helper functions

(defun add1 (x) (+ x 1))
(defun mul2 (x) (* x 2))
(defun add (x y) (+ x y))

;;; Compose tests

(deftest test-compose-basic
  (assert-true (= 4 (funcall (fn:compose #'mul2 #'add1) 1)))  ; (* (+ 1 1) 2)
  (assert-true (= 3 (funcall (fn:compose #'add1 #'mul2) 1)))) ; (+ (* 1 2) 1)

(deftest test-compose-single
  (assert-true (= 2 (funcall (fn:compose #'add1) 1))))

(deftest test-compose-many
  (let ((f (fn:compose #'add1 #'add1 #'add1)))
    (assert-true (= 4 (funcall f 1)))))

;;; Partial tests

(deftest test-partial-basic
  (let ((add5 (fn:partial #'+ 5)))
    (assert-true (= 8 (funcall add5 3)))
    (assert-true (= 15 (funcall add5 10)))))

(deftest test-partial-multiple-args
  (let ((make-list-with-prefix (fn:partial #'list 'prefix)))
    (assert-true (equal '(prefix a b) (funcall make-list-with-prefix 'a 'b)))))

;;; Integration tests

(deftest test-compose-with-partial
  (let ((process (fn:compose (fn:partial #'* 2)    ; multiply by 2
                             (fn:partial #'+ 10)))) ; add 10
    (assert-true (= 24 (funcall process 2)))))  ; (* (+ 2 10) 2) = 24

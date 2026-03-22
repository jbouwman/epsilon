(defpackage epsilon.reader-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.reader reader))
  (:enter t))

;;; Helper to read with epsilon syntax

(defun read-fn (string)
  "Read a string using epsilon reader extensions"
  (reader:with-epsilon-reader
    (read-from-string string)))

;;; Helper to check lambda structure
(defun lambda-form-p (form num-args)
  "Check if FORM is a lambda with NUM-ARGS parameters"
  (and (listp form)
       (eq (first form) 'lambda)
       (listp (second form))
       (= (length (second form)) num-args)))

;;; Basic parsing tests

(deftest test-nullary-function
  (let ((fn (read-fn "#f(list)")))
    (assert-true (lambda-form-p fn 0))
    (assert-true (equal '(list) (third fn)))))

(deftest test-single-arg-percent
  (let ((fn (read-fn "#f(+ % 1)")))
    (assert-true (lambda-form-p fn 1))
    ;; Check the body has the right structure
    (assert-true (eq '+ (first (third fn))))))

(deftest test-single-arg-execution
  (let ((fn (eval (read-fn "#f(+ % 1)"))))
    (assert-true (= 2 (funcall fn 1)))
    (assert-true (= 11 (funcall fn 10)))))

(deftest test-two-args
  (let ((fn (read-fn "#f(+ %1 %2)")))
    (assert-true (lambda-form-p fn 2))))

(deftest test-two-args-execution
  (let ((fn (eval (read-fn "#f(+ %1 %2)"))))
    (assert-true (= 3 (funcall fn 1 2)))
    (assert-true (= 15 (funcall fn 10 5)))))

(deftest test-three-args
  (let ((fn (read-fn "#f(list %1 %2 %3)")))
    (assert-true (lambda-form-p fn 3))))

(deftest test-three-args-execution
  (let ((fn (eval (read-fn "#f(list %1 %2 %3)"))))
    (assert-true (equal '(a b c) (funcall fn 'a 'b 'c)))))

(deftest test-non-sequential-args
  ;; Using %1 and %3 should still create 3-arg lambda
  (let ((fn (read-fn "#f(list %1 %3)")))
    (assert-true (lambda-form-p fn 3))))

(deftest test-non-sequential-execution
  (let ((fn (eval (read-fn "#f(cons %1 %3)"))))
    (assert-true (equal '(a . c) (funcall fn 'a 'b 'c)))))

;;; Nested expression tests

(deftest test-nested-expression
  (let ((fn (read-fn "#f(string-upcase (first %))")))
    (assert-true (lambda-form-p fn 1))))

(deftest test-nested-execution
  (let ((fn (eval (read-fn "#f(string-upcase (first %))"))))
    (assert-true (equal "HELLO" (funcall fn '("hello" "world"))))))

(deftest test-deeply-nested
  (let ((fn (eval (read-fn "#f(car (cdr (car %)))"))))
    (assert-true (= 2 (funcall fn '((1 2 3)))))))

;;; Edge cases

(deftest test-nullary-execution
  (let ((fn (eval (read-fn "#f(+ 1 2)"))))
    (assert-true (= 3 (funcall fn)))))

(deftest test-percent-in-string
  ;; % in a string should not be treated as an argument
  (let ((fn (read-fn "#f(format nil \"~a%\" %)")))
    (assert-true (lambda-form-p fn 1))))

(deftest test-complex-body
  (let ((fn (eval (read-fn "#f(if (> % 0) :positive :non-positive)"))))
    (assert-true (eq :positive (funcall fn 5)))
    (assert-true (eq :non-positive (funcall fn -5)))
    (assert-true (eq :non-positive (funcall fn 0)))))

;;; Integration tests with standard functions

(deftest test-mapcar-integration
  (reader:with-epsilon-reader
    (assert-true (equal '(2 3 4)
               (mapcar (eval (read-from-string "#f(+ % 1)"))
                       '(1 2 3))))))

(deftest test-remove-if-integration
  (reader:with-epsilon-reader
    (assert-true (equal '(2 4 6)
               (remove-if (eval (read-from-string "#f(oddp %)"))
                          '(1 2 3 4 5 6))))))

(deftest test-reduce-integration
  (reader:with-epsilon-reader
    (assert-true (= 15
           (reduce (eval (read-from-string "#f(+ %1 %2)"))
                   '(1 2 3 4 5))))))

(deftest test-sort-integration
  (reader:with-epsilon-reader
    (let* ((compare-fn (eval (read-from-string "#f(< %1 %2)")))
           (result (sort (list 3 1 4 1 5 9 2 6) compare-fn)))
      ;; Just check it's sorted in ascending order
      (assert-true (= 1 (first result)))
      (assert-true (= 9 (car (last result)))))))

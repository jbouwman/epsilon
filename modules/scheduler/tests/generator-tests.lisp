;;;; Generator tests

(defpackage :epsilon.scheduler.generator.tests
  (:use :cl :epsilon.test)
  (:import (epsilon.scheduler.generator gen)))

(in-package :epsilon.scheduler.generator.tests)

;;; ---- Basic counting --------------------------------------------------------

(deftest test-generator-yields-sequence
  "A generator yields each value passed to YIELD in order."
  (let ((g (gen:make-generator
             (lambda ()
               (gen:yield 1)
               (gen:yield 2)
               (gen:yield 3)))))
    (multiple-value-bind (v s) (gen:generator-next g)
      (assert-= 1 v) (assert-eq t s))
    (multiple-value-bind (v s) (gen:generator-next g)
      (assert-= 2 v) (assert-eq t s))
    (multiple-value-bind (v s) (gen:generator-next g)
      (assert-= 3 v) (assert-eq t s))
    (multiple-value-bind (v s) (gen:generator-next g)
      (assert-true (null v))
      (assert-eq :done s))))

;;; ---- Defgenerator ----------------------------------------------------------

(gen:defgenerator integers (n)
  (dotimes (i n) (gen:yield i)))

(deftest test-defgenerator-and-do-generator
  "DEFGENERATOR builds a constructor and DO-GENERATOR iterates."
  (let ((collected '()))
    (gen:do-generator (v (integers 5))
      (push v collected))
    (assert-equal '(0 1 2 3 4) (nreverse collected))))

;;; ---- Yield through nested calls --------------------------------------------

(deftest test-yield-through-recursive-call
  "YIELD works from arbitrary call depth (the whole point of stackful
fibers)."
  (let* ((collected '())
         (g (gen:make-generator
              (lambda ()
                (labels ((recur (n)
                           (when (plusp n)
                             (gen:yield n)
                             (recur (1- n)))))
                  (recur 3))))))
    (gen:do-generator (v g)
      (push v collected))
    (assert-equal '(3 2 1) (nreverse collected))))

;;; ---- Errors propagate ------------------------------------------------------

(deftest test-error-propagates-on-next
  "An error raised inside the generator body is raised out of the
GENERATOR-NEXT that triggered it."
  (let ((g (gen:make-generator
             (lambda ()
               (gen:yield :first)
               (error "boom")))))
    (multiple-value-bind (v s) (gen:generator-next g)
      (assert-eq :first v)
      (assert-eq t s))
    (handler-case
        (progn (gen:generator-next g)
               (assert-true nil))
      (error (e)
        (assert-true (search "boom" (format nil "~A" e)))))))

;;; ---- Close mid-iteration ---------------------------------------------------

(deftest test-close-mid-iteration
  "Closing a live generator destroys its fiber and marks it done."
  (let ((g (gen:make-generator
             (lambda ()
               (loop (gen:yield :tick))))))
    (multiple-value-bind (v s) (gen:generator-next g)
      (assert-eq :tick v)
      (assert-eq t s))
    (gen:generator-close g)
    (assert-true (gen:generator-done-p g))
    (multiple-value-bind (v s) (gen:generator-next g)
      (assert-true (null v))
      (assert-eq :done s))))

;;; ---- Large sequence ---------------------------------------------------------

(deftest test-large-sequence
  "Generator producing many values matches the equivalent list."
  (let ((expected (loop for i below 1000 collect i))
        (collected '())
        (g (gen:make-generator
             (lambda () (dotimes (i 1000) (gen:yield i))))))
    (gen:do-generator (v g)
      (push v collected))
    (assert-equal expected (nreverse collected))))

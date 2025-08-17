;;;; Test module for epsilon-mode testing

(defpackage test-project.test
  (:use cl)
  (:export
   #:add-numbers
   #:multiply-numbers
   #:example-function))

(in-package test-project.test)

(defun add-numbers (a b)
  "Add two numbers together."
  (+ a b))

(defun multiply-numbers (a b)
  "Multiply two numbers."
  (* a b))

(defun example-function (x)
  "An example function for testing."
  (if (numberp x)
      (* x 2)
      nil))
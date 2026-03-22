;;;;  tests for epsilon.lexer

(defpackage epsilon.lexer-tests
  (:use :cl :epsilon.test)
  (:require (epsilon.lexer lexer))
  (:enter t))

;;; Helper functions

(defun make-test-lexer (input-string)
  "Create a lexer from a string for testing"
  (lexer:make-lexer (make-string-input-stream input-string)))

;;; Basic Lexer Tests

(deftest lexer-creation-test
  "Test lexer creation and initial state"
  (let ((lex (make-test-lexer "hello")))
    (assert-true (typep lex 'lexer:lexer))
    (multiple-value-bind (pos line col) (lexer:lexer-position lex)
      (assert-= pos 0)
      (assert-= line 1)
      (assert-= col 1))))

(deftest lexer-peek-test
  "Test basic peek functionality"
  (let ((lex (make-test-lexer "abc")))
    ;; Peek should return first character without consuming
    (assert-equal (lexer:peek lex) #\a)
    (assert-equal (lexer:peek lex) #\a)  ; Should still be #\a

    ;; Position should not change
    (multiple-value-bind (pos line col) (lexer:lexer-position lex)
      (declare (ignore line))
      (assert-= pos 0)
      (assert-= col 1))))

(deftest lexer-next-test
  "Test next functionality for consuming characters"
  (let ((lex (make-test-lexer "hello")))
    ;; Consume first character
    (assert-equal (lexer:next lex) #\h)
    (multiple-value-bind (pos line col) (lexer:lexer-position lex)
      (declare (ignore line))
      (assert-= pos 1)
      (assert-= col 2))

    ;; Peek after next
    (assert-equal (lexer:peek lex) #\e)

    ;; Consume more
    (assert-equal (lexer:next lex) #\e)
    (assert-equal (lexer:next lex) #\l)
    (assert-equal (lexer:next lex) #\l)
    (assert-equal (lexer:next lex) #\o)

    ;; At end
    (assert-true (lexer:at-end-p lex))
    (assert-equal (lexer:next lex) nil)))

(deftest lexer-at-end-test
  "Test end-of-input detection"
  (let ((lex (make-test-lexer "")))
    (assert-true (lexer:at-end-p lex))
    (assert-equal (lexer:peek lex) nil)
    (assert-equal (lexer:next lex) nil))

  (let ((lex (make-test-lexer "a")))
    (assert-not (lexer:at-end-p lex))
    (lexer:next lex)
    (assert-true (lexer:at-end-p lex))))

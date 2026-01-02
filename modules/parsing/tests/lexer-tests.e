;;;;  tests for epsilon.lexer

(package epsilon.lexer-tests
  (use epsilon.test)
  (import (epsilon.lexer lexer)))

;;; Helper functions

(defun make-test-lexer (input-string)
  "Create a lexer from a string for testing"
  (lexer:make-lexer (make-string-input-stream input-string)))

;;; Basic Lexer Tests

(deftest lexer-creation-test
  "Test lexer creation and initial state"
  (let ((lex (make-test-lexer "hello")))
    (is (typep lex 'lexer:lexer))
    (multiple-value-bind (pos line col) (lexer:lexer-position lex)
      (is-= pos 0)
      (is-= line 1)
      (is-= col 1))))

(deftest lexer-peek-test
  "Test basic peek functionality"
  (let ((lex (make-test-lexer "abc")))
    ;; Peek should return first character without consuming
    (is-equal (lexer:peek lex) #\a)
    (is-equal (lexer:peek lex) #\a)  ; Should still be #\a

    ;; Position should not change
    (multiple-value-bind (pos line col) (lexer:lexer-position lex)
      (declare (ignore line))
      (is-= pos 0)
      (is-= col 1))))

(deftest lexer-next-test
  "Test next functionality for consuming characters"
  (let ((lex (make-test-lexer "hello")))
    ;; Consume first character
    (is-equal (lexer:next lex) #\h)
    (multiple-value-bind (pos line col) (lexer:lexer-position lex)
      (declare (ignore line))
      (is-= pos 1)
      (is-= col 2))

    ;; Peek after next
    (is-equal (lexer:peek lex) #\e)

    ;; Consume more
    (is-equal (lexer:next lex) #\e)
    (is-equal (lexer:next lex) #\l)
    (is-equal (lexer:next lex) #\l)
    (is-equal (lexer:next lex) #\o)

    ;; At end
    (is (lexer:at-end-p lex))
    (is-equal (lexer:next lex) nil)))

(deftest lexer-at-end-test
  "Test end-of-input detection"
  (let ((lex (make-test-lexer "")))
    (is (lexer:at-end-p lex))
    (is-equal (lexer:peek lex) nil)
    (is-equal (lexer:next lex) nil))

  (let ((lex (make-test-lexer "a")))
    (is-not (lexer:at-end-p lex))
    (lexer:next lex)
    (is (lexer:at-end-p lex))))

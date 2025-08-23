;;;;  tests for epsilon.lexer

(defpackage epsilon.lexer.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (lexer epsilon.lexer)))

(in-package epsilon.lexer.tests)

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

;;; Peek-N Tests

(deftest lexer-peek-n-test
  "Test peeking at multiple characters"
  (let ((lex (make-test-lexer "hello world")))
    ;; Peek at 5 characters
    (is-equal (lexer:peek-n lex 5) '(#\h #\e #\l #\l #\o))
    
    ;; Position should not change
    (multiple-value-bind (pos line col) (lexer:lexer-position lex)
			 (declare (ignore line))
			 (is-= pos 0)
			 (is-= col 1))
    
    ;; Peek should still return first character
    (is-equal (lexer:peek lex) #\h)
    
    ;; Peek more than available
    (is-equal (lexer:peek-n lex 20) 
              '(#\h #\e #\l #\l #\o #\Space #\w #\o #\r #\l #\d))))

(deftest lexer-peek-n-after-consume-test
  "Test peek-n after consuming some characters"
  (let ((lex (make-test-lexer "0123456789")))
    ;; Consume first 3 characters
    (lexer:next lex)
    (lexer:next lex)
    (lexer:next lex)
    
    ;; Peek next 4
    (is-equal (lexer:peek-n lex 4) '(#\3 #\4 #\5 #\6))
    
    ;; Position should be at 3
    (is-= (lexer:lexer-position lex) 3)))

(deftest lexer-peek-n-empty-test
  "Test peek-n on empty input"
  (let ((lex (make-test-lexer "")))
    (is-equal (lexer:peek-n lex 5) '()))
  
  (let ((lex (make-test-lexer "ab")))
    (lexer:next lex)
    (lexer:next lex)
    (is-equal (lexer:peek-n lex 1) '())))

;;; Peek-Ahead Tests

(deftest lexer-peek-ahead-test
  "Test peeking at specific positions ahead"
  (let ((lex (make-test-lexer "0123456789")))
    ;; Peek at various positions
    (is-equal (lexer:peek-ahead lex 0) #\0)
    (is-equal (lexer:peek-ahead lex 1) #\1)
    (is-equal (lexer:peek-ahead lex 5) #\5)
    (is-equal (lexer:peek-ahead lex 9) #\9)
    (is-equal (lexer:peek-ahead lex 10) nil)  ; Beyond end
    
    ;; Position should not change
    (is-= (lexer:lexer-position lex) 0)))

(deftest lexer-peek-ahead-after-consume-test
  "Test peek-ahead after consuming characters"
  (let ((lex (make-test-lexer "abcdefgh")))
    ;; Consume 3 characters
    (lexer:next lex)
    (lexer:next lex)
    (lexer:next lex)
    
    ;; Now at position 3, looking at 'd'
    (is-equal (lexer:peek-ahead lex 0) #\d)
    (is-equal (lexer:peek-ahead lex 1) #\e)
    (is-equal (lexer:peek-ahead lex 4) #\h)
    (is-equal (lexer:peek-ahead lex 5) nil)))

;;; Buffer Management Tests

(deftest lexer-buffer-consistency-test
  "Test that buffering maintains consistency"
  (let ((lex (make-test-lexer "abcdef")))
    ;; Fill buffer with peek-n
    (is-equal (lexer:peek-n lex 4) '(#\a #\b #\c #\d))
    
    ;; Consume from buffer
    (is-equal (lexer:next lex) #\a)
    (is-equal (lexer:next lex) #\b)
    
    ;; Peek should now show 'c'
    (is-equal (lexer:peek lex) #\c)
    
    ;; Peek-n should work correctly
    (is-equal (lexer:peek-n lex 3) '(#\c #\d #\e))
    
    ;; Continue consuming
    (is-equal (lexer:next lex) #\c)
    (is-equal (lexer:next lex) #\d)
    (is-equal (lexer:next lex) #\e)
    (is-equal (lexer:next lex) #\f)
    
    ;; Should be at end
    (is (lexer:at-end-p lex))))

(deftest lexer-mixed-operations-test
  "Test mixing peek, peek-n, peek-ahead, and next"
  (let ((lex (make-test-lexer "0123456789")))
    (is-equal (lexer:peek lex) #\0)
    (is-equal (lexer:peek-n lex 3) '(#\0 #\1 #\2))
    (is-equal (lexer:peek-ahead lex 5) #\5)
    
    (is-equal (lexer:next lex) #\0)
    
    (is-equal (lexer:peek lex) #\1)
    (is-equal (lexer:peek-n lex 4) '(#\1 #\2 #\3 #\4))
    (is-equal (lexer:peek-ahead lex 3) #\4)
    
    (is-equal (lexer:next lex) #\1)
    (is-equal (lexer:next lex) #\2)
    
    (is-equal (lexer:peek lex) #\3)))

;;; Position Tracking Tests

(deftest lexer-line-column-tracking-test
  "Test line and column tracking"
  (let ((lex (make-test-lexer (format nil "ab~%cd~%ef"))))
    ;; Initial position
    (multiple-value-bind (pos line col) (lexer:lexer-position lex)
			 (is-= pos 0)
			 (is-= line 1)
			 (is-= col 1))
    
    ;; After 'a'
    (lexer:next lex)
    (multiple-value-bind (pos line col) (lexer:lexer-position lex)
			 (is-= pos 1)
			 (is-= line 1)
			 (is-= col 2))
    
    ;; After 'b'
    (lexer:next lex)
    (multiple-value-bind (pos line col) (lexer:lexer-position lex)
			 (is-= pos 2)
			 (is-= line 1)
			 (is-= col 3))
    
    ;; After newline
    (lexer:next lex)  ; consume newline
    (multiple-value-bind (pos line col) (lexer:lexer-position lex)
			 (is-= pos 3)
			 (is-= line 2)
			 (is-= col 1))
    
    ;; After 'c'
    (lexer:next lex)
    (multiple-value-bind (pos line col) (lexer:lexer-position lex)
			 (is-= pos 4)
			 (is-= line 2)
			 (is-= col 2))))

(deftest lexer-position-with-peek-test
  "Test that peek operations don't affect position"
  (let ((lex (make-test-lexer "hello")))
    ;; Multiple peek operations
    (lexer:peek lex)
    (lexer:peek-n lex 3)
    (lexer:peek-ahead lex 2)
    
    ;; Position should still be at start
    (multiple-value-bind (pos line col) (lexer:lexer-position lex)
			 (is-= pos 0)
			 (is-= line 1)
			 (is-= col 1))))

;;; Token Creation Tests

(deftest lexer-make-token-test
  "Test token creation with position information"
  (let ((lex (make-test-lexer (format nil "hello~%world"))))
    ;; Create token at start
    (let ((token1 (lexer:make-token lex :word "hello")))
      (is-eq (lexer:token-type token1) :word)
      (is-equal (lexer:token-value token1) "hello")
      (is-= (lexer:token-position token1) 0)
      (is-= (lexer:token-line token1) 1)
      (is-= (lexer:token-column token1) 1))
    
    ;; Move to next line
    (dotimes (i 6) (lexer:next lex))  ; consume "hello\n"
    
    ;; Create token on second line
    (let ((token2 (lexer:make-token lex :word "world")))
      (is-eq (lexer:token-type token2) :word)
      (is-equal (lexer:token-value token2) "world")
      (is-= (lexer:token-position token2) 6)
      (is-= (lexer:token-line token2) 2)
      (is-= (lexer:token-column token2) 1))))

;;; Utility Function Tests

(deftest lexer-skip-while-test
  "Test skip-while utility"
  (let ((lex (make-test-lexer "   hello")))
    ;; Skip whitespace
    (lexer:skip-while lex (lambda (ch) (char= ch #\Space)))
    (is-equal (lexer:peek lex) #\h)
    (is-= (lexer:lexer-position lex) 3)))

(deftest lexer-skip-whitespace-test
  "Test skip-whitespace utility"
  (let ((lex (make-test-lexer (format nil "  ~%~C hello" #\Tab))))
    (lexer:skip-whitespace lex)
    (is-equal (lexer:peek lex) #\h)
    (is-= (lexer:lexer-position lex) 5)
    (multiple-value-bind (pos line col) (lexer:lexer-position lex)
			 (declare (ignore pos))
			 (is-= line 2)
			 (is-= col 3))))

(deftest lexer-consume-while-test
  "Test consume-while utility"
  (let ((lex (make-test-lexer "123abc456")))
    ;; Consume digits
    (let ((digits (lexer:consume-while lex #'digit-char-p)))
      (is-equal digits "123"))
    (is-equal (lexer:peek lex) #\a)
    
    ;; Consume letters
    (let ((letters (lexer:consume-while lex #'alpha-char-p)))
      (is-equal letters "abc"))
    (is-equal (lexer:peek lex) #\4)))

(deftest lexer-consume-while-with-buffer-test
  "Test consume-while with pre-filled buffer"
  (let ((lex (make-test-lexer "abcd1234")))
    ;; Pre-fill buffer
    (lexer:peek-n lex 6)
    
    ;; Consume while should work with buffered data
    (let ((letters (lexer:consume-while lex #'alpha-char-p)))
      (is-equal letters "abcd"))
    (is-equal (lexer:peek lex) #\1)))

(deftest lexer-consume-string-test
  "Test consume-string utility"
  (let ((lex (make-test-lexer "hello world")))
    ;; Successful consume
    (is (lexer:consume-string lex "hello"))
    (is-equal (lexer:peek lex) #\Space)
    
    ;; Skip space
    (lexer:next lex)
    
    ;; Try to consume wrong string (should error)
    (handler-case
        (progn
          (lexer:consume-string lex "worlds")
          (is nil "Should have signaled an error"))
      (error ()
	     (is t "Correctly signaled an error")))))

;;; Error Handling Tests

(deftest lexer-error-test
  "Test lexer error with position information"
  (let ((lex (make-test-lexer "hello")))
    ;; Move to position 2
    (lexer:next lex)
    (lexer:next lex)
    
    ;; Trigger error
    (handler-case
        (lexer:lexer-error lex "Test error message")
      (lexer:lexer-error (e)
			 (is-equal (lexer:lexer-error-message e) "Test error message")
			 (is-= (lexer:lexer-error-position e) 2)
			 (is-= (lexer:lexer-error-line e) 1)
			 (is-= (lexer:lexer-error-column e) 3)))))

;;; Edge Cases and Performance Tests

(deftest lexer-empty-input-test
  "Test lexer with empty input"
  (let ((lex (make-test-lexer "")))
    (is (lexer:at-end-p lex))
    (is-equal (lexer:peek lex) nil)
    (is-equal (lexer:peek-n lex 5) '())
    (is-equal (lexer:peek-ahead lex 0) nil)
    (is-equal (lexer:next lex) nil)))

(deftest lexer-single-character-test
  "Test lexer with single character"
  (let ((lex (make-test-lexer "x")))
    (is-not (lexer:at-end-p lex))
    (is-equal (lexer:peek lex) #\x)
    (is-equal (lexer:peek-n lex 1) '(#\x))
    (is-equal (lexer:peek-n lex 5) '(#\x))
    (is-equal (lexer:peek-ahead lex 0) #\x)
    (is-equal (lexer:peek-ahead lex 1) nil)
    (is-equal (lexer:next lex) #\x)
    (is (lexer:at-end-p lex))))

(deftest lexer-large-peek-test
  "Test peeking with large amounts"
  (let ((lex (make-test-lexer (make-string 1000 :initial-element #\a))))
    ;; Large peek-n
    (let ((chars (lexer:peek-n lex 500)))
      (is-= (length chars) 500)
      (is (every (lambda (ch) (char= ch #\a)) chars)))
    
    ;; Position should not change
    (is-= (lexer:lexer-position lex) 0)))

(deftest lexer-unicode-test
  "Test lexer with unicode characters"
  (let ((lex (make-test-lexer "hello 世界 мир")))
    ;; ASCII part
    (dotimes (i 6) (lexer:next lex))
    
    ;; Unicode characters
    (is-equal (lexer:peek lex) #\世)
    (is-equal (lexer:next lex) #\世)
    (is-equal (lexer:next lex) #\界)
    (is-equal (lexer:next lex) #\Space)
    (is-equal (lexer:next lex) #\м)
    (is-equal (lexer:next lex) #\и)
    (is-equal (lexer:next lex) #\р)))

;;; Complex Scenarios

(deftest lexer-repeated-peek-n-test
  "Test that repeated peek-n calls work correctly"
  (let ((lex (make-test-lexer "abcdefghij")))
    ;; Multiple peek-n calls
    (is-equal (lexer:peek-n lex 3) '(#\a #\b #\c))
    (is-equal (lexer:peek-n lex 5) '(#\a #\b #\c #\d #\e))
    (is-equal (lexer:peek-n lex 2) '(#\a #\b))
    
    ;; Consume one
    (is-equal (lexer:next lex) #\a)
    
    ;; Peek-n should now start from 'b'
    (is-equal (lexer:peek-n lex 3) '(#\b #\c #\d))
    (is-equal (lexer:peek-n lex 7) '(#\b #\c #\d #\e #\f #\g #\h))))

(deftest lexer-alternating-peek-consume-test
  "Test alternating peek and consume operations"
  (let ((lex (make-test-lexer "0123456789")))
    (is-equal (lexer:peek lex) #\0)
    (is-equal (lexer:next lex) #\0)
    
    (is-equal (lexer:peek-n lex 2) '(#\1 #\2))
    (is-equal (lexer:next lex) #\1)
    
    (is-equal (lexer:peek-ahead lex 3) #\5)
    (is-equal (lexer:next lex) #\2)
    
    (is-equal (lexer:peek lex) #\3)
    (is-equal (lexer:peek-n lex 3) '(#\3 #\4 #\5))
    (is-equal (lexer:next lex) #\3)
    (is-equal (lexer:next lex) #\4)
    
    (is-equal (lexer:peek lex) #\5)))

(deftest lexer-whitespace-handling-test
  "Test various whitespace handling scenarios"
  (let ((lex (make-test-lexer (format nil " ~C~%  text  ~%" #\Tab))))
    ;; Skip leading whitespace
    (lexer:skip-whitespace lex)
    (is-equal (lexer:peek lex) #\t)
    
    ;; Consume "text"
    (is-equal (lexer:consume-while lex #'alpha-char-p) "text")
    
    ;; Skip trailing whitespace
    (lexer:skip-whitespace lex)
    (is (lexer:at-end-p lex))))

(deftest lexer-multiple-newlines-test
  "Test handling multiple newlines"
  (let ((lex (make-test-lexer (format nil "a~%~%b~%~%~%c"))))
    ;; Read 'a'
    (is-equal (lexer:next lex) #\a)
    (is-= (lexer:lexer-position lex) 1)
    
    ;; Skip newlines
    (lexer:skip-while lex (lambda (ch) (char= ch #\Newline)))
    
    ;; Should be at 'b'
    (is-equal (lexer:peek lex) #\b)
    (multiple-value-bind (pos line col) (lexer:lexer-position lex)
			 (declare (ignore pos col))
			 (is-= line 3))))

(deftest lexer-buffer-edge-cases-test
  "Test buffer edge cases"
  (let ((lex (make-test-lexer "abc")))
    ;; Peek beyond available
    (is-equal (lexer:peek-n lex 10) '(#\a #\b #\c))
    
    ;; Consume all through buffer
    (is-equal (lexer:next lex) #\a)
    (is-equal (lexer:next lex) #\b)
    (is-equal (lexer:next lex) #\c)
    
    ;; Peek at end
    (is-equal (lexer:peek lex) nil)
    (is-equal (lexer:peek-n lex 5) '())
    (is-equal (lexer:peek-ahead lex 0) nil)))

;;;; Tests for epsilon.parser

(defpackage epsilon.parser-tests
  (:use :cl :epsilon.test)
  (:require (epsilon.parser parser)
            (epsilon.sequence seq))
  (:enter t))

;;; Helper functions for testing

(defun make-test-input (items)
 "Create a test input sequence from a list of items"
 (seq:seq items))

(defun parse-success-p (result)
 "Check if parse result is a success"
 (parser:success-p result))

(defun parse-failure-p (result)
 "Check if parse result is a failure"
 (parser:failure-p result))

(defun success-value (result)
 "Extract value from successful parse result"
 (parser:success-value result))

(defun failure-message (result)
 "Extract message from failed parse result"
 (parser:failure-message result))

;;; Data Structure Tests

(deftest parse-state-structure-test
  "Test parse state data structure"
 (let ((state (parser:make-parse-state
        :position 5
        :remaining (make-test-input '(a b c))
        :context '("test context")
        :consumed-p t)))

  (assert-true (parser:parse-state-p state))
  (assert-= (parser:parse-state-position state) 5)
  (assert-equal (parser:parse-state-context state) '("test context"))
  (assert-true (parser:parse-state-consumed-p state))))

(deftest success-structure-test
  "Test success result structure"
 (let ((success (parser:make-success
         :value 42
         :state (parser:make-parse-state :position 1))))

  (assert-true (parser:success-p success))
  (assert-= (parser:success-value success) 42)
  (assert-true (parser:parse-state-p (parser:success-state success)))))

(deftest failure-structure-test
  "Test failure result structure"
 (let ((failure (parser:make-failure
         :message "Test error"
         :state (parser:make-parse-state :position 0)
         :expected "something")))

  (assert-true (parser:failure-p failure))
  (assert-equal (parser:failure-message failure) "Test error")
  (assert-equal (parser:failure-expected failure) "something")
  (assert-true (parser:parse-state-p (parser:failure-state failure)))))

;;; Basic Combinator Tests

(deftest return-combinator-test
  "Test return combinator"
 (let ((input (make-test-input '(a b c))))
  (let ((result (parser:parse (parser:return 42) input)))
   (assert-true (parse-success-p result))
   (assert-= (success-value result) 42)

   ;; Should not consume input
   (assert-= (parser:parse-state-position (parser:success-state result)) 0))))

(deftest fail-combinator-test
  "Test fail combinator"
 (let ((input (make-test-input '(a b c))))
  (let ((result (parser:parse (parser:fail "Test failure") input)))
   (assert-true (parse-failure-p result))
   (assert-equal (failure-message result) "Test failure"))))

(deftest satisfy-combinator-test
  "Test satisfy combinator"
 (let ((input (make-test-input '(42 "hello" :symbol))))

  ;; Test successful match
  (let ((result (parser:parse (parser:satisfy #'numberp) input)))
   (assert-true (parse-success-p result))
   (assert-= (success-value result) 42)
   (assert-= (parser:parse-state-position (parser:success-state result)) 1))

  ;; Test failed match
  (let ((result (parser:parse (parser:satisfy #'stringp) input)))
   (assert-true (parse-failure-p result))
   (assert-true (search "Unexpected token" (failure-message result))))

  ;; Test end of input
  (let ((empty-input (make-test-input '())))
   (let ((result (parser:parse (parser:satisfy #'numberp) empty-input)))
    (assert-true (parse-failure-p result))
    (assert-true (search "end of input" (failure-message result)))))))

(deftest token-combinator-test
  "Test token combinator"
 (let ((input (make-test-input '(:hello :world))))

  ;; Test successful token match
  (let ((result (parser:parse (parser:token :hello) input)))
   (assert-true (parse-success-p result))
   (assert-eq (success-value result) :hello))

  ;; Test failed token match
  (let ((result (parser:parse (parser:token :goodbye) input)))
   (assert-true (parse-failure-p result))
   (assert-true (search ":GOODBYE" (failure-message result))))))

;;; Monadic Combinator Tests

(deftest bind-macro-test
  "Test bind macro for sequential parsing"
 (let ((input (make-test-input '(:let x := 42))))
  (let ((let-parser
      (parser:bind ((keyword (parser:token :let))
             (var (parser:satisfy #'symbolp))
             (equals (parser:token :=))
             (value (parser:satisfy #'numberp)))
            (parser:return (list :assignment var value)))))

   (let ((result (parser:parse let-parser input)))
    (assert-true (parse-success-p result))
    (assert-equal (success-value result) '(:assignment x 42))))))

#|
(deftest bind-with-ignored-values-test
 "Test bind with ignored intermediate values"
 (let ((input (make-test-input '(#\( :name "Alice" #\))))
    (paren-parser
      (parser:bind ((_ (parser:token #\()))
             (keyword (parser:token :name))
             (value (parser:satisfy #'stringp))
             (_ (parser:token #\))))
       (parser:return (cons keyword value)))))

  (let ((result (parser:parse paren-parser input)))
    (assert-true (parse-success-p result))
    (assert-equal (success-value result) '(:name . "Alice"))))))
|#

;;; Choice Combinator Tests

(deftest choice-combinator-test
  "Test choice combinator"
 (let ((number-or-string
     (parser:choice (parser:satisfy #'numberp)
            (parser:satisfy #'stringp))))

  ;; Test first alternative
  (let ((result (parser:parse number-or-string (make-test-input '(42)))))
   (assert-true (parse-success-p result))
   (assert-= (success-value result) 42))

  ;; Test second alternative
  (let ((result (parser:parse number-or-string (make-test-input '("hello")))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) "hello"))

  ;; Test no alternatives match
  (let ((result (parser:parse number-or-string (make-test-input '(:symbol)))))
   (assert-true (parse-failure-p result)))))

(deftest choice-ordering-test
  "Test that choice tries alternatives in order"
 (let ((ordered-choice
     (parser:choice (parser:token :first)
            (parser:token :second)
            (parser:satisfy #'symbolp)))) ; Would match both :first and :second

  ;; Should match first alternative, not the general one
  (let ((result (parser:parse ordered-choice (make-test-input '(:first)))))
   (assert-true (parse-success-p result))
   (assert-eq (success-value result) :first))))

;;; Sequence Combinator Tests

(deftest sequence-combinator-test
  "Test sequence combinator"
 (let ((three-tokens
     (parser:sequence (parser:token :a)
             (parser:token :b)
             (parser:token :c))))

  ;; Test successful sequence
  (let ((result (parser:parse three-tokens (make-test-input '(:a :b :c :d)))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '(:a :b :c)))

  ;; Test partial sequence failure
  (let ((result (parser:parse three-tokens (make-test-input '(:a :b :x)))))
   (assert-true (parse-failure-p result)))))

;;; Repetition Combinator Tests

(deftest many-combinator-test
  "Test many combinator (zero or more)"
 (let ((many-numbers (parser:many (parser:satisfy #'numberp))))

  ;; Test multiple matches
  (let ((result (parser:parse many-numbers (make-test-input '(1 2 3 :end)))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '(1 2 3)))

  ;; Test zero matches
  (let ((result (parser:parse many-numbers (make-test-input '(:start)))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '()))

  ;; Test empty input
  (let ((result (parser:parse many-numbers (make-test-input '()))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '()))))

(deftest many1-combinator-test
  "Test many1 combinator (one or more)"
 (let ((many1-numbers (parser:many1 (parser:satisfy #'numberp))))

  ;; Test multiple matches
  (let ((result (parser:parse many1-numbers (make-test-input '(1 2 3 :end)))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '(1 2 3)))

  ;; Test single match
  (let ((result (parser:parse many1-numbers (make-test-input '(42 :end)))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '(42)))

  ;; Test zero matches (should fail)
  (let ((result (parser:parse many1-numbers (make-test-input '(:start)))))
   (assert-true (parse-failure-p result)))))

(deftest optional-combinator-test
  "Test optional combinator"
 (let ((maybe-number (parser:optional (parser:satisfy #'numberp) :not-found)))

  ;; Test present value
  (let ((result (parser:parse maybe-number (make-test-input '(42)))))
   (assert-true (parse-success-p result))
   (assert-= (success-value result) 42))

  ;; Test absent value (should use default)
  (let ((result (parser:parse maybe-number (make-test-input '(:symbol)))))
   (assert-true (parse-success-p result))
   (assert-eq (success-value result) :not-found))))

;;; Separated List Tests

(deftest sepBy-combinator-test
  "Test sepBy combinator (zero or more separated)"
 (let ((comma-numbers (parser:sepBy (parser:satisfy #'numberp) (parser:token :comma))))

  ;; Test multiple values
  (let ((result (parser:parse comma-numbers (make-test-input '(1 :comma 2 :comma 3)))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '(1 2 3)))

  ;; Test single value
  (let ((result (parser:parse comma-numbers (make-test-input '(42)))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '(42)))

  ;; Test empty (should succeed with empty list)
  (let ((result (parser:parse comma-numbers (make-test-input '(:other)))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '()))))

(deftest sep+-combinator-test
  "Test sep+ combinator (one or more separated)"
 (let ((comma-numbers (parser:sep+ (parser:satisfy #'numberp) (parser:token :comma))))

  ;; Test multiple values
  (let ((result (parser:parse comma-numbers (make-test-input '(1 :comma 2 :comma 3)))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '(1 2 3)))

  ;; Test single value
  (let ((result (parser:parse comma-numbers (make-test-input '(42)))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '(42)))

  ;; Test empty (should fail)
  (let ((result (parser:parse comma-numbers (make-test-input '(:other)))))
   (assert-true (parse-failure-p result)))))

;;; Left-Associative Operator Tests

(deftest chainl1-combinator-test
  "Test chainl1 combinator for left-associative operators"
 (let ((add-expr
     (parser:chainl1 (parser:satisfy #'numberp)
             (parser:bind ((_ (parser:token :+)))
                   (parser:return (lambda (a b) (+ a b)))))))

  ;; Test single operand
  (let ((result (parser:parse add-expr (make-test-input '(42)))))
   (assert-true (parse-success-p result))
   (assert-= (success-value result) 42))

  ;; Test multiple operands (should be left-associative)
  (let ((result (parser:parse add-expr (make-test-input '(1 :+ 2 :+ 3)))))
   (assert-true (parse-success-p result))
   (assert-= (success-value result) 6)) ; ((1 + 2) + 3) = 6

  ;; Test that it stops at non-operator
  (let ((result (parser:parse add-expr (make-test-input '(1 :+ 2 :end)))))
   (assert-true (parse-success-p result))
   (assert-= (success-value result) 3))))

;;; Delimiter Tests

(deftest between-combinator-test
  "Test between combinator"
 (let ((parens (parser:between (parser:token :lparen)
                (parser:token :rparen)
                (parser:satisfy #'symbolp))))

  ;; Test successful parse
  (let ((result (parser:parse parens (make-test-input '(:lparen hello :rparen)))))
   (assert-true (parse-success-p result))
   (assert-eq (success-value result) 'hello))

  ;; Test missing closing delimiter
  (let ((result (parser:parse parens (make-test-input '(:lparen hello :end)))))
   (assert-true (parse-failure-p result)))))

;;; Control Flow Tests

(deftest try-combinator-test
  "Test try combinator for backtracking"
 (let ((long-or-short
     (parser:choice
     (parser:try (parser:sequence (parser:token :long) (parser:token :keyword)))
     (parser:token :long))))

  ;; Test successful long parse
  (let ((result (parser:parse long-or-short (make-test-input '(:long :keyword)))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '(:long :keyword)))

  ;; Test backtracking to short parse
  (let ((result (parser:parse long-or-short (make-test-input '(:long :other)))))
   (assert-true (parse-success-p result))
   (assert-eq (success-value result) :long))))

(deftest lookahead-combinator-test
  "Test lookahead combinator"
 (let ((peek-then-consume
     (parser:sequence (parser:lookahead (parser:token :hello))
             (parser:token :hello)
             (parser:token :world))))

  ;; Test successful lookahead
  (let ((result (parser:parse peek-then-consume (make-test-input '(:hello :world)))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '(:hello :hello :world)))

  ;; Test failed lookahead
  (let ((result (parser:parse peek-then-consume (make-test-input '(:goodbye :world)))))
   (assert-true (parse-failure-p result)))))

(deftest eof-combinator-test
  "Test end-of-input combinator"
 (let ((number-then-eof
     (parser:sequence (parser:satisfy #'numberp) (parser:eof))))

  ;; Test successful end-of-input
  (let ((result (parser:parse number-then-eof (make-test-input '(42)))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '(42 nil)))

  ;; Test failed end-of-input (more input remaining)
  (let ((result (parser:parse number-then-eof (make-test-input '(42 :more)))))
   (assert-true (parse-failure-p result)))))

(deftest label-combinator-test
  "Test label combinator for better error messages"
 (let ((labeled-number (parser:label (parser:satisfy #'numberp) "integer")))

  ;; Test successful parse
  (let ((result (parser:parse labeled-number (make-test-input '(42)))))
   (assert-true (parse-success-p result))
   (assert-= (success-value result) 42))

  ;; Test failed parse with custom label
  (let ((result (parser:parse labeled-number (make-test-input '(:symbol)))))
   (assert-true (parse-failure-p result))
   (assert-equal (parser:failure-expected result) "integer"))))

;;; Complex Parser Tests

#|
(deftest json-like-parser-test
  "Test a JSON-like parser"
 (labels ((json-value ()
       (parser:choice #'json-number #'json-string #'json-array))

      (json-number ()
       (parser:satisfy #'numberp))

      (json-string ()
       (parser:satisfy #'stringp))

      (json-array ()
       (parser:between (parser:token :lbracket)
               (parser:token :rbracket)
               (parser:sepBy #'json-value (parser:token :comma)))))

  ;; Test simple number
  (let ((result (parser:parse (json-number) (make-test-input '(42)))))
   (assert-true (parse-success-p result))
   (assert-= (success-value result) 42))

  ;; Test simple array
  (let ((result (parser:parse (json-array) (make-test-input '(:lbracket 1 :comma 2 :comma 3 :rbracket)))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '(1 2 3)))

  ;; Test empty array
  (let ((result (parser:parse (json-array) (make-test-input '(:lbracket :rbracket)))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '()))

  ;; Test mixed array
  (let ((result (parser:parse (json-array) (make-test-input '(:lbracket 42 :comma "hello" :rbracket)))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '(42 "hello"))))
|#

#|
(deftest arithmetic-expression-parser-test
  "Test arithmetic expression parser with precedence"
 (labels ((expression ()
       (parser:chainl1 #'term #'add-op))

      (term ()
       (parser:chainl1 #'factor #'mul-op))

      (factor ()
       (parser:choice
       (parser:between (parser:token :lparen) (parser:token :rparen) #'expression)
       (parser:satisfy #'numberp)))

      (add-op ()
       (parser:choice
       (parser:bind ((_ (parser:token :+))) (parser:return #'+))
       (parser:bind ((_ (parser:token :-))) (parser:return #'-))))

      (mul-op ()
       (parser:choice
       (parser:bind ((_ (parser:token :*))) (parser:return #'*))
       (parser:bind ((_ (parser:token :/))) (parser:return #'/))))))

 ;; Test simple expression
 (let ((result (parser:parse (expression) (make-test-input '(2 :+ 3)))))
  (assert-true (parse-success-p result))
  (assert-= (success-value result) 5))

 ;; Test precedence (multiplication before addition)
 (let ((result (parser:parse (expression) (make-test-input '(2 :+ 3 :* 4)))))
  (assert-true (parse-success-p result))
  (assert-= (success-value result) 14)) ; 2 + (3 * 4) = 14

 ;; Test parentheses
 (let ((result (parser:parse (expression) (make-test-input '(:lparen 2 :+ 3 :rparen :* 4)))))
  (assert-true (parse-success-p result))
  (assert-= (success-value result) 20)) ; (2 + 3) * 4 = 20

 ;; Test left associativity
 (let ((result (parser:parse (expression) (make-test-input '(10 :- 3 :- 2)))))
  (assert-true (parse-success-p result))
  (assert-= (success-value result) 5))) ; (10 - 3) - 2 = 5

;;; Error Handling Tests

(deftest error-message-quality-test
  "Test that error messages are informative"
 (let ((expected-keyword (parser:token :expected)))

  ;; Test specific token error
  (let ((result (parser:parse expected-keyword (make-test-input '(:actual)))))
   (assert-true (parse-failure-p result))
   (let ((message (failure-message result)))
    (assert-true (search "Unexpected token" message))
    (assert-true (search ":ACTUAL" message))))

  ;; Test end of input error
  (let ((result (parser:parse expected-keyword (make-test-input '()))))
   (assert-true (parse-failure-p result))
   (let ((message (failure-message result)))
    (assert-true (search "end of input" message))))))

(deftest complex-error-scenario-test
  "Test error handling in complex parsing scenarios"
 (let ((config-parser
     (parser:bind ((keyword (parser:token :config))
            (name (parser:satisfy #'symbolp))
            (equals (parser:token :=))
            (value (parser:satisfy #'stringp)))
           (parser:return (list name value)))))

  ;; Test error in middle of complex parse
  (let ((result (parser:parse config-parser (make-test-input '(:config name := 42)))))
   (assert-true (parse-failure-p result))
   ;; Should indicate expectation of string, not just generic error
   (let ((message (failure-message result)))
    (assert-true (or (search "string" (string-downcase message))
        (search "42" message)))))))

;;; Performance and Edge Case Tests

(deftest large-input-test
  "Test parsing with large input"
 (let ((large-input (make-test-input (make-list 1000 :initial-element 42)))
    (count-numbers (parser:many (parser:satisfy #'numberp))))

  (let ((result (parser:parse count-numbers large-input)))
   (assert-true (parse-success-p result))
   (assert-= (length (success-value result)) 1000))))

#|
(deftest deeply-nested-test
  "Test deeply nested parsing"
 (let ((nested-parens
     (labels ((nested ()
          (parser:choice
           (parser:between (parser:token :lparen) (parser:token :rparen) #'nested)
           (parser:satisfy #'numberp))))
      #'nested)))

  ;; Test moderately nested structure
  (let ((input (make-test-input '(:lparen :lparen :lparen 42 :rparen :rparen :rparen)))
     (result (parser:parse nested-parens input)))
   (assert-true (parse-success-p result))
   (assert-= (success-value result) 42))))
|#

(deftest empty-input-handling-test
  "Test various parsers with empty input"
 ;; many should succeed with empty result
 (let ((result (parser:parse (parser:many (parser:satisfy #'numberp)) (make-test-input '()))))
  (assert-true (parse-success-p result))
  (assert-equal (success-value result) '()))

 ;; many1 should fail
 (let ((result (parser:parse (parser:many1 (parser:satisfy #'numberp)) (make-test-input '()))))
  (assert-true (parse-failure-p result)))

 ;; optional should succeed with default
 (let ((result (parser:parse (parser:optional (parser:satisfy #'numberp) :default) (make-test-input '()))))
  (assert-true (parse-success-p result))
  (assert-eq (success-value result) :default))

 ;; eof should succeed
 (let ((result (parser:parse (parser:eof) (make-test-input '()))))
  (assert-true (parse-success-p result))))

;;; Position Management and Peek Tests

(deftest peek-combinator-test
  "Test peek combinator for looking ahead without consuming"
 (let ((input (make-test-input '(#\a #\b #\c #\d))))
  ;; Test single peek
  (let ((result (parser:parse (parser:peek) input)))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) #\a)
   ;; Verify position not changed
   (assert-= (parser:parse-state-position (parser:success-state result)) 0)
   (assert-not (parser:parse-state-consumed-p (parser:success-state result))))

  ;; Test peek on empty input
  (let ((result (parser:parse (parser:peek) (make-test-input '()))))
   (assert-true (parse-failure-p result))
   (assert-true (search "end of input" (failure-message result))))))

(deftest peek-n-combinator-test
  "Test peek-n combinator for looking ahead at multiple tokens"
 (let ((input (make-test-input '(#\h #\e #\l #\l #\o))))
  ;; Test peeking at 3 characters
  (let ((result (parser:parse (parser:peek-n 3) input)))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '(#\h #\e #\l))
   ;; Verify position not changed
   (assert-= (parser:parse-state-position (parser:success-state result)) 0)
   (assert-not (parser:parse-state-consumed-p (parser:success-state result))))

  ;; Test peeking more than available
  (let ((result (parser:parse (parser:peek-n 10) input)))
   (assert-true (parse-failure-p result))
   (assert-true (search "Expected 10 tokens" (failure-message result))))))

(deftest save-restore-position-test
  "Test save and restore position functionality"
 (let ((input (make-test-input '(1 2 3 4 5))))
  ;; Create initial state
  (let* ((state (parser:make-parse-state
          :position 0
          :remaining input
          :context '()
          :consumed-p nil))
      ;; Save initial position
      (saved-state (parser:save-position state)))

   ;; Verify saved state matches original
   (assert-= (parser:parse-state-position saved-state) 0)
   (assert-equal (seq:first (parser:parse-state-remaining saved-state)) 1)

   ;; Advance the state manually
   (let* ((advanced-state (parser:make-parse-state
               :position 2
               :remaining (seq:drop 2 input)
               :context '()
               :consumed-p t))
       ;; Now restore from saved
       (restore-parser (parser:restore-position saved-state))
       (result (funcall restore-parser advanced-state)))

    ;; Should return to saved position
    (assert-true (parse-success-p result))
    (assert-= (parser:parse-state-position (parser:success-state result)) 0)))))

(deftest with-saved-position-test
  "Test with-saved-position macro for backtracking"
 (let ((input (make-test-input '(:try :this :first :or :that))))
  ;; Parser that tries two alternatives with backtracking
  (let ((backtrack-parser
      (parser:with-saved-position (checkpoint)
       (parser:choice
       ;; First try this sequence
       (parser:sequence (parser:token :try)
                (parser:token :this)
                (parser:token :wrong))
       ;; On failure, restore and try different approach
       (parser:bind ((_ (funcall (parser:restore-position checkpoint))))
        (parser:sequence (parser:token :try)
                 (parser:token :this)
                 (parser:token :first)))))))

   (let ((result (parser:parse backtrack-parser input)))
    (assert-true (parse-success-p result))
    (assert-equal (success-value result) '(:try :this :first))))))

(deftest peek-with-combinators-test
  "Test peek integration with other parser combinators"
 (let ((input (make-test-input '(#\+ 1 2 3))))
  ;; Peek at operator then parse expression
  (let ((peek-expr
      (parser:bind ((op (parser:peek)) ; Peek at operator
             (_ (parser:token #\+)) ; Consume it
             (nums (parser:many1 (parser:satisfy #'numberp))))
       (parser:return (list :op op :values nums)))))

   (let ((result (parser:parse peek-expr input)))
    (assert-true (parse-success-p result))
    (assert-equal (parser:assoc :op (success-value result)) #\+)
    (assert-equal (parser:assoc :values (success-value result)) '(1 2 3))))))

(deftest multiple-peek-test
  "Test multiple consecutive peeks return same value"
 (let ((input (make-test-input '(:a :b :c))))
  (let ((multi-peek
      (parser:bind ((p1 (parser:peek))
             (p2 (parser:peek)) ; Should be same as p1
             (p3 (parser:peek)) ; Should be same as p1
             (actual (parser:token :a)))
       (parser:return (list p1 p2 p3 actual)))))

   (let ((result (parser:parse multi-peek input)))
    (assert-true (parse-success-p result))
    (let ((values (success-value result)))
     (assert-eq (first values) :a)
     (assert-eq (second values) :a)
     (assert-eq (third values) :a)
     (assert-eq (fourth values) :a))))))

(deftest peek-n-partial-test
  "Test peek-n behavior with partial availability"
 (let ((input (make-test-input '(1 2))))
  ;; Request 5 but only 2 available
  (let ((result (parser:parse (parser:peek-n 5) input)))
   (assert-true (parse-failure-p result))
   (assert-true (search "only 2 available" (failure-message result))))))

;;; Integration Tests

(deftest parser-state-preservation-test
  "Test that parser state is properly maintained"
 (let ((two-numbers
     (parser:bind ((first (parser:satisfy #'numberp))
            (second (parser:satisfy #'numberp)))
           (parser:return (list first second)))))

  (let ((result (parser:parse two-numbers (make-test-input '(1 2 3)))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '(1 2))

   ;; Remaining input should be at position 2
   (let ((final-state (parser:success-state result)))
    (assert-= (parser:parse-state-position final-state) 2)
    (assert-equal (seq:first (parser:parse-state-remaining final-state)) 3)))))

(deftest backtracking-state-test
  "Test that backtracking properly restores parser state"
 (let ((choice-parser
     (parser:choice
     (parser:try (parser:sequence (parser:token :a) (parser:token :b) (parser:token :c)))
     (parser:sequence (parser:token :a) (parser:token :x)))))

  ;; Should backtrack and succeed with second alternative
  (let ((result (parser:parse choice-parser (make-test-input '(:a :x :y)))))
   (assert-true (parse-success-p result))
   (assert-equal (success-value result) '(:a :x))

   ;; Should have consumed exactly 2 tokens
   (assert-= (parser:parse-state-position (parser:success-state result)) 2))))
|#

;;; Cut Operator Tests

(deftest cut-basic-test
  "Test basic cut operator functionality"
  (let ((input (make-test-input '(:let x := 42))))
    ;; Parser with cut after seeing :let
    (let ((let-parser
            (parser:bind ((keyword (parser:token :let))
                          (_ (parser:cut))  ; Commit to let-statement
                          (var (parser:satisfy #'symbolp))
                          (equals (parser:token :=))
                          (value (parser:satisfy #'numberp)))
              (parser:return (list :let var value)))))
      (let ((result (parser:parse let-parser input)))
        (assert-true (parse-success-p result))
        (assert-equal (success-value result) '(:let x 42))))))

(deftest cut-prevents-backtracking-test
  "Test that cut prevents backtracking in choice"
  (let ((input (make-test-input '(:let x :bad))))
    ;; Choice between let-statement (with cut) and expression
    (let ((statement-parser
            (parser:choice
             ;; Let statement with cut - after :let, we commit
             (parser:bind ((keyword (parser:token :let))
                           (_ (parser:cut))
                           (var (parser:satisfy #'symbolp))
                           (equals (parser:token :=))  ; Will fail here
                           (value (parser:satisfy #'numberp)))
               (parser:return (list :let var value)))
             ;; Expression fallback - should NOT be tried after cut
             (parser:bind ((expr (parser:token :let)))
               (parser:return (list :expr expr))))))
      (let ((result (parser:parse statement-parser input)))
        ;; Should fail with cut, not try second alternative
        (assert-true (parse-failure-p result))
        (assert-true (parser:failure-cut-p result))))))

(deftest cut-choice-without-cut-backtracks-test
  "Test that choice without cut does backtrack"
  (let ((input (make-test-input '(:a :x))))
    ;; Without cut, should backtrack and try second alternative
    (let ((parser
            (parser:choice
             (parser:try (parser:sequence (parser:token :a) (parser:token :b)))
             (parser:sequence (parser:token :a) (parser:token :x)))))
      (let ((result (parser:parse parser input)))
        (assert-true (parse-success-p result))
        (assert-equal (success-value result) '(:a :x))))))

(deftest commit-combinator-test
  "Test commit combinator for wrapping parsers"
  (let ((input (make-test-input '(:if :cond :bad))))
    ;; Commit wrapper - entire if-statement is committed after first token
    (let ((if-parser
            (parser:commit
             (parser:bind ((keyword (parser:token :if))
                           (cond (parser:token :cond))
                           (then (parser:token :then)))  ; Will fail
               (parser:return (list :if cond then))))))
      (let ((result (parser:parse if-parser input)))
        (assert-true (parse-failure-p result))
        (assert-true (parser:failure-cut-p result))))))

;;; Source Location Tests

(deftest source-location-in-errors-test
  "Test that parse errors include line/column information"
  (let ((input (make-test-input '(:a :b :c))))
    (let ((result (parser:parse
                   (parser:bind ((_ (parser:token :a))
                                 (_ (parser:token :b))
                                 (_ (parser:token :wrong)))  ; Fails here
                     (parser:return :ok))
                   input)))
      (assert-true (parse-failure-p result))
      ;; Error should have line/column info
      (assert-true (parser:failure-line result))
      (assert-true (parser:failure-column result)))))

(deftest get-position-combinator-test
  "Test get-position combinator returns current location"
  (let ((input (make-test-input '(:a :b :c))))
    (let ((result (parser:parse
                   (parser:bind ((_ (parser:token :a))
                                 (_ (parser:token :b))
                                 (pos (parser:get-position)))
                     (parser:return pos))
                   input)))
      (assert-true (parse-success-p result))
      (let ((pos (success-value result)))
        ;; Position should be 2 (after :a and :b)
        (assert-= (first pos) 2)))))

(deftest format-error-test
  "Test format-error produces readable messages"
  (let ((input (make-test-input '(:wrong))))
    (let ((result (parser:parse (parser:token :expected) input)))
      (assert-true (parse-failure-p result))
      (let ((msg (parser:format-error result)))
        ;; Should contain location and expected info
        (assert-true (stringp msg))
        (assert-true (search "error" (string-downcase msg)))))))

;;; Error Recovery Tests

(deftest recover-basic-test
  "Test basic error recovery with default value"
  (let ((input (make-test-input '(:bad :data))))
    ;; Parser that expects :good but recovers with :default
    (let ((result (parser:parse
                   (parser:recover (parser:token :good)
                                   :default :recovered)
                   input)))
      (assert-true (parse-success-p result))
      (assert-eq (success-value result) :recovered))))

(deftest recover-with-skip-until-test
  "Test error recovery that skips to recovery point"
  (let ((input (make-test-input '(:bad :bad :semicolon :good))))
    ;; Skip to semicolon on error
    (let ((result (parser:parse
                   (parser:bind ((_ (parser:recover (parser:token :good)
                                                    :skip-until (parser:token :semicolon)
                                                    :default :skipped))
                                 (good (parser:token :good)))
                     (parser:return (list :recovered good)))
                   input)))
      (assert-true (parse-success-p result))
      (assert-equal (success-value result) '(:recovered :good)))))

(deftest recover-with-on-error-test
  "Test error recovery calls on-error handler"
  (let ((input (make-test-input '(:bad)))
        (error-called nil))
    (let ((result (parser:parse
                   (parser:recover (parser:token :good)
                                   :on-error (lambda (err)
                                               (declare (ignore err))
                                               (setf error-called t))
                                   :default :recovered)
                   input)))
      (assert-true (parse-success-p result))
      (assert-true error-called))))

(deftest recover-many-test
  "Test recover-many for parsing multiple items with recovery"
  (let ((input (make-test-input '(1 2 :bad 4 5 :end))))
    ;; Parse numbers, skip :bad, stop at :end
    (let ((result (parser:parse
                   (parser:recover-many (parser:satisfy #'numberp)
                                        :skip-until (parser:satisfy #'numberp)
                                        :sentinel (parser:token :end))
                   input)))
      (assert-true (parse-success-p result))
      ;; Should have parsed 1, 2, 4, 5 (skipping :bad)
      (let ((values (success-value result)))
        (assert-true (member 1 values))
        (assert-true (member 2 values))
        (assert-true (member 4 values))
        (assert-true (member 5 values))
        (assert-not (member :bad values))))))

;;; Memoization Tests

(deftest memo-basic-test
  "Test basic memoization functionality"
  (let ((input (make-test-input '(:a :b :c)))
        (call-count 0))
    ;; Create a memoized parser that counts calls
    (let* ((counting-parser
             (lambda (state)
               (incf call-count)
               (funcall (parser:token :a) state)))
           (memoized (parser:memo counting-parser 'test-parser)))
      ;; Parse with memoization enabled
      (let ((result (parser:parse
                     (parser:choice
                      (parser:try (parser:bind ((_ memoized)
                                                (_ (parser:token :wrong)))
                                    (parser:return :first)))
                      (parser:bind ((_ memoized)  ; Same position, should use cache
                                    (_ (parser:token :b)))
                        (parser:return :second)))
                     input
                     :memoize t)))
        (assert-true (parse-success-p result))
        (assert-eq (success-value result) :second)
        ;; Parser should have been called only once due to memoization
        (assert-= call-count 1)))))

(deftest memo-without-table-test
  "Test that memo works without memoization table (no caching)"
  (let ((input (make-test-input '(:a))))
    ;; Without :memoize t, memo should still work (just no caching)
    (let ((result (parser:parse
                   (parser:memo (parser:token :a) 'test)
                   input)))
      (assert-true (parse-success-p result))
      (assert-eq (success-value result) :a))))

(deftest enable-memoization-test
  "Test enable-memoization creates memo table"
  (let ((state (parser:make-parse-state
                :position 0
                :remaining (make-test-input '(:a))
                :context '()
                :consumed-p nil)))
    ;; Initially no memo table
    (assert-not (parser:parse-state-memo-table state))
    ;; After enabling, should have memo table
    (let ((new-state (parser:enable-memoization state)))
      (assert-true (parser:parse-state-memo-table new-state)))))

;;; Integration Tests for New Features

(deftest cut-with-source-location-test
  "Test that cut failures preserve source location"
  (let ((input (make-test-input '(:start :a :bad))))
    (let ((result (parser:parse
                   (parser:bind ((_ (parser:token :start))
                                 (_ (parser:cut))
                                 (_ (parser:token :a))
                                 (_ (parser:token :good)))  ; Fails here
                     (parser:return :ok))
                   input)))
      (assert-true (parse-failure-p result))
      (assert-true (parser:failure-cut-p result))
      ;; Should have location info
      (assert-true (parser:failure-line result)))))

(deftest recovery-with-memoization-test
  "Test that recovery works with memoization enabled"
  (let ((input (make-test-input '(:bad :good))))
    (let ((result (parser:parse
                   (parser:bind ((_ (parser:recover (parser:token :expected)
                                                    :skip-until (parser:token :good)))
                                 (_ (parser:eof)))
                     (parser:return :recovered))
                   input
                   :memoize t)))
      (assert-true (parse-success-p result)))))

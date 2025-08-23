;;;;  tests for epsilon.parser

(defpackage epsilon.parser.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (parser epsilon.parser)
   (seq epsilon.sequence)))

(in-package epsilon.parser.tests)

;;; Helper functions for testing

(defun make-test-input (items)
  "Create a test input sequence from a list of items"
  (seq:from-list items))

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
    
    (is (parser:parse-state-p state))
    (is-= (parser:parse-state-position state) 5)
    (is-equal (parser:parse-state-context state) '("test context"))
    (is (parser:parse-state-consumed-p state))))

(deftest success-structure-test
    "Test success result structure"
  (let ((success (parser:make-success
                  :value 42
                  :state (parser:make-parse-state :position 1))))
    
    (is (parser:success-p success))
    (is-= (parser:success-value success) 42)
    (is (parser:parse-state-p (parser:success-state success)))))

(deftest failure-structure-test
    "Test failure result structure"
  (let ((failure (parser:make-failure
                  :message "Test error"
                  :state (parser:make-parse-state :position 0)
                  :expected "something")))
    
    (is (parser:failure-p failure))
    (is-equal (parser:failure-message failure) "Test error")
    (is-equal (parser:failure-expected failure) "something")
    (is (parser:parse-state-p (parser:failure-state failure)))))

;;; Basic Combinator Tests

(deftest return-combinator-test
    "Test return combinator"
  (let ((input (make-test-input '(a b c))))
    (let ((result (parser:parse (parser:return 42) input)))
      (is (parse-success-p result))
      (is-= (success-value result) 42)
      
      ;; Should not consume input
      (is-= (parser:parse-state-position (parser:success-state result)) 0))))

(deftest fail-combinator-test
    "Test fail combinator"
  (let ((input (make-test-input '(a b c))))
    (let ((result (parser:parse (parser:fail "Test failure") input)))
      (is (parse-failure-p result))
      (is-equal (failure-message result) "Test failure"))))

(deftest satisfy-combinator-test
    "Test satisfy combinator"
  (let ((input (make-test-input '(42 "hello" :symbol))))
    
    ;; Test successful match
    (let ((result (parser:parse (parser:satisfy #'numberp) input)))
      (is (parse-success-p result))
      (is-= (success-value result) 42)
      (is-= (parser:parse-state-position (parser:success-state result)) 1))
    
    ;; Test failed match
    (let ((result (parser:parse (parser:satisfy #'stringp) input)))
      (is (parse-failure-p result))
      (is (search "Unexpected token" (failure-message result))))
    
    ;; Test end of input
    (let ((empty-input (make-test-input '())))
      (let ((result (parser:parse (parser:satisfy #'numberp) empty-input)))
        (is (parse-failure-p result))
        (is (search "end of input" (failure-message result)))))))

(deftest token-combinator-test
    "Test token combinator"
  (let ((input (make-test-input '(:hello :world))))
    
    ;; Test successful token match
    (let ((result (parser:parse (parser:token :hello) input)))
      (is (parse-success-p result))
      (is-eq (success-value result) :hello))
    
    ;; Test failed token match
    (let ((result (parser:parse (parser:token :goodbye) input)))
      (is (parse-failure-p result))
      (is (search ":GOODBYE" (failure-message result))))))

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
        (is (parse-success-p result))
        (is-equal (success-value result) '(:assignment x 42))))))

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
        (is (parse-success-p result))
        (is-equal (success-value result) '(:name . "Alice"))))))
|#

;;; Choice Combinator Tests

(deftest choice-combinator-test
    "Test choice combinator"
  (let ((number-or-string
         (parser:choice (parser:satisfy #'numberp)
                        (parser:satisfy #'stringp))))
    
    ;; Test first alternative
    (let ((result (parser:parse number-or-string (make-test-input '(42)))))
      (is (parse-success-p result))
      (is-= (success-value result) 42))
    
    ;; Test second alternative
    (let ((result (parser:parse number-or-string (make-test-input '("hello")))))
      (is (parse-success-p result))
      (is-equal (success-value result) "hello"))
    
    ;; Test no alternatives match
    (let ((result (parser:parse number-or-string (make-test-input '(:symbol)))))
      (is (parse-failure-p result)))))

(deftest choice-ordering-test
    "Test that choice tries alternatives in order"
  (let ((ordered-choice
         (parser:choice (parser:token :first)
                        (parser:token :second)
                        (parser:satisfy #'symbolp))))  ; Would match both :first and :second
    
    ;; Should match first alternative, not the general one
    (let ((result (parser:parse ordered-choice (make-test-input '(:first)))))
      (is (parse-success-p result))
      (is-eq (success-value result) :first))))

;;; Sequence Combinator Tests

(deftest sequence-combinator-test
    "Test sequence combinator"
  (let ((three-tokens
         (parser:sequence (parser:token :a)
                          (parser:token :b)
                          (parser:token :c))))
    
    ;; Test successful sequence
    (let ((result (parser:parse three-tokens (make-test-input '(:a :b :c :d)))))
      (is (parse-success-p result))
      (is-equal (success-value result) '(:a :b :c)))
    
    ;; Test partial sequence failure
    (let ((result (parser:parse three-tokens (make-test-input '(:a :b :x)))))
      (is (parse-failure-p result)))))

;;; Repetition Combinator Tests

(deftest many-combinator-test
    "Test many combinator (zero or more)"
  (let ((many-numbers (parser:many (parser:satisfy #'numberp))))
    
    ;; Test multiple matches
    (let ((result (parser:parse many-numbers (make-test-input '(1 2 3 :end)))))
      (is (parse-success-p result))
      (is-equal (success-value result) '(1 2 3)))
    
    ;; Test zero matches
    (let ((result (parser:parse many-numbers (make-test-input '(:start)))))
      (is (parse-success-p result))
      (is-equal (success-value result) '()))
    
    ;; Test empty input
    (let ((result (parser:parse many-numbers (make-test-input '()))))
      (is (parse-success-p result))
      (is-equal (success-value result) '()))))

(deftest many1-combinator-test
    "Test many1 combinator (one or more)"
  (let ((many1-numbers (parser:many1 (parser:satisfy #'numberp))))
    
    ;; Test multiple matches
    (let ((result (parser:parse many1-numbers (make-test-input '(1 2 3 :end)))))
      (is (parse-success-p result))
      (is-equal (success-value result) '(1 2 3)))
    
    ;; Test single match
    (let ((result (parser:parse many1-numbers (make-test-input '(42 :end)))))
      (is (parse-success-p result))
      (is-equal (success-value result) '(42)))
    
    ;; Test zero matches (should fail)
    (let ((result (parser:parse many1-numbers (make-test-input '(:start)))))
      (is (parse-failure-p result)))))

(deftest optional-combinator-test
    "Test optional combinator"
  (let ((maybe-number (parser:optional (parser:satisfy #'numberp) :not-found)))
    
    ;; Test present value
    (let ((result (parser:parse maybe-number (make-test-input '(42)))))
      (is (parse-success-p result))
      (is-= (success-value result) 42))
    
    ;; Test absent value (should use default)
    (let ((result (parser:parse maybe-number (make-test-input '(:symbol)))))
      (is (parse-success-p result))
      (is-eq (success-value result) :not-found))))

;;; Separated List Tests

(deftest sepBy-combinator-test
    "Test sepBy combinator (zero or more separated)"
  (let ((comma-numbers (parser:sepBy (parser:satisfy #'numberp) (parser:token :comma))))
    
    ;; Test multiple values
    (let ((result (parser:parse comma-numbers (make-test-input '(1 :comma 2 :comma 3)))))
      (is (parse-success-p result))
      (is-equal (success-value result) '(1 2 3)))
    
    ;; Test single value
    (let ((result (parser:parse comma-numbers (make-test-input '(42)))))
      (is (parse-success-p result))
      (is-equal (success-value result) '(42)))
    
    ;; Test empty (should succeed with empty list)
    (let ((result (parser:parse comma-numbers (make-test-input '(:other)))))
      (is (parse-success-p result))
      (is-equal (success-value result) '()))))

(deftest sep+-combinator-test
    "Test sep+ combinator (one or more separated)"
  (let ((comma-numbers (parser:sep+ (parser:satisfy #'numberp) (parser:token :comma))))
    
    ;; Test multiple values
    (let ((result (parser:parse comma-numbers (make-test-input '(1 :comma 2 :comma 3)))))
      (is (parse-success-p result))
      (is-equal (success-value result) '(1 2 3)))
    
    ;; Test single value
    (let ((result (parser:parse comma-numbers (make-test-input '(42)))))
      (is (parse-success-p result))
      (is-equal (success-value result) '(42)))
    
    ;; Test empty (should fail)
    (let ((result (parser:parse comma-numbers (make-test-input '(:other)))))
      (is (parse-failure-p result)))))

;;; Left-Associative Operator Tests

(deftest chainl1-combinator-test
    "Test chainl1 combinator for left-associative operators"
  (let ((add-expr
         (parser:chainl1 (parser:satisfy #'numberp)
                         (parser:bind ((_ (parser:token :+)))
                                      (parser:return (lambda (a b) (+ a b)))))))
    
    ;; Test single operand
    (let ((result (parser:parse add-expr (make-test-input '(42)))))
      (is (parse-success-p result))
      (is-= (success-value result) 42))
    
    ;; Test multiple operands (should be left-associative)
    (let ((result (parser:parse add-expr (make-test-input '(1 :+ 2 :+ 3)))))
      (is (parse-success-p result))
      (is-= (success-value result) 6))  ; ((1 + 2) + 3) = 6
    
    ;; Test that it stops at non-operator
    (let ((result (parser:parse add-expr (make-test-input '(1 :+ 2 :end)))))
      (is (parse-success-p result))
      (is-= (success-value result) 3))))

;;; Delimiter Tests

(deftest between-combinator-test
    "Test between combinator"
  (let ((parens (parser:between (parser:token :lparen)
                                (parser:token :rparen)
                                (parser:satisfy #'symbolp))))
    
    ;; Test successful parse
    (let ((result (parser:parse parens (make-test-input '(:lparen hello :rparen)))))
      (is (parse-success-p result))
      (is-eq (success-value result) 'hello))
    
    ;; Test missing closing delimiter
    (let ((result (parser:parse parens (make-test-input '(:lparen hello :end)))))
      (is (parse-failure-p result)))))

;;; Control Flow Tests

(deftest try-combinator-test
    "Test try combinator for backtracking"
  (let ((long-or-short
         (parser:choice 
          (parser:try (parser:sequence (parser:token :long) (parser:token :keyword)))
          (parser:token :long))))
    
    ;; Test successful long parse
    (let ((result (parser:parse long-or-short (make-test-input '(:long :keyword)))))
      (is (parse-success-p result))
      (is-equal (success-value result) '(:long :keyword)))
    
    ;; Test backtracking to short parse
    (let ((result (parser:parse long-or-short (make-test-input '(:long :other)))))
      (is (parse-success-p result))
      (is-eq (success-value result) :long))))

(deftest lookahead-combinator-test
    "Test lookahead combinator"
  (let ((peek-then-consume
         (parser:sequence (parser:lookahead (parser:token :hello))
                          (parser:token :hello)
                          (parser:token :world))))
    
    ;; Test successful lookahead
    (let ((result (parser:parse peek-then-consume (make-test-input '(:hello :world)))))
      (is (parse-success-p result))
      (is-equal (success-value result) '(:hello :hello :world)))
    
    ;; Test failed lookahead
    (let ((result (parser:parse peek-then-consume (make-test-input '(:goodbye :world)))))
      (is (parse-failure-p result)))))

(deftest eof-combinator-test
    "Test end-of-input combinator"
  (let ((number-then-eof
         (parser:sequence (parser:satisfy #'numberp) (parser:eof))))
    
    ;; Test successful end-of-input
    (let ((result (parser:parse number-then-eof (make-test-input '(42)))))
      (is (parse-success-p result))
      (is-equal (success-value result) '(42 nil)))
    
    ;; Test failed end-of-input (more input remaining)
    (let ((result (parser:parse number-then-eof (make-test-input '(42 :more)))))
      (is (parse-failure-p result)))))

(deftest label-combinator-test
    "Test label combinator for better error messages"
  (let ((labeled-number (parser:label (parser:satisfy #'numberp) "integer")))
    
    ;; Test successful parse
    (let ((result (parser:parse labeled-number (make-test-input '(42)))))
      (is (parse-success-p result))
      (is-= (success-value result) 42))
    
    ;; Test failed parse with custom label
    (let ((result (parser:parse labeled-number (make-test-input '(:symbol)))))
      (is (parse-failure-p result))
      (is-equal (parser:failure-expected result) "integer"))))

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
      (is (parse-success-p result))
      (is-= (success-value result) 42))
    
    ;; Test simple array
    (let ((result (parser:parse (json-array) (make-test-input '(:lbracket 1 :comma 2 :comma 3 :rbracket)))))
      (is (parse-success-p result))
      (is-equal (success-value result) '(1 2 3)))
    
    ;; Test empty array
    (let ((result (parser:parse (json-array) (make-test-input '(:lbracket :rbracket)))))
      (is (parse-success-p result))
      (is-equal (success-value result) '()))
    
    ;; Test mixed array
    (let ((result (parser:parse (json-array) (make-test-input '(:lbracket 42 :comma "hello" :rbracket)))))
      (is (parse-success-p result))
      (is-equal (success-value result) '(42 "hello"))))
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
    (is (parse-success-p result))
    (is-= (success-value result) 5))
  
  ;; Test precedence (multiplication before addition)
  (let ((result (parser:parse (expression) (make-test-input '(2 :+ 3 :* 4)))))
    (is (parse-success-p result))
    (is-= (success-value result) 14))  ; 2 + (3 * 4) = 14
  
  ;; Test parentheses
  (let ((result (parser:parse (expression) (make-test-input '(:lparen 2 :+ 3 :rparen :* 4)))))
    (is (parse-success-p result))
    (is-= (success-value result) 20))  ; (2 + 3) * 4 = 20
  
  ;; Test left associativity
  (let ((result (parser:parse (expression) (make-test-input '(10 :- 3 :- 2)))))
    (is (parse-success-p result))
    (is-= (success-value result) 5)))  ; (10 - 3) - 2 = 5

;;; Error Handling Tests

(deftest error-message-quality-test
    "Test that error messages are informative"
  (let ((expected-keyword (parser:token :expected)))
    
    ;; Test specific token error
    (let ((result (parser:parse expected-keyword (make-test-input '(:actual)))))
      (is (parse-failure-p result))
      (let ((message (failure-message result)))
        (is (search "Unexpected token" message))
        (is (search ":ACTUAL" message))))
    
    ;; Test end of input error
    (let ((result (parser:parse expected-keyword (make-test-input '()))))
      (is (parse-failure-p result))
      (let ((message (failure-message result)))
        (is (search "end of input" message))))))

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
      (is (parse-failure-p result))
      ;; Should indicate expectation of string, not just generic error
      (let ((message (failure-message result)))
        (is (or (search "string" (string-downcase message))
                (search "42" message)))))))

;;; Performance and Edge Case Tests

(deftest large-input-test
    "Test parsing with large input"
  (let ((large-input (make-test-input (make-list 1000 :initial-element 42)))
        (count-numbers (parser:many (parser:satisfy #'numberp))))
    
    (let ((result (parser:parse count-numbers large-input)))
      (is (parse-success-p result))
      (is-= (length (success-value result)) 1000))))

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
      (is (parse-success-p result))
      (is-= (success-value result) 42))))
|#

(deftest empty-input-handling-test
    "Test various parsers with empty input"
  ;; many should succeed with empty result
  (let ((result (parser:parse (parser:many (parser:satisfy #'numberp)) (make-test-input '()))))
    (is (parse-success-p result))
    (is-equal (success-value result) '()))
  
  ;; many1 should fail
  (let ((result (parser:parse (parser:many1 (parser:satisfy #'numberp)) (make-test-input '()))))
    (is (parse-failure-p result)))
  
  ;; optional should succeed with default
  (let ((result (parser:parse (parser:optional (parser:satisfy #'numberp) :default) (make-test-input '()))))
    (is (parse-success-p result))
    (is-eq (success-value result) :default))
  
  ;; eof should succeed
  (let ((result (parser:parse (parser:eof) (make-test-input '()))))
    (is (parse-success-p result))))

;;; Position Management and Peek Tests

(deftest peek-combinator-test
    "Test peek combinator for looking ahead without consuming"
  (let ((input (make-test-input '(#\a #\b #\c #\d))))
    ;; Test single peek
    (let ((result (parser:parse (parser:peek) input)))
      (is (parse-success-p result))
      (is-equal (success-value result) #\a)
      ;; Verify position not changed
      (is-= (parser:parse-state-position (parser:success-state result)) 0)
      (is-not (parser:parse-state-consumed-p (parser:success-state result))))
    
    ;; Test peek on empty input
    (let ((result (parser:parse (parser:peek) (make-test-input '()))))
      (is (parse-failure-p result))
      (is (search "end of input" (failure-message result))))))

(deftest peek-n-combinator-test
    "Test peek-n combinator for looking ahead at multiple tokens"
  (let ((input (make-test-input '(#\h #\e #\l #\l #\o))))
    ;; Test peeking at 3 characters
    (let ((result (parser:parse (parser:peek-n 3) input)))
      (is (parse-success-p result))
      (is-equal (success-value result) '(#\h #\e #\l))
      ;; Verify position not changed
      (is-= (parser:parse-state-position (parser:success-state result)) 0)
      (is-not (parser:parse-state-consumed-p (parser:success-state result))))
    
    ;; Test peeking more than available
    (let ((result (parser:parse (parser:peek-n 10) input)))
      (is (parse-failure-p result))
      (is (search "Expected 10 tokens" (failure-message result))))))

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
      (is-= (parser:parse-state-position saved-state) 0)
      (is-equal (seq:first (parser:parse-state-remaining saved-state)) 1)
      
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
        (is (parse-success-p result))
        (is-= (parser:parse-state-position (parser:success-state result)) 0)))))

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
        (is (parse-success-p result))
        (is-equal (success-value result) '(:try :this :first))))))

(deftest peek-with-combinators-test
    "Test peek integration with other parser combinators"
  (let ((input (make-test-input '(#\+ 1 2 3))))
    ;; Peek at operator then parse expression
    (let ((peek-expr
           (parser:bind ((op (parser:peek))  ; Peek at operator
                         (_ (parser:token #\+))  ; Consume it
                         (nums (parser:many1 (parser:satisfy #'numberp))))
             (parser:return (list :op op :values nums)))))
      
      (let ((result (parser:parse peek-expr input)))
        (is (parse-success-p result))
        (is-equal (parser:assoc :op (success-value result)) #\+)
        (is-equal (parser:assoc :values (success-value result)) '(1 2 3))))))

(deftest multiple-peek-test
    "Test multiple consecutive peeks return same value"
  (let ((input (make-test-input '(:a :b :c))))
    (let ((multi-peek
           (parser:bind ((p1 (parser:peek))
                         (p2 (parser:peek))  ; Should be same as p1
                         (p3 (parser:peek))  ; Should be same as p1
                         (actual (parser:token :a)))
             (parser:return (list p1 p2 p3 actual)))))
      
      (let ((result (parser:parse multi-peek input)))
        (is (parse-success-p result))
        (let ((values (success-value result)))
          (is-eq (first values) :a)
          (is-eq (second values) :a)
          (is-eq (third values) :a)
          (is-eq (fourth values) :a))))))

(deftest peek-n-partial-test
    "Test peek-n behavior with partial availability"
  (let ((input (make-test-input '(1 2))))
    ;; Request 5 but only 2 available
    (let ((result (parser:parse (parser:peek-n 5) input)))
      (is (parse-failure-p result))
      (is (search "only 2 available" (failure-message result))))))

;;; Integration Tests

(deftest parser-state-preservation-test
    "Test that parser state is properly maintained"
  (let ((two-numbers
         (parser:bind ((first (parser:satisfy #'numberp))
                       (second (parser:satisfy #'numberp)))
                      (parser:return (list first second)))))
    
    (let ((result (parser:parse two-numbers (make-test-input '(1 2 3)))))
      (is (parse-success-p result))
      (is-equal (success-value result) '(1 2))
      
      ;; Remaining input should be at position 2
      (let ((final-state (parser:success-state result)))
        (is-= (parser:parse-state-position final-state) 2)
        (is-equal (seq:first (parser:parse-state-remaining final-state)) 3)))))

(deftest backtracking-state-test
    "Test that backtracking properly restores parser state"
  (let ((choice-parser
         (parser:choice
          (parser:try (parser:sequence (parser:token :a) (parser:token :b) (parser:token :c)))
          (parser:sequence (parser:token :a) (parser:token :x)))))
    
    ;; Should backtrack and succeed with second alternative
    (let ((result (parser:parse choice-parser (make-test-input '(:a :x :y)))))
      (is (parse-success-p result))
      (is-equal (success-value result) '(:a :x))
      
      ;; Should have consumed exactly 2 tokens
      (is-= (parser:parse-state-position (parser:success-state result)) 2))))
|#

(defpackage :epsilon.lib.parser
  (:use :cl)
  (:local-nicknames 
   (:seq :epsilon.lib.sequence))
  (:shadow
   :return
   :sequence)
  (:export
   ;; Parser type and state
   :parser
   :parse-state
   :make-parse-state
   :success-p
   :success-value
   :parse-position
   :parse-remaining
   :parse-context
   
   ;; Core combinators
   :parse
   :return
   :fail
   :bind
   :satisfy
   :token
   :choice
   :sequence
   :many
   :many1
   :optional
   :sepBy
   :sepBy1
   :chainl1
   :between
   
   ;; Utility combinators
   :lookahead
   :try
   :eof
   :label
   :match))

(in-package :epsilon.lib.parser)

;;;; Parser Combinator Library
;;;;
;;;; This library implements monadic parser combinators inspired by Haskell's Parsec.
;;;; Parser combinators provide a compositional approach to building parsers from
;;;; simple, reusable primitives.
;;;;
;;;; Key Design Principles:
;;;; - Monadic composition (Functor → Applicative → Monad hierarchy)
;;;; - Lazy evaluation for efficient parsing
;;;; - Backtracking with soft/hard failure distinction
;;;; - Clear error reporting with context
;;;; - Type-safe parser composition

;;;; Parser State and Result Types
;;;;
;;;; The parser state tracks position, remaining input, and error context.
;;;; Results are either parse-success or parse-failure, enabling proper
;;;; error handling and backtracking.

;; Parser state representation
(defstruct parse-state
  position        ; Current position in input
  remaining       ; Remaining input (lazy sequence)
  context         ; Error context stack
  consumed-p)     ; Whether input has been consumed

;; Parser result types
(defstruct success
  value
  state)

(defstruct failure
  message
  state
  expected)

;; Parser type - function from state to result
(deftype parser () '(function (parse-state) (or success failure)))

;;;; Monadic Operations
;;;;
;;;; The parser monad provides the mathematical foundation for composition.
;;;; - return: Lift a value into the parser monad
;;;; - fail: Signal parser failure 
;;;; - bind: Sequential composition where later parsers depend on earlier results

;; Core monadic operations

(defun return (value)
  "Return a value without consuming input."
  (lambda (state)
    (make-success :value value :state state)))

(defun fail (message &optional expected)
  "Fail with error message without consuming input."
  (lambda (state)
    (make-failure :message message 
                  :state state
                  :expected expected)))

(defun bind (parser func)
  "Monadic bind - sequence two parsers where second depends on first result."
  (lambda (state)
    (let ((result (funcall parser state)))
      (if (success-p result)
          (funcall (funcall func (success-value result))
                   (success-state result))
          result))))

;;;; Primitive Combinators
;;;;
;;;; These form the foundation for all other combinators:
;;;; - satisfy: Parse tokens matching a predicate
;;;; - token: Parse a specific token value

;; Fundamental primitives
(defun satisfy (predicate &optional expected)
  "Parse a token satisfying the predicate."
  (lambda (state)
    (if (seq:empty-p (parse-state-remaining state))
        (make-failure :message "Unexpected end of input"
                      :state state
                      :expected (or expected "token"))
        (let ((token (seq:first (parse-state-remaining state))))
          (if (funcall predicate token)
              (make-success 
               :value token
               :state (make-parse-state 
                       :position (1+ (parse-state-position state))
                       :remaining (seq:rest (parse-state-remaining state))
                       :context (parse-state-context state)
                       :consumed-p t))
              (make-failure :message (format nil "Unexpected token: ~A" token)
                            :state state
                            :expected (or expected "different token")))))))

(defun token (expected-token)
  "Parse a specific token."
  (satisfy (lambda (tok) (equal tok expected-token))
           (format nil "~A" expected-token)))

;;;; Choice and Sequence Combinators
;;;;
;;;; Essential combinators for building complex parsers:
;;;; - choice: Try alternatives in order (ordered choice)
;;;; - sequence: Parse all elements in order, collecting results

;; Choice combinator
(defun choice (&rest parsers)
  "Try parsers in order, returning first success."
  (lambda (state)
    (labels ((try-parsers (parsers)
               (if (null parsers)
                   (make-failure :message "No alternatives succeeded"
                                 :state state
                                 :expected "one of alternatives")
                   (let ((result (funcall (try (first parsers)) state)))
                     (if (or (success-p result)
                             (parse-state-consumed-p (failure-state result)))
                         result
                         (try-parsers (rest parsers)))))))
      (try-parsers parsers))))

;; Sequence combinator  
(defun sequence (&rest parsers)
  "Parse all parsers in sequence, collecting results."
  (lambda (state)
    (labels ((parse-seq (parsers state results)
               (if (null parsers)
                   (make-success :value (nreverse results) :state state)
                   (let ((result (funcall (first parsers) state)))
                     (if (success-p result)
                         (parse-seq (rest parsers)
                                    (success-state result)
                                    (cons (success-value result) results))
                         result)))))
      (parse-seq parsers state '()))))

;;;; Repetition Combinators
;;;;
;;;; Handle repeated structures with different cardinalities:
;;;; - many: Zero or more occurrences (Kleene star)
;;;; - many1: One or more occurrences (plus)
;;;; - optional: Zero or one occurrence

;; Repetition combinators

(defun many (parser)
  "Parse zero or more occurrences."
  (lambda (state)
    (labels ((parse-many (state results)
               (let ((result (funcall parser state)))
                 (if (success-p result)
                     (parse-many (success-state result)
                                 (cons (success-value result) results))
                     (make-success :value (nreverse results) :state state)))))
      (parse-many state '()))))

(defun many1 (parser)
  "Parse one or more occurrences."
  (bind parser
        (lambda (first)
          (bind (many parser)
                (lambda (rest)
                  (return (cons first rest)))))))

(defun optional (parser &optional default)
  "Parse optional element."
  (choice parser (return default)))

;;;; Utility Combinators
;;;;
;;;; Higher-level combinators for common parsing patterns:
;;;; - sepBy/sepBy1: Parse separated lists (e.g., comma-separated values)
;;;; - chainl1: Left-associative operator parsing
;;;; - between: Parse content between delimiters

;; Utility combinators
(defun sepBy (parser separator)
  "Parse zero or more elements separated by separator."
  (optional (sepBy1 parser separator) '()))

(defun sepBy1 (parser separator)
  "Parse one or more elements separated by separator."
  (bind parser
        (lambda (first)
          (bind (many (bind separator (lambda (_) parser)))
                (lambda (rest)
                  (return (cons first rest)))))))

(defun chainl1 (parser operator)
  "Left-associative operator parsing."
  (bind parser
        (lambda (first)
          (labels ((rest (acc)
                     (choice
                      (bind operator
                            (lambda (op)
                              (bind parser
                                    (lambda (next)
                                      (rest (funcall op acc next))))))
                      (return acc))))
            (rest first)))))

(defun between (open close parser)
  "Parse parser between open and close."
  (bind open
        (lambda (_)
          (bind parser
                (lambda (result)
                  (bind close
                        (lambda (_)
                          (return result))))))))

;;;; Advanced Combinators
;;;;
;;;; Sophisticated parsing control:
;;;; - lookahead: Parse without consuming input
;;;; - try: Backtrack on failure without committing
;;;; - eof: Match end of input
;;;; - label: Provide better error messages

(defun lookahead (parser)
  "Look ahead without consuming input."
  (lambda (state)
    (let ((result (funcall parser state)))
      (if (success-p result)
          (make-success :value (success-value result) :state state)
          result))))

(defun try (parser)
  "Try parser without consuming input on failure."
  (lambda (state)
    (let ((result (funcall parser state)))
      (if (success-p result)
          result
          (make-failure :message (failure-message result)
                        :state (make-parse-state :remaining (parse-state-remaining state)
                                                 :position (parse-state-position state)
                                                 :consumed-p nil
                                                 :context (parse-state-context state))
                        :expected (failure-expected result))))))

(defun eof ()
  "Parse end of input."
  (lambda (state)
    (if (seq:empty-p (parse-state-remaining state))
        (make-success :value nil :state state)
        (make-failure :message "Expected end of input"
                      :state state
                      :expected "end of input"))))

(defun label (parser name)
  "Label parser for better error messages."
  (lambda (state)
    (let ((result (funcall parser state)))
      (if (failure-p result)
          (make-failure :message (failure-message result)
                        :state (failure-state result)
                        :expected name)
          result))))

(defmacro match (bindings &body body)
  "Syntax for sequential parsing with variable binding.
   Example: (match ((key (json-atom :string)) 
                    (colon (token-p :colon))
                    (value (json-value)))
             (return (cons key value)))"
  (if (null bindings)
      `(progn ,@body)
      (let ((var (first (first bindings)))
            (parser (second (first bindings)))
            (rest-bindings (rest bindings)))
        `(bind ,parser
               (lambda (,var)
                 (declare (ignorable ,var))
                 (match ,rest-bindings ,@body))))))

;;;; Parser Execution
;;;;
;;;; The main entry point for running parsers on input.

;; Parse runner
(defun parse (parser input &key (position 0))
  "Run parser on input sequence."
  (let ((initial-state (make-parse-state :position position
                                         :remaining input
                                         :context '()
                                         :consumed-p nil)))
    (funcall parser initial-state)))

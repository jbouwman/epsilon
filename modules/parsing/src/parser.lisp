;;;; This library implements parser combinators, providing a way to
;;;; build parsers from reusable primitives.

(defpackage :epsilon.parser
  (:use :cl)
  (:local-nicknames 
   (:seq :epsilon.sequence))
  (:shadow
   :return
   :sequence)
  (:export
   ;; Parser type and state
   :parser
   :parse-state
   :parse-state-p
   :make-parse-state
   :parse-state-p
   :parse-state-position
   :parse-state-remaining
   :parse-state-context
   :parse-state-consumed-p
   
   ;; Result structures
   :success-p
   :success-value
   :success-state
   :make-success
   :failure-p
   :failure-message
   :failure-state
   :failure-expected
   :make-failure
   :parse-position
   :parse-remaining
   :parse-context
   :parse-state-position
   :parse-state-remaining
   :parse-state-context
   :parse-state-consumed-p
   
   ;; Core combinators
   :bind
   :return
   :fail
   :satisfy
   :token
   :choice
   :sequence
   :many
   :many1
   :optional
   :sepBy
   :sep+
   :chainl1
   :between
   
   ;; Utility combinators
   :lookahead
   :try
   :eof
   :label

   ;; Execution
   :parse))

(in-package :epsilon.parser)

;;; Parser State and Result Types
;;;
;;; The parser state tracks position, remaining input, and error context.

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

(deftype parser () '(function (parse-state) (or success failure)))

;;; Monadic Operations
;;;
;;;   return: Lift a value into the parser monad
;;;   fail: Signal parser failure 
;;;   bind: Sequential composition where later parsers depend on earlier results

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

(defun %bind (parser func)
  "Monadic bind - sequence two parsers where second depends on first result."
  (lambda (state)
    (let ((result (funcall parser state)))
      (if (success-p result)
          (funcall (funcall func (success-value result))
                   (success-state result))
          result))))

(defmacro bind (bindings &body body)
  "Monadic bind - sequence two parsers where latter depend on former.

   Example: (bind ((key (json-atom :string)) 
                   (colon (token-p :colon))
                   (value (json-value)))
             (return (cons key value)))"
  (if (null bindings)
      `(progn ,@body)
      (let ((var (first (first bindings)))
            (parser (second (first bindings)))
            (rest-bindings (rest bindings)))
        `(%bind ,parser
                (lambda (,var)
                  (declare (ignorable ,var))
                  (bind ,rest-bindings ,@body))))))

;;; Primitive Combinators
;;;
;;; These form the foundation for all other combinators:
;;;   satisfy: Parse tokens matching a predicate
;;;   token: Parse a specific token value

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
              (make-failure :message (if expected
                                                      (format nil "Expected ~A, got ~A" expected token)
                                                      (format nil "Unexpected token: ~A" token))
                            :state state
                            :expected (or expected "different token")))))))

(defun token (expected-token)
  "Parse a specific token."
  (satisfy (lambda (tok) (equal tok expected-token))
           (format nil "~S" expected-token)))

;;; Choice and Sequence Combinators
;;;
;;;   choice: Try alternatives in order (ordered choice)
;;;   sequence: Parse all elements in order, collecting results

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

;;; Repetition Combinators
;;;
;;; Handle repeated structures with different cardinalities:
;;;   many: Zero or more occurrences (Kleene star)
;;;   many1: One or more occurrences (plus)
;;;   optional: Zero or one occurrence

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
  (bind ((first parser)
         (rest (many parser)))
    (return (cons first rest))))

(defun optional (parser &optional default)
  "Parse optional element."
  (choice parser (return default)))

;;; Utility Combinators
;;;
;;;   sepBy/sep+: Parse separated lists (e.g., comma-separated values)
;;;   chainl1: Left-associative operator parsing
;;;   between: Parse content between delimiters

(defun sepBy (parser separator)
  "Parse zero or more elements separated by separator."
  (optional (sep+ parser separator) '()))

(defun sep+ (parser separator)
  "Parse one or more elements separated by separator."
  (bind ((first parser)
         (rest (many (bind ((_ separator)) parser))))
    (return (cons first rest))))

(defun chainl1 (parser operator)
  "Left-associative operator parsing."
  (bind ((first parser))
    (labels ((%rest (acc)
               (choice
                (bind ((op operator)
                       (next parser))
                  (%rest (funcall op acc next)))
                (return acc))))
      (%rest first))))

(defun between (open close parser)
  "Parse parser between open and close."
  (bind ((_ open)
         (result parser)
         (_ close))
    (return result)))

;;; Control Flow Combinators
;;;
;;;   lookahead: Parse without consuming input
;;;   try: Backtrack on failure without committing
;;;   eof: Match end of input
;;;   label: Label failures with specific error messages

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

;;; Parser Execution

(defun parse (parser input &key (position 0))
  "Run parser on input sequence."
  (funcall parser
           (make-parse-state :position position
                             :remaining input
                             :context '()
                             :consumed-p nil)))

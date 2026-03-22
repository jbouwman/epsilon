;;;; This library implements parser combinators, providing a way to
;;;; build parsers from reusable primitives.
;;;;
;;;; Features:
;;;;   - Monadic parser combinators with bind/return
;;;;   - Backtracking with try, cut for error commitment
;;;;   - Source location tracking (line/column) for errors
;;;;   - Error recovery for fault-tolerant parsing
;;;;   - Memoization for packrat parsing (prevents exponential blowup)
;;;;   - Lookahead and peek combinators

(defpackage epsilon.parser
  (:use :cl)
  (:shadow return sequence)
  (:require (epsilon.sequence seq))
  (:enter t))

;;; Parser State and Result Types
;;;
;;; The parser state tracks position, remaining input, source location,
;;; and error context. Source location (line/column) enables precise
;;; error messages.

(defstruct parse-state
  position        ; Current position in input (token index)
  remaining       ; Remaining input (lazy sequence)
  context         ; Error context stack for nested error messages
  consumed-p      ; Whether input has been consumed
  (line 1)        ; Current line number (1-based)
  (column 1)      ; Current column number (1-based)
  memo-table)     ; Memoization table for packrat parsing (hash-table or nil)

;; Parser result types
(defstruct success
  value
  state)

(defstruct failure
  message
  state
  expected
  (cut-p nil)     ; If true, this is a committed failure (no backtracking)
  (line nil)      ; Line where error occurred
  (column nil))   ; Column where error occurred

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
                  :expected expected
                  :line (parse-state-line state)
                  :column (parse-state-column state))))

(defun fail-with-location (message line column &optional expected)
  "Fail with error message at a specific location."
  (lambda (state)
    (make-failure :message message
                  :state state
                  :expected expected
                  :line line
                  :column column)))

(defun %bind (parser func)
  "Monadic bind - sequence two parsers where second depends on first result."
  (lambda (state)
    (let ((result (funcall parser state)))
      (if (success-p result)
          (let* ((new-state (success-state result))
                 (continuation-result (funcall (funcall func (success-value result))
                                               new-state)))
            ;; If we're past a cut point and got a failure, mark it as a cut failure
            (if (and (failure-p continuation-result)
                     (member :cut (parse-state-context new-state))
                     (not (failure-cut-p continuation-result)))
                (make-failure :message (failure-message continuation-result)
                              :state (failure-state continuation-result)
                              :expected (failure-expected continuation-result)
                              :cut-p t
                              :line (failure-line continuation-result)
                              :column (failure-column continuation-result))
                continuation-result))
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
                      :expected (or expected "token")
                      :line (parse-state-line state)
                      :column (parse-state-column state))
        (let ((token (seq:first (parse-state-remaining state))))
          (if (funcall predicate token)
              ;; Calculate new position - if token has newlines, update line/column
              (let* ((new-line (parse-state-line state))
                     (new-column (1+ (parse-state-column state))))
                (make-success
                 :value token
                 :state (make-parse-state
                         :position (1+ (parse-state-position state))
                         :remaining (seq:rest (parse-state-remaining state))
                         :context (parse-state-context state)
                         :consumed-p t
                         :line new-line
                         :column new-column
                         :memo-table (parse-state-memo-table state))))
              (make-failure :message (if expected
                                         (format nil "Expected ~A, got ~A" expected token)
                                         (format nil "Unexpected token: ~A" token))
                            :state state
                            :expected (or expected "different token")
                            :line (parse-state-line state)
                            :column (parse-state-column state)))))))

(defun token (expected-token)
  "Parse a specific token."
  (satisfy (lambda (tok) (equal tok expected-token))
           (format nil "~S" expected-token)))

;;; Choice and Sequence Combinators
;;;
;;;   choice: Try alternatives in order (ordered choice)
;;;   sequence: Parse all elements in order, collecting results

(defun choice (&rest parsers)
  "Try parsers in order, returning first success.
   Stops and returns a failure if a cut failure is encountered."
  (lambda (state)
    (labels ((try-parsers (parsers)
               (if (null parsers)
                   (make-failure :message "No alternatives succeeded"
                                 :state state
                                 :expected "one of alternatives"
                                 :line (parse-state-line state)
                                 :column (parse-state-column state))
                   (let ((result (funcall (try (first parsers)) state)))
                     (cond
                       ;; Success - return immediately
                       ((success-p result) result)
                       ;; Cut failure - no backtracking, return immediately
                       ((failure-cut-p result) result)
                       ;; Normal failure with consumed input - return
                       ((parse-state-consumed-p (failure-state result)) result)
                       ;; Normal failure without consumed input - try next
                       (t (try-parsers (rest parsers))))))))
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
  "Try parser without consuming input on failure.
   Preserves cut-p flag to allow cut failures to propagate."
  (lambda (state)
    (let ((result (funcall parser state)))
      (if (success-p result)
          result
          (make-failure :message (failure-message result)
                        :state (make-parse-state :remaining (parse-state-remaining state)
                                                 :position (parse-state-position state)
                                                 :consumed-p nil
                                                 :context (parse-state-context state)
                                                 :line (parse-state-line state)
                                                 :column (parse-state-column state)
                                                 :memo-table (parse-state-memo-table state))
                        :expected (failure-expected result)
                        :cut-p (failure-cut-p result)  ; Preserve cut marker
                        :line (failure-line result)
                        :column (failure-column result))))))

(defun eof ()
  "Parse end of input."
  (lambda (state)
    (if (seq:empty-p (parse-state-remaining state))
        (make-success :value nil :state state)
        (make-failure :message "Expected end of input"
                      :state state
                      :expected "end of input"
                      :line (parse-state-line state)
                      :column (parse-state-column state)))))

(defun label (parser name)
  "Label parser for better error messages."
  (lambda (state)
    (let ((result (funcall parser state)))
      (if (failure-p result)
          (make-failure :message (failure-message result)
                        :state (failure-state result)
                        :expected name
                        :cut-p (failure-cut-p result)
                        :line (or (failure-line result) (parse-state-line state))
                        :column (or (failure-column result) (parse-state-column state)))
          result))))

;;; Cut Operator
;;;
;;; The cut operator commits to the current parse path, preventing backtracking.
;;; This provides better error messages by indicating that once a certain point
;;; is reached, the parser has committed to this alternative.
;;;
;;; Example:
;;;   (bind ((keyword (token :if))
;;;          (_ (cut))           ; After seeing 'if', commit to if-statement parse
;;;          (condition (expr))  ; Failure here won't try other statement types
;;;          ...)
;;;     ...)

(defun cut ()
  "Return a parser that succeeds but marks subsequent failures as committed.
   Once a cut is passed, the choice combinator will not try alternatives.

   This is useful for providing better error messages. For example:

   (bind ((key (json-string))
          (_ (cut))           ; After seeing a key, commit to key-value parse
          (_ (token :colon))  ; Error here: 'expected colon' not 'expected key or ...'
          (value (json-value)))
     (return (cons key value)))

   Compare to Rust's nom 'cut' combinator or Haskell's Parsec 'commit'."
  (lambda (state)
    (make-success :value :cut
                  :state (make-parse-state
                          :position (parse-state-position state)
                          :remaining (parse-state-remaining state)
                          :context (cons :cut (parse-state-context state))
                          :consumed-p (parse-state-consumed-p state)
                          :line (parse-state-line state)
                          :column (parse-state-column state)
                          :memo-table (parse-state-memo-table state)))))

(defun cut-failure-p (state)
  "Check if the current parse state is past a cut point."
  (member :cut (parse-state-context state)))

(defun commit (parser)
  "Run parser and convert any failure to a cut failure (no backtracking).
   This is equivalent to (bind ((_ (cut)) (result parser)) (return result))
   but more convenient for wrapping existing parsers.

   Example:
     (choice (commit (keyword-statement))  ; If this starts, commit to it
             (expression-statement))"
  (lambda (state)
    (let ((result (funcall parser state)))
      (if (failure-p result)
          ;; Convert to cut failure if we consumed input or are past a cut
          (if (or (parse-state-consumed-p (failure-state result))
                  (cut-failure-p state))
              (make-failure :message (failure-message result)
                            :state (failure-state result)
                            :expected (failure-expected result)
                            :cut-p t
                            :line (or (failure-line result) (parse-state-line state))
                            :column (or (failure-column result) (parse-state-column state)))
              result)
          result))))

;;; Position Management
;;;
;;; Support for efficient save and restore of parser position

(defun save-position (state)
  "Save current parser position and return a checkpoint."
  (make-parse-state :position (parse-state-position state)
                    :remaining (parse-state-remaining state)
                    :context (parse-state-context state)
                    :consumed-p (parse-state-consumed-p state)
                    :line (parse-state-line state)
                    :column (parse-state-column state)
                    :memo-table (parse-state-memo-table state)))

(defun restore-position (checkpoint)
  "Return a parser that restores to the given checkpoint."
  (lambda (state)
    (declare (ignore state))
    (make-success :value nil :state checkpoint)))

(defmacro with-saved-position ((var) &body body)
  "Execute body with saved position available for restoration."
  `(lambda (state)
     (let ((,var (save-position state)))
       (funcall (progn ,@body) state))))

;;; Peek Combinators
;;;
;;; Look ahead without consuming input

(defun peek ()
  "Peek at the next token without consuming it."
  (lambda (state)
    (if (seq:empty-p (parse-state-remaining state))
        (make-failure :message "Unexpected end of input"
                      :state state
                      :expected "token"
                      :line (parse-state-line state)
                      :column (parse-state-column state))
        (make-success :value (seq:first (parse-state-remaining state))
                      :state state))))

(defun peek-n (n)
  "Peek at the next N tokens without consuming them."
  (lambda (state)
    (let ((remaining (parse-state-remaining state))
          (tokens '()))
      (labels ((collect-tokens (seq count)
                 (if (or (zerop count) (seq:empty-p seq))
                     (nreverse tokens)
                     (progn
                       (push (seq:first seq) tokens)
                       (collect-tokens (seq:rest seq) (1- count))))))
        (let ((result (collect-tokens remaining n)))
          (if (< (length result) n)
              (make-failure :message (format nil "Expected ~D tokens, only ~D available" n (length result))
                            :state state
                            :expected (format nil "~D tokens" n)
                            :line (parse-state-line state)
                            :column (parse-state-column state))
              (make-success :value result :state state)))))))

;;; Error Recovery
;;;
;;; Error recovery allows parsers to continue after encountering errors,
;;; enabling fault-tolerant parsing for better error reporting or partial
;;; results.

(defun recover (parser &key on-error skip-until default)
  "Run parser with error recovery. If parser fails, recovery is attempted.

   Keyword arguments:
     :on-error   - Function called on failure. Receives the failure result.
                   Can log, record error, or transform. Return value is ignored.
     :skip-until - Parser that matches recovery point. Tokens are skipped
                   until this parser succeeds.
     :default    - Default value to return on recovery. If not provided,
                   returns :recovery-error.

   Example:
     ;; Skip to next statement on error
     (recover (statement)
       :on-error (lambda (err) (log-error err))
       :skip-until (token :semicolon)
       :default nil)

   Compare to Python's lark error recovery or Java ANTLR error recovery."
  (lambda (state)
    (let ((result (funcall parser state)))
      (if (success-p result)
          result
          ;; Failure - attempt recovery
          (progn
            ;; Call error handler if provided
            (when on-error
              (funcall on-error result))
            ;; Skip to recovery point if specified
            (if skip-until
                (let ((recovery-state (failure-state result)))
                  ;; Try to find recovery point
                  (labels ((try-skip (s)
                             (if (seq:empty-p (parse-state-remaining s))
                                 ;; End of input - return with default
                                 (make-success :value (or default :recovery-error)
                                               :state s)
                                 ;; Try skip-until parser
                                 (let ((skip-result (funcall skip-until s)))
                                   (if (success-p skip-result)
                                       ;; Found recovery point
                                       (make-success :value (or default :recovery-error)
                                                     :state (success-state skip-result))
                                       ;; Skip one token and try again
                                       (try-skip (make-parse-state
                                                  :position (1+ (parse-state-position s))
                                                  :remaining (seq:rest (parse-state-remaining s))
                                                  :context (parse-state-context s)
                                                  :consumed-p t
                                                  :line (parse-state-line s)
                                                  :column (1+ (parse-state-column s))
                                                  :memo-table (parse-state-memo-table s))))))))
                    (try-skip recovery-state)))
                ;; No skip-until, just return default
                (make-success :value (or default :recovery-error)
                              :state (failure-state result))))))))

(defun recover-many (parser &key on-error skip-until sentinel)
  "Parse zero or more items with error recovery between failures.
   Continues parsing until SENTINEL matches or end of input.

   This is useful for parsing lists of items where some may be malformed.

   Keyword arguments:
     :on-error   - Function called for each parse error
     :skip-until - Parser to find next valid item position
     :sentinel   - Parser that marks end of sequence (e.g., closing brace)

   Returns a list of successfully parsed items."
  (lambda (state)
    (labels ((skip-to-recovery (s)
               ;; Skip tokens until skip-until parser succeeds
               (if (seq:empty-p (parse-state-remaining s))
                   s  ; End of input
                   (let ((skip-result (funcall skip-until s)))
                     (if (success-p skip-result)
                         s  ; Found recovery point, return state before consuming
                         ;; Skip one token and try again
                         (skip-to-recovery
                          (make-parse-state
                           :position (1+ (parse-state-position s))
                           :remaining (seq:rest (parse-state-remaining s))
                           :context (parse-state-context s)
                           :consumed-p t
                           :line (parse-state-line s)
                           :column (1+ (parse-state-column s))
                           :memo-table (parse-state-memo-table s)))))))
             (parse-items (s items)
               ;; Check for sentinel or end of input
               (cond
                 ((seq:empty-p (parse-state-remaining s))
                  (make-success :value (nreverse items) :state s))
                 (sentinel
                  (let ((sentinel-result (funcall (try sentinel) s)))
                    (if (success-p sentinel-result)
                        ;; Don't consume sentinel, just return items
                        (make-success :value (nreverse items) :state s)
                        ;; Try to parse item
                        (parse-one s items))))
                 (t (parse-one s items))))
             (parse-one (s items)
               (let ((result (funcall parser s)))
                 (if (success-p result)
                     (parse-items (success-state result)
                                  (cons (success-value result) items))
                     ;; Error - attempt recovery
                     (progn
                       (when on-error
                         (funcall on-error result))
                       (if skip-until
                           ;; Skip one token first (to make progress), then find recovery
                           (let* ((error-state (failure-state result))
                                  (skipped-state
                                   (if (seq:empty-p (parse-state-remaining error-state))
                                       error-state
                                       (make-parse-state
                                        :position (1+ (parse-state-position error-state))
                                        :remaining (seq:rest (parse-state-remaining error-state))
                                        :context (parse-state-context error-state)
                                        :consumed-p t
                                        :line (parse-state-line error-state)
                                        :column (1+ (parse-state-column error-state))
                                        :memo-table (parse-state-memo-table error-state))))
                                  (recovery-state (skip-to-recovery skipped-state)))
                             (parse-items recovery-state items))
                           ;; No recovery - stop parsing
                           (make-success :value (nreverse items)
                                         :state (failure-state result))))))))
      (parse-items state '()))))

;;; Memoization for Packrat Parsing
;;;
;;; Memoization caches parser results to avoid re-parsing the same input
;;; at the same position. This converts exponential-time parsers into
;;; linear-time (packrat parsing).
;;;
;;; Use memoization for expensive parsers or grammars with significant
;;; backtracking. Note: adds memory overhead proportional to input size.

(defun memo (parser &optional (id (gensym "PARSER")))
  "Return a memoized version of PARSER that caches results by position.

   Optional ID identifies this parser in the memo table. If not provided,
   a unique ID is generated. Use explicit IDs for parsers that are called
   from multiple places.

   Example:
     ;; Memoize an expensive expression parser
     (defvar *expr-parser* (memo (expr) 'expr))

     ;; Or inline
     (memo (many1 (choice (number) (identifier) (parenthesized-expr)))
           'expression)

   Note: The parse-state must have a memo-table set for memoization to work.
   Use PARSE-WITH-MEMO to automatically set up the memo table."
  (lambda (state)
    (let ((table (parse-state-memo-table state))
          (pos (parse-state-position state)))
      (if (null table)
          ;; No memo table - run parser directly
          (funcall parser state)
          ;; Check memo table
          (let ((key (cons id pos)))
            (multiple-value-bind (cached foundp)
                (gethash key table)
              (if foundp
                  ;; Return cached result
                  cached
                  ;; Compute and cache
                  (let ((result (funcall parser state)))
                    (setf (gethash key table) result)
                    result))))))))

(defun enable-memoization (state)
  "Return a new parse state with memoization enabled.
   Creates a fresh memo table if none exists."
  (if (parse-state-memo-table state)
      state
      (make-parse-state :position (parse-state-position state)
                        :remaining (parse-state-remaining state)
                        :context (parse-state-context state)
                        :consumed-p (parse-state-consumed-p state)
                        :line (parse-state-line state)
                        :column (parse-state-column state)
                        :memo-table (make-hash-table :test #'equal))))

;;; Source Location Utilities

(defun get-position ()
  "Return a parser that succeeds with current position info: (position line column)."
  (lambda (state)
    (make-success :value (list (parse-state-position state)
                               (parse-state-line state)
                               (parse-state-column state))
                  :state state)))

(defun format-location (failure)
  "Format a failure's location as a string like 'line 5, column 10'."
  (let ((line (failure-line failure))
        (col (failure-column failure)))
    (if (and line col)
        (format nil "line ~D, column ~D" line col)
        (format nil "position ~D" (parse-state-position (failure-state failure))))))

(defun format-error (failure)
  "Format a parse failure as a human-readable error message with location."
  (format nil "Parse error at ~A: ~A (expected ~A)"
          (format-location failure)
          (failure-message failure)
          (or (failure-expected failure) "valid input")))

;;; Parser Execution

(defun parse (parser input &key (position 0) (line 1) (column 1) memoize)
  "Run parser on input sequence.

   Keyword arguments:
     :position - Starting position (default 0)
     :line     - Starting line number (default 1)
     :column   - Starting column number (default 1)
     :memoize  - If true, enable memoization for packrat parsing

   Example:
     ;; Basic parsing
     (parse my-parser input-tokens)

     ;; With memoization for complex grammars
     (parse expression-parser tokens :memoize t)"
  (funcall parser
           (make-parse-state :position position
                             :remaining input
                             :context '()
                             :consumed-p nil
                             :line line
                             :column column
                             :memo-table (when memoize
                                           (make-hash-table :test #'equal)))))

(defun parse-with-error (parser input &key (position 0) (line 1) (column 1) memoize)
  "Run parser and signal an error on failure with formatted message.

   This is a convenience function that wraps PARSE and signals a
   human-readable error on parse failure.

   Returns the parsed value on success, signals error on failure."
  (let ((result (parse parser input
                       :position position
                       :line line
                       :column column
                       :memoize memoize)))
    (if (success-p result)
        (success-value result)
        (error "~A" (format-error result)))))

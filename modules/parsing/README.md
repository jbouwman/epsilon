# Epsilon Parser Combinators

A complete parser combinator library providing composable, reusable parsers for building complex parsing systems with excellent error reporting and backtracking support.

## Overview

The Epsilon parser combinator library offers:

- **Monadic parser combinators** with clean compositional semantics
- **Lazy sequence integration** for efficient input handling
- ** error reporting** with context tracking
- **Backtracking support** with `try` combinator
- **Rich combinator library** for common parsing patterns
- **Left-recursive parser support** via `chainl1`
- **Separated list parsing** with `sepBy` and `sep+`
- **Lookahead capabilities** without input consumption

## Quick Start

### Basic Token Parsing

```lisp
(use-package :epsilon.parser)

;; Parse specific tokens
(let ((input (epsilon.sequence:from-list '(:name :equals :value))))
  (parse (sequence (token :name) (token :equals) (token :value)) input))
;; => SUCCESS: (:name :equals :value)

;; Parse with predicates  
(let ((input (epsilon.sequence:from-list '(42 "hello" :symbol))))
  (parse (sequence (satisfy #'numberp)
                   (satisfy #'stringp) 
                   (satisfy #'symbolp)) input))
;; => SUCCESS: (42 "hello" :symbol)
```

### Monadic Composition

```lisp
;; Extract and transform parsed values
(let ((key-value-parser
        (bind ((key (satisfy #'symbolp))
               (_ (token :equals))
               (value (satisfy #'stringp)))
          (return (cons key value)))))
  (parse key-value-parser 
         (epsilon.sequence:from-list '(:name :equals "Alice"))))
;; => SUCCESS: (:name . "Alice")
```

### Choice and Repetition

```lisp
;; Parse alternatives
(let ((number-or-string 
        (choice (satisfy #'numberp)
                (satisfy #'stringp))))
  (parse number-or-string (epsilon.sequence:from-list '("hello"))))
;; => SUCCESS: "hello"

;; Parse repeated elements
(let ((numbers (many (satisfy #'numberp))))
  (parse numbers (epsilon.sequence:from-list '(1 2 3 :end))))
;; => SUCCESS: (1 2 3)
```

## Core Combinators

### Monadic Operations

**`return`** `(value)`

Lift a value into the parser monad without consuming input:

```lisp
(parse (return 42) input)  ; Always succeeds with value 42
```

**`fail`** `(message &optional expected)`

Signal parser failure with error message:

```lisp
(parse (fail "Custom error") input)  ; Always fails
```

**`bind`** `(bindings &body body)`

Monadic bind for sequential parser composition:

```lisp
(bind ((first-token (token :start))
       (content (many (satisfy #'symbolp)))
       (last-token (token :end)))
  (return (list :block content)))
```

### Primitive Combinators

**`satisfy`** `(predicate &optional expected)`

Parse tokens matching a predicate:

```lisp
(satisfy #'numberp "number")      ; Parse any number
(satisfy #'evenp "even number")   ; Parse even numbers only
(satisfy (lambda (x) (> x 10)))   ; Parse numbers > 10
```

**`token`** `(expected-token)`

Parse a specific token value:

```lisp
(token :if)        ; Parse exactly :if
(token "hello")    ; Parse exactly "hello"  
(token 42)         ; Parse exactly 42
```

### Choice Combinators

**`choice`** `(&rest parsers)`

Try parsers in order, return first success:

```lisp
(choice (token :if)
        (token :when) 
        (token :unless))  ; Parse any conditional keyword

;; Ordered choice - earlier alternatives take precedence
(choice (token :else-if)  ; Must come before :else
        (token :else))
```

**`optional`** `(parser &optional default)`

Parse optional elements with default values:

```lisp
(optional (token :optional-keyword) :not-found)
(optional (satisfy #'numberp) 0)  ; Default to 0 if no number
```

### Sequence Combinators

**`sequence`** `(&rest parsers)`

Parse all parsers in order, collect results:

```lisp
(sequence (token :let)
          (satisfy #'symbolp)
          (token :equals)
          (satisfy #'numberp))
;; Parses: :let symbol :equals number
;; Returns: (:let symbol :equals number)
```

### Repetition Combinators

**`many`** `(parser)`

Parse zero or more occurrences (Kleene star):

```lisp
(many (satisfy #'numberp))        ; Parse list of numbers
(many (choice (token :a) (token :b)))  ; Parse sequence of :a and :b
```

**`many1`** `(parser)`

Parse one or more occurrences (plus):

```lisp
(many1 (satisfy #'symbolp))       ; At least one symbol required
(many1 (token :item))             ; At least one :item required
```

### Separated Lists

**`sepBy`** `(parser separator)`

Parse zero or more elements separated by separator:

```lisp
;; Parse comma-separated numbers: "1,2,3" or empty
(sepBy (satisfy #'numberp) (token :comma))
```

**`sep+`** `(parser separator)`

Parse one or more elements separated by separator:

```lisp
;; Parse comma-separated identifiers: required at least one  
(sep+ (satisfy #'symbolp) (token :comma))
```

### Operator Parsing

**`chainl1`** `(parser operator)`

Left-associative operator parsing:

```lisp
;; Parse arithmetic expressions: 1 + 2 + 3 => ((1 + 2) + 3)
(chainl1 (satisfy #'numberp)
         (bind ((_ (token :plus)))
           (return (lambda (a b) (+ a b)))))
```

### Delimiter Parsing

**`between`** `(open close parser)`

Parse content between delimiters:

```lisp
(between (token :lparen)
         (token :rparen)
         (many (satisfy #'symbolp)))  ; Parse (symbol1 symbol2 ...)
```

## Advanced Combinators

### Control Flow

**`try`** `(parser)`

Try parser without consuming input on failure (backtracking):

```lisp
(choice (try (sequence (token :long) (token :keyword)))
        (token :long))  ; Backtrack if :long not followed by :keyword
```

**`lookahead`** `(parser)`

Parse without consuming input:

```lisp
(sequence (lookahead (token :if))  ; Check for :if ahead
          (token :if)              ; Now actually consume it
          condition-parser)
```

**`eof`** `()`

Match end of input:

```lisp
(sequence expression-parser (eof))  ; Ensure entire input consumed
```

### Error Handling

**`label`** `(parser name)`

Label parser for better error messages:

```lisp
(label (satisfy #'numberp) "integer")
(label (many1 (satisfy #'symbolp)) "identifier list")
```

## Complex Parsing Examples

### JSON-like Parser

```lisp
(defun json-parser ()
  (labels ((json-value ()
             (choice (json-object)
                     (json-array)
                     (json-string)
                     (json-number)))
           
           (json-object ()
             (between (token :lbrace)
                      (token :rbrace)
                      (sepBy (json-pair) (token :comma))))
           
           (json-pair ()
             (bind ((key (json-string))
                    (_ (token :colon))
                    (value (json-value)))
               (return (cons key value))))
           
           (json-array ()
             (between (token :lbracket)
                      (token :rbracket)
                      (sepBy (json-value) (token :comma))))
           
           (json-string ()
             (satisfy #'stringp))
           
           (json-number ()
             (satisfy #'numberp)))
    (json-value)))
```

### Arithmetic Expression Parser

```lisp
(defun arithmetic-parser ()
  (labels ((expression () (chainl1 (term) (add-op)))
           (term () (chainl1 (factor) (mul-op)))
           (factor () 
             (choice (between (token :lparen) (token :rparen) (expression))
                     (satisfy #'numberp)))
           (add-op ()
             (choice (bind ((_ (token :plus))) (return #'+))
                     (bind ((_ (token :minus))) (return #'-))))
           (mul-op ()
             (choice (bind ((_ (token :times))) (return #'*))
                     (bind ((_ (token :divide))) (return #'/))))))
    (expression)))

;; Usage:
(let ((tokens (epsilon.sequence:from-list '(2 :times 3 :plus 4))))
  (parse (arithmetic-parser) tokens))
;; => SUCCESS: 10 (evaluates (2 * 3) + 4)
```

### Configuration File Parser

```lisp
(defun config-parser ()
  (bind ((sections (many (config-section))))
    (return (make-hash-table-from-alist sections))))

(defun config-section ()
  (bind ((name (between (token :lbracket) (token :rbracket) 
                        (satisfy #'symbolp)))
         (entries (many (config-entry))))
    (return (cons name entries))))

(defun config-entry ()
  (bind ((key (satisfy #'symbolp))
         (_ (token :equals))
         (value (choice (satisfy #'stringp)
                        (satisfy #'numberp)
                        (satisfy #'symbolp))))
    (return (cons key value))))
```

## Error Handling and Debugging

### Parse Result Inspection

```lisp
(let ((result (parse some-parser input)))
  (cond ((success-p result)
         (format t "Parsed: ~A~%" (success-value result)))
        ((failure-p result)
         (format t "Error: ~A~%" (failure-message result))
         (format t "Expected: ~A~%" (failure-expected result)))))
```

### Error Context

```lisp
;; Add context to errors for better debugging
(bind ((start-pos (return (parse-position *current-state*)))
       (result (complex-parser))
       (end-pos (return (parse-position *current-state*))))
  (return (list :parsed result 
                :location (list start-pos end-pos))))
```

### Custom Error Messages

```lisp
(choice (try (sequence (token :if) condition-parser))
        (fail "Expected 'if' followed by condition"))
```

## Performance Characteristics

### Time Complexity

- **Basic combinators**: O(1) overhead
- **Choice combinators**: O(n) where n is number of alternatives
- **Many/repetition**: O(m) where m is number of repetitions
- **Backtracking**: May cause exponential behavior without memoization

### Memory Usage

- **Lazy sequences**: Constant memory for streaming
- **Parse state**: Linear in input position tracking
- **Backtracking**: Additional memory for choice points

### Optimization Tips

```lisp
;; Use try sparingly - it can impact performance
(choice (try complex-parser)    ; Only when necessary
        simple-alternative)

;; Order choices by likelihood
(choice common-case             ; Put frequent cases first
        rare-case)

;; Use specific tokens before general predicates
(choice (token :specific-keyword)
        (satisfy #'symbolp))    ; More specific first
```

## Integration

### With Lexical Analysis

```lisp
;; Combine with lexer for complete parsing pipeline
(defun parse-program (source-code)
  (let* ((tokens (lexer:tokenize source-code))
         (token-seq (epsilon.sequence:from-list tokens)))
    (parse (program-parser) token-seq)))
```

### Error Recovery

```lisp
;; Skip to synchronization points on errors
(defun program-parser ()
  (many (choice (try (statement-parser))
                (error-recovery))))

(defun error-recovery ()
  (bind ((_ (many (satisfy (lambda (tok) 
                             (not (member tok '(:semicolon :newline)))))))
         (sync-token (choice (token :semicolon) (token :newline))))
    (return :error-recovered)))
```

## Best Practices

### Parser Design

```lisp
;; Good: Compose small, focused parsers
(defun identifier () (satisfy #'symbolp))
(defun assignment () 
  (bind ((var (identifier))
         (_ (token :equals))
         (val (expression)))
    (return (list :assign var val))))

;; Avoid: Monolithic parsers
(defun bad-parser () ...)  ; Tries to parse everything at once
```

### Error Messages

```lisp
;; Good: Descriptive error context
(label (many1 (satisfy #'symbolp)) "parameter list")
(label (between (token :lbrace) (token :rbrace) body-parser) "block")

;; Good: Specific failure messages  
(choice (token :end)
        (fail "Expected 'end' to close block"))
```

### Testing Parsers

```lisp
(deftest identifier-parser-test
  "Test identifier parsing"
  (is-equal (parse (identifier) (seq:from-list '(hello)))
            (make-success :value 'hello))
  (is (failure-p (parse (identifier) (seq:from-list '(123))))))
```

The Epsilon parser combinator library provides a solid foundation for building parsers with excellent composability, clear error messages, and good performance characteristics suitable for production parsing tasks.
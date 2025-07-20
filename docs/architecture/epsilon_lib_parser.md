# epsilon.parser - Parser Combinator Library

## Overview

The `epsilon.parser` module implements monadic parser combinators based on Haskell's Parsec library. Parser combinators provide a compositional approach to building parsers from primitive functions.

## Core Architecture

### Compositional Design

Parser combinators combine primitive parsers (`satisfy`, `token`, `choice`, `sequence`, `many`, `optional`) to construct complex parsers.

**Composition Example:**
```lisp
;; Arithmetic expression parser
(defun number-parser ()
  (satisfy (lambda (token) (numberp token))))

(defun operator-parser ()
  (choice 
    (token '+')
    (token '-')
    (token '*')
    (token '/')))

(defun expression-parser ()
  (chainl1 (number-parser) (operator-parser)))
```

### Monadic Foundation

The parser library follows the Functor → Applicative → Monad hierarchy:

- **Functor**: Transform parser results with `fmap`
- **Applicative**: Apply parsers to arguments with `<*>`  
- **Monad**: Sequential composition where later parsers depend on earlier results with `>>=` (bind)

**Monadic Laws:**
1. **Left Identity**: `(bind (return a) f) ≡ (f a)`
2. **Right Identity**: `(bind m return) ≡ m`
3. **Associativity**: `(bind (bind m f) g) ≡ (bind m (lambda (x) (bind (f x) g)))`

### State and Error Management

Parser combinators distinguish between:
- **Soft failures**: No input consumed, backtracking possible
- **Hard failures**: Input consumed, committed to current parse branch

## API Reference

### Parser Types and State

#### `parse-state`
Current parsing state:
- `position`: Current position in input
- `remaining`: Remaining input (lazy sequence)
- `context`: Error context stack
- `consumed-p`: Whether input has been consumed

#### `parse-success` / `parse-failure`
Result types:
```lisp
(defstruct parse-success value state)
(defstruct parse-failure message state expected)
```

### Monadic Operations

#### `return`
```lisp
(return value) → parser
```
Lift a value into the parser monad without consuming input.

#### `fail`
```lisp
(fail message &optional expected) → parser
```
Signal parser failure with error message.

#### `bind`
```lisp
(bind parser function) → parser
```
Monadic bind - sequence parsers where the second depends on the first result.

**Example:**
```lisp
(bind (keyword "let")
      (lambda (_)
        (bind (identifier)
              (lambda (name)
                (bind (symbol "=")
                      (lambda (_)
                        (bind (expression)
                              (lambda (value)
                                (return (make-let-binding name value))))))))))
```

#### `match`
```lisp
(match bindings &body body) → parser
```
Macro for sequential parsing with variable binding. Provides syntactic sugar for nested `bind` operations.

**Example:**
```lisp
(match ((name (identifier))
        (_ (symbol "="))
        (value (expression)))
  (return (make-let-binding name value)))
```

### Primitive Combinators

#### `satisfy`
```lisp
(satisfy predicate &optional expected) → parser
```
Parse a token satisfying the predicate.

#### `token`
```lisp
(token expected-token) → parser
```
Parse a specific token value.

### Choice and Sequence

#### `choice`
```lisp
(choice &rest parsers) → parser
```
Try parsers in order, returning first success. Implements ordered choice with backtracking.

#### `sequence`
```lisp
(sequence &rest parsers) → parser
```
Parse all parsers in sequence, collecting results into a list.

### Repetition Combinators

#### `many`
```lisp
(many parser) → parser
```
Parse zero or more occurrences (Kleene star `*`).

#### `many1`
```lisp
(many1 parser) → parser
```
Parse one or more occurrences (plus `+`).

#### `optional`
```lisp
(optional parser &optional default) → parser
```
Parse zero or one occurrence (question mark `?`).

### Utility Combinators

#### `sepBy` / `sepBy1`
```lisp
(sepBy parser separator) → parser
(sepBy1 parser separator) → parser
```
Parse separated lists.

**Example:**
```lisp
;; Parse "1,2,3,4"
(sepBy (number-parser) (symbol ","))
```

#### `chainl1`
```lisp
(chainl1 parser operator) → parser
```
Left-associative operator parsing for expressions.

**Example:**
```lisp
;; Parse "1+2+3" as ((1+2)+3)
(chainl1 (number-parser) 
         (bind (symbol "+") 
               (lambda (_) (return #'+))))
```

#### `between`
```lisp
(between open close parser) → parser
```
Parse content between delimiters.

**Example:**
```lisp
;; Parse "(expression)"
(between (symbol "(") (symbol ")") (expression-parser))
```

### Advanced Combinators

#### `lookahead`
```lisp
(lookahead parser) → parser
```
Parse without consuming input (positive lookahead).

#### `try`
```lisp
(try parser) → parser
```
Attempt parser without consuming input on failure, enabling backtracking.

#### `eof`
```lisp
(eof) → parser
```
Match end of input.

#### `label`
```lisp
(label parser name) → parser
```
Provide error messages by labeling parser expectations.

### Parser Execution

#### `parse`
```lisp
(parse parser input &key (position 0)) → parse-result
```
Run parser on input sequence, returning `parse-success` or `parse-failure`.

## Parsing Patterns

### Expression Parsing with Operator Precedence

```lisp
(defun expression ()
  (or-expression))

(defun or-expression ()
  (chainl1 (and-expression) (or-operator)))

(defun and-expression ()
  (chainl1 (equality-expression) (and-operator)))

(defun equality-expression ()
  (chainl1 (relational-expression) (equality-operator)))
```

### Recursive Descent with Mutual Recursion

```lisp
(defvar *statement* nil)
(defvar *expression* nil)

(defun if-statement ()
  (match ((_ (keyword "if"))
          (condition (parens (funcall *expression*)))
          (then-stmt (funcall *statement*))
          (else-stmt (optional (bind (keyword "else")
                                     (lambda (_) (funcall *statement*))))))
    (return (make-if-statement condition then-stmt else-stmt))))

;; Initialize forward references
(setf *statement* (choice (if-statement) (expression-statement))
      *expression* (choice (literal) (identifier)))
```

### Error Recovery and Reporting

```lisp
(defun statement-with-recovery ()
  (choice
    (try (if-statement))
    (try (while-statement))
    (try (expression-statement))
    (bind (sync-to-semicolon)
          (lambda (_)
            (fail "Invalid statement")))))

(defun sync-to-semicolon ()
  "Skip tokens until semicolon for error recovery"
  (choice
    (symbol ";")
    (bind (satisfy (lambda (_) t))
          (lambda (_) (sync-to-semicolon)))))
```

## Performance Considerations

### Lazy Evaluation

Parser combinators support lazy evaluation through epsilon's sequence library:

```lisp
;; Parse only required amount
(parse (take 5 (many (identifier))) infinite-token-stream)
```

### Memoization

For expensive parsers:

```lisp
(defvar *memo-table* (make-hash-table :test 'equal))

(defun memo-parser (parser)
  (lambda (state)
    (let ((key (list parser (parse-state-position state))))
      (or (gethash key *memo-table*)
          (setf (gethash key *memo-table*)
                (funcall parser state))))))
```

### Left Recursion Elimination

Transform left-recursive grammars to right-recursive:

```lisp
;; Instead of: expr := expr '+' term | term
;; Use: expr := term ('+' term)*
(defun expression ()
  (bind (term)
        (lambda (first)
          (bind (many (bind (symbol "+") (lambda (_) (term))))
                (lambda (rest)
                  (return (reduce #'make-addition (cons first rest))))))))
```

## Integration with Other Epsilon Modules

### With epsilon.sequence

Parser combinators integrate with lazy sequences:

```lisp
;; Parse from lazy token stream
(let ((tokens (tokenize (lazy-file-reader "input.txt"))))
  (parse (translation-unit) tokens))
```

### With epsilon.lexer

Build token-based parsers on character-based lexers:

```lisp
(defun token-parser (token-type)
  (satisfy (lambda (token) 
             (eq (lexer:token-type token) token-type))))
```

## Examples

### Arithmetic Calculator

```lisp
(defun calculator ()
  (match ((result (expression))
          (_ (eof)))
    (return result)))

(defun expression ()
  (chainl1 (term) (add-op)))

(defun term ()
  (chainl1 (factor) (mul-op)))

(defun factor ()
  (choice
    (number)
    (between (symbol "(") (symbol ")") (expression))))

(defun add-op ()
  (choice
    (bind (symbol "+") (lambda (_) (return #'+)))
    (bind (symbol "-") (lambda (_) (return #'-)))))

(defun mul-op ()
  (choice
    (bind (symbol "*") (lambda (_) (return #'*)))
    (bind (symbol "/") (lambda (_) (return #'/)))))
```

### JSON Parser

```lisp
(defun json-value ()
  (choice
    (json-object)
    (json-array)
    (json-atom :string)
    (json-atom :number)
    (json-atom :boolean)
    (json-atom :null)))

(defun json-object ()
  (match ((_ (symbol "{"))
          (pairs (sepBy (json-pair) (symbol ",")))
          (_ (symbol "}")))
    (return (make-object pairs))))

(defun json-pair ()
  (match ((key (json-atom :string))
          (_ (symbol ":"))
          (value (json-value)))
    (return (cons key value))))
```

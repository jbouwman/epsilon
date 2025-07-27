# Epsilon Parsing Module

Parsing and lexing utilities for Common Lisp.

## Components

### Parser
General parsing utilities and combinators for building parsers.

### Lexer
Lexical analysis tools for tokenizing input streams.

## Usage

```lisp
;; Load the module
(epsilon:load "epsilon.parsing")

;; Parser combinators
(epsilon.parser:parse-sequence ...)

;; Lexer utilities
(epsilon.lexer:tokenize ...)
```

## Features

### Parser Utilities
- Parser combinators
- Error recovery
- Location tracking
- Backtracking support

### Lexer Tools
- Token stream generation
- Character predicates
- Position tracking
- Lookahead support

## Dependencies

- epsilon.core

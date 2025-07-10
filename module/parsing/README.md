# Epsilon Parsing Module

Parsing and lexing utilities with JSON support for Common Lisp.

## Components

### Parser
General parsing utilities and combinators for building parsers.

### Lexer
Lexical analysis tools for tokenizing input streams.

### JSON
Complete JSON parsing and encoding implementation.

## Usage

```lisp
;; Load the module
(epsilon.lib.module:load-module "epsilon.parsing")

;; JSON parsing
(json:parse "{\"name\": \"John\", \"age\": 30}")
(json:encode '(("name" . "John") ("age" . 30)))

;; Parser combinators
(parser:parse-sequence ...)

;; Lexer utilities
(lexer:tokenize ...)
```

## Features

### JSON Support
- Full JSON specification compliance
- Streaming parser support
- Pretty printing
- Custom encoding/decoding hooks
- Validation support

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
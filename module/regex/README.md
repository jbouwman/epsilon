# Epsilon Regex Module

A complete regular expression engine for Common Lisp with NFA/DFA conversion and optimization.

## Features

- Full regular expression syntax support
- NFA (Non-deterministic Finite Automaton) construction
- DFA (Deterministic Finite Automaton) conversion
- Optimization passes for better performance
- Character class support
- Quantifiers: *, +, ?, {n}, {n,}, {n,m}
- Grouping and capturing
- Anchors: ^, $
- Escape sequences
- Unicode support

## Usage

```lisp
;; Load the module
(epsilon.lib.module:load-module "epsilon.regex")

;; Create a regex pattern
(defparameter *pattern* (regex:make-regex "\\d{3}-\\d{4}"))

;; Match against a string
(regex:match *pattern* "555-1234")  ; => match object

;; Find all matches
(regex:find-all *pattern* "Call 555-1234 or 555-5678")

;; Replace matches
(regex:replace *pattern* "555-1234" "XXX-XXXX")
```

## Implementation

This regex engine uses a multi-stage approach:
1. Parse regex syntax into AST
2. Convert AST to NFA
3. Optionally convert NFA to DFA for better performance
4. Execute matching using the automaton

## Performance

The engine includes several optimizations:
- DFA conversion for frequently used patterns
- Character class optimization
- Dead state elimination
- State minimization

## Dependencies

- epsilon.core
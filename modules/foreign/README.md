# Epsilon Foreign Module

Foreign Function Interface (FFI) and C language integration for Common Lisp.

## Components

### FFI Core (epsilon.foreign)
Complete foreign function interface for loading shared libraries and calling C functions.

### C Language Parser (epsilon.clang)
C language parser for header analysis and type extraction.

## Features

### FFI Interface
- Dynamic library loading and management
- Function calling interface with type safety
- Memory management for foreign objects
- Platform-specific library path resolution
- Structure definition and mapping
- Header parsing integration

### C Language Support
- Complete C tokenizer and lexer
- Parser combinators for C syntax
- AST generation for C constructs
- Support for typedef, struct, union, enum declarations
- Function declaration parsing
- Type database for C type information

## Usage

```lisp
;; Load the module
(epsilon.module:load-module "epsilon.foreign")

;; Load a shared library
(lib:lib-open "libm.so")

;; Define a foreign function
(lib:defshared sin "sin" :double :double)

;; Call the function
(sin 1.5707963267948966)  ; => 1.0

;; Parse C header
(clang:parse "int foo(double x);")
```

## Platform Support

The module integrates with platform-specific code for:
- **Darwin**: BSD kqueue, TLS, networking
- **Linux**: epoll, networking  
- **Windows**: IOCP, networking

## Dependencies

- epsilon.core - Core utilities
- epsilon.parsing - Parser combinators for C language parsing

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

### Unified API

The epsilon.foreign module provides a single, unified `defshared` macro for all FFI definitions:

```lisp
;; Load the module
(epsilon.module:load-module "epsilon.foreign")

;; Basic function definition
(defshared my-strlen "strlen" "libc" :unsigned-long ((str :string)))

;; Optimized function (uses trampolines)
(defshared my-malloc "malloc" "libc" :pointer ((size :unsigned-long))
           :optimize t)

;; Auto-discover signature from headers
(defshared complex-func "complex_func" "mylib" nil ()
           :auto-discover t)

;; Inline short functions for maximum performance
(defshared get-pid "getpid" "libc" :int ()
           :optimize t :inline t)

;; Direct FFI calls
(shared-call '("strlen" "libc") :unsigned-long '(:string) "Hello")

;; Batch definition of multiple functions
(defcfuns "libc"
  (c-strlen "strlen" :unsigned-long (:string))
  (c-malloc "malloc" :pointer (:unsigned-long))
  (c-free "free" :void (:pointer))
  (c-getpid "getpid")  ; Auto-discover signature
)
```

### Key Features

- **Single defshared macro**: All FFI function definitions use one macro with options
- **Automatic optimization**: The system chooses the best backend (libffi, trampolines, eval)
- **Smart routing**: shared-call intelligently routes to the optimal implementation
- **Signature caching**: Function addresses and signatures are cached for performance
- **JIT compilation ready**: Framework in place for future JIT optimization
- **Batch operations**: Efficient batch processing of multiple FFI calls

## Platform Support

The module integrates with platform-specific code for:
- **Darwin**: BSD kqueue, TLS, networking
- **Linux**: epoll, networking  
- **Windows**: IOCP, networking

## Dependencies

- epsilon.core - Core utilities
- epsilon.parsing - Parser combinators for C language parsing

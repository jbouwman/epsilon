# Epsilon Core Package

The foundational package for the Epsilon Lisp environment, providing essential data structures, utilities, and system interfaces built on SBCL.

## Overview

Epsilon Core contains 57+ source files organized into three main categories:

- **Core Libraries (`lib/`)** - Fundamental data structures and algorithms
- **System Interface (`sys/`)** - Low-level system access and utilities  
- **Build Tools (`tool/`)** - Package management and build system

## Core Libraries (`modules/core/`)

### Data Structures

- **`map`** - Immutable Hash Array Mapped Trie (HAMT) with O(log32 n) operations
- **`sequence`** - Lazy, immutable sequences with delayed evaluation
- **`set`** - Persistent sets built on HAMT
- **`table`** - Hash table operations and utilities
- **`list`** - List processing functions
- **`array`** - Array manipulation utilities

### String Processing

- **`string`** -  string operations including splitting, joining, case conversion
- **`char`** - Character classification and operations
- **`binary`** - Binary data handling and conversion

### Functional Programming

- **`function`** - Function composition and higher-order utilities
- **`control`** - Control flow constructs and macros
- **`transducer`** - Composable transformation functions
- **`collect`** - Collection processing utilities

### System Integration

- **`process`** - Process execution and management
- **`stream`** - Stream processing and I/O utilities
- **`path`** - File path manipulation (platform-aware)
- **`url`** - URL parsing and construction
- **`time`** - Time and date operations

### Data Formats

- **`edn`** - Extensible Data Notation parsing/emission
- **`hex`** - Hexadecimal encoding/decoding
- **`digest/`** - Cryptographic hash functions (SHA-2, generic interface)

### Language Support

- **`syntax`** - Threading macros (->, ->>, cond->)
- **`protocol`** - Protocol-based polymorphism
- **`type`** - Type checking utilities
- **`symbol`** - Symbol manipulation
- **`condition`** - Condition handling utilities

### Command Line

- **`argparse`** - Command-line argument parsing
- **`log`** - Logging utilities
- **`writer`** - Output formatting

### Other Utilities

- **`diff`** - Difference calculation between data structures

## System Interface (`modules/core/src/sys/`)

Low-level system access and concurrency:

- **`fs`** - File system operations (cross-platform)
- **`env`** - Environment variable access
- **`thread`** - Thread utilities and management
- **`lock`** - Locking primitives
- **`semaphore`** - Semaphore implementation
- **`atomic`** - Atomic operations
- **`gc`** - Garbage collection utilities
- **`timeout`** - Timeout handling
- **`error`** - Error handling utilities
- **`variable`** - Dynamic variable utilities
- **`pkg`** - Package system utilities

## Usage Examples

### Maps (HAMT)
```lisp
(use-package :epsilon.map)

;; Create and manipulate immutable maps
(defparameter *map* (make-map :name "Alice" :age 30))
(get *map* :name)                    ; => "Alice"
(assoc *map* :city "Boston")         ; => new map with :city added
(dissoc *map* :age)                  ; => new map without :age
```

### Lazy Sequences
```lisp
(use-package :epsilon.sequence)

;; Lazy evaluation with sequences
(defparameter *nums* (from-list '(1 2 3 4 5)))
(realize (map #'1+ *nums*))          ; => (2 3 4 5 6)
(realize (filter #'evenp *nums*))    ; => (2 4)
```

### String Processing
```lisp
(use-package :epsilon.string)

(split "," "a,b,c")                  ; => ("a" "b" "c")
(join "-" '("x" "y" "z"))           ; => "x-y-z"
(starts-with-p "hello" "he")         ; => T
```

### Threading Macros
```lisp
(use-package :epsilon.syntax)

(->> '(1 2 3 4 5)
     (map #'1+)
     (filter #'evenp)
     realize)                        ; => (2 4 6)
```

### File System Operations
```lisp
(use-package :epsilon.sys.fs)

(ensure-dir "output/")
(write-file "output/test.txt" "Hello World")
(read-file "output/test.txt")        ; => "Hello World"
```

## Building and Testing

```bash
# Build the core package
./epsilon build epsilon.core

# Run tests
./epsilon test --module epsilon.core

# Build with tests
./epsilon build --test epsilon.core
```

## Dependencies

Epsilon Core is the foundation package with minimal external dependencies:
- SBCL (Steel Bank Common Lisp)
- Standard Common Lisp libraries

## Architecture

- **Immutable by Default** - Core data structures are persistent and immutable
- **Lazy Evaluation** - Sequences compute values on-demand
- **Protocol-Based** - Uses protocols for polymorphic dispatch
- **Cross-Platform** - File system and path operations work across platforms
- **Build System Integration** - Content-based incremental compilation

## Performance

- **HAMT Maps** - O(log32 n) operations with excellent cache performance
- **Structural Sharing** - Memory-efficient immutable data structures
- **Lazy Sequences** - Process infinite sequences with constant memory
- **Parallel Building** - Multi-threaded compilation support

Epsilon Core provides the essential foundation for functional programming in the Epsilon environment with performance characteristics suitable for production use.

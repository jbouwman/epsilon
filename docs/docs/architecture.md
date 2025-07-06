# Architecture

Epsilon's architecture emphasizes modularity, performance, and developer experience through a layered design.

## Design Principles

### Functional-First
- Immutable data structures by default
- Lazy evaluation where beneficial
- Minimal side effects in core operations

### Zero External Dependencies
- Uses only SBCL built-in modules
- No external libraries required
- Self-contained distribution

### Performance-Oriented
- Efficient data structure implementations (HAMT)
- Structural sharing for memory efficiency
- Optimized hot paths with type declarations

## System Layers

### Core Library (`epsilon.lib.*`)

The foundation layer providing functional data structures and utilities:

```
epsilon.lib
├── map          # Hash Array Mapped Trie implementation
├── sequence     # Lazy sequences with memoization
├── set          # Immutable sets based on HAMT
├── vector       # Vector operations and utilities
├── list         # Extended list operations
├── collect      # Collection building macros
├── json         # JSON encoding/decoding
├── yaml         # YAML document processing
├── msgpack      # Binary serialization
├── base64       # Base64 encoding
├── hex          # Hexadecimal conversion
├── digest       # SHA-2 cryptographic hashing
├── checksum     # CRC-32, Adler-32 checksums
├── string       # Unicode-aware string processing
├── character    # Character operations
├── regex        # Regular expression matching
├── uri          # URI parsing and construction
├── uuid         # UUID generation
└── time         # Time and date utilities
```

### System Layer (`epsilon.sys.*`)

Platform integration and system services:

```
epsilon.sys
├── thread       # Threading primitives and pools
├── lock         # Synchronization mechanisms
├── atomic       # Lock-free operations
├── fs           # Filesystem operations
├── env          # Environment variable access
├── process      # Process creation and management
├── pkg          # Package utilities
└── gc           # Garbage collection controls
```

### Network Layer (`epsilon.net.*`)

Network programming with platform-specific optimizations:

```
epsilon.net
├── socket       # Low-level socket operations
├── dns          # DNS resolution and caching
└── http         # HTTP client and server
```

Platform-specific event systems:
- Linux: `epsilon.sys.epoll`
- macOS: `epsilon.sys.kqueue`  
- Windows: `epsilon.sys.iocp`

### Tool Layer (`epsilon.tool.*`)

Development and build tools:

```
epsilon.tool
├── build        # Dependency-tracking build system
├── test         # Test framework with metrics
├── benchmark    # Performance measurement
├── format       # Code formatting
└── dev          # Development CLI
```

## Package Organization

### Local Nicknames Pattern

Each package uses local nicknames for clean integration:

```lisp
(defpackage #:my-application
  (:use #:common-lisp)
  (:local-nicknames
    (#:map #:epsilon.lib.map)
    (#:seq #:epsilon.lib.sequence)
    (#:json #:epsilon.lib.json)
    (#:http #:epsilon.net.http)))
```

This provides:
- **Namespace isolation** - No global symbol conflicts
- **Readable code** - Short, meaningful prefixes
- **Easy refactoring** - Change implementations without affecting client code

### Export Conventions

- **Predicates** end with `-p`: `empty-p`, `contains-p`
- **Constructors** start with `make-`: `make-map`, `make-set`
- **Converters** use patterns: `from-list`, `to-vector`
- **Destructive operations** end with `!`: `assoc!`, `update!`

## Data Structure Design

### Hash Array Mapped Trie (HAMT)

Core data structure for maps and sets:

```
HAMT Node Structure:
┌─────────────────┐
│ Bitmap (32-bit) │  ← Indicates which children exist
├─────────────────┤
│ Children Array  │  ← Sparse array of child nodes
└─────────────────┘
```

Benefits:
- **O(log₃₂ n)** operations (effectively constant for practical sizes)
- **Structural sharing** between versions
- **Memory efficient** through sparse representation
- **Cache friendly** with good locality

### Lazy Sequences

Support infinite and computed sequences:

```lisp
(defstruct lazy-seq
  realized     ; Boolean flag
  value        ; Cached first element
  rest-fn      ; Function to compute rest
  cached-rest) ; Memoized rest sequence
```

Features:
- **Memoization** - Computed values are cached
- **Infinite sequences** - Can represent unbounded data
- **Composable operations** - Chain transformations efficiently

## Build System

### Dependency Tracking

Uses content-based hashing for incremental builds:

```lisp
(defstruct module-info
  name           ; Module name
  sources        ; Source file paths
  dependencies   ; Required modules
  content-hash   ; Hash of all source content
  build-time)    ; Last successful build
```

### Package Definition

Each module has a `package.yaml` configuration:

```yaml
name: my-module
description: Module description
version: 1.0.0

sources:
  - src

tests:  
  - tests

dependencies:
  - core
  - another-module
```

## Performance Characteristics

### Memory Usage

- **Immutable structures** use structural sharing
- **Lazy sequences** compute elements on demand
- **Pooled allocations** for frequently used objects

### Time Complexity

| Operation | Map | Set | Sequence |
|-----------|-----|-----|----------|
| Lookup    | O(log n) | O(log n) | O(1) first, O(n) nth |
| Insert    | O(log n) | O(log n) | O(1) cons |
| Delete    | O(log n) | O(log n) | N/A |
| Iteration | O(n) | O(n) | O(n) |

### Space Complexity

- **Maps/Sets**: O(n) with structural sharing
- **Sequences**: O(1) for lazy, O(n) when realized
- **Functional updates**: O(log n) new nodes

## Error Handling

### Condition System

Uses Common Lisp's condition system consistently:

```lisp
(define-condition epsilon-error (error)
  ((context :initarg :context :reader error-context)))

(define-condition key-not-found (epsilon-error)
  ((key :initarg :key :reader missing-key)))
```

### Recovery Strategies

- **Default values** for missing keys/elements
- **Graceful degradation** where possible
- **Explicit error conditions** for exceptional cases

## Testing Strategy

### Test Organization

Tests are co-located with source code:

```
module/
├── src/
│   └── my-package.lisp
└── tests/
    └── my-package-tests.lisp
```

### Test Framework Features

- **Hierarchical organization** by package
- **Timing and metrics** collection
- **Multiple output formats** (TAP, JUnit XML)
- **Assertion macros**: `is`, `is-equal`, `is-thrown-p`

## Platform Considerations

### SBCL-Specific Features

Leverages SBCL capabilities:
- **sb-posix** for system calls
- **sb-bsd-sockets** for networking
- **sb-rotate-byte** for bit manipulation
- **Compiler optimization** declarations

### Cross-Platform Support

- **Conditional compilation** for platform differences
- **Feature detection** at runtime
- **Graceful fallbacks** for unsupported features

---

*Next: [Core Library](core/data-structures.md) - Functional data structures in detail*
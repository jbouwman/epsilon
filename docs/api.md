# API Reference

API documentation for Epsilon packages.

## Core Data Structures

### Collections
- **[Map](api/map.md)** - Hash-array mapped trie (HAMT) implementation
  - O(log32 n) operations for get, assoc, dissoc
  - Nested access with `get-in`, `assoc-in`, `update-in`
  - Transformations: `map`, `filter`, `reduce`

- **[Sequence](api/sequence.md)** - Lazy sequences
  - Lazy evaluation with memoization
  - Operations: `map`, `filter`, `take`, `drop`

- **[Set](api/set.md)** - Sets based on HAMT
  - Set operations: union, intersection, difference
  - O(log32 n) membership testing

## Data Encoding & Serialization

### Text Formats
- **JSON** - JSON encoding/decoding
  - Streaming parser
  - Configurable encoding

- **YAML** - YAML processing
  - YAML 1.2 specification
  - Document and stream processing

- **XML** - XML parsing and generation

### Binary Formats
- **Base64** - Base64 encoding and decoding *(documentation pending)*
- **Hex** - Hexadecimal string conversion *(documentation pending)*
- **MessagePack** - Binary serialization *(documentation pending)*
- **Binary** - Binary data handling *(documentation pending)*

## Cryptography & Hashing

### Hash Functions
- **Digest** - SHA-2 hash functions *(documentation pending)*
  - SHA-224, SHA-256, SHA-384, SHA-512
  - Output formats: bytes, hex, base64

- **Checksum** - Checksums *(documentation pending)*
  - CRC-32 and Adler-32
  - Stream processing

## Text Processing

### String Operations
- **String** - String manipulation *(documentation pending)*
  - Unicode string processing
  - Pattern matching and replacement

- **Character** - Character operations *(documentation pending)*
- **Regex** - Regular expressions *(documentation pending)*

### Parsing & Generation
- **URI** - URI parsing, construction, and manipulation *(documentation pending)*
- **UUID** - UUID generation and parsing *(documentation pending)*
- **Time** - Time and date utilities *(documentation pending)*

## System Integration

### Threading & Concurrency
- **Thread** - Threading primitives *(documentation pending)*
- **Lock** - Synchronization primitives *(documentation pending)*
- **Atomic** - Atomic operations *(documentation pending)*

### System Services
- **FS** - Filesystem operations *(documentation pending)*
- **Env** - Environment variables *(documentation pending)*
- **Process** - Process management *(documentation pending)*

### Package System
- **Package** - Package utilities *(documentation pending)*

## Network Programming

### HTTP
- **HTTP** - HTTP client and server *(documentation pending)*
  - HTTP/1.1 specification
  - Connection pooling
  - TLS/SSL support

### Core Networking
- **Socket** - Socket operations *(documentation pending)*
- **DNS** - DNS resolution *(documentation pending)*

## Development Tools

### Build System
- **Build** - Build system *(documentation pending)*
  - Dependency tracking
  - Incremental compilation
  - Module dependencies

### Testing
- **Test** - Test framework *(documentation pending)*
  - Test organization
  - Output formats: TAP, JUnit XML
  - Performance metrics collection

### Performance
- **Benchmark** - Benchmarking framework *(documentation pending)*
  - Statistical analysis of performance
  - Comparison between implementations
  - Memory usage profiling

### Code Quality
- **Format** - Code formatting and pretty-printing *(documentation pending)*

## Usage Patterns

### Package Integration
All API packages use local nicknames:

```lisp
(defpackage #:my-application
  (:use #:common-lisp)
  (:local-nicknames
    (#:map #:epsilon.map)
    (#:seq #:epsilon.sequence)
    (#:json #:epsilon.json)
    (#:http #:epsilon.net.http)))

(in-package #:my-application)

(let ((data (map:make-map :users (seq:from-list users))))
  (json:encode data))
```

### Function Naming Conventions
- **Predicates** end with `-p`: `empty-p`, `contains-p`
- **Constructors** often start with `make-`: `make-map`, `make-set`
- **Converters** use `from-`/`to-` patterns: `from-list`, `to-vector`
- **Destructive operations** end with `!`: `assoc!`, `update!`

### Error Handling
All functions follow consistent error handling patterns:
- Invalid arguments signal appropriate condition types
- Optional default values for missing keys/elements
- Graceful degradation where possible

### Performance Characteristics
Function documentation includes performance notes where relevant:
- Time complexity (O(1), O(log n), O(n))
- Space complexity for large data structures
- Memory allocation patterns

---

## Documentation Generation

This API reference is partially generated from source code using:

```bash
# Generate documentation for a specific package
./scripts/generate-api-docs.sh epsilon.map

# Generate docs with output file
./scripts/generate-api-docs.sh epsilon.sequence docs/docs/reference/api/sequence.md
```

The generator extracts:
- Function signatures with parameter lists
- Documentation strings
- Symbol types (function, macro, variable)
- Source code comments

---

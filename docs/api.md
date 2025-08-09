# API Reference

API documentation for Epsilon packages.

## Core Data Structures

### Collections
- **[Map](api/map.md)** - Immutable hash-array mapped trie (HAMT) implementation
  - Key-value associations with O(log n) operations
  - Nested access with `get-in`, `assoc-in`, `update-in`
  - Functional transformations: `map`, `filter`, `reduce`

- **[Sequence](api/sequence.md)** - Lazy sequences with functional operations
  - Lazy evaluation with memoization support
  - Infinite sequence capabilities
  - Operation library: `map`, `filter`, `take`, `drop`

- **[Set](api/set.md)** - Immutable sets based on HAMT structure
  - Mathematical set operations (union, intersection, difference)
  - O(log n) membership testing and modification

## Data Encoding & Serialization

### Text Formats
- **JSON** - JSON encoding/decoding with Unicode support
  - Streaming parser for large documents
  - Configurable encoding options

- **YAML** - YAML document processing
  - YAML 1.2 specification support
  - Document and stream processing

- **XML** - XML parsing and generation

### Binary Formats
- **Base64** - Base64 encoding and decoding *(documentation pending)*
- **Hex** - Hexadecimal string conversion *(documentation pending)*
- **MessagePack** - Binary serialization format *(documentation pending)*
- **Binary** - Low-level binary data handling *(documentation pending)*

## Cryptography & Hashing

### Hash Functions
- **Digest** - SHA-2 family hash functions *(documentation pending)*
  - SHA-224, SHA-256, SHA-384, SHA-512 implementations
  - Multiple output formats (bytes, hex, base64)

- **Checksum** - Non-cryptographic checksums *(documentation pending)*
  - CRC-32 and Adler-32 implementations
  - Stream-based processing support

## Text Processing

### String Operations
- **String** - String manipulation and utilities *(documentation pending)*
  - Unicode-aware string processing
  - Pattern matching and replacement

- **Character** - Character operations and Unicode support *(documentation pending)*
- **Regex** - Regular expression matching *(documentation pending)*

### Parsing & Generation
- **URI** - URI parsing, construction, and manipulation *(documentation pending)*
- **UUID** - UUID generation and parsing *(documentation pending)*
- **Time** - Time and date utilities *(documentation pending)*

## System Integration

### Threading & Concurrency
- **Thread** - Threading primitives and thread pools *(documentation pending)*
- **Lock** - Synchronization primitives *(documentation pending)*
- **Atomic** - Atomic operations and lock-free data structures *(documentation pending)*

### System Services
- **FS** - Filesystem operations and path manipulation *(documentation pending)*
- **Env** - Environment variable access *(documentation pending)*
- **Process** - Process creation and management *(documentation pending)*

### Package System
- **Package** - Package utilities and introspection *(documentation pending)*

## Network Programming

### HTTP
- **HTTP** - Complete HTTP client and server *(documentation pending)*
  - HTTP/1.1 specification compliance
  - Connection pooling and keep-alive
  - TLS/SSL support

### Core Networking
- **Socket** - Low-level socket operations *(documentation pending)*
- **DNS** - DNS resolution and caching *(documentation pending)*

## Development Tools

### Build System
- **Build** - Dependency-tracking build system *(documentation pending)*
  - Content-based change detection
  - Incremental compilation
  - Module dependency resolution

### Testing
- **Test** - Test definition and execution framework *(documentation pending)*
  - Hierarchical test organization
  - Multiple output formats (TAP, JUnit XML)
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

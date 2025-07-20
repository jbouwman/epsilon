# Package Index

Complete index of all packages in the Epsilon system.

## Package Hierarchy

Epsilon organizes functionality into a hierarchical package structure:

```
epsilon
├── lib/          # Core library functions
├── sys/          # System-level utilities  
├── net/          # Network protocols
└── tool/         # Development tools
```

## Core Library (`epsilon.*`)

### Data Structures
- **`epsilon.map`** - Immutable maps (HAMT implementation)
- **`epsilon.sequence`** - Lazy sequences with functional operations
- **`epsilon.set`** - Immutable sets based on HAMT
- **`epsilon.list`** - Extended list operations and utilities
- **`epsilon.vector`** - Vector operations and bounds checking
- **`epsilon.collect`** - Collection macros for efficient list building

### Data Encoding/Decoding
- **`epsilon.json`** - JSON parsing and generation with Unicode support
- **`epsilon.yaml`** - YAML document processing
- **`epsilon.base64`** - Base64 encoding and decoding
- **`epsilon.hex`** - Hexadecimal string conversion
- **`epsilon.msgpack`** - MessagePack binary serialization

### Cryptography & Hashing
- **`epsilon.digest`** - SHA-2 family hash functions (SHA-224, SHA-256, SHA-384, SHA-512)
- **`epsilon.checksum`** - CRC-32 and Adler-32 checksums

### Text & String Processing
- **`epsilon.string`** - String manipulation and utilities
- **`epsilon.character`** - Character operations and Unicode support
- **`epsilon.regex`** - Regular expression matching and replacement

### Utilities
- **`epsilon.time`** - Time and date utilities
- **`epsilon.uuid`** - UUID generation and parsing
- **`epsilon.uri`** - URI parsing, construction, and manipulation
- **`epsilon.reader`** - Enhanced reader macros and syntax

## System Utilities (`epsilon.sys.*`)

### Threading & Concurrency
- **`epsilon.sys.thread`** - Threading primitives and thread pools
- **`epsilon.sys.lock`** - Synchronization primitives (locks, semaphores)
- **`epsilon.sys.atomic`** - Atomic operations and lock-free data structures

### Filesystem
- **`epsilon.sys.fs`** - Filesystem operations and path manipulation
- **`epsilon.sys.file`** - File I/O utilities and streaming
- **`epsilon.sys.path`** - Path construction and resolution

### Environment & System
- **`epsilon.sys.env`** - Environment variable access and management
- **`epsilon.sys.process`** - Process creation and management
- **`epsilon.sys.signal`** - Signal handling
- **`epsilon.sys.gc`** - Garbage collection controls and monitoring

### Package System
- **`epsilon.sys.package`** - Package utilities and introspection
- **`epsilon.sys.asdf`** - ASDF integration (when available)

## Network Protocols (`epsilon.net.*`)

### Core Networking
- **`epsilon.net.core`** - Low-level networking primitives
- **`epsilon.net.socket`** - Socket operations and management
- **`epsilon.net.dns`** - DNS resolution and caching

### HTTP
- **`epsilon.net.http`** - Complete HTTP client and server implementation
- **`epsilon.net.client`** - HTTP client with connection pooling
- **`epsilon.net.server`** - HTTP server with request routing
- **`epsilon.net.middleware`** - HTTP middleware for common operations

### Security
- **`epsilon.net.tls`** - TLS/SSL support for secure connections
- **`epsilon.net.auth`** - Authentication and authorization utilities

## Development Tools (`epsilon.tool.*`)

### Build System
- **`epsilon.tool.build`** - Dependency-tracking build system
- **`epsilon.tool.dependency`** - Dependency analysis and resolution
- **`epsilon.tool.hash`** - Content hashing for incremental builds

### Testing Framework
- **`epsilon.tool.test`** - Test definition and execution framework
- **`epsilon.tool.assert`** - Assertion macros and utilities
- **`epsilon.tool.mock`** - Test doubles and mocking
- **`epsilon.tool.fixture`** - Test fixtures and setup utilities

### Performance & Analysis
- **`epsilon.tool.bench`** - Benchmarking framework with statistical analysis
- **`epsilon.tool.profile`** - Performance profiling utilities
- **`epsilon.tool.metric`** - Metrics collection and reporting

### Code Quality
- **`epsilon.tool.format`** - Code formatting and pretty-printing
- **`epsilon.tool.lint`** - Static analysis and linting
- **`epsilon.tool.doc`** - Documentation generation
- **`epsilon.tool.check`** - Type checking and validation

## Package Conventions

### Naming Patterns
- **Predicates**: End with `-p` (e.g., `empty-p`, `valid-p`)
- **Constructors**: Often start with `make-` (e.g., `make-map`)
- **Converters**: Use `from-`/`to-` or `->` patterns
- **Constants**: Use `+constant-name+` convention

### Local Nicknames
All packages define local nicknames for dependencies:

```lisp
(defpackage #:epsilon.map
  (:use #:common-lisp)
  (:local-nicknames
    (#:collect #:epsilon.collect)
    (#:hash #:epsilon.hash))
  (:export ...))
```

### Export Patterns
- **Core functions**: Always exported
- **Implementation details**: Not exported
- **Utilities**: Exported if generally useful
- **Constants**: Exported with `+name+` convention

## Integration Guidelines

### Package Dependencies
- **Core packages** depend only on other core packages
- **System packages** may depend on core packages
- **Network packages** depend on core and system packages
- **Tool packages** may depend on any other packages

### Local Nickname Usage
```lisp
(defpackage #:my-package
  (:use #:common-lisp)
  (:local-nicknames
    (#:map #:epsilon.map)
    (#:seq #:epsilon.sequence)
    (#:json #:epsilon.json)))

(in-package #:my-package)

;; Clean, readable code
(map:get my-map :key)
(seq:map #'process data)
(json:encode object)
```

### Shadowing Imports
Some packages shadow Common Lisp symbols:

```lisp
;; epsilon.map shadows: map, reduce, count
;; epsilon.sequence shadows: map, reduce, count, remove
;; epsilon.list shadows: remove, delete

;; Access original CL functions with cl: prefix when needed
(cl:map 'list #'identity my-list)
```

## Package Documentation

Each package includes:
- **Docstring** describing purpose and scope
- **Usage examples** in package-level documentation
- **Function documentation** for all exported symbols
- **Integration guide** showing usage with other packages

---

*For detailed API documentation, see [API Reference](api.md) or use the documentation generator.*

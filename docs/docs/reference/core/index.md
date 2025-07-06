# epsilon.core Module

The core module provides fundamental functionality for functional programming in Common Lisp.

## Package Groups

### Data Structures
Immutable, functional data structures with structural sharing:

- **[epsilon.lib.map](data-structures/map.md)** - Hash Array Mapped Trie implementation
- **[epsilon.lib.sequence](data-structures/sequence.md)** - Lazy sequences with memoization  
- **[epsilon.lib.vector](data-structures/vector.md)** - Vector operations and utilities
- **[epsilon.lib.set](data-structures/set.md)** - Immutable sets based on HAMT
- **[epsilon.lib.list](data-structures/list.md)** - Extended list operations

### Data Encoding
Serialization and parsing for common data formats:

- **[epsilon.lib.json](encoding/json.md)** - JSON encoding and decoding
- **[epsilon.lib.yaml](encoding/yaml.md)** - YAML document processing
- **[epsilon.lib.msgpack](encoding/msgpack.md)** - Binary MessagePack serialization
- **[epsilon.lib.base64](encoding/base64.md)** - Base64 encoding
- **[epsilon.lib.hex](encoding/hex.md)** - Hexadecimal conversion

### Cryptography
Hashing and checksum algorithms:

- **[epsilon.lib.digest](crypto/digest.md)** - SHA-2 cryptographic hashing
- **[epsilon.lib.checksum](crypto/checksum.md)** - CRC-32 and Adler-32 checksums

### String Processing
Unicode-aware text manipulation:

- **[epsilon.lib.string](strings/string.md)** - String processing utilities
- **[epsilon.lib.char](strings/char.md)** - Character operations
- **[epsilon.lib.regex](strings/regex.md)** - Regular expression matching

### System
Platform integration and system services:

- **[epsilon.sys.fs](system/fs.md)** - Filesystem operations
- **[epsilon.sys.thread](system/thread.md)** - Threading primitives and pools
- **[epsilon.sys.env](system/env.md)** - Environment variable access

### Networking
Interim networking solution for cross-platform socket programming:

- **[epsilon.lib.net](networking/net.md)** - TCP/UDP sockets based on sb-bsd-sockets

### Tools
Development and build utilities:

- **[epsilon.tool.build](tools/build.md)** - Dependency-tracking build system
- **[epsilon.tool.test](tools/test.md)** - Test framework with metrics
- **[epsilon.tool.benchmark](tools/benchmark.md)** - Performance measurement

## Design Principles

All packages in epsilon.core follow these principles:

- **Immutability**: Data structures don't modify in place
- **Functional interfaces**: Pure functions where possible  
- **Structural sharing**: Efficient memory usage through shared structure
- **Local nicknames**: Clean integration with user code
- **No dependencies**: Built only on SBCL primitives

## Common Patterns

```lisp
;; Local nicknames for clean syntax
(defpackage :my-app
  (:use :cl)
  (:local-nicknames
   (:map :epsilon.lib.map)
   (:seq :epsilon.lib.sequence)
   (:json :epsilon.lib.json)))

;; Functional data manipulation
(-> (map:make-map :users '("alice" "bob"))
    (map:assoc :timestamp (get-universal-time))
    (json:encode))
```
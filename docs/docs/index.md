# Epsilon

Epsilon is a Common Lisp distribution based on SBCL that provides
modern data structures, cryptography, networking, and development
tools.

## Overview

Epsilon provides:

- **Functional data structures**: Immutable maps, sequences, and collections
- **Data encoding**: JSON, YAML, MessagePack, Base64, and hexadecimal utilities
- **Cryptography**: SHA-2 family hashing, CRC-32, and Adler-32 checksums
- **Development tools**: Build system, test framework, and benchmarking utilities

## Quick Start

```bash
# Download and install Epsilon
curl -sSL https://github.com/jbouwman/epsilon/releases/latest/download/install.sh | bash

# Run
epsilon
```

## Key Features

### Modern Data Structures

```lisp
;; Functional maps with persistent updates
(map:make-map :a 1 :b 2 :c 3)
;=> {:a 1, :b 2, :c 3}

;; Efficient sequence operations
(seq:map #'1+ '(1 2 3 4 5))
;=> (2 3 4 5 6)
```

### JSON Processing

```lisp
;; Parse and generate JSON with full Unicode support
(json:decode "{\"name\": \"Epsilon\", \"version\": 1.0}")
;=> (("name" . "Epsilon") ("version" . 1.0))

(json:encode '((:name . "Epsilon") (:features . ("functional" "fast"))))
;=> "{\"name\":\"Epsilon\",\"features\":[\"functional\",\"fast\"]}"
```

### Cryptographic Operations

```lisp
;; SHA-256 hashing with multiple output formats
(digest:sha256-string "Hello, Epsilon!")
;=> "A1B2C3D4E5F6..."

(digest:sha256-bytes "Hello, Epsilon!")
;=> #(161 178 195 212 ...)
```

### HTTP Client

```lisp
;; Simple HTTP requests with JSON support
(http:get "https://api.example.com/data" 
          :headers '(("Accept" . "application/json")))
```

## Architecture

Epsilon follows a modular architecture with clear separation of concerns:

- **`epsilon.lib.*`**: Core library functions and data structures
- **`epsilon.sys.*`**: System-level utilities (threading, filesystem, environment)
- **`epsilon.net.*`**: Network protocols and communication
- **`epsilon.tool.*`**: Development tools (build, test, benchmark, format)

### Design Principles

1. **Functional-first**: Immutable data structures and pure functions where possible
2. **Performance-oriented**: Optimized implementations for real-world usage
3. **Standards-compliant**: Full adherence to relevant specifications (JSON, HTTP, TLS)
4. **SBCL-native**: Leverages SBCL-specific features for optimal performance
5. **Zero external dependencies**: Built entirely on SBCL's included modules

## Documentation

- [Getting Started](getting-started.md) - Installation and basic usage
- [Architecture](architecture.md) - System design and package organization  
- [Core Library](core/data-structures.md) - Data structures and algorithms
- [Development Tools](tools/build.md) - Build system and testing framework
- [Roadmaps](roadmaps.md) - Future development plans

## License

Epsilon is open source software. See the repository for licensing details.

---

*Built with functional programming principles for modern Lisp development.*

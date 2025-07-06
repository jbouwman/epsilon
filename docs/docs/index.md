# Epsilon

Epsilon is a Common Lisp environment based on SBCL with functional data structures, cryptography, networking, and development tools.

## Quick Start

```bash
# Download and install
curl -sSL https://github.com/jbouwman/epsilon/releases/latest/download/install.sh | bash

# Run
epsilon
```

## Example

```lisp
;; Functional maps
(map:make-map :a 1 :b 2 :c 3)
;=> {:a 1, :b 2, :c 3}

;; JSON processing
(json:decode "{\"name\": \"Epsilon\", \"version\": 1.0}")
;=> (("name" . "Epsilon") ("version" . 1.0))
```

## Design Principles

- **Functional programming**: Immutable data structures with structural sharing
- **No external dependencies**: Uses only SBCL built-in modules
- **Performance**: Type declarations and optimized implementations where needed

## System Overview

### Core Library (`epsilon.lib.*`)

Functional data structures and utilities:

- **Data structures**: map, sequence, set, vector, list
- **Data encoding**: json, yaml, msgpack, base64, hex
- **Cryptography**: digest (SHA-2), checksum (CRC-32, Adler-32)
- **Text processing**: string, character, regex
- **Utilities**: uri, uuid, time

### System (`epsilon.sys.*`)

Platform integration and system services:

- **Concurrency**: thread, lock, atomic
- **System**: fs, env, process, pkg, gc

### Networking (`epsilon.net.*`)

Network programming with platform-specific optimizations:

- **Core**: socket, dns, http
- **Platform support**: epoll (Linux), kqueue (macOS), iocp (Windows)

### Tools (`epsilon.tool.*`)

Development and build tools:

- **build**: Dependency-tracking build system
- **test**: Test framework with metrics
- **benchmark**: Performance measurement
- **format**: Code formatting

## Documentation

- [Getting Started](getting-started.md) - First steps with Epsilon
- [Installation](installation.md) - Detailed installation instructions
- [API Reference](reference/api.md) - Complete API documentation
- [Examples](reference/examples.md) - Code samples and patterns
- [Roadmaps](roadmaps.md) - Development plans
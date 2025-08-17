# Epsilon

[![CI - Multi-Platform Build](https://github.com/jbouwman/epsilon/actions/workflows/ci.yml/badge.svg)](https://github.com/jbouwman/epsilon/actions/workflows/ci.yml)
[![Documentation](https://img.shields.io/badge/docs-github%20pages-blue)](https://jbouwman.github.io/epsilon/)

Epsilon is a Lisp programming environment built on top of SBCL that
provides functional data structures, data encoding, cryptographic
functionality, and network programming capabilities.

It is a new project and is evolving. Feedback is welcome!

## Features

- **Structures** - Immutable maps, sets, and sequences with structural sharing
- **Encoding** - JSON, YAML, MessagePack, Base64, and binary format support  
- **Cryptography** - SHA-2 family hashing, CRC-32, and Adler-32 checksums
- **Networking** - HTTP client/server with TLS support
- **Tools** - Build system, test framework, and benchmarking utilities
- **No Dependencies** - Depends only upon SBCL built-in modules

## Installation

### Quick Install

Install Epsilon runtime:

```bash
curl -sSL https://raw.githubusercontent.com/jbouwman/epsilon/main/scripts/install.sh | bash
```

This installs a complete runtime.

### Manual Installation

Download the appropriate release for your platform from [GitHub Releases](https://github.com/jbouwman/epsilon/releases):
- `epsilon-macos-arm64.tar.gz` - macOS Apple Silicon
- `epsilon-linux-x86_64.tar.gz` - Linux x86_64

### Usage

#### Unix (Linux/macOS)
```bash
# Interactive REPL
./epsilon

# Evaluate expressions  
./epsilon --eval "(format t \"Hello, World\")"

# Load specific module
./epsilon --module epsilon.json --eval "(json:encode '(:foo \"bar\"))"

# List available modules
./epsilon --modules
```

### Testing

```bash
# Test specific module
./epsilon --test epsilon.core

# Test specific package within a module
./epsilon --test epsilon.core:epsilon.log.tests

# Test specific test by name
./epsilon --test epsilon.core:epsilon.log.tests:test-detailed-formatter

# Test with verbose output for debugging
./epsilon --test epsilon.core --verbose
./epsilon --test epsilon.core:epsilon.log.tests:test-detailed-formatter --verbose

# Self-test all modules
./epsilon --exec epsilon.release:selftest

# Run all tests with JUnit output
./epsilon --exec epsilon.release:selftest --format junit --file target/TEST-results.xml

# Run CLI smoke tests
./scripts/smoke.sh
```

### Development

For development setup and contributing:

```bash
# Clone repository
git clone https://github.com/jbouwman/epsilon.git
cd epsilon

# Run tests
./scripts/test.sh

# Build documentation
./scripts/build.sh

# Create a release (maintainers)
./scripts/release.sh --dry-run 0.11.0
```

See [RELEASE.md](RELEASE.md) for complete release documentation.

## Architecture

Epsilon is a Lisp programming environment built on SBCL with a modular architecture. The codebase follows functional programming principles with immutable data structures and protocol-based extensibility.

### Module System
Each module in `/modules/` contains a `module.lisp` file defining dependencies and exports. Modules are loaded via `epsilon.loader` which handles dependency resolution and incremental compilation. Core bootstrap happens through `/modules/core/src/boot.lisp`.

### Key Architectural Patterns
- **Functional Data Structures**: Maps, sets, and sequences with structural sharing (modules/core/src/lib/)
- **Protocol-based Design**: Extension points via epsilon.protocol for polymorphic behavior
- **Platform Abstraction**: Platform-specific modules (darwin/linux/windows) handle OS differences
- **Resource Management**: Connection pooling in HTTP client, proper cleanup in all I/O operations
- **Transducers**: Composable algorithmic transformations in modules/core/src/lib/transducer.lisp

### Testing Framework
The custom test framework (epsilon.test) supports hierarchical test organization with fixtures. Tests use `deftest` macro and assertions like `is`, `is-=`, `is-equal`. Tests can output to shell, REPL, or JUnit XML format.

### Module Dependencies
Core modules form a dependency hierarchy:
- epsilon.core: Foundation (no dependencies)
- epsilon.test: Testing framework (depends on core)
- epsilon.http: HTTP client/server (depends on core, tls)
- epsilon.json/yaml/msgpack: Data formats (depend on core)
- epsilon.lsp: Language Server Protocol (depends on json, http)

### Code Style
- Lisp naming conventions: lowercase with hyphens
- Packages export symbols explicitly via defpackage
- Functional style preferred: avoid mutation, use pure functions
- Proper docstrings for public APIs
- Indentation follows standard Lisp conventions

## Documentation

**[Documentation](https://jbouwman.github.io/epsilon/)**

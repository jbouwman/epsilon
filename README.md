# Epsilon

[![CI - Multi-Platform Build](https://github.com/jbouwman/epsilon/actions/workflows/ci.yml/badge.svg)](https://github.com/jbouwman/epsilon/actions/workflows/ci.yml)
[![Documentation](https://img.shields.io/badge/docs-github%20pages-blue)](https://jbouwman.github.io/epsilon/)

Epsilon is a Lisp programming environment built on top of SBCL that provides functional data structures, data encoding, cryptographic functionality, and network programming capabilities.

## Key Features

- **Functional Data Structures** - Immutable maps, sets, and sequences with efficient structural sharing
- **Data Encoding** - JSON, YAML, MessagePack, Base64, and binary format support  
- **Cryptography** - SHA-2 family hashing, CRC-32, and Adler-32 checksums
- **Network Programming** - HTTP client/server with TLS support
- **Development Tools** - Build system, test framework, and benchmarking utilities
- **Zero Dependencies** - Uses only SBCL built-in modules for maximum portability

## Installation

### Quick Install

Install Epsilon runtime:

```bash
curl -sSL https://raw.githubusercontent.com/jbouwman/epsilon/main/scripts/install.sh | bash
```

This installs a complete SBCL runtime with Epsilon preloaded.

### Manual Installation

Download the appropriate release for your platform from [GitHub Releases](https://github.com/jbouwman/epsilon/releases):
- `epsilon-macos-arm64.tar.gz` - macOS Apple Silicon
- `epsilon-macos-x86_64.tar.gz` - macOS Intel
- `epsilon-linux-x86_64.tar.gz` - Linux x86_64

### Usage

```bash
# Interactive REPL with Epsilon loaded
epsilon

# Evaluate expressions
epsilon --eval "(format t \"Hello, Epsilon!\")" --eval "(sb-ext:quit)"

# Use Epsilon libraries
epsilon --eval "(epsilon.lib.map:make-map :a 1 :b 2)" --eval "(sb-ext:quit)"
```

## Documentation

**[Complete Documentation](https://jbouwman.github.io/epsilon/)**

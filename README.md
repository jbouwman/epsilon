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
# Interactive REPL with Epsilon loaded
epsilon

# Evaluate expressions  
epsilon --eval "(format t \"Hello, Epsilon!\")"

# Use Epsilon libraries
epsilon --eval "(epsilon.lib.map:make-map :a 1 :b 2)"
```

## Documentation

**[Documentation](https://jbouwman.github.io/epsilon/)**

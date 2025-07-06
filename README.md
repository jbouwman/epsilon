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

📖 **[Complete Documentation](https://jbouwman.github.io/epsilon/)**

## Installation

### Quick Install (Recommended)

Install Epsilon as a value-added SBCL runtime:

```bash
curl -sSL https://raw.githubusercontent.com/jbouwman/epsilon/main/scripts/install.sh | bash
```

Run tests:

This installs a complete SBCL runtime with Epsilon preloaded, providing fast startup and zero external dependencies.

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

## Development

### Building from Source

```bash
./run.sh build epsilon.core
```

### Running Tests

```bash
./run.sh test epsilon.core
```

### Creating Distribution Package

```bash
chmod +x scripts/build-runtime.sh
./scripts/build-runtime.sh
```

## Packages

```
epsilon (platform-specific)
├── epsilon.core (shared)
│   ├── epsilon.lib.*     # Functional data structures, codecs, crypto
│   ├── epsilon.sys.*     # System utilities, FFI, threading
│   └── epsilon.tool.*    # Build system, testing, formatting
└── Platform-specific networking:
    ├── Linux:   epsilon.sys.epoll  → epsilon.net
    ├── Darwin:  epsilon.sys.kqueue → epsilon.net
    └── Windows: epsilon.sys.iocp   → epsilon.net
```

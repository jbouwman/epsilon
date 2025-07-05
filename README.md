sh# Epsilon

![Under Construction](construction.gif)

[![CI - Multi-Platform Build](https://github.com/jbouwman/epsilon/actions/workflows/ci.yml/badge.svg)](https://github.com/jbouwman/epsilon/actions/workflows/ci.yml)

Epsilon is a library for SBCL that provides data structures, data formats, compression, cryptography and networking.

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

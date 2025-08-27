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

## Testing in Epsilon

Epsilon has a comprehensive testing system built around the `epsilon.test` framework. Unlike Common Lisp systems that use ASDF or Quicklisp, Epsilon uses its own module system with dependency resolution based on `module.lisp` files.

### Understanding Epsilon's Module System

Epsilon determines load order by examining `defpackage` forms within each module. Inter-module dependencies are specified in `module.lisp` files using these key fields:

- **`name`**: The module's unique identifier (e.g., "epsilon.linux")
- **`requires`**: List of module names this module depends on
- **`provides`**: Virtual packages this module implements (e.g., "epsilon.net")
- **`platform`**: Target platform ("linux", "darwin", "windows", or omit for all)

Example `module.lisp`:
```lisp
(:name "epsilon.linux"
 :version "1.0.0" 
 :description "Linux-specific functions (epoll networking)"
 :requires ("epsilon.foreign")
 :platform "linux"
 :provides ("epsilon.net" "epsilon.async"))
```

### Running Tests

#### Basic Test Commands

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
```

#### Platform-Specific Testing

```bash
# Test Linux networking (when on Linux)
./epsilon --test epsilon.linux

# Test Darwin/macOS networking (when on macOS)
./epsilon --test epsilon.darwin

# Test platform-agnostic modules
./epsilon --test epsilon.json
```

#### Comprehensive Testing

```bash
# Self-test all modules (discovers and runs all tests)
./epsilon --exec epsilon.release:selftest

# Run all tests with JUnit XML output
./epsilon --exec epsilon.release:selftest --format junit --file target/TEST-results.xml

# Run CLI smoke tests
./scripts/smoke.sh

# Run development test suite
./scripts/test.sh
```

### Test Framework Features

#### Test Definition

Tests use the `deftest` macro with assertion functions:

```lisp
(deftest test-socket-creation ()
  "Test TCP socket creation"
  (let ((fd (create-socket +af-inet+ +sock-stream+ +ipproto-tcp+)))
    (is (integerp fd))
    (is (>= fd 0))
    (when (>= fd 0)
      (%close fd))))
```

#### Assertion Functions

- `(is condition)` - Assert condition is true
- `(is-equal expected actual)` - Assert equality
- `(is-thrown (condition-type) form)` - Assert form throws specific condition
- `(skip "reason")` - Skip test with reason

#### Test Fixtures and Utilities

Tests can define helper functions and fixtures:

```lisp
(defclass test-echo-server ()
  ((listener :initarg :listener :accessor server-listener)
   (port :initarg :port :accessor server-port)))

(defmacro with-echo-server ((server &key (port 0)) &body body)
  "Run body with an echo server"
  `(let ((,server (start-echo-server :port ,port)))
     (unwind-protect
          (progn ,@body)
       (stop-echo-server ,server))))
```

### Module Testing Structure

Each module organizes tests in a `tests/` directory:

```
modules/
├── linux/
│   ├── module.lisp           # Module definition
│   ├── src/
│   │   ├── net.lisp         # Main implementation
│   │   └── net/             # Modular components
│   └── tests/
│       ├── net/
│       │   ├── socket-tests.lisp    # Basic socket tests
│       │   ├── tcp-tests.lisp       # TCP integration tests
│       │   ├── udp-tests.lisp       # UDP tests
│       │   └── async-tests.lisp     # Async operations tests
│       └── core-tests.lisp          # Legacy tests
```

### Writing Effective Tests

#### Integration Testing

Epsilon encourages integration tests with real network fixtures:

```lisp
(deftest test-tcp-echo-string ()
  "Test sending and receiving string data"
  (with-echo-server (server)
    (let* ((port (server-port server))
           (addr (make-socket-address "127.0.0.1" port))
           (client (tcp-connect addr :timeout 2.0))
           (test-string "Hello, Echo Server!"))
      
      ;; Send string
      (let ((bytes-sent (tcp-write client test-string)))
        (is-equal (length test-string) bytes-sent))
      
      ;; Receive echo  
      (let ((buffer (make-array 100 :element-type '(unsigned-byte 8))))
        (let ((bytes-read (tcp-read client buffer :timeout 2.0)))
          (is-equal (length test-string) bytes-read)
          (let ((received (sb-ext:octets-to-string (subseq buffer 0 bytes-read))))
            (is-equal test-string received))))
      
      (tcp-shutdown client :how :both))))
```

#### Error Condition Testing

Test error conditions and edge cases:

```lisp
(deftest test-tcp-connect-refused ()
  "Test connection to non-existent server"
  (let ((addr (make-socket-address "127.0.0.1" 54321)))
    (is-thrown (connection-refused)
      (tcp-connect addr :timeout 0.5))))
```

#### Performance and Load Testing

Include performance tests for critical paths:

```lisp
(deftest test-tcp-multiple-clients ()
  "Test handling multiple concurrent clients"
  (with-echo-server (server)
    (let* ((port (server-port server))
           (num-clients 10)
           (clients '()))
      
      ;; Create multiple clients
      (dotimes (i num-clients)
        (let ((client (tcp-connect 
                       (make-socket-address "127.0.0.1" port) 
                       :timeout 2.0)))
          (push client clients)))
      
      ;; All should be connected
      (dolist (client clients)
        (is (tcp-connected-p client)))
      
      ;; Clean up
      (dolist (client clients)
        (tcp-shutdown client :how :both)))))
```

### Test Output Formats

Epsilon supports multiple output formats:

- **Shell**: Human-readable output for development
- **REPL**: Interactive testing with detailed feedback  
- **JUnit**: XML format for CI/CD integration

### Debugging Failed Tests

Use verbose mode for detailed test output:

```bash
# Debug specific failing test
./epsilon --test epsilon.linux:epsilon.net.tcp-tests:test-tcp-echo-string --verbose

# Debug all tests in a module
./epsilon --test epsilon.linux --verbose
```

The verbose output shows:
- Test execution timeline
- Assertion details 
- Error stack traces
- Resource cleanup status

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

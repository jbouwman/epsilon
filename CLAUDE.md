# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Epsilon is a Lisp programming environment built on SBCL that provides functional data structures, data encoding, cryptographic functionality, and network programming capabilities. The project is organized as a modular system with packages in `src/` and comprehensive documentation in `docs/`.

## Core Architecture

- **Modular Design**: Each module in `src/` has its own `package.lisp` and follows the pattern: `src/module-name/src/` for implementation, `src/module-name/tests/` for tests
- **Core Libraries**: `src/core/` contains fundamental libraries (map, sequence, string, etc.) and system utilities (fs, thread, etc.)
- **Specialized Modules**: HTTP, JSON, WebSocket, LSP, cryptography, and other functionality in separate modules
- **Build System**: Content-based incremental build system with dependency tracking
- **Package System**: Uses `package.edn` files for module configuration and dependencies

## Development Commands

### Building
```bash
# Build specific module
./epsilon build epsilon.core

# Build all modules  
./epsilon build

# Clean and rebuild
./epsilon build --clean epsilon.core

# Build with tests
./epsilon build --test epsilon.core
```

### Testing
```bash
# Run tests for specific module
./epsilon test --module epsilon.core

# Run all tests
./epsilon test

# Test a single file
./epsilon test path/to/test-file.lisp
```

### Development Environment
```bash
# Start interactive REPL
./epsilon

# Evaluate expression
./epsilon --eval "(format t \"Hello\")" --eval "(sb-ext:quit)"

# Runtime distribution build
bash scripts/build-runtime.sh
```

## Key Conventions

- **File Organization**: Each module follows `src/module-name/src/implementation.lisp` and `src/module-name/tests/module-tests.lisp`
- **Package Definitions**: Located in `package.lisp` files at module root
- **Test Framework**: Uses custom test framework in `src/test/` with `deftest` macro and assertions like `is`, `is-equal`, `is-thrown`
- **Documentation**: Architecture and API docs in `docs/` with MkDocs formatting
- **Build Artifacts**: Generated in `target/` directories (do not edit manually)

## Module Dependencies

Core dependency hierarchy:
- `epsilon.core` - foundational libraries (required by most modules)
- Platform-specific modules: `epsilon.darwin`, `epsilon.linux`, `epsilon.windows`
- Network stack: `epsilon.http` depends on platform networking
- Data formats: `epsilon.json`, `epsilon.yaml`, `epsilon.msgpack` are independent
- Development tools: `epsilon.lsp`, `epsilon.test` for tooling

## Important Files

- `scripts/boot.lisp` - Bootstrap loader defining core module load order
- `scripts/build-runtime.sh` - Creates standalone distribution packages
- `docs/operations/build.md` - Build system documentation
- Module `package.edn` files define dependencies and structure

## Test Framework Usage

```lisp
(deftest test-name
  "Test description"
  (is (= 2 (+ 1 1)))
  (is-equal "hello" (string-downcase "HELLO"))
  (is-thrown 'error (error "test")))
```

Run with `(epsilon.test:run)` or via command line.

## Known Implementation Details

- Package definitions are in Lisp, not EDN
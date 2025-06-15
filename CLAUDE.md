# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

### Building and Testing
- `./make build` - Build the Epsilon library using SBCL
- `./make test` - Run all tests and exit with appropriate code (0 for success, 1 for failure)
- `./make coverage` - Generate test coverage report and open in browser

The `make` script is written in Nushell and provides the primary interface for development tasks.

## Architecture Overview

Epsilon is a utility library for SBCL that provides functional data structures, compression, cryptography, data encoding, and network protocols.

### Package Structure
The codebase is organized into several major modules:

**Core Library (`src/lib/`)**
- Functional data structures (map, sequence, vector, list)
- Data encoding/decoding (json, yaml, msgpack, base64, hex)
- Cryptography (digest, checksum with SHA-2, CRC-32, Adler-32)
- String/character processing with Unicode support
- Stream processing and I/O utilities
- Time, UUID, and URI handling
- Regular expressions and pattern matching

**System Layer (`src/sys/`)**
- Threading primitives (locks, semaphores, atomic operations)
- Filesystem operations
- Environment variable handling
- Package system utilities
- Garbage collection controls
- Error handling and timeouts

**Network Layer (`src/net/`)**
- Core networking primitives
- HTTP client and server implementation
- TLS/SSL support

**Tools (`src/tool/`)**
- Build system with dependency tracking and hashing
- Test framework with metrics collection and reporting
- Code formatting utilities

### Key Design Patterns

**Local Nicknames**: Each package uses local nicknames extensively to create clean, readable code while avoiding naming conflicts.

**Functional Style**: The library emphasizes immutable data structures and functional programming patterns, particularly in the `map`, `sequence`, and `vector` modules.

**Test Framework**: Custom test framework (`epsilon.tool.test`) with features like:
- Hierarchical test organization by package
- Timing and metrics collection
- Assertion macros (`is`, `is-equal`, `is-equalp`, `is-thrown-p`)
- Test skipping and failure reporting
- Configurable verbosity and logging

**Build System**: The build tool (`epsilon.tool.build`) implements dependency tracking with content hashing for incremental builds.

### Dependencies
The system depends only on SBCL built-in modules:
- `sb-posix` for POSIX system calls
- `sb-rotate-byte` for bit manipulation
- `sb-cltl2` for code walking
- `sb-bsd-sockets` for networking

## Major Differences from Standard Common Lisp Practices

Epsilon diverges from conventional Common Lisp patterns in several key areas:

**Build System**: ASDF is not used. Instead, `epsilon.tool.build` provides dependency tracking with content hashing for incremental builds. Use `./make build` rather than ASDF operations.

**Path Handling**: Logical pathnames are superseded by URIs throughout the system. File and resource references use URI syntax and are handled by the URI utilities in `epsilon.lib.uri`.

**Data Structures**: Built-in hash tables are superseded by `epsilon.lib.map`, which provides immutable, functional maps with better performance characteristics and a more consistent API.

## Development Notes

### Testing
Tests are co-located with source code and follow the naming convention `*-tests.lisp`. The test framework can be configured for different verbosity levels and failure handling.

### Package Naming
Packages follow the pattern `epsilon.{module}.{submodule}` with hierarchical organization. The main package exports high-level functions and uses local nicknames for internal dependencies.

### File Organization
- Source files are organized by functionality in `src/`
- Tests mirror the source structure in `tests/`
- Build artifacts and coverage reports go in `target/`

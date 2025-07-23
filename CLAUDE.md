# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

### Basic Usage
- `./epsilon` - Show epsilon help and options

### Development Mode
For development work on epsilon itself, you may use SBCL directly:
- `sbcl --core lib/sbcl/sbcl.core --load script.lisp` - Load and run script
- `sbcl --core lib/sbcl/sbcl.core --eval "(require 'module-name)"` - Load module

## Architecture Overview

Epsilon is a utility library for SBCL that provides functional data structures, compression, cryptography, data encoding, and network protocols.

### Package Structure
The codebase is organized into several major modules:

**Core Library (`src/core/src/lib/`)**
- Functional data structures (map, sequence, vector, list)
- Data encoding/decoding (json, yaml, msgpack, base64, hex)
- Cryptography (digest, checksum with SHA-2, CRC-32, Adler-32)
- String/character processing with Unicode support
- Stream processing and I/O utilities
- Time, UUID, and URI handling
- Regular expressions and pattern matching

**System Layer (`src/core/src/sys/`)**
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

**Tools (`src/core/src/tool/`)**
- Build system with dependency tracking and hashing

**Testing (`src/test`)
- Test framework with metrics collection and reporting
- Benchmark framework with performance measurement and comparison
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

## Major Differences from Standard Common Lisp Practices

Epsilon diverges from conventional Common Lisp patterns in several areas:

**Build System**: ASDF is not used. Instead, `epsilon.tool.build` provides dependency tracking with content hashing for incremental builds. Use `./make build` rather than ASDF operations.

**Path Handling**: Logical pathnames are superseded by Paths and URIs throughout the system.

**Data Structures**: Built-in hash tables are superseded by `epsilon.lib.map`, which provides immutable, functional maps with better performance characteristics and a more consistent API.
- Never use Lisp hashtables: use `epsilon.lib.map`

## Development Notes

### Testing
Tests are co-located with source code and follow the naming convention `*-tests.lisp`. The test framework can be configured for different verbosity levels and failure handling.

### Package Naming
Packages follow the pattern `epsilon.{module}.{submodule}` with hierarchical organization. The main package exports high-level functions and uses local nicknames for internal dependencies.

### File Organization
- Source files are organized by functionality in `src/`
- Tests mirror the source structure in `tests/`
- Build artifacts and coverage reports go in `target/`

### Documentation Style
- Use dry, matter-of-fact technical style for all documentation in this project
- Avoiding subjective language like "elegant", "good", "simple", "powerful", etc.
- Focus on factual descriptions of functionality, API specifications, and implementation details
  
## Project Resources
- The file `docs/wishlist.md` contains a structured list of desired future capabilities of various degrees of difficulty

## Logging System (`epsilon.lib.log`)
- Powerful hierarchical logging facility with level-based filtering
- Support for structured logging with key-value context
- Multiple appenders: console, file with different formatters (simple, detailed, JSON)
- Wildcard pattern matching for logger configuration
- Thread-safe with inheritance from parent loggers
- Performance-optimized macros that avoid evaluation when logging is disabled

### Logging Configuration
Configure logging via command line:
```bash
./epsilon --log 'debug:epsilon.lib.*,trace:epsilon.yaml,info:epsilon.tool.*' test --module epsilon.core
```

## Argument Parsing
The development tool supports argument parsing:

### Global Options (before command)
- `--log SPEC` - Configure logging with wildcard patterns
- `--verbose` - Enable debug logging  
- `--quiet` - Only show warnings and errors

### Enhanced Commands

**Build Command:**
```bash
./epsilon build --module epsilon.core
```

**Test Command (with powerful filtering):**
```bash
# Test multiple modules
./epsilon test --module 'epsilon.core,lsp,http'

# Filter tests by name pattern (supports wildcards)
./epsilon test --module epsilon.core --test 'parse-*'

# Filter by package pattern  
./epsilon test --module epsilon.core --package 'epsilon.yaml*'

# Multiple output formats
./epsilon test --module epsilon.core --format junit --file results.xml
```

**Benchmark Command:**
```bash
# Run specific benchmark suite
./epsilon benchmark --suite msgpack

# Run multiple specific benchmarks
./epsilon benchmark --benchmarks 'string-concat,arithmetic'
```

### Combined Example
```bash
./epsilon --log 'debug:epsilon.lib.*,trace:epsilon.tool.test' test --module 'epsilon.core,lsp' --test 'parse-yaml-*' --format detailed
```

This provides diagnostic capabilities for troubleshooting issues.

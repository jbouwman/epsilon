# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

### Building and Testing
- `./run.sh build` - Build the Epsilon library using SBCL
- `./run.sh test` - Run all tests and exit with appropriate code (0 for success, 1 for failure)
- `./run.sh test --package epsilon.lib.json.tests --name parse-empty-structures` - Run a single, specific test

### Benchmarking
- `./run.sh benchmark` - List available benchmarks (if any are registered)
- `./run.sh benchmark --suite msgpack` - Run MessagePack performance benchmarks
- `./run.sh benchmark --suite all` - Run all available benchmark suites
- `./run.sh benchmark arithmetic string-concat` - Compare specific benchmarks
- `./run.sh bench` - Short alias for benchmark command

Available benchmark suites:
- `msgpack` - Compare original MessagePack vs binary structure implementation
- `all` - Run complete benchmark suite with validation and memory analysis

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

Epsilon diverges from conventional Common Lisp patterns in several key areas:

**Build System**: ASDF is not used. Instead, `epsilon.tool.build` provides dependency tracking with content hashing for incremental builds. Use `./make build` rather than ASDF operations.

**Path Handling**: Logical pathnames are superseded by URIs throughout the system. File and resource references use URI syntax and are handled by the URI utilities in `epsilon.lib.uri`.

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

## Documentation Workflow
- Write little summaries of the things I ask for in docs/transcript.md

## Build System Improvements
- The boot process now logs compilation output to `target/boot.log` with progress reporting
- Build system shows progress as `[current/total]` for all compilation steps  
- Test registry properly initializes on demand to avoid stale metadata issues
- Build process is fully idempotent - can run `git clean -dxf` and rebuild from scratch

## Logging System (`epsilon.lib.log`)
- Powerful hierarchical logging facility with level-based filtering
- Support for structured logging with key-value context
- Multiple appenders: console, file with different formatters (simple, detailed, JSON)
- Wildcard pattern matching for logger configuration
- Thread-safe with proper inheritance from parent loggers
- Performance-optimized macros that avoid evaluation when logging is disabled

### Logging Configuration
Configure logging via command line:
```bash
./run.sh --log 'debug:epsilon.lib.*,trace:epsilon.lib.yaml,info:epsilon.tool.*' test --module epsilon.core
```

## Advanced Argument Parsing
The development tool now supports sophisticated argument parsing:

### Global Options (before command)
- `--log SPEC` - Configure logging with wildcard patterns
- `--verbose` - Enable debug logging  
- `--quiet` - Only show warnings and errors

### Enhanced Commands

**Build Command:**
```bash
./run.sh build --module epsilon.core
```

**Test Command (with powerful filtering):**
```bash
# Test multiple modules
./run.sh test --module 'epsilon.core,lsp,http'

# Filter tests by name pattern (supports wildcards)
./run.sh test --module epsilon.core --test 'parse-*'

# Filter by package pattern  
./run.sh test --module epsilon.core --package 'epsilon.lib.yaml*'

# Multiple output formats
./run.sh test --module epsilon.core --format junit --file results.xml
```

**Benchmark Command:**
```bash
# Run specific benchmark suite
./run.sh benchmark --suite msgpack

# Run multiple specific benchmarks
./run.sh benchmark --benchmarks 'string-concat,arithmetic'
```

### Combined Example
```bash
./run.sh --log 'debug:epsilon.lib.*,trace:epsilon.tool.test' test --module 'epsilon.core,lsp' --test 'parse-yaml-*' --format detailed
```

This provides extremely precise diagnostic capabilities for troubleshooting issues.

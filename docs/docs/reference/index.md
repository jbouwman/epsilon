# API Reference Overview

Complete reference documentation organized by module and package.

## Modules

### [epsilon.core](core/index.md)
Core functionality including data structures, encoding, cryptography, and development tools.

**Major Package Groups:**
- **Data Structures** - Maps, sequences, vectors, sets, lists
- **Data Encoding** - JSON, YAML, MessagePack, Base64, hex
- **Cryptography** - SHA-2 hashing, CRC-32, Adler-32 checksums
- **String Processing** - String manipulation, character operations, regex
- **System** - Filesystem, threading, environment
- **Tools** - Build system, testing, benchmarks

### [epsilon.net](net/index.md)
Platform-specific networking primitives with optimized event loops.

**Implementations:**
- macOS (kqueue)
- Linux (epoll)  
- Windows (IOCP)

### [epsilon.http](http/index.md)
HTTP client and server implementation built on epsilon.net.

**Components:**
- HTTP client
- HTTP server
- Request/response handling

## Documentation Format

Each package page includes:

- **Package overview** and purpose
- **Exported symbols** with signatures
- **Usage examples** and patterns
- **Related packages** and dependencies

## Symbol Conventions

- **Functions**: `function-name`
- **Macros**: `macro-name`
- **Variables**: `*variable-name*`
- **Constants**: `+constant-name+`
- **Conditions**: `condition-name`
- **Classes**: `class-name`

## Cross-References

Use the left navigation to browse packages within each module, or see [Examples](examples.md) for usage patterns across multiple packages.
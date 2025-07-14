# Module Catalog

Epsilon's modular architecture organizes functionality into focused modules grouped by role.

## Core

**epsilon.core** - Foundation data structures and essential libraries
- Immutable maps, sequences, vectors, sets, and lists
- Data encoding (JSON, YAML, MessagePack, Base64, hex)
- Cryptographic functions (SHA-2, CRC-32, Adler-32, checksums)
- String and character processing with Unicode support
- Stream processing, I/O utilities, time, UUID, and URI handling
- Regular expressions and pattern matching

## Data Formats

**epsilon.xml** - XML parsing and generation
- SAX-style XML processing
- Document object model

**epsilon.yaml** - YAML data serialization
- Human-readable data interchange format

**epsilon.msgpack** - MessagePack binary serialization
- Efficient binary data format with benchmark optimizations

## Platform-Specific

**epsilon.darwin** - macOS platform support
- Native networking and TLS integration
- Platform-specific optimizations

**epsilon.linux** - Linux platform support  
- epoll-based event handling
- Linux-specific system interfaces

**epsilon.windows** - Windows platform support
- IOCP (I/O Completion Ports) networking
- Windows system integration

## Development Tools

**epsilon.test** - Testing framework
- Hierarchical test organization
- Assertion macros and metrics collection
- Configurable reporting and failure handling

**epsilon.format** - Code formatting utilities
- Consistent code style enforcement
- Automated formatting tools

**epsilon.package** - Package management system
- Module registration and discovery
- Dependency resolution

## Network & Communication

**epsilon.net** - Core networking primitives
- Socket management and network I/O
- Cross-platform networking abstractions

**epsilon.http** - HTTP client and server
- Full HTTP/1.1 implementation
- TLS/SSL support
- Request/response handling

**epsilon.websocket** - WebSocket protocol implementation
- Real-time bidirectional communication
- Frame processing and connection management

**epsilon.lsp** - Language Server Protocol
- Code analysis and language services
- IDE integration support

## Compression & Encoding

**epsilon.inflate** - Data decompression
- DEFLATE algorithm implementation
- ZIP and gzip support foundation

**epsilon.gzip** - gzip compression
- File compression and decompression
- Stream-based processing

**epsilon.zlib** - zlib compression
- Raw DEFLATE compression
- Memory-efficient processing

**epsilon.bzip** - bzip2 compression
- Alternative compression algorithm
- High compression ratio support

## Low-Level Support

**epsilon.foreign** - Foreign function interface
- C library integration
- Cross-platform FFI abstractions

**epsilon.parsing** - Parser combinators
- Composable parsing primitives
- Language and data format parsing

**epsilon.regex** - Regular expression engine
- Pattern matching and text processing
- Optimized regex compilation

## Statistics

- **Total modules**: 19
- **Core dependencies**: All modules depend on epsilon.core
- **Platform modules**: 3 (darwin, linux, windows)
- **Network stack**: 4 modules (net, http, websocket, lsp)
- **Compression suite**: 4 modules (inflate, gzip, zlib, bzip)
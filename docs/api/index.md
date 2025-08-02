# API Reference

Documentation for all public APIs in Epsilon.

## Organization

The API documentation is organized by package, with each package having its own dedicated section. Within each package, you'll find:

- **Overview** - Package purpose and design philosophy
- **Quick Start** - Getting started examples
- **Functions** - Detailed function reference
- **Macros** - Macro documentation where applicable
- **Types** - Data types and structures
- **Examples** - Real-world usage examples
- **Best Practices** - Recommendations and patterns

## Core Libraries

### Utility Libraries

- **[epsilon.argparse](epsilon.argparse/)** - Command-line argument parsing with subcommand support
- **[epsilon.lib.map](epsilon.lib.map/)** - Immutable hash-array mapped trie (HAMT) implementation *(coming soon)*
- **[epsilon.lib.sequence](epsilon.lib.sequence/)** - Lazy sequences with functional operations *(coming soon)*
- **[epsilon.lib.string](epsilon.lib.string/)** - Unicode-aware string manipulation *(coming soon)*
- **[epsilon.lib.path](epsilon.lib.path/)** - Cross-platform path manipulation *(coming soon)*

### Data Structures

- **[epsilon.lib.set](epsilon.lib.set/)** - Immutable sets based on HAMT *(coming soon)*
- **[epsilon.lib.table](epsilon.lib.table/)** - Table formatting and display *(coming soon)*
- **[epsilon.lib.list](epsilon.lib.list/)** - Extended list operations *(coming soon)*

### System Integration

- **[epsilon.sys.fs](epsilon.sys.fs/)** - Filesystem operations *(coming soon)*
- **[epsilon.sys.env](epsilon.sys.env/)** - Environment variable access *(coming soon)*
- **[epsilon.sys.thread](epsilon.sys.thread/)** - Threading and concurrency *(coming soon)*
- **[epsilon.sys.process](epsilon.sys.process/)** - Process management *(coming soon)*

### Cryptography

- **[epsilon.lib.digest](epsilon.lib.digest/)** - Cryptographic hash functions *(coming soon)*
- **[epsilon.lib.checksum](epsilon.lib.checksum/)** - CRC and checksum algorithms *(coming soon)*
- **[epsilon.lib.uuid](epsilon.lib.uuid/)** - UUID generation and parsing *(coming soon)*

### Data Formats

- **[epsilon.json](epsilon.json/)** - JSON encoding and decoding *(coming soon)*
- **[epsilon.yaml](epsilon.yaml/)** - YAML processing *(coming soon)*
- **[epsilon.msgpack](epsilon.msgpack/)** - MessagePack binary serialization *(coming soon)*
- **[epsilon.xml](epsilon.xml/)** - XML parsing and generation *(coming soon)*

### Networking

- **[epsilon.http](epsilon.http/)** - HTTP client and server *(coming soon)*
- **[epsilon.websocket](epsilon.websocket/)** - WebSocket client and server *(coming soon)*
- **[epsilon.tls](epsilon.tls/)** - TLS/SSL support *(coming soon)*

### Development Tools

- **[epsilon.test](epsilon.test/)** - Testing framework *(coming soon)*
- **[epsilon.tool.build](epsilon.tool.build/)** - Build system *(coming soon)*
- **[epsilon.tool.package](epsilon.tool.package/)** - Package management *(coming soon)*

## Using the API Documentation

### Package Naming

All Epsilon packages follow a consistent naming scheme:

- `epsilon.lib.*` - Core utility libraries
- `epsilon.sys.*` - System integration libraries
- `epsilon.tool.*` - Development tools
- `epsilon.*` - Specialized modules (json, yaml, http, etc.)

### Local Nicknames

Epsilon packages are designed to be used with local nicknames for cleaner code:

```lisp
(defpackage #:my-app
  (:use #:cl)
  (:local-nicknames
    (#:map #:epsilon.map)
    (#:str #:epsilon.string)
    (#:fs #:epsilon.sys.fs)))
```

### Function Naming Conventions

Epsilon follows consistent naming conventions across all packages:

- **Predicates** end with `-p`: `empty-p`, `directory-p`
- **Constructors** use `make-`: `make-map`, `make-path`
- **Converters** use `from-`/`to-`: `from-list`, `to-string`
- **Destructive operations** end with `!`: `delete!`, `update!`
- **Internal functions** start with `%`: `%validate-input`

### Error Handling

All Epsilon APIs use a consistent error handling approach:

1. **Conditions** - Each package defines specific condition types
2. **Restarts** - Where appropriate, restarts are provided
3. **Defaults** - Many functions accept default values for missing data
4. **Validation** - Input validation with clear error messages

### Performance Notes

API documentation includes performance characteristics where relevant:

- **Time Complexity** - Big-O notation for operations
- **Space Complexity** - Memory usage patterns
- **Thread Safety** - Concurrency considerations
- **Allocation** - Notes on memory allocation

## Contributing

To contribute to the API documentation:

1. Documentation is generated from source code docstrings
2. Follow the existing format and style
3. Include examples for non-trivial functions
4. Document edge cases and error conditions
5. Add performance notes where relevant

## Getting Help

- **Examples** - See the [Examples](../development/examples/) section
- **Tutorials** - Check out the [Tutorials](../tutorials/) section
- **Community** - Join the Epsilon community forums
- **Issues** - Report documentation issues on GitHub
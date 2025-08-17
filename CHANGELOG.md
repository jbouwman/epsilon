# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.11.0] - 2025-08-17

### Overview
Initial release of Epsilon, a Lisp programming environment built on SBCL.

### Main Features

#### Modules
- Modular architecture with dependency resolution
- Incremental compilation 
- Platform-specific module support (Darwin, Linux)

#### Functional Data Structures
- Persistent maps
- Immutable sets and sequences
- Transducers

#### Encoding
- JSON encoding/decoding with streaming support
- YAML support for configuration files
- EDN (Extensible Data Notation) support
- Base64 encoding/decoding
- URL encoding utilities

#### Cryptographic Hash Functions
- SHA-1, SHA-256, Keccak

#### Network Programming
- HTTP client and server

#### Testing Framework
- Test fixtures and lifecycle management

### Platform Support
- Linux (x86_64)
- macOS (arm64)

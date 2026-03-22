# Changelog

Notable changes to Epsilon are documented in this file.

## [Unreleased]

## [0.16.0]

- Refocus of bootstrapped runtime on module loading

## [0.15.0] - 2026-02-25

### Added
- INSTALL.md installation guide
- CHANGELOG.md (this file)
- Module READMEs for core, http, json, crypto, and test modules

### Changed
- Ejected devtools modules (lint, server, benchmark) into `epsilon-contrib/` (originally `epsilon-devtools/`, later merged back)
- Consolidated `epsilon.digest` and `epsilon.html` into `epsilon.crypto` and `epsilon.markup` respectively
- Cleaned workspace.plist: removed phantom entries for non-existent modules
- Updated `epsilon.pkg` registry URL and fixed hardcoded timestamp
- Replaced Unicode status symbols with ASCII equivalents for terminal portability
- Updated HTTP module description to accurately reflect HTTP/3 as experimental

### Removed
- Ghost module directories (`modules/digest/`, `modules/html/`) containing only stale build artifacts
- Obsolete disabled test files from HTTP module
- Phantom workspace entries for never-created modules (foreign-static-call, foreign-jit, enum, cli, nx, loader)
- Windows platform excluded from release (stub only)

## [0.14.0] - 2025-01-25

### Added
- **epsilon-mode for Emacs** (v0.3.0)
  - MELPA-ready package with proper headers and license
  - Comint-based REPL with command history
  - Company completion integration
  - Eldoc integration for function signatures
  - Flymake integration via epsilon.lint
  - Auto-detection of epsilon installation

### Changed
- Improved REPL experience with better completion and documentation

## [0.13.0] - 2025-01-15

### Added
- **Project initialization**: `epsilon --init` creates project structure
- **Module scaffolding**: `epsilon --new NAME` with templates (basic, library, cli)
- Enhanced help system with categorized command sections

### Changed
- Reorganized CLI commands for better discoverability

## [0.12.0] - 2025-01-05

### Added
- **GitHub Actions CI workflow** for automated testing
- **GitHub Actions release workflow** for binary releases
- **Enhanced install script** with new options:
  - `--version` to install specific versions
  - `--nightly` for nightly builds
  - `--check` to verify installation
  - `--uninstall` to remove installation
  - `--completions` for shell completion scripts
- **Self-update mechanism**: `epsilon update` command
- **Shell completions** for bash, zsh, and fish
- **Improved version display**: `--version-json` for machine-readable output
- Issue and PR templates for GitHub

### Changed
- Version information now includes build metadata
- Installation paths are configurable via environment variables

## [0.11.0] - 2024-12-15

### Added
- **epsilon.http 2.0**: Unified HTTP client and server
  - HTTP/1.1 and HTTP/2 support
  - Connection pooling with keep-alive
  - TLS/SSL with certificate validation
  - Streaming request/response bodies
  - Retry policies with exponential backoff
  - Middleware support
- **epsilon.crypto 2.0**: Cryptographic operations
  - TLS/SSL via OpenSSL FFI
  - X.509 certificate handling
  - Key generation (RSA, EC)
  - Certificate signing and verification
- **Platform abstraction layer**
  - Interface-based design for OS-specific code
  - macOS support via kqueue
  - Linux support via epoll

### Changed
- Restructured module system for cleaner dependencies
- Improved error handling with custom condition types

## [0.10.0] - 2024-11-01

### Added
- **epsilon.test framework**
  - Property-based testing (QuickCheck-style)
  - Snapshot testing (Jest/Insta-style)
  - Parallel test execution
  - Process isolation for crash-prone tests
  - Benchmarking with statistical analysis
  - Mock/spy/stub support
- **epsilon.foreign**: C FFI with libclang integration
- **epsilon.lint**: Static analysis module

### Changed
- Test output format improved for readability
- Module loading performance optimized

## [0.9.0] - 2024-09-15

### Added
- **Functional data structures**
  - Persistent maps (HAMT)
  - Persistent sets
  - Persistent vectors
- **epsilon.json**: JSON parser and encoder
- **epsilon.regex**: Regular expressions (Thompson NFA)
- **epsilon.io**: Stream composition and binary encoding

### Changed
- Core module reorganized for better modularity

## [0.8.0] - 2024-07-01

### Added
- Initial module system with semantic versioning
- Package macro for module definition
- Dependency resolution and loading
- Basic CLI framework

### Changed
- Project restructured from monolithic to modular architecture

---

## Version History Summary

| Version | Date | Highlights |
|---------|------|------------|
| 0.15.0 | 2026-02 | Release prep, module consolidation, workspace cleanup |
| 0.14.0 | 2025-01 | Emacs integration, epsilon-mode |
| 0.12.0 | 2025-01 | CI/CD, installer, self-update |
| 0.11.0 | 2024-12 | HTTP 2.0, Crypto 2.0, platform layer |
| 0.10.0 | 2024-11 | Test framework, FFI, linting |
| 0.9.0 | 2024-09 | Functional data structures, JSON |
| 0.8.0 | 2024-07 | Module system, package macro |

[Unreleased]: https://github.com/jbouwman/epsilon/compare/v0.15.0...HEAD
[0.15.0]: https://github.com/jbouwman/epsilon/compare/v0.14.0...v0.15.0
[0.14.0]: https://github.com/jbouwman/epsilon/compare/v0.13.0...v0.14.0
[0.13.0]: https://github.com/jbouwman/epsilon/compare/v0.12.0...v0.13.0
[0.12.0]: https://github.com/jbouwman/epsilon/compare/v0.11.0...v0.12.0
[0.11.0]: https://github.com/jbouwman/epsilon/compare/v0.10.0...v0.11.0
[0.10.0]: https://github.com/jbouwman/epsilon/compare/v0.9.0...v0.10.0
[0.9.0]: https://github.com/jbouwman/epsilon/compare/v0.8.0...v0.9.0
[0.8.0]: https://github.com/jbouwman/epsilon/releases/tag/v0.8.0

# Epsilon

![Under Construction](construction.gif)

[![CI - Multi-Platform Build](https://github.com/jbouwman/epsilon/actions/workflows/ci.yml/badge.svg)](https://github.com/jbouwman/epsilon/actions/workflows/ci.yml)
[![Validate - Quick Build Check](https://github.com/jbouwman/epsilon/actions/workflows/validate.yml/badge.svg)](https://github.com/jbouwman/epsilon/actions/workflows/validate.yml)

Epsilon is a library for SBCL that provides data structures, data formats, compression, cryptography and networking.

## Quick Start

```bash
./run.sh build
```

Run tests:

```bash
./run.sh test
```

## Packages

```
epsilon (platform-specific)
├── epsilon.core (shared)
│   ├── epsilon.lib.*     # Functional data structures, codecs, crypto
│   ├── epsilon.sys.*     # System utilities, FFI, threading
│   └── epsilon.tool.*    # Build system, testing, formatting
└── Platform-specific networking:
    ├── Linux:   epsilon.sys.epoll  → epsilon.net
    ├── Darwin:  epsilon.sys.kqueue → epsilon.net
    └── Windows: epsilon.sys.iocp   → epsilon.net
```

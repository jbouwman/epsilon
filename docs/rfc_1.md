# RFC 1: Refocus Epsilon to a Minimal Secure Package Repository Core

**Status:** Proposed
**Author:** Epsilon Team
**Created:** 2025-12-09

## Abstract

This RFC proposes refactoring Epsilon from a general-purpose Lisp environment into a focused, minimal core optimized for running a modern, secure networked Lisp package repository. Non-essential modules will be migrated to a separate `epsilon-contrib` repository, reducing the core footprint while maintaining a thriving ecosystem.

## Motivation

Epsilon has grown to include 24+ modules with approximately 55,000 lines of code spanning data analysis, web frameworks, multiple data formats, and various utilities. While comprehensive, this breadth:

1. **Increases attack surface** for a security-critical package registry
2. **Complicates maintenance** with interdependencies across unrelated domains
3. **Slows development** of core registry functionality
4. **Obscures purpose** - unclear what Epsilon's primary mission is

A package repository requires a specific, well-defined set of capabilities. By focusing on these essentials, we can:

- Harden security with a smaller, auditable codebase
- Accelerate development of registry features
- Establish clear architectural boundaries
- Enable the community to maintain extended functionality independently

## Design Overview

### Repository Structure

```
epsilon/                     # Core repository (this repo)
├── modules/
│   ├── core/               # Foundation (streamlined)
│   ├── parsing/            # Parser combinators
│   ├── json/               # JSON encoding (API format)
│   ├── digest/             # SHA-256, BLAKE2 (integrity)
│   ├── crypto/             # TLS, X.509, signatures
│   ├── library/            # Shared library management
│   ├── foreign/            # FFI for OpenSSL
│   ├── io/                 # Async I/O
│   ├── http/               # HTTP client/server
│   ├── linux/              # Linux async networking
│   ├── darwin/             # macOS async networking
│   ├── windows/            # Windows async networking
│   ├── loader/             # Module system
│   ├── test/               # Testing framework
│   └── registry/           # NEW: Package registry service
├── docs/
├── scripts/
└── ...

epsilon-contrib/             # Extended modules (new repository)
├── modules/
│   ├── xml/
│   ├── yaml/
│   ├── regex/
│   ├── frame/
│   ├── sql/
│   ├── http2/
│   ├── websocket/
│   ├── web/
│   ├── benchmark/
│   └── release/
└── ...
```

### Core Module Classification

#### Tier 1: Essential (Remain in epsilon/)

These modules are **required** for a secure networked package repository:

| Module | Purpose | Justification |
|--------|---------|---------------|
| `epsilon.core` | Foundation | Data structures, streams, paths, logging |
| `epsilon.parsing` | Parser combinators | Required by JSON module |
| `epsilon.json` | JSON encoding | Registry API format |
| `epsilon.digest` | Cryptographic hashes | Package integrity verification (SHA-256) |
| `epsilon.crypto` | TLS/mTLS, X.509 | Secure connections, certificate auth |
| `epsilon.library` | Shared library mgmt | OpenSSL integration |
| `epsilon.foreign` | FFI | OpenSSL bindings |
| `epsilon.io` | Async I/O | Efficient network operations |
| `epsilon.http` | HTTP client/server | Registry API endpoints |
| `epsilon.linux` | Linux networking | Provides `epsilon.net`, `epsilon.async` |
| `epsilon.darwin` | macOS networking | Provides `epsilon.net`, `epsilon.async` |
| `epsilon.windows` | Windows networking | Provides `epsilon.net`, `epsilon.async` |
| `epsilon.loader` | Module system | Package loading infrastructure |
| `epsilon.test` | Testing | **Modified** - remove xml/regex deps |

#### Tier 2: Extended (Move to epsilon-contrib/)

These modules are valuable but not essential for a package repository:

| Module | Current Purpose | Migration Notes |
|--------|-----------------|-----------------|
| `epsilon.xml` | XML parsing | Legacy format, not needed for modern APIs |
| `epsilon.yaml` | YAML parsing | Config alternative, JSON suffices |
| `epsilon.regex` | Regular expressions | General text processing |
| `epsilon.frame` | Columnar data | Data analysis, not package mgmt |
| `epsilon.sql` | Database interface | Registry can use direct SQLite FFI |
| `epsilon.http2` | HTTP/2 protocol | Nice-to-have, HTTP/1.1 sufficient |
| `epsilon.websocket` | WebSocket protocol | Real-time features not critical |
| `epsilon.web` | Web framework | High-level abstraction, HTTP suffices |
| `epsilon.benchmark` | Performance testing | Development tooling |
| `epsilon.release` | Release management | Build tooling |

## Detailed Technical Plan

### Phase 1: Dependency Decoupling

**Goal:** Enable clean separation of core from contrib modules.

#### 1.1 Modify epsilon.test

Current dependencies: `epsilon.regex`, `epsilon.xml`, `epsilon.digest`

The test framework currently depends on regex (for test filtering) and xml (for JUnit output). These need to be made optional or replaced:

```lisp
;; Before: Hard dependency
(:requires ("epsilon.regex" "epsilon.xml" "epsilon.digest"))

;; After: Minimal dependencies
(:requires ("epsilon.digest"))
(:optional ("epsilon.regex" "epsilon.xml"))
```

**Changes required:**
- Make JUnit XML output optional (only when epsilon.xml is loaded)
- Replace regex-based test filtering with simple string matching
- Keep digest for test result checksums

#### 1.2 Audit epsilon.core

The core module should be reviewed for functionality that belongs elsewhere:

**Keep in core:**
- HAMT maps, sets, sequences, lists, arrays
- String operations, binary handling
- Path, URL, stream abstractions
- Logging, argument parsing
- Module/compilation system
- Process execution
- Threading, locking primitives

**Consider moving from core:**
- EDN encoding (evaluate if JSON alone suffices)
- Any regex-dependent utilities
- Non-essential format conversions

#### 1.3 Create Compatibility Layer

For modules that move to contrib, provide graceful degradation:

```lisp
;; In epsilon.core or a thin shim
(defun ensure-module (name &key optional)
  "Load a module, optionally failing gracefully."
  (handler-case
      (load-module name)
    (module-not-found (c)
      (if optional
          (warn "Optional module ~A not available" name)
          (error c)))))
```

### Phase 2: Repository Separation

#### 2.1 Create epsilon-contrib Repository

```bash
# Initialize new repository
git init epsilon-contrib
cd epsilon-contrib

# Set up structure
mkdir -p modules docs scripts

# Create module definitions that reference epsilon as dependency
```

#### 2.2 Module Migration

For each contrib module:

1. **Copy module directory** to epsilon-contrib
2. **Update module.lisp** to declare epsilon dependency:
   ```lisp
   (:name "epsilon-contrib.xml"
    :version "1.0.0"
    :requires ("epsilon.regex")  ; from main epsilon
    :epsilon-version ">=2.0.0")
   ```
3. **Update package prefixes** if needed
4. **Add to contrib registry** for discoverability
5. **Remove from epsilon** main repository

#### 2.3 Migration Order

Based on dependency analysis, migrate in this order (fewest dependents first):

1. `epsilon.frame` - No dependents
2. `epsilon.benchmark` - No dependents in core
3. `epsilon.yaml` - No dependents
4. `epsilon.sql` - No dependents in core
5. `epsilon.http2` - Depends on http, no core dependents
6. `epsilon.websocket` - Depends on http, digest
7. `epsilon.web` - Depends on http, websocket, json
8. `epsilon.release` - Depends on test, digest
9. `epsilon.xml` - Required by test (make optional first)
10. `epsilon.regex` - Required by test, xml (make optional first)

### Phase 3: Core Hardening

#### 3.1 Security Audit

With a smaller codebase, conduct thorough security review:

- **Input validation** on all HTTP endpoints
- **TLS configuration** - enforce TLS 1.3, strong cipher suites
- **Certificate handling** - proper chain validation
- **Memory safety** - review FFI boundaries
- **Authentication** - token handling, timing attacks
- **Rate limiting** - protect against abuse

#### 3.2 Streamline epsilon.core

Review and potentially remove from core:

| Component | Decision | Rationale |
|-----------|----------|-----------|
| EDN encoding | Keep | Useful for Lisp configs, small footprint |
| Base64/Hex | Keep | Required for crypto operations |
| Transducers | Keep | Core functional programming |
| Threading macros | Keep | Essential ergonomics |
| Format utilities | Review | May be over-featured |

#### 3.3 New epsilon.registry Module

Create a dedicated module for package registry functionality:

```lisp
(:name "epsilon.registry"
 :version "1.0.0"
 :description "Secure package registry service"
 :requires ("epsilon.http"
            "epsilon.json"
            "epsilon.crypto"
            "epsilon.digest")
 :sources ("src/api.lisp"          ; REST API handlers
           "src/storage.lisp"       ; Package storage
           "src/auth.lisp"          ; Authentication
           "src/integrity.lisp"     ; Checksum verification
           "src/resolution.lisp"    ; Dependency resolution
           "src/publish.lisp"       ; Package publishing
           "src/search.lisp"))      ; Package discovery
```

### Phase 4: API Finalization

#### 4.1 Registry API Endpoints

Implement the API specified in `docs/development/registry-specification.md`:

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/api/v1/packages` | GET | List/search packages |
| `/api/v1/packages` | POST | Publish new package |
| `/api/v1/packages/:name` | GET | Package metadata |
| `/api/v1/packages/:name/versions` | GET | Version list |
| `/api/v1/packages/:name/:version` | GET | Version details |
| `/api/v1/packages/:name/:version/archive` | GET | Download package |

#### 4.2 Security Headers

All responses include:

```lisp
(def +security-headers+
  '(("Strict-Transport-Security" . "max-age=31536000; includeSubDomains")
    ("X-Content-Type-Options" . "nosniff")
    ("X-Frame-Options" . "DENY")
    ("Content-Security-Policy" . "default-src 'none'")
    ("X-XSS-Protection" . "1; mode=block")))
```

#### 4.3 Authentication Model

- **Read operations**: Public (no auth required)
- **Write operations**: Bearer token authentication
- **Admin operations**: mTLS client certificates

### Phase 5: Testing and Validation

#### 5.1 Core Test Coverage

Ensure comprehensive testing of:
- All HTTP endpoints
- TLS handshake scenarios
- Package integrity verification
- Authentication flows
- Error handling paths

#### 5.2 Security Testing

- Penetration testing of API endpoints
- Fuzzing of JSON parser
- TLS configuration validation
- Certificate validation testing

#### 5.3 Performance Baseline

Establish benchmarks for:
- Package upload throughput
- Package download latency
- Search query response time
- Concurrent connection handling

## Dependency Graph: Before and After

### Before (Current State)

```
                          epsilon.core
                               │
        ┌──────────┬──────────┼──────────┬──────────┐
        │          │          │          │          │
   parsing     regex      library    digest     frame
        │          │          │          │
        ├────┐     │          │          │
        │    │     │          ├──────────┤
      json  foreign├──────────┤          │
             │     │          │          │
             │   xml        crypto      test ─────┤
             │                │          │        │
     ┌───────┴───────┐        │          │        │
     │               │        │          │        │
   linux          darwin      │       release     │
     │               │        │                   │
     └───────┬───────┘        │                   │
             │                │                   │
            net ──────────────┤                   │
             │                │                   │
          ┌──┴──┐             │                   │
          │     │             │                   │
         io    http ──────────┘                   │
                │                                 │
         ┌──────┼──────┐                          │
         │      │      │                          │
      http2  websocket │                          │
                │      │                          │
                └──┬───┘                          │
                   │                              │
                  web                             │
                                                  │
   yaml    sql    benchmark ──────────────────────┘
```

### After (Proposed State)

**epsilon/ (core)**
```
                          epsilon.core
                               │
        ┌──────────┬──────────┼──────────┐
        │          │          │          │
   parsing      library    digest      test (simplified)
        │          │          │
        │          │          │
      json      foreign      │
                   │         │
     ┌─────────────┤         │
     │             │         │
   linux        darwin       │
     │             │         │
     └──────┬──────┘         │
            │                │
           net               │
            │                │
         ┌──┴──┐             │
         │     │             │
        io    http           │
               │             │
               └─────────────┤
                             │
                         registry (NEW)
                             │
                    (crypto + digest + http + json)
```

**epsilon-contrib/**
```
   epsilon (external dependency)
            │
   ┌────────┼────────┬────────┬────────┐
   │        │        │        │        │
 regex    xml      yaml     frame    sql
   │        │
   │        │
   └────┬───┘
        │
   ┌────┴────┬────────┐
   │         │        │
 http2  websocket  benchmark
   │         │
   └────┬────┘
        │
       web
        │
     release
```

## Size Impact Analysis

### Current State
- **Total modules**: 24
- **Total source files**: 340+
- **Estimated lines of code**: ~55,000

### Proposed Core
- **Core modules**: 14 (including new registry module)
- **Estimated source files**: ~150
- **Estimated lines of code**: ~25,000

### Reduction
- **Modules removed from core**: 10 (42% reduction)
- **Code reduction**: ~30,000 lines (55% reduction)
- **Attack surface reduction**: Significant (fewer parsers, less complexity)

## Migration Checklist

### Pre-Migration
- [ ] Create epsilon-contrib repository
- [ ] Set up CI/CD for epsilon-contrib
- [ ] Document contribution guidelines for contrib
- [ ] Announce deprecation timeline to community

### Phase 1: Decoupling
- [ ] Make epsilon.test XML output optional
- [ ] Replace regex test filtering with string matching
- [ ] Audit epsilon.core for removable components
- [ ] Create module compatibility shims

### Phase 2: Separation
- [ ] Migrate epsilon.frame
- [ ] Migrate epsilon.benchmark
- [ ] Migrate epsilon.yaml
- [ ] Migrate epsilon.sql
- [ ] Migrate epsilon.http2
- [ ] Migrate epsilon.websocket
- [ ] Migrate epsilon.web
- [ ] Migrate epsilon.release
- [ ] Migrate epsilon.xml
- [ ] Migrate epsilon.regex

### Phase 3: Hardening
- [ ] Security audit of core modules
- [ ] Streamline epsilon.core
- [ ] Implement epsilon.registry module
- [ ] Configure TLS 1.3 enforcement
- [ ] Implement rate limiting

### Phase 4: API
- [ ] Implement all registry API endpoints
- [ ] Add security headers middleware
- [ ] Implement authentication system
- [ ] Add request validation

### Phase 5: Validation
- [ ] Achieve 90%+ test coverage on core
- [ ] Complete security penetration testing
- [ ] Establish performance baselines
- [ ] Document API for consumers

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Breaking existing users | High | Semantic versioning, deprecation warnings, migration guide |
| Contrib modules become unmaintained | Medium | Establish maintainer program, clear ownership |
| Hidden dependencies discovered late | Medium | Thorough testing before each migration step |
| Performance regression in core | Medium | Benchmark before/after each phase |
| Security vulnerability in contrib | Low | Contrib is not security-critical path |

## Success Criteria

1. **Core repository** contains only modules necessary for package registry
2. **All tests pass** after migration
3. **No functionality loss** - contrib modules work when installed
4. **Security audit** passes with no critical findings
5. **Performance** meets or exceeds current baselines
6. **Documentation** updated for new architecture

## Future Considerations

### Package Repository Features
Once the core is focused, prioritize:
- Package signing with GPG/minisign
- Namespace/organization support
- Webhook notifications
- Usage analytics
- CDN integration for package distribution

### Contrib Ecosystem
- Establish contrib package as first-party in registry
- Create "blessed" contrib bundles for common use cases
- Community maintainer program

## References

- [Package Registry Specification](development/registry-specification.md)
- [Project 6: Package Registry](projects/6_package_registry.md)
- [Module Catalog](architecture/module-catalog.md)
- [Module Management](architecture/module-management.md)

## Appendix A: Module Dependency Matrix

| Module | Layer | Dependencies | Dependents |
|--------|-------|--------------|------------|
| epsilon.core | 0 | - | All modules |
| epsilon.parsing | 0 | - | json, foreign |
| epsilon.regex | 0 | - | xml, test |
| epsilon.library | 0 | - | foreign, crypto, sql |
| epsilon.digest | 0 | - | test, websocket, release, registry |
| epsilon.frame | 0 | - | - |
| epsilon.yaml | 0 | - | - |
| epsilon.json | 1 | parsing | http, web, registry |
| epsilon.xml | 1 | regex | test |
| epsilon.foreign | 1 | parsing, library | crypto, sql, linux, darwin |
| epsilon.benchmark | 1 | core | - |
| epsilon.linux | 2 | foreign | (provides net, async) |
| epsilon.darwin | 2 | foreign | (provides net, async) |
| epsilon.windows | 2 | - | (provides net, async) |
| epsilon.crypto | 3 | foreign, net, library | http, http2, registry |
| epsilon.io | 3 | core, async | - |
| epsilon.sql | 3 | foreign, library | - |
| epsilon.http | 4 | core, json, crypto, net | http2, web, websocket, registry |
| epsilon.http2 | 4 | core, net, crypto, http | - |
| epsilon.websocket | 4 | core, digest, http | web |
| epsilon.test | 4 | regex*, xml*, digest | release |
| epsilon.web | 5 | core, http, json, websocket | - |
| epsilon.release | 5 | digest, test | - |
| epsilon.registry | 5 | http, json, crypto, digest | - |

*\* = dependency to be made optional*

## Appendix B: Core Module Line Counts

Approximate sizes to understand refactoring scope:

| Module | Files | ~Lines | Notes |
|--------|-------|--------|-------|
| epsilon.core | 40+ | 15,000 | Needs audit for reduction |
| epsilon.parsing | 5 | 800 | Stable, minimal |
| epsilon.json | 1 | 400 | Stable, minimal |
| epsilon.digest | 9 | 1,200 | Pure Lisp implementations |
| epsilon.crypto | 13 | 2,000 | OpenSSL bindings |
| epsilon.library | 3 | 200 | Minimal |
| epsilon.foreign | 15 | 3,000 | C parser, FFI |
| epsilon.io | 5 | 600 | Async streams |
| epsilon.http | 15 | 2,500 | Client + server |
| epsilon.linux | 8 | 1,500 | epoll, networking |
| epsilon.darwin | 6 | 1,200 | kqueue, networking |
| epsilon.windows | 8 | 1,500 | IOCP, networking |
| epsilon.loader | 4 | 500 | Module loading |
| epsilon.test | 8 | 1,000 | After simplification |
| epsilon.registry | - | ~2,000 | New module |
| **Total** | ~140 | ~33,400 | |

# Stability

This document describes Epsilon 1.0's stability commitments: which
parts of the system carry compatibility guarantees, which are still
evolving, and what kinds of changes can be expected at each
post-1.0 version bump.

## Tiers

Each module carries one of three stability tiers. The tier is
listed below and recorded in the module's `module.sexp` under
`:stability` once the 1.0 release ships.

- **stable** — Public API is committed. After 1.0 a backward-incompatible
  change to an exported symbol's signature, semantics, or structure
  requires a major version bump and a deprecation cycle (one minor
  release of warnings before removal).
- **experimental** — The module is part of 1.0 because it is in
  production use, but the API is wide and not yet load-bearing-frozen.
  Backward-incompatible changes can land in minor releases with a
  changelog note; they will not happen in patch releases.
- **internal** — Used by the runtime; not part of the public surface.
  Importing internal packages from outside Epsilon's source tree is
  not supported.

## Stable modules (1.0)

The following 36 modules are committed surface. `epsilon test
<module>` runs each one's tests; CI runs the full set.

| Module | Description |
|---|---|
| `epsilon.annotate` | `#@`-style annotation reader macro and runtime. |
| `epsilon.channel` | Buffered channels for cooperative scheduling. |
| `epsilon.compiler` | Compile driver with content-addressed FASL caching. |
| `epsilon.compression` | Gzip, deflate, raw inflate. Wraps zlib. |
| `epsilon.crypto` | TLS primitives: hashes, AEAD, X.509 parsing, key formats. |
| `epsilon.darwin` | Darwin platform shim (kqueue, async IO). |
| `epsilon.diagnostic` | Ambient structured-diagnostic facility. |
| `epsilon.encode` | Base16/32/64, bech32, Z-base32. |
| `epsilon.foreign` | FFI surface, libclang-driven binding generator. *(experimental — see below)* |
| `epsilon.fs` | Filesystem operations and watch. |
| `epsilon.http` | HTTP/1.1 and embedded HTTP/2: client, server, h2 frames. |
| `epsilon.io` | Byte streams, buffers, framing. |
| `epsilon.json` | JSON parser and encoder. |
| `epsilon.library` | Shared-library loading and symbol resolution. |
| `epsilon.linux` | Linux platform shim (epoll, async IO). |
| `epsilon.log` | Ambient structured logging. |
| `epsilon.markup` | Hiccup-style HTML and SVG generation. |
| `epsilon.parse` | Parser-combinator and lexer toolkit. |
| `epsilon.pool` | Generic resource pool. |
| `epsilon.process` | Subprocess spawning and lifecycle. |
| `epsilon.reader` | Extended Lisp reader (`#@`, literal map/vector/set syntax). |
| `epsilon.scheduler` | M:N cooperative scheduler over `sb-fiber`. |
| `epsilon.sql` | PostgreSQL client with pool. |
| `epsilon.ssh` | SSH-2 (RFC 4251–4256). |
| `epsilon.crypto` | TLS 1.3 implementation. |
| `epsilon.stacktrace` | Backtrace capture and rendering. |
| `epsilon.test` | Test framework (`deftest`, assertions, runner). |
| `epsilon.time` | Time and duration types. |
| `epsilon.url` | URL parsing and composition. |
| `epsilon.uuid` | UUID v4/v7 generation and parsing. |
| `epsilon.web` | Routing, interceptors, LiveView, CSS, Hiccup-HTML. |
| `epsilon.websocket` | RFC 6455 framing. |
| `epsilon.windows` | Windows platform shim (currently minimal). |

## Stable but contingent on `epsilon.commands` (resolved)

`epsilon.build` and `epsilon.test` previously depended on
`epsilon.commands`, the bootstrap CLI parser. As of 1.0 that package
has been renamed `epsilon.commands` and lives in the runtime. Both
modules are now self-contained and ship as stable.

## Experimental (1.0)

- **`epsilon.foreign`** — The FFI surface and libclang-driven binding
  generator are in production use, but the API is wide (~22 source
  files) and likely to evolve. Specifically: the JIT layer, the
  binding-IR shape, and the auto-binding entry points may change
  shape in 1.x without breaking the simple FFI primitives. A 1.x
  release that narrows or stabilises this surface will move it to
  the stable tier.

## Internal (not part of the 1.0 surface)

The runtime that lives under `epsilon/src/` (loader, bootstrap,
extended `defpackage`, `epsilon.commands`, FASL key derivation) is
internal. Symbols there are accessed via the documented module APIs
(`epsilon.loader`, `epsilon.compiler`, etc.). Direct imports from
`epsilon.X` packages defined inside `epsilon/src/` are not supported
and may change without notice.

## What 1.0 deliberately omits

- **Coexistence with ASDF/Quicklisp libraries.** Existing ASDF
  systems are not loadable through the Epsilon module system without
  modification. A bridge that lets ASDF systems appear as Epsilon
  modules is on the roadmap.
- **Implementation portability beyond SBCL.** The FASL key
  derivation, the FFI primitives, and the cooperative scheduler all
  target SBCL specifically. CCL or ECL support is not committed for
  any 1.x release.
- **Distributed builds.** The Nix integration produces per-module
  derivations, and a single-host subprocess pool handles parallel
  compilation. Sharing those derivations across machines (Bazel-style
  remote caching) is on the roadmap but not in 1.0.
- **Module hot-reload.** Modules can be recompiled and loaded into a
  running image, but there is no committed protocol for the system
  to coordinate state migration on top of that.

## Versioning

Epsilon follows [Semantic Versioning 2.0](https://semver.org/):

- **Major (X.0.0)** — Breaking changes to stable API (signatures
  removed, semantics changed, exported symbols renamed). Stable
  modules will see at most one minor release of deprecation
  warnings before a major change lands.
- **Minor (1.X.0)** — New stable APIs, new modules, behaviour
  changes inside the experimental tier.
- **Patch (1.0.X)** — Bug fixes that don't change the API.

The FASL key version (`epsilon-fasl-v3` at 1.0) bumps with any
change to how content keys are derived. A FASL key bump invalidates
prior build caches; it always rides a major or minor release, never
a patch.

## What "API" means

For a stable module `epsilon.X`, the committed surface is:

- The names of all exported symbols.
- The arity, keyword arguments, and return-shape contract of every
  exported function.
- The slot layout and accessor names of every exported struct or
  class.
- The expansion shape of every exported macro (the form a caller's
  source compiles into is observable through tools like
  `macroexpand`).

What is **not** part of the API:

- Internal symbols (those not in the package's `:export` list).
- Performance characteristics, memory footprint, or thread-safety
  guarantees not explicitly documented in the module's own docs.
- The exact text of error conditions (the condition class is part of
  the API; the message string is not).
- The on-disk format of build artifacts (`.fasl`, `.key`).

## Reporting incompatibilities

If a 1.x release breaks code that relied only on the documented stable
surface, that is a bug: file an issue at
[github.com/jbouwman/epsilon](https://github.com/jbouwman/epsilon).

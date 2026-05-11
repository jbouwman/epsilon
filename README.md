# Epsilon

[![CI - Multi-Platform Build](https://github.com/jbouwman/epsilon/actions/workflows/ci.yml/badge.svg)](https://github.com/jbouwman/epsilon/actions/workflows/ci.yml)

Epsilon is a Common Lisp module and build system for SBCL.

Epsilon ships a module declaration system, a runtime, a content-addressed build,
and a compact standard library sufficient to load further modules.

```bash
./epsilon repl
epsilon> (+ 1 2 3)
6
```

See `epsilon commands` for a list of commands.

The standard library provides HAMT-backed persistent maps/sets, HTTP, JSON, SQL,
MessagePack, WebSocket, regex, time, UUID, base64, compression, TLS/X.509, an
FFI with libclang-driven binding generation, an `sb-fiber` M:N coroutine
scheduler, and a content-addressed build pool.

## Documentation

- [`docs/MODULE.md`](docs/MODULE.md) — manifest reference and module conventions
- [`demos/`](demos/) — worked examples
- [`CHANGELOG.md`](CHANGELOG.md) — release notes

## License

MIT — see [LICENSE](LICENSE).

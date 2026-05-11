# HTTP Tests

## Current Status

### Working Tests
- `basic-tests.lisp` - Basic HTTP functionality tests (17/18 passing)
- `http-tests.lisp` - HTTP protocol tests (all passing)

### Known Issues
1. `test-extract-content-length` - One test case fails with "expected NIL but got 100". This appears to be a test isolation issue rather than a functional problem.

## Migration to Web Package

For new tests involving HTTP handlers and routing, use the `epsilon.web` package (in `epsilon-contrib`) which provides:
- `defhandler` - Define request handlers with error handling
- `defroutes` - Define route tables
- Higher-level response helpers (json, html, text, redirect)

See the `epsilon-contrib/modules/web/tests/` directory for examples of modern test patterns.

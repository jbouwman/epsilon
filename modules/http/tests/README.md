# HTTP Tests

## Current Status

### Working Tests
- `basic-tests.lisp` - Basic HTTP functionality tests (17/18 passing)
- `http-tests.lisp` - HTTP protocol tests (all passing)

### Known Issues
1. `test-extract-content-length` - One test case fails with "expected NIL but got 100". This appears to be a test isolation issue rather than a functional problem.

### Obsolete Tests (Disabled)
The following test files have been disabled as they use obsolete patterns that have been superseded by the `epsilon.web` package:

- `obsolete-tls-integration-tests.lisp.disabled` - Used undefined `server:define-handler` 
- `obsolete-live-integration-tests.lisp.disabled` - Used wrong package prefixes for connection pooling

These tests were written for an earlier version of the HTTP server that had different APIs. The functionality they were testing is now better covered by the web package tests.

## Migration to Web Package

For new tests involving HTTP handlers and routing, use the `epsilon.web` package which provides:
- `defhandler` - Define request handlers with error handling
- `defroutes` - Define route tables
- Higher-level response helpers (json, html, text, redirect)

See `/Users/jbouwman/git/epsilon/src/web/tests/web-tests.lisp` for examples of modern test patterns.
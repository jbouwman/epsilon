# HTTP/2 Implementation Documentation

## Overview
This directory contains documentation for the Epsilon HTTP/2 implementation, including mTLS support, HPACK compression, and protocol compliance testing.

## Contents

### Implementation Status
- [Progress Report](progress.md) - Current implementation status and test results
- [h2spec Results](h2spec-results.txt) - Latest conformance test output
- [Full h2spec Results](h2spec-full.txt) - Detailed test results

### Design Documents
- [HPACK Options](hpack-options.md) - Analysis of HPACK compression library options
- [Connection Pooling](CONNECTION_POOLING_AND_STREAMING.md) - Connection management design

### mTLS Documentation
- [mTLS Status Report](MTLS_STATUS_REPORT.md) - Initial mTLS implementation status
- [mTLS Coverage](MTLS_HTTP2_COVERAGE.md) - Test coverage for mTLS features
- [mTLS Final Status](MTLS_HTTP2_FINAL_STATUS.md) - Final mTLS implementation status
- [mTLS Complete Summary](MTLS_HTTP2_COMPLETE_SUMMARY.md) - Complete mTLS summary

## Quick Start

### Running Tests

```bash
# Run unit tests
./epsilon --module epsilon.linux --module epsilon.crypto --module epsilon.http2 \
          --test epsilon.http2

# Run h2spec conformance tests
./epsilon --module epsilon.linux --module epsilon.crypto \
          --load modules/http2/test/h2spec-server.lisp

# In another terminal
docker run --rm --network host summerwind/h2spec -p 8080
```

### Module Structure

```
modules/http2/
├── module.lisp          # Module definition
├── src/
│   ├── frames.lisp      # Frame handling
│   ├── hpack-simple.lisp # HPACK compression
│   ├── http2.lisp       # Main protocol implementation
│   └── server.lisp      # Server implementation
└── test/
    ├── test-frames.lisp  # Frame tests
    ├── test-hpack.lisp   # HPACK tests
    ├── test-http2.lisp   # Protocol tests
    ├── h2spec-server.lisp # h2spec test server
    └── full-server.lisp  # Complete HTTP/2 server

modules/crypto/test/
├── test-mtls-simple.lisp # Simple mTLS tests
└── test-mtls-e2e.lisp   # End-to-end mTLS tests
```

## Current Status

### Implemented Features
✅ Connection preface handling
✅ Frame parsing and serialization
✅ Basic HPACK compression
✅ SETTINGS negotiation
✅ PING/PONG keepalive
✅ Stream creation and management (basic)
✅ Error handling (RST_STREAM, GOAWAY)
✅ mTLS support with client certificates

### Test Results
- **18/~100 h2spec tests passing**
- Key passing areas:
  - Connection establishment
  - Frame type handling
  - Stream multiplexing basics
  - Priority frames

### TODO for Full Compliance
1. **Flow Control** - Implement proper window management
2. **Stream States** - Complete state machine
3. **HPACK Dynamic Table** - Add dynamic header compression
4. **Server Push** - Implement PUSH_PROMISE
5. **Error Codes** - Send proper protocol violation errors
6. **Huffman Encoding** - Add to HPACK implementation

## Development Guide

### Adding New Frame Types
1. Define constants in `frames.lisp`
2. Add frame creation function
3. Add handler in server/client code
4. Write tests in `test-frames.lisp`

### Testing Changes
1. Run unit tests first
2. Test with h2spec for protocol compliance
3. Check against real HTTP/2 clients (curl, browsers)

### Debugging
- Enable verbose logging in test server
- Use Wireshark with HTTP/2 dissector
- Check frame hex dumps in server output

## References
- [RFC 7540](https://tools.ietf.org/html/rfc7540) - HTTP/2 Protocol
- [RFC 7541](https://tools.ietf.org/html/rfc7541) - HPACK Compression
- [h2spec](https://github.com/summerwind/h2spec) - Conformance Testing Tool
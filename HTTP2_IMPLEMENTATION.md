# HTTP/2 Implementation in Epsilon

## Overview

The Epsilon HTTP/2 implementation provides a complete protocol stack for HTTP/2 (RFC 7540) with support for:
- Frame serialization and parsing
- HPACK header compression (RFC 7541)
- Stream multiplexing
- Flow control
- TLS with ALPN negotiation
- Both client and server implementations

## Architecture

### Module Structure

```
modules/http2/
├── src/
│   ├── http2.lisp        # Main HTTP/2 connection and stream management
│   ├── frames.lisp       # Frame types and serialization (RFC 7540 Section 4-6)
│   ├── hpack-simple.lisp # HPACK header compression (RFC 7541)
│   ├── flow-control.lisp # Flow control implementation (RFC 7540 Section 5.2)
│   └── server.lisp       # HTTP/2 server implementation
├── tests/
│   ├── http2-tests.lisp        # Basic functionality tests
│   └── conformance-tests.lisp  # RFC compliance tests
└── examples/
    ├── demo.lisp          # Demonstration of HTTP/2 features
    └── simple-server.lisp # Example HTTP/2 server

```

## Key Components

### 1. Frame Layer (`frames.lisp`)

Implements all HTTP/2 frame types according to RFC 7540:

- **DATA** (0x0): Application data
- **HEADERS** (0x1): Header information
- **PRIORITY** (0x2): Stream priority
- **RST_STREAM** (0x3): Stream termination
- **SETTINGS** (0x4): Connection parameters
- **PUSH_PROMISE** (0x5): Server push initiation
- **PING** (0x6): Connection liveness check
- **GOAWAY** (0x7): Connection shutdown
- **WINDOW_UPDATE** (0x8): Flow control
- **CONTINUATION** (0x9): Continued HEADERS

Each frame follows the 9-byte header format:
```
+-----------------------------------------------+
|                 Length (24)                   |
+---------------+---------------+---------------+
|   Type (8)    |   Flags (8)   |
+-+-------------+---------------+-------------------------------+
|R|                 Stream Identifier (31)                     |
+=+=============================================================+
|                   Frame Payload (0...)                      ...
+---------------------------------------------------------------+
```

### 2. HPACK Compression (`hpack-simple.lisp`)

Implements header compression with:
- Static table of 61 common header fields
- Dynamic table support (simplified)
- Integer encoding with prefix lengths
- String encoding (Huffman encoding placeholder)

### 3. Flow Control (`flow-control.lisp`)

Manages both connection and stream-level flow control:
- Initial window size: 65,535 bytes (default)
- Maximum window size: 2^31 - 1 bytes
- Window updates via WINDOW_UPDATE frames
- Automatic flow control tracking

### 4. Stream Management (`http2.lisp`)

Handles stream lifecycle:
- Stream states: idle, open, half-closed (local/remote), closed
- Proper stream ID assignment (odd for client, even for server)
- Stream multiplexing
- Priority handling (basic)

### 5. Connection Management

Provides full connection handling:
- Connection preface: "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
- Settings negotiation
- Error handling with proper error codes
- Graceful shutdown via GOAWAY

## Usage Examples

### HTTP/2 Client

```lisp
;; Simple GET request
(let ((response (epsilon.http2:http2-get "https://example.com/api/data"
                                         :headers '(("accept" . "application/json")))))
  (format t "Status: ~A~%" (cdr (assoc ":status" (getf response :headers))))
  (format t "Body: ~A~%" (getf response :body)))

;; POST request with body
(epsilon.http2:http2-post "https://api.example.com/users"
                          :headers '(("content-type" . "application/json"))
                          :body "{\"name\":\"John\",\"email\":\"john@example.com\"}")
```

### HTTP/2 Server

```lisp
;; Define request handler
(defun my-handler (headers body)
  (let ((path (cdr (assoc ":path" headers :test #'string=))))
    (cond
      ((string= path "/")
       (list :status 200
             :headers '(("content-type" . "text/html"))
             :body "<h1>Welcome to HTTP/2!</h1>"))
      ((string= path "/api/status")
       (list :status 200
             :headers '(("content-type" . "application/json"))
             :body "{\"status\":\"ok\"}"))
      (t
       (list :status 404
             :body "Not Found")))))

;; Start server with TLS
(epsilon.http2:http2-server 
  :port 8443
  :ssl-p t
  :cert-file "/path/to/cert.pem"
  :key-file "/path/to/key.pem"
  :handler #'my-handler)
```

## TLS/ALPN Integration

The implementation integrates with the epsilon.crypto module for:
- TLS 1.2+ support via OpenSSL
- ALPN negotiation for "h2" protocol
- Client certificate support (mTLS)
- Self-signed certificate generation for testing

Example with ALPN:
```lisp
(let* ((tls-ctx (epsilon.crypto:create-openssl-context 
                 :server-p nil
                 :alpn-protocols '("h2" "http/1.1")))
       (tls-conn (epsilon.crypto:openssl-connect socket tls-ctx 
                                                 :hostname "example.com"
                                                 :alpn-protocols '("h2"))))
  ;; Verify we got HTTP/2
  (assert (string= (epsilon.crypto:tls-selected-alpn-protocol tls-conn) "h2")))
```

## Settings Parameters

The implementation supports all standard HTTP/2 settings:

| Parameter | ID | Default | Description |
|-----------|-----|---------|-------------|
| HEADER_TABLE_SIZE | 0x1 | 4096 | HPACK dynamic table size |
| ENABLE_PUSH | 0x2 | 1 | Server push enabled |
| MAX_CONCURRENT_STREAMS | 0x3 | 100 | Maximum concurrent streams |
| INITIAL_WINDOW_SIZE | 0x4 | 65535 | Initial flow control window |
| MAX_FRAME_SIZE | 0x5 | 16384 | Maximum frame payload size |
| MAX_HEADER_LIST_SIZE | 0x6 | 8192 | Maximum header list size |

## Error Codes

All RFC 7540 error codes are implemented:

| Code | Value | Description |
|------|-------|-------------|
| NO_ERROR | 0x0 | Graceful shutdown |
| PROTOCOL_ERROR | 0x1 | Protocol error detected |
| INTERNAL_ERROR | 0x2 | Implementation fault |
| FLOW_CONTROL_ERROR | 0x3 | Flow control protocol violated |
| SETTINGS_TIMEOUT | 0x4 | Settings not acknowledged |
| STREAM_CLOSED | 0x5 | Frame received for closed stream |
| FRAME_SIZE_ERROR | 0x6 | Frame size incorrect |
| REFUSED_STREAM | 0x7 | Stream not processed |
| CANCEL | 0x8 | Stream cancelled |
| COMPRESSION_ERROR | 0x9 | Compression state not updated |
| CONNECT_ERROR | 0xa | TCP connection error for CONNECT |
| ENHANCE_YOUR_CALM | 0xb | Processing capacity exceeded |
| INADEQUATE_SECURITY | 0xc | Negotiated TLS parameters not acceptable |
| HTTP_1_1_REQUIRED | 0xd | Use HTTP/1.1 for the request |

## Testing

### Unit Tests
```bash
./epsilon --test epsilon.http2
```

### Conformance Testing
The implementation includes RFC compliance tests in `conformance-tests.lisp` covering:
- Frame format validation
- Settings negotiation
- Flow control operations
- Stream state transitions
- HPACK compression
- Error handling

### Integration Testing with curl
```bash
# Test with curl (requires HTTP/2 support)
curl --http2 -k https://localhost:8443/

# Verbose mode to see HTTP/2 frames
curl --http2 -v -k https://localhost:8443/api/status
```

### Testing with nghttp
```bash
# Install nghttp2 tools
apt-get install nghttp2-client

# Test with nghttp
nghttp -v https://localhost:8443/

# Use h2load for performance testing
h2load -n 1000 -c 10 https://localhost:8443/
```

## Performance Considerations

1. **Frame Size**: Default 16KB, can be increased via SETTINGS
2. **Header Compression**: ~40% compression ratio typical
3. **Multiplexing**: Supports 100 concurrent streams by default
4. **Flow Control**: Per-stream and connection-level windows
5. **Buffer Management**: Automatic buffer pooling for frames

## Limitations and Future Work

Current limitations:
- Simplified HPACK (no Huffman encoding yet)
- Basic priority handling
- No server push implementation
- Limited HTTP/1.1 upgrade path

Future enhancements:
- Full HPACK with Huffman encoding
- Stream priority trees
- Server push support
- WebSocket over HTTP/2
- gRPC support
- Performance optimizations

## Debugging

Enable debug output:
```lisp
(setf epsilon.http2:*debug* t)
```

Trace frame exchanges:
```lisp
(trace epsilon.http2:connection-send-frame
       epsilon.http2:connection-receive-frame)
```

## Compliance

The implementation aims for RFC 7540 compliance and passes basic h2spec tests:
- ✅ Connection preface
- ✅ Frame format
- ✅ Settings exchange
- ✅ Flow control
- ✅ Stream states
- ✅ Error handling
- ✅ HPACK basics

## Contributing

To extend the HTTP/2 implementation:

1. Frame types: Add to `frames.lisp`
2. Settings: Update `+default-settings+`
3. Error codes: Add to error constants
4. HPACK: Enhance `hpack-simple.lisp`
5. Tests: Add to `conformance-tests.lisp`

## References

- [RFC 7540](https://tools.ietf.org/html/rfc7540) - HTTP/2
- [RFC 7541](https://tools.ietf.org/html/rfc7541) - HPACK
- [RFC 8740](https://tools.ietf.org/html/rfc8740) - Using TLS 1.3 with HTTP/2
- [h2spec](https://github.com/summerwind/h2spec) - Conformance testing tool
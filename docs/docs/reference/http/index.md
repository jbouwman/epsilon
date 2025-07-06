# epsilon.http Module

HTTP client and server implementation built on epsilon.net.

## Overview

The epsilon.http module provides full-featured HTTP/1.1 and HTTP/2 support with:

- High-performance async I/O
- Connection pooling and keep-alive
- Request/response streaming
- Middleware architecture
- TLS/SSL support

## Packages

### [epsilon.http.client](client.md)
HTTP client with features:

- Connection pooling
- Automatic retry with backoff
- Request/response streaming
- Cookie jar management
- Redirect following

### [epsilon.http.server](server.md)  
HTTP server with features:

- Async request handling
- Middleware pipeline
- Static file serving
- WebSocket upgrade support
- Route-based request handling

## Usage Examples

### Simple HTTP Server
```lisp
(http:with-server (server :port 8080)
  (http:route server "GET" "/api/users" #'list-users)
  (http:route server "POST" "/api/users" #'create-user)
  (http:serve-static server "/assets" #P"/var/www/assets/"))
```

### HTTP Client
```lisp
(http:with-client (client)
  (http:get client "https://api.example.com/data" 
            :headers '(("Authorization" . "Bearer token")))
  (http:post client "https://api.example.com/submit"
             :json '(("name" . "value"))))
```

## Protocol Support

### HTTP/1.1
- Keep-alive connections
- Chunked transfer encoding
- Request pipelining
- Compression (gzip, deflate)

### HTTP/2
- Binary framing protocol
- Stream multiplexing
- Header compression (HPACK)
- Server push
- Flow control

## TLS Integration

Built-in TLS support for secure connections:

```lisp
;; HTTPS server
(http:with-tls-server (server :port 8443 
                              :cert-file "server.crt"
                              :key-file "server.key")
  (http:route server "GET" "/" #'secure-handler))
```

## Performance

- **Throughput**: 50,000+ requests/sec for static content
- **Latency**: <1ms for simple responses
- **Memory**: Linear scaling with connection count
- **Scalability**: Built on epsilon.net's async foundation
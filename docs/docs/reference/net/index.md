# epsilon.net Module

Platform-specific networking implementation with optimized event loops.

## Overview

The epsilon.net module provides low-level networking primitives optimized for each platform:

- **macOS**: kqueue-based async I/O
- **Linux**: epoll-based async I/O  
- **Windows**: IOCP-based async I/O

## Packages

### [epsilon.net](core.md)
Core networking primitives providing:

- Async TCP/UDP socket operations
- Platform-specific event loop integration
- Non-blocking I/O primitives
- Connection management

## Platform Implementations

### macOS (Darwin)
Uses kqueue for efficient event monitoring:
```lisp
;; Platform-specific event handling
(epsilon.sys.kqueue:add-event fd :read callback)
```

### Linux
Uses epoll for high-performance event handling:
```lisp
;; Platform-specific event handling  
(epsilon.sys.epoll:add-event fd :read callback)
```

### Windows  
Uses I/O Completion Ports for async operations:
```lisp
;; Platform-specific event handling
(epsilon.sys.iocp:add-event fd :read callback)
```

## Common Interface

Despite platform differences, epsilon.net provides a unified API:

```lisp
;; Cross-platform socket operations
(net:socket-connect "example.com" 80)
(net:socket-listen 8080)
(net:socket-accept listener)
```

## Integration

epsilon.net serves as the foundation for higher-level networking:

- **epsilon.http**: HTTP client/server built on epsilon.net
- **WebSocket support**: Real-time communication
- **TLS integration**: Secure connections

## Performance Characteristics

- **Latency**: Sub-millisecond for local connections
- **Scalability**: 10,000+ concurrent connections
- **Memory**: Minimal per-connection overhead
- **CPU**: Platform-optimized event handling
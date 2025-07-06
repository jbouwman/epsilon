# Roadmap 005: Per-Platform Async Network Stacks

**Status**: Planning | **Timeline**: 12-16 weeks

## Overview

Platform-specific async networking implementation using native event loops (kqueue, epoll, IOCP).

**Development Order**: macOS → Linux → Windows

## Goals

- Sub-millisecond latency for local connections
- Support 10,000+ concurrent connections
- Error handling and connection management
- Clean API design

## Technical Scope

- Async TCP/UDP sockets with platform event loops
- HTTP/1.1 and HTTP/2 client and server
- TLS/SSL encryption
- WebSocket support
- Connection pooling

## Platform Implementations

### Phase 1: macOS (Weeks 1-4)
**Event Loop**: kqueue-based async I/O

**Core Components**:
- `epsilon.net.darwin` - macOS-specific networking implementation
- kqueue integration for efficient event monitoring
- TCP/UDP socket abstractions
- Non-blocking I/O primitives

**Deliverables**:
- Async socket creation and management
- Event-driven connection handling
- Basic HTTP server capable of 1000+ concurrent connections
- Performance benchmarks vs sb-bsd-sockets baseline

### Phase 2: Linux (Weeks 5-8)
**Event Loop**: epoll-based async I/O

**Core Components**:
- `epsilon.net.linux` - Linux-specific networking implementation
- epoll integration for high-performance event handling
- Advanced socket options (SO_REUSEPORT, TCP_NODELAY)
- Edge-triggered and level-triggered modes

**Deliverables**:
- Feature parity with macOS implementation
- Linux-optimized performance tuning
- Container-friendly networking (Docker, Kubernetes)
- Load testing with 10,000+ concurrent connections

### Phase 3: Windows (Weeks 9-12)
**Event Loop**: IOCP-based async I/O

**Core Components**:
- `epsilon.net.windows` - Windows-specific networking implementation
- I/O Completion Port integration
- Windows-specific socket handling
- Overlapped I/O for maximum throughput

**Deliverables**:
- Cross-platform API compatibility
- Windows performance optimization
- Integration with Windows networking stack
- PowerShell-friendly tooling

## HTTP Implementation

### HTTP/1.1 Server (Weeks 6-8)
**Features**:
- Request parsing with streaming support
- Response generation with chunked encoding
- Keep-alive connection management
- Static file serving with caching headers
- Middleware architecture for extensibility

**API Design**:
```lisp
(net:with-http-server (server :port 8080 :host "0.0.0.0")
  (net:route server "GET" "/api/*" #'api-handler)
  (net:route server "POST" "/webhook" #'webhook-handler)
  (net:serve-static server "/assets" #P"/var/www/assets/"))
```

### HTTP/1.1 Client (Weeks 7-9)
**Features**:
- Connection pooling with configurable limits
- Automatic retry with exponential backoff
- Request/response streaming
- Cookie jar management
- Redirect following

**API Design**:
```lisp
(net:with-http-client (client)
  (net:get client "https://api.example.com/data" 
           :headers '(("Authorization" . "Bearer token")))
  (net:post client "https://api.example.com/submit"
            :json '(("name" . "value"))))
```

### HTTP/2 Support (Weeks 10-12)
**Features**:
- Binary framing protocol implementation
- Stream multiplexing over single connection
- Header compression (HPACK)
- Server push capabilities
- Flow control and prioritization

## TLS/SSL Integration

### TLS Client (Weeks 8-10)
**Features**:
- Modern cipher suite support (TLS 1.2, TLS 1.3)
- Certificate validation with custom CA support
- SNI (Server Name Indication) support
- ALPN negotiation for HTTP/2

### TLS Server (Weeks 9-11)
**Features**:
- Certificate loading from files or memory
- Perfect Forward Secrecy (PFS)
- OCSP stapling support
- Configurable security policies

**Integration**:
```lisp
(net:with-tls-server (server :port 8443 
                             :cert-file "server.crt"
                             :key-file "server.key")
  (net:route server "GET" "/" #'secure-handler))
```

## WebSocket Support

### WebSocket Server (Weeks 11-12)
**Features**:
- RFC 6455 compliance
- Frame parsing and generation
- Ping/pong heartbeat
- Extension negotiation
- Broadcasting to multiple clients

### WebSocket Client (Weeks 11-12)
**Features**:
- Automatic connection upgrade
- Reconnection with backoff
- Message queuing during disconnection
- Event-driven message handling

## Architecture Design

### Core Abstractions

**Socket Interface**:
```lisp
(defgeneric async-connect (address port &key timeout callback))
(defgeneric async-accept (listener &key callback))
(defgeneric async-read (socket buffer &key callback))
(defgeneric async-write (socket data &key callback))
```

**Event Loop**:
```lisp
(defgeneric start-event-loop (loop))
(defgeneric stop-event-loop (loop))
(defgeneric add-socket (loop socket events callback))
(defgeneric remove-socket (loop socket))
```

**HTTP Abstractions**:
```lisp
(defclass http-request ()
  ((method :reader request-method)
   (uri :reader request-uri)
   (headers :reader request-headers)
   (body :reader request-body)))

(defclass http-response ()
  ((status :accessor response-status)
   (headers :accessor response-headers)
   (body :accessor response-body)))
```

### Platform Abstraction
- Common API across all platforms
- Platform-specific optimizations hidden behind abstractions
- Graceful fallback to blocking I/O when async unavailable
- Runtime platform detection and capability discovery

## Performance Targets

### Latency
- **Local connections**: < 0.1ms response time
- **HTTP requests**: < 1ms for simple responses
- **TLS handshake**: < 5ms for session resumption

### Throughput
- **TCP connections**: 10,000+ concurrent on modern hardware
- **HTTP requests**: 50,000+ requests/sec for static content
- **WebSocket messages**: 100,000+ messages/sec

### Memory Usage
- **Per connection**: < 8KB overhead
- **Total server**: Linear scaling with connection count
- **GC pressure**: Minimal allocation in hot paths

## Testing Strategy

### Unit Tests
- Platform-specific event loop functionality
- HTTP protocol parsing and generation
- TLS certificate validation
- WebSocket frame handling

### Integration Tests
- Cross-platform compatibility
- HTTP client/server interaction
- TLS end-to-end encryption
- WebSocket bidirectional communication

### Performance Tests
- Connection scalability benchmarks
- Latency measurement under load
- Memory usage profiling
- Comparison with other HTTP servers

### Compatibility Tests
- Browser compatibility for WebSockets
- HTTP client library compatibility
- Reverse proxy integration (nginx, HAProxy)
- Container deployment scenarios

## Migration Path

### From sb-bsd-sockets
- Gradual API migration with compatibility interface
- Performance comparison and optimization
- Documentation for porting existing code
- Examples showing before/after implementations

### Integration with Epsilon
- Build system integration for platform detection
- Test framework integration for async testing
- Documentation generation for new APIs
- Package organization following epsilon conventions

## Success Metrics

- Benchmark performance in top 10% of Lisp HTTP servers
- Test coverage >95% for core networking
- Complete API documentation
- HTTP server in <10 lines of code
- Production deployments at 3+ organizations

## Implementation Phases

### Phase 1: Foundation (Weeks 1-4)
- macOS kqueue implementation
- Core async socket abstractions
- Basic HTTP parsing
- Initial performance benchmarks

### Phase 2: HTTP Stack (Weeks 5-8)
- Linux epoll implementation
- Complete HTTP/1.1 server and client
- TLS client implementation
- Cross-platform testing

### Phase 3: Advanced Features (Weeks 9-12)
- Windows IOCP implementation
- HTTP/2 support
- TLS server implementation
- WebSocket protocol support

### Phase 4: Polish and Optimization (Weeks 13-16)
- Performance optimization across all platforms
- Documentation completion
- Production readiness testing
- Community feedback integration

## Dependencies

### External Libraries
- **TLS**: OpenSSL or platform-native TLS libraries
- **HTTP/2**: Custom implementation or NGHTTP2 bindings
- **Testing**: Integration with epsilon.tool.test framework

### Platform Requirements
- **macOS**: 10.12+ (kqueue support)
- **Linux**: 2.6+ (epoll support)
- **Windows**: Windows 7+ (IOCP support)
- **SBCL**: Current version with threading support

## Risk Mitigation

### Technical Risks
- **Platform Differences**: Extensive testing and abstraction interfaces
- **Performance Bottlenecks**: Continuous profiling and optimization
- **TLS Complexity**: Leverage existing proven libraries
- **HTTP/2 Complexity**: Incremental implementation with thorough testing

### Project Risks
- **Scope Creep**: Clear phase boundaries and deliverables
- **Resource Constraints**: Focus on core functionality first
- **Compatibility Issues**: Early and frequent testing across platforms
- **Adoption Challenges**: Clear migration guides and examples

---

*This roadmap establishes the foundation for high-performance network programming in Epsilon, enabling the development of modern web applications and network services.*
# epsilon.io Platform Integration

## Overview

epsilon.io now properly integrates with existing platform-specific async I/O infrastructure instead of reinventing the wheel. This provides:

- **Leverages existing code**: Uses epsilon.linux (epoll), epsilon.darwin (kqueue), epsilon.windows (IOCP)
- **Zero duplication**: No reimplementation of platform async mechanisms
- **Better maintainability**: Platform experts can focus on their backends
- **Consistent API**: Unified interface across all platforms

## Architecture

```
┌─────────────────┐
│   epsilon.io    │  ← Unified async I/O API
│   (buffers,     │
│    pipelines,   │
│    completion-  │
│    based API)   │
└─────────────────┘
         │
         ▼
┌─────────────────┐
│   Backend       │  ← Platform abstraction layer
│   Protocol      │
└─────────────────┘
         │
    ┌────┴────┬────────────┬─────────────┐
    ▼         ▼            ▼             ▼
┌─────────┐ ┌──────────┐ ┌───────────┐ ┌──────────┐
│ Linux   │ │ Darwin   │ │ Windows   │ │ Fallback │
│ (epoll) │ │ (kqueue) │ │  (IOCP)   │ │ (select) │
└─────────┘ └──────────┘ └───────────┘ └──────────┘
     │           │             │             │
     ▼           ▼             ▼             ▼
┌─────────┐ ┌──────────┐ ┌───────────┐ ┌──────────┐
│epsilon. │ │epsilon.  │ │epsilon.   │ │Standard  │
│linux    │ │darwin    │ │windows    │ │CL I/O    │
└─────────┘ └──────────┘ └───────────┘ └──────────┘
```

## Platform Integration Examples

### Linux Integration (epsilon.linux)

```lisp
;; epsilon.io provides the high-level API
(with-io-context (ctx :backend-type :linux)
  (let ((buffer (allocate-buffer 4096)))
    (async-read socket-fd buffer
      :context ctx
      :on-complete (lambda (bytes-read)
                     (process-data buffer bytes-read)))))

;; Internally uses epsilon.linux epoll infrastructure:
;; - epsilon.sys.epoll:epoll-create1
;; - epsilon.sys.epoll:epoll-ctl  
;; - epsilon.sys.epoll:epoll-wait
;; - All existing Linux networking code
```

### Darwin Integration (epsilon.darwin)

```lisp
;; Same high-level API
(with-io-context (ctx :backend-type :darwin)
  (async-read socket-fd buffer
    :context ctx
    :on-complete #'handle-data))

;; Internally uses epsilon.darwin kqueue infrastructure:
;; - epsilon.kqueue:kqueue
;; - epsilon.net.async:register-async-operation
;; - Existing Darwin async event loop
```

### Zero-Copy Operations (Linux-specific)

```lisp
;; High-level zero-copy API
(splice-async ctx source-fd dest-fd 65536
  :flags '(:move :more)
  :on-complete (lambda (bytes-transferred)
                 (format t "Transferred ~D bytes~%" bytes-transferred)))

;; Uses epsilon.linux splice infrastructure
;; Could integrate with existing epsilon.net zero-copy code
```

## Benefits of This Approach

### 1. **Code Reuse**
- epsilon.linux already has mature epoll handling
- epsilon.darwin has kqueue + async operation management  
- epsilon.windows has IOCP foundation
- No need to reimplement any of this

### 2. **Better Testing**
- Platform backends already tested in their modules
- epsilon.io tests focus on high-level abstractions
- Integration tests verify backend communication

### 3. **Specialized Optimization**
- Linux team can optimize epoll usage
- Darwin team can optimize kqueue integration
- Each platform gets specialized attention

### 4. **Clean Separation**
- epsilon.io: High-level async patterns (buffers, pipelines, completion-based API)
- Platform modules: Low-level kernel integration
- Clear boundaries and responsibilities

## Example: HTTP Server Integration

```lisp
;; High-level epsilon.io API
(defun start-modern-http-server (port)
  (with-io-context (ctx)  ; Auto-selects best platform backend
    (let ((server-socket (create-server-socket port))
          (buffer-pool (make-buffer-pool :size 8192 :count 1000)))
      
      (async-accept server-socket
        :context ctx
        :on-complete (lambda (client-socket)
                       (handle-client ctx client-socket buffer-pool))))))

(defun handle-client (ctx client buffer-pool)
  (with-pooled-buffer (buffer buffer-pool)
    (async-read client buffer
      :context ctx
      :on-complete (lambda (bytes-read)
                     (if (> bytes-read 0)
                         (process-http-request buffer bytes-read)
                         (close-connection client))))))

;; This automatically uses:
;; - Linux: epsilon.linux epoll + epsilon.net TCP handling
;; - Darwin: epsilon.darwin kqueue + epsilon.net.async  
;; - Windows: epsilon.windows IOCP (when implemented)
```

## Migration Path

### Phase 1 (Current): Foundation
- ✅ epsilon.io provides unified buffer management
- ✅ Backend protocol defined
- ✅ Platform detection and backend creation
- ✅ Basic integration with epsilon.linux epoll

### Phase 2: Full Integration  
- Implement Darwin kqueue backend
- Connect to epsilon.darwin async infrastructure
- Add Windows IOCP backend when available
- Performance optimization

### Phase 3: Advanced Features
- Zero-copy operations using platform facilities
- Advanced flow control with platform-specific optimizations
- Protocol-specific optimizations (HTTP/2, QUIC)

## Code Organization

```
modules/io/
├── src/
│   ├── package.lisp          # Core types and exports
│   ├── buffers.lisp          # Buffer management (platform-agnostic)
│   ├── backends.lisp         # Platform backend integration
│   ├── pipelines.lisp        # Stream processing (platform-agnostic)
│   └── completion.lisp       # Completion-based API
├── tests/
│   ├── buffer-tests.lisp     # Buffer management tests
│   ├── backend-tests.lisp    # Backend integration tests
│   └── integration-tests.lisp  # End-to-end platform tests
└── README.md

modules/linux/src/
├── sys/epoll.lisp           # Low-level epoll (existing)
├── net.lisp                 # TCP/UDP networking (existing)
└── async-integration.lisp   # epsilon.io integration (new)

modules/darwin/src/
├── kqueue.lisp              # Low-level kqueue (existing)
├── net/async.lisp          # Async operations (existing)
└── io-integration.lisp      # epsilon.io integration (new)
```

This architecture leverages the existing excellent platform-specific code while providing a modern, unified async I/O API on top.
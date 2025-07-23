# Windows Module for Epsilon

This module provides Windows-specific system and networking
functionality for the Epsilon library, built around Microsoft's I/O
Completion Ports (IOCP) for asynchronous operations.

## Architecture

### Core Components

**System Layer (`src/sys/`)**
- `iocp.lisp` - Complete IOCP implementation with foreign function interface
- `lib.lisp` - Foreign function interface utilities and memory management

**Network Layer (`src/net/`)**
- `core.lisp` - High-level networking API using IOCP for async operations

### Features

- Windows IOCP API binding with memory management
- Async connect, accept, read, and write operations
- Future/await pattern
- Socket stream wrappers
- Connection and listener abstractions

## Usage Examples

### Basic IOCP Operations

```lisp
;; Create IOCP and associate with socket
(iocp:with-iocp (iocp-handle)
  (let ((socket (iocp:create-tcp-socket)))
    (iocp:associate-socket iocp-handle socket completion-key)
    
    ;; Wait for completion
    (multiple-value-bind (result bytes key overlapped)
        (iocp:wait-for-completion iocp-handle 1000)
      (when (iocp:completion-success-p result)
        (format t "Operation completed: ~A bytes~%" bytes)))))
```

### High-Level Networking

```lisp
;; TCP Server
(net:with-listener (listener "127.0.0.1" 8080)
  (loop
    (let ((conn (net:socket-accept listener)))
      (when conn
        (let ((stream (net:socket-stream conn)))
          (format stream "Hello from Windows IOCP server!~%")
          (net:socket-close conn))))))

;; TCP Client  
(net:with-connection (conn "127.0.0.1" 8080)
  (let ((stream (net:socket-stream conn)))
    (format t "Server says: ~A~%" (read-line stream))))
```

### Asynchronous Operations

```lisp
;; Async connect with callback
(net:async-connect "example.com" 80 
  :callback (lambda (connection)
              (format t "Connected: ~A~%" connection)))

;; Async read/write
(let ((buffer (make-array 1024 :element-type '(unsigned-byte 8))))
  (net:async-read connection buffer
    :callback (lambda (buf bytes)
                (format t "Read ~A bytes~%" bytes))))

;; Future-based async programming
(let ((future (net:async-connect "example.com" 80)))
  (let ((connection (net:await future 5000))) ; 5 second timeout
    (format t "Connection established: ~A~%" connection)))
```

## API Reference

### IOCP System Layer

**Core Functions:**
- `create-io-completion-port` - Create IOCP or associate file handle
- `get-queued-completion-status` - Wait for completion notification
- `post-queued-completion-status` - Post artificial completion
- `associate-socket` - Associate socket with IOCP

**High-Level Interface:**
- `with-iocp` - Execute body with automatic IOCP cleanup
- `wait-for-completion` - Simplified completion waiting
- `process-completions` - Process completions with handler function

**Data Structures:**
- `overlapped` - Windows OVERLAPPED structure representation
- `completion-key` - User-defined completion context
- `pack-overlapped`/`unpack-overlapped` - Memory serialization

### Network Layer

**Core Types:**
- `socket` - Base socket class with handle and options
- `listener` - Listening socket with IOCP integration
- `connection` - Connected socket with peer information
- `future` - Async operation result container

**Socket Operations:**
- `socket-listen` - Create listening socket
- `socket-connect` - Connect to remote host
- `socket-accept` - Accept incoming connection
- `socket-close` - Close socket with cleanup

**Async Operations:**
- `async-connect` - Asynchronous connection establishment
- `async-accept` - Asynchronous connection acceptance
- `async-read` - Asynchronous data reading
- `async-write` - Asynchronous data writing

**Future Pattern:**
- `future` - Create async operation future
- `await` - Wait for future completion with timeout
- `select` - Wait for any of multiple futures

## Implementation Details

### Memory Management

All foreign memory operations use automatic cleanup through `unwind-protect` and the `with-foreign-memory` macro. OVERLAPPED structures are packed/unpacked to handle x64 padding requirements.

### Error Handling

The implementation uses condition-based error handling with specific error types:
- `network-error` - Base networking error
- `connection-error` - Connection-specific errors  
- `timeout-error` - Operation timeout errors

### Platform Requirements

- Windows Vista or later (for modern IOCP features)
- SBCL with foreign function interface support
- ws2_32.dll and kernel32.dll (standard Windows libraries)

### Performance Characteristics

IOCP provides excellent scalability for high-concurrency applications:
- Single completion port can handle thousands of sockets
- O(1) completion notification regardless of socket count
- Kernel-level async I/O with minimal user-space overhead
- Integration with Windows thread scheduling

## Testing

The module includes tests covering:
- IOCP creation and basic operations
- OVERLAPPED structure serialization
- Completion key management
- Socket creation and error handling
- Async operation patterns
- Future/await functionality

Run tests with:
```bash
./epsilon test --module windows
```

## Comparison with Linux Module

| Feature     | Windows (IOCP)   | Linux (epoll)   |
|-------------|------------------|-----------------|
| Async Model | Completion-based | Readiness-based |
| Scalability | Excellent        | Excellent       |
| Memory Copy | Kernel-managed   | User-managed    |
| Threading   | Thread pool      | Manual threads  |
| Platform    | Windows only     | Linux only      |

Both implementations provide the same high-level API for cross-platform compatibility.

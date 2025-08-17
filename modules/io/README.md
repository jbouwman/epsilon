# epsilon.io - Modern Asynchronous I/O for Lisp

## Overview

epsilon.io provides a modern, high-performance I/O system for Common Lisp that moves beyond traditional Gray streams to embrace contemporary async I/O patterns. It features zero-copy operations, completion-based async I/O, composable stream pipelines, and deep integration with kernel facilities like io_uring and epoll.

## Key Features

- **Buffer-centric design** with explicit ownership semantics preventing use-after-free
- **Zero-copy operations** via splice, sendfile, and memory-mapped I/O
- **Completion-based async I/O** with futures and callbacks instead of readiness-based polling
- **Composable stream pipelines** for building transformation chains
- **Automatic backpressure** and flow control with adaptive algorithms
- **Native transcoding** for streaming character encoding conversion
- **Buffer pooling** for efficient memory management
- **Platform-specific optimizations** (io_uring on Linux, kqueue on BSD/Darwin)
- **TLS/SSL support** via OpenSSL BIO abstraction

## Installation

The module is included with epsilon. Load it with:

```lisp
(epsilon:load-module :epsilon.io)
```

## Quick Start

### Basic Async Read

```lisp
(use-package :epsilon.io)

;; Create I/O context for async operations
(with-io-context (ctx)
  (let ((buffer (allocate-buffer 4096)))
    (async-read fd buffer
      :context ctx
      :on-complete (lambda (bytes-read)
                     (format t "Read ~D bytes: ~A~%" 
                            bytes-read
                            (buffer-to-string buffer :end bytes-read)))
      :on-error (lambda (error)
                  (warn "Read failed: ~A" error)))))
```

### Stream Pipeline

```lisp
;; Create a pipeline: read file → compress → encrypt → write
(with-stream-pipeline (pipeline
                       :source (file-source "input.txt")
                       :sink (file-sink "output.enc.gz"))
  (add-transform pipeline (gzip-compress-transform :level 9))
  (add-transform pipeline (aes-transform :key secret-key))
  (run-pipeline pipeline))
```

### Buffer Pools

```lisp
;; Create buffer pool for efficient memory reuse
(let ((pool (make-buffer-pool :size 8192 :count 100)))
  (with-pooled-buffer (buffer pool)
    ;; Buffer automatically returned to pool
    (process-data buffer)))
```

## Core Concepts

### Buffers

Buffers are first-class objects with position, limit, and ownership tracking:

```lisp
(let ((buffer (allocate-buffer 1024)))
  ;; Write data
  (buffer-put-bytes buffer data)
  
  ;; Flip for reading
  (buffer-flip buffer)
  
  ;; Read data
  (buffer-get-bytes buffer output))
```

### Async Operations

All I/O operations are completion-based rather than readiness-based:

```lisp
(async-write fd buffer
  :on-complete (lambda (bytes-written)
                 (format t "Wrote ~D bytes~%" bytes-written))
  :on-error #'handle-error)

;; Or use futures
(let ((completion (async-read fd buffer)))
  (await completion :timeout 5000))
```

### Pipelines

Compose streams of transformations:

```lisp
(make-pipeline
  :source (string-source "Hello, World!")
  :transforms (list (map-transform #'string-upcase)
                   (base64-encode-transform))
  :sink (string-sink))
```

## Use Cases

### High-Performance Network Server

```lisp
(defun start-echo-server (port)
  (with-io-context (ctx)
    (let ((server (tcp-listen port))
          (pool (make-buffer-pool :size 8192)))
      (loop
        (async-accept server
          :context ctx
          :on-complete (lambda (client)
                         (handle-echo-client ctx client pool)))))))

(defun handle-echo-client (ctx client pool)
  (with-pooled-buffer (buffer pool)
    (async-read client buffer
      :context ctx
      :on-complete (lambda (n)
                     (if (> n 0)
                         ;; Echo back
                         (async-write client buffer
                           :length n
                           :context ctx
                           :on-complete (lambda ()
                                          (handle-echo-client ctx client pool)))
                         ;; Connection closed
                         (close-connection client))))))
```

### Zero-Copy File Transfer

```lisp
(defun transfer-file (source-path dest-fd size)
  (with-io-context (ctx)
    (let ((source-fd (open-file source-path :read)))
      (splice-async ctx source-fd dest-fd size
        :flags '(:move :more)
        :on-complete (lambda (transferred)
                       (format t "Transferred ~D bytes with zero copies~%" 
                              transferred))))))
```

### TLS/SSL Communication

```lisp
(defun https-client (host port request)
  (with-tls-context (tls-ctx :method :tls-client)
    (let ((socket (tcp-connect host port)))
      (with-tls-bio-stream (stream socket :context tls-ctx)
        ;; Perform TLS handshake
        (tls-handshake-async stream
          :on-complete (lambda ()
                         ;; Send HTTPS request
                         (write-string-async stream request
                           :on-complete (lambda ()
                                          (read-response stream)))))))))
```

### Stream Processing with Backpressure

```lisp
(defun process-log-stream (input-file output-file)
  (let ((controller (make-flow-controller :high-water 1048576)))
    (with-stream-pipeline (pipeline
                          :source (file-source input-file)
                          :sink (file-sink output-file)
                          :controller controller)
      ;; Add transformations
      (add-transform pipeline (line-split-transform))
      (add-transform pipeline (json-parse-transform))
      (add-transform pipeline 
        (filter-transform (lambda (entry)
                            (> (getf entry :severity) 3))))
      
      ;; Run with automatic backpressure
      (run-pipeline pipeline))))
```

### Character Encoding Conversion

```lisp
(defun convert-encoding (input-file output-file from-enc to-enc)
  (let ((transcoder (make-transcoder :from from-enc 
                                    :to to-enc
                                    :on-malformed :replace)))
    (with-stream-pipeline (pipeline
                          :source (file-source input-file)
                          :sink (file-sink output-file))
      (add-transform pipeline
        (make-transform :function (lambda (data state)
                                   (transcode-bytes transcoder data))))
      (run-pipeline pipeline))))

;; Example: Convert UTF-8 to UTF-16LE
(convert-encoding "input.txt" "output.txt" :utf-8 :utf-16le)
```

## API Reference

### Buffer Management

#### Functions

- `(allocate-buffer size &key alignment)` - Allocate buffer with optional alignment
- `(free-buffer buffer)` - Free buffer resources
- `(buffer-flip buffer)` - Flip buffer from write to read mode
- `(buffer-clear buffer)` - Clear buffer for writing
- `(buffer-rewind buffer)` - Reset position to 0
- `(buffer-get buffer &optional index)` - Get byte at position
- `(buffer-put buffer byte &optional index)` - Put byte at position
- `(buffer-remaining buffer)` - Bytes remaining between position and limit

#### Buffer Pools

- `(make-buffer-pool &key size count alignment)` - Create buffer pool
- `(get-buffer pool)` - Get buffer from pool
- `(return-buffer pool buffer)` - Return buffer to pool
- `(with-pooled-buffer (var pool) &body body)` - Use pooled buffer

### Async I/O

#### Context Management

- `(make-io-context &key backend)` - Create I/O context
- `(with-io-context (var &rest args) &body body)` - Execute with context
- `(wait-for-completion context &key timeout)` - Wait for any completion
- `(wait-for-all-completions context &key timeout)` - Wait for all completions

#### Async Operations

- `(async-read fd buffer &key on-complete on-error context timeout)` - Async read
- `(async-write fd buffer &key length on-complete on-error context)` - Async write
- `(async-connect address &key on-complete on-error context)` - Async connect
- `(async-accept server &key on-complete on-error context)` - Async accept
- `(splice-async ctx source dest size &key flags on-complete)` - Zero-copy splice
- `(sendfile-async source dest size &key on-complete context)` - Zero-copy sendfile

#### Cancellation

- `(make-cancellation-token)` - Create cancellation token
- `(cancel-token token)` - Cancel operations
- `(with-cancellation-token (token) &body body)` - Execute with cancellation

### Stream Pipelines

- `(make-pipeline &key source transforms sink)` - Create pipeline
- `(add-transform pipeline transform)` - Add transformation
- `(run-pipeline pipeline &key on-complete on-error async)` - Execute pipeline
- `(pause-pipeline pipeline)` - Pause processing
- `(resume-pipeline pipeline)` - Resume processing

### Transforms

- `(identity-transform)` - Pass through unchanged
- `(map-transform function)` - Apply function to data
- `(filter-transform predicate)` - Filter based on predicate
- `(gzip-compress-transform &key level)` - Compress with gzip
- `(gzip-decompress-transform)` - Decompress gzip
- `(line-split-transform)` - Split into lines
- `(aggregate-transform &key window-size)` - Aggregate in windows

### Flow Control

- `(make-flow-controller &key high-water low-water)` - Create controller
- `(write-with-backpressure fd data &key controller on-drain)` - Write with backpressure
- `(make-rate-limiter &key rate burst)` - Create rate limiter
- `(acquire-tokens limiter count &key wait-p)` - Acquire rate limit tokens

### Transcoding

- `(make-transcoder &key from to on-malformed)` - Create transcoder
- `(transcode-string string transcoder)` - Transcode string
- `(transcode-bytes transcoder bytes)` - Transcode bytes
- `(with-transcoding-stream (var transcoder) &body body)` - Transcoding stream

### TLS/BIO

- `(make-tls-bio socket context &key mode)` - Create TLS BIO
- `(with-tls-bio-stream (var socket &key context) &body body)` - TLS stream
- `(tls-handshake-async bio &key on-complete on-error)` - Async handshake
- `(write-string-async bio string &key on-complete encoding)` - Write string
- `(read-until bio delimiter &key on-complete max-length)` - Read until delimiter

## Performance Considerations

### Memory Management

- Use buffer pools to avoid allocation overhead
- Align buffers to page boundaries for DMA operations
- Reuse buffers across operations when possible

### Concurrency

- Buffers are not thread-safe; use one per thread or synchronize access
- I/O contexts can handle multiple concurrent operations
- Completion callbacks run in separate threads

### Platform Optimization

- **Linux**: io_uring provides best performance for high connection counts
- **BSD/macOS**: kqueue with kevent for efficient event notification
- **Windows**: IOCP (I/O Completion Ports) for scalability
- **Fallback**: select/poll for maximum portability

### Best Practices

1. **Buffer Ownership**: Always respect buffer ownership during async operations
2. **Error Handling**: Provide both error callbacks and condition handlers
3. **Resource Cleanup**: Use unwind-protect or with-* macros
4. **Backpressure**: Implement flow control for streaming operations
5. **Batch Operations**: Submit multiple async operations together when possible

## Comparison with Gray Streams

| Feature | Gray Streams | epsilon.io |
|---------|-------------|------------|
| **I/O Model** | Synchronous, blocking | Async, completion-based |
| **Extension** | Inheritance-based | Composition-based |
| **Memory** | Implicit copying | Explicit ownership, zero-copy |
| **Backpressure** | Manual | Automatic flow control |
| **Platform Integration** | Generic | Platform-specific optimizations |
| **Transcoding** | Full buffering | Streaming conversion |

## Limitations

- Platform-specific backends not fully implemented (io_uring, kqueue)
- Some transforms are stubs pending integration with other epsilon modules
- TLS/BIO implementation requires epsilon.crypto module
- Windows IOCP backend not yet implemented

## Future Enhancements

- Full io_uring implementation for Linux
- kqueue backend for BSD/macOS
- IOCP backend for Windows
- QUIC protocol support
- Memory-mapped file operations
- Vectored I/O support
- Direct I/O for database workloads

## Contributing

Contributions are welcome! Areas of focus:

1. Platform-specific backend implementations
2. Additional stream transforms
3. Performance optimizations
4. Protocol implementations (HTTP/2, QUIC)
5. Integration with other epsilon modules

## License

Part of the Epsilon project. See LICENSE for details.
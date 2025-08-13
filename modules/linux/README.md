# Epsilon Linux Package

Linux-specific system programming interfaces providing high-performance networking with epoll, TLS support, and native socket operations built on epsilon.foreign.

## Overview

The Epsilon Linux package provides:

- **Epoll-based event loop** for scalable I/O multiplexing
- **Native socket operations** with full Linux socket API access
- **TLS/SSL support** for secure communications
- **High-performance networking** optimized for Linux systems
- **Foreign function interface** integration via epsilon.foreign
- **Edge-triggered and level-triggered** event notification modes

**Platform**: Linux-specific (requires Linux kernel 2.6+)

## Quick Start

### Basic Epoll Usage

```lisp
(use-package :epsilon.sys.epoll)

;; Create epoll instance and monitor sockets
(with-epoll (epfd)
  ;; Add socket to epoll
  (add-event epfd socket-fd (logior +epollin+ +epollet+))
  
  ;; Event loop
  (loop
    (let ((events (wait-for-events epfd 10 1000)))  ; 10 events, 1s timeout
      (dolist (event events)
        (let ((fd (epoll-event-data event))
              (event-mask (epoll-event-events event)))
          (cond
            ((logand event-mask +epollin+)
             (handle-read-event fd))
            ((logand event-mask +epollout+)
             (handle-write-event fd))))))))
```

### Socket Operations

```lisp
(use-package :epsilon.net.core)

;; Create TCP server
(with-server-socket (server-fd :port 8080)
  (loop
    (let ((client-fd (socket-accept server-fd)))
      (unwind-protect
          (let ((data (socket-recv client-fd 1024)))
            (socket-send client-fd "HTTP/1.1 200 OK\r\n\r\nHello World"))
        (socket-close client-fd)))))

;; Create TCP client
(with-client-socket (client-fd :host "localhost" :port 8080)
  (socket-send client-fd "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n")
  (let ((response (socket-recv client-fd 4096)))
    (format t "Server response: ~A~%" response)))
```

## Core Components

### Epoll Event Loop (`epsilon.sys.epoll`)

**High-Performance I/O Multiplexing**

Linux epoll provides scalable I/O event notification, supporting thousands of concurrent connections with excellent performance characteristics.

#### Core Functions

**`epoll-create1`** `(flags)`

Create an epoll file descriptor:

```lisp
;; Create epoll instance
(let ((epfd (epoll-create1 0)))
  ;; Use epfd for event monitoring
  (epoll-close epfd))

;; Create with close-on-exec flag
(let ((epfd (epoll-create1 +epoll-cloexec+)))
  ;; Automatically closes on exec()
  )
```

**`epoll-ctl`** `(epfd op fd event)`

Control epoll instance:

```lisp
;; Add file descriptor
(epoll-ctl epfd +epoll-ctl-add+ socket-fd 
           (make-epoll-event :events (logior +epollin+ +epollet+)
                            :data socket-fd))

;; Modify existing descriptor
(epoll-ctl epfd +epoll-ctl-mod+ socket-fd
           (make-epoll-event :events +epollout+ :data socket-fd))

;; Remove descriptor
(epoll-ctl epfd +epoll-ctl-del+ socket-fd nil)
```

**`epoll-wait`** `(epfd events max-events timeout)`

Wait for events:

```lisp
;; Wait for up to 10 events with 1 second timeout
(let ((events (epoll-wait epfd event-array 10 1000)))
  (loop for i from 0 below events
        for event = (aref event-array i)
        do (process-event event)))
```

#### Event Types

**Input/Output Events:**
- `+EPOLLIN+` - Available for read operations
- `+EPOLLOUT+` - Available for write operations  
- `+EPOLLRDHUP+` - Stream socket peer closed connection
- `+EPOLLPRI+` - Urgent data available

**Error Conditions:**
- `+EPOLLERR+` - Error condition occurred
- `+EPOLLHUP+` - Hang up occurred

**Behavior Modifiers:**
- `+EPOLLET+` - Edge-triggered mode (default is level-triggered)
- `+EPOLLONESHOT+` - One-shot mode (disable after event)
- `+EPOLLWAKEUP+` - Prevent system suspend
- `+EPOLLEXCLUSIVE+` - Exclusive wakeup mode

#### High-Level Interface

**`with-epoll`** `(epfd &body body)`

Automatic epoll management:

```lisp
(with-epoll (epfd)
  ;; epfd is created and automatically cleaned up
  (add-event epfd socket +epollin+)
  (wait-for-events epfd 1 1000))
```

**`add-event`** `(epfd fd events &optional data)`

Simplified event addition:

```lisp
;; Monitor socket for read events
(add-event epfd socket-fd +epollin+)

;; Edge-triggered read monitoring
(add-event epfd socket-fd (logior +epollin+ +epollet+))
```

### Network Operations (`epsilon.net.core`)

**Native Linux Socket API**

Direct access to Linux socket operations with performance optimizations.

#### Socket Creation

**`socket-create`** `(domain type protocol)`

Create a socket:

```lisp
;; TCP socket
(let ((fd (socket-create +af-inet+ +sock-stream+ +ipproto-tcp+)))
  ;; Use socket
  (socket-close fd))

;; UDP socket  
(let ((fd (socket-create +af-inet+ +sock-dgram+ +ipproto-udp+)))
  ;; Use socket
  (socket-close fd))

;; Unix domain socket
(let ((fd (socket-create +af-unix+ +sock-stream+ 0)))
  ;; Use socket
  (socket-close fd))
```

#### Server Operations

**`socket-bind`** `(fd address)`
**`socket-listen`** `(fd backlog)`
**`socket-accept`** `(fd)`

Create TCP servers:

```lisp
(let ((server-fd (socket-create +af-inet+ +sock-stream+ +ipproto-tcp+)))
  ;; Allow address reuse
  (set-socket-option server-fd +sol-socket+ +so-reuseaddr+ 1)
  
  ;; Bind to port 8080
  (socket-bind server-fd (make-inet-address "0.0.0.0" 8080))
  
  ;; Listen for connections
  (socket-listen server-fd 128)
  
  ;; Accept connections
  (loop
    (let ((client-fd (socket-accept server-fd)))
      (handle-client client-fd))))
```

#### Client Operations

**`socket-connect`** `(fd address)`

Connect to servers:

```lisp
(let ((client-fd (socket-create +af-inet+ +sock-stream+ +ipproto-tcp+)))
  ;; Connect to server
  (socket-connect client-fd (make-inet-address "192.168.1.100" 8080))
  
  ;; Use connection
  (socket-send client-fd "Hello Server")
  (let ((response (socket-recv client-fd 1024)))
    (format t "Response: ~A~%" response))
  
  (socket-close client-fd))
```

#### Data Transfer

**`socket-send`** `(fd data &optional flags)`
**`socket-recv`** `(fd size &optional flags)`

Send and receive data:

```lisp
;; Send string data
(socket-send fd "Hello World")

;; Send binary data  
(socket-send fd byte-array)

;; Receive data
(let ((data (socket-recv fd 4096)))
  (format t "Received: ~A~%" data))

;; Non-blocking receive
(let ((data (socket-recv fd 1024 +msg-dontwait+)))
  (when data
    (process-data data)))
```

#### Address Management

**`make-inet-address`** `(host port)`
**`make-inet6-address`** `(host port)`

Create network addresses:

```lisp
;; IPv4 addresses
(make-inet-address "127.0.0.1" 8080)
(make-inet-address "0.0.0.0" 80)     ; Bind all interfaces
(make-inet-address "192.168.1.10" 3000)

;; IPv6 addresses  
(make-inet6-address "::1" 8080)      ; Localhost
(make-inet6-address "::" 80)         ; Bind all interfaces
(make-inet6-address "2001:db8::1" 443)
```

### TLS Support (`epsilon.crypto`)

**Secure Socket Layer / Transport Layer Security**

```lisp
(use-package :epsilon.crypto)

;; Create TLS context
(with-tls-context (ctx :method :tls-v1.2)
  ;; Load certificates
  (tls-load-cert-file ctx "server.crt")
  (tls-load-key-file ctx "server.key")
  
  ;; Create TLS connection
  (with-tls-connection (tls-conn ctx socket-fd)
    ;; Perform TLS handshake
    (tls-handshake tls-conn)
    
    ;; Send encrypted data
    (tls-send tls-conn "Secure Hello World")
    
    ;; Receive encrypted data
    (let ((secure-data (tls-recv tls-conn 1024)))
      (format t "Secure response: ~A~%" secure-data))))
```

## Advanced Features

### High-Performance Server Pattern

```lisp
(defun create-epoll-server (port max-connections)
  "Create high-performance epoll-based server"
  (let ((server-fd (socket-create +af-inet+ +sock-stream+ +ipproto-tcp+)))
    ;; Configure socket
    (set-socket-option server-fd +sol-socket+ +so-reuseaddr+ 1)
    (socket-bind server-fd (make-inet-address "0.0.0.0" port))
    (socket-listen server-fd 128)
    
    ;; Create epoll instance
    (with-epoll (epfd)
      ;; Add server socket to epoll (level-triggered for accept)
      (add-event epfd server-fd +epollin+)
      
      ;; Event loop
      (loop
        (let ((events (wait-for-events epfd max-connections 1000)))
          (dolist (event events)
            (let ((fd (epoll-event-data event))
                  (event-mask (epoll-event-events event)))
              (cond
                ;; New connection on server socket
                ((= fd server-fd)
                 (loop
                   (let ((client-fd (socket-accept server-fd)))
                     (when client-fd
                       ;; Add client to epoll (edge-triggered)
                       (add-event epfd client-fd (logior +epollin+ +epollet+)))
                     (unless client-fd (return)))))
                
                ;; Data available on client socket
                ((logand event-mask +epollin+)
                 (handle-client-read fd epfd))
                
                ;; Client ready for writing
                ((logand event-mask +epollout+)
                 (handle-client-write fd epfd))
                
                ;; Error or hangup
                ((logand event-mask (logior +epollerr+ +epollhup+))
                 (delete-event epfd fd)
                 (socket-close fd))))))))))

(defun handle-client-read (fd epfd)
  "Handle read event on client socket"
  (loop
    (let ((data (socket-recv fd 4096 +msg-dontwait+)))
      (cond
        ((null data) (return))          ; No more data
        ((zerop (length data))          ; Connection closed
         (delete-event epfd fd)
         (socket-close fd)
         (return))
        (t                              ; Process data
         (process-client-data fd data)
         ;; Switch to write mode if response ready
         (when (response-ready-p fd)
           (modify-event epfd fd +epollout+)))))))
```

### Connection Pool Management

```lisp
(defstruct connection-pool
  host
  port
  max-connections
  active-connections
  idle-connections
  connection-timeout)

(defun create-connection-pool (host port &key (max-connections 10))
  "Create a connection pool for efficient connection reuse"
  (make-connection-pool
    :host host
    :port port
    :max-connections max-connections
    :active-connections (make-hash-table)
    :idle-connections '()
    :connection-timeout 300)) ; 5 minutes

(defun get-pooled-connection (pool)
  "Get connection from pool or create new one"
  (or (pop (connection-pool-idle-connections pool))
      (when (< (hash-table-count (connection-pool-active-connections pool))
               (connection-pool-max-connections pool))
        (let ((fd (socket-create +af-inet+ +sock-stream+ +ipproto-tcp+)))
          (socket-connect fd (make-inet-address 
                             (connection-pool-host pool)
                             (connection-pool-port pool)))
          (setf (gethash fd (connection-pool-active-connections pool))
                (get-universal-time))
          fd))))

(defun return-pooled-connection (pool fd)
  "Return connection to pool for reuse"
  (remhash fd (connection-pool-active-connections pool))
  (push fd (connection-pool-idle-connections pool)))
```

### Async I/O Patterns

```lisp
(defstruct async-operation
  type        ; :read, :write, :connect
  fd          ; file descriptor
  buffer      ; data buffer
  callback    ; completion callback
  user-data)  ; user-defined data

(defun async-read (fd size callback &optional user-data)
  "Initiate asynchronous read operation"
  (let ((op (make-async-operation
             :type :read
             :fd fd
             :buffer (make-array size :element-type '(unsigned-byte 8))
             :callback callback
             :user-data user-data)))
    (add-to-async-queue op)
    op))

(defun async-write (fd data callback &optional user-data)
  "Initiate asynchronous write operation"
  (let ((op (make-async-operation
             :type :write
             :fd fd
             :buffer data
             :callback callback
             :user-data user-data)))
    (add-to-async-queue op)
    op))

(defun process-async-operations (epfd)
  "Process completed async operations"
  (let ((events (wait-for-events epfd 100 0))) ; Non-blocking
    (dolist (event events)
      (let ((fd (epoll-event-data event))
            (mask (epoll-event-events event)))
        (when-let ((op (find-operation-by-fd fd)))
          (cond
            ((and (eq (async-operation-type op) :read)
                  (logand mask +epollin+))
             (complete-read-operation op))
            ((and (eq (async-operation-type op) :write)
                  (logand mask +epollout+))
             (complete-write-operation op))))))))
```

## Error Handling

### Socket Errors

```lisp
(handler-case
    (socket-connect fd address)
  (socket-error (e)
    (case (socket-error-code e)
      (+econnrefused+ (error "Connection refused"))
      (+etimedout+ (error "Connection timeout"))
      (+enetunreach+ (error "Network unreachable"))
      (t (error "Socket error: ~A" e)))))
```

### Epoll Errors

```lisp
(handler-case
    (epoll-wait epfd events 10 1000)
  (epoll-error (e)
    (case (epoll-error-code e)
      (+eintr+ (continue))              ; Interrupted by signal
      (+ebadf+ (error "Invalid epoll fd"))
      (+efault+ (error "Events buffer invalid"))
      (t (error "Epoll error: ~A" e)))))
```

## Performance Optimization

### Memory Management

```lisp
;; Pre-allocate event arrays
(defparameter *event-array* (make-array 1000 :element-type 'epoll-event))

;; Reuse buffers
(defparameter *recv-buffer-pool* 
  (loop repeat 100 collect (make-array 4096 :element-type '(unsigned-byte 8))))

(defun get-recv-buffer ()
  (or (pop *recv-buffer-pool*)
      (make-array 4096 :element-type '(unsigned-byte 8))))

(defun return-recv-buffer (buffer)
  (when (< (length *recv-buffer-pool*) 100)
    (push buffer *recv-buffer-pool*)))
```

### Edge-Triggered Optimizations

```lisp
(defun handle-edge-triggered-read (fd)
  "Efficiently handle edge-triggered read events"
  (loop
    (let ((data (socket-recv fd 4096 +msg-dontwait+)))
      (cond
        ((null data) (return))          ; EAGAIN - no more data
        ((zerop (length data))          ; EOF - connection closed
         (handle-connection-close fd)
         (return))
        (t                              ; Got data - process it
         (process-data data)
         ;; Continue reading until EAGAIN
         )))))
```

## Integration Examples

### HTTP Server

```lisp
(defun create-http-server (port)
  "Simple HTTP server using epoll"
  (let ((server-fd (socket-create +af-inet+ +sock-stream+ +ipproto-tcp+)))
    (set-socket-option server-fd +sol-socket+ +so-reuseaddr+ 1)
    (socket-bind server-fd (make-inet-address "0.0.0.0" port))
    (socket-listen server-fd 128)
    
    (with-epoll (epfd)
      (add-event epfd server-fd +epollin+)
      
      (format t "HTTP server listening on port ~D~%" port)
      (loop
        (let ((events (wait-for-events epfd 100 1000)))
          (dolist (event events)
            (handle-http-event epfd event)))))))

(defun handle-http-event (epfd event)
  "Handle HTTP server event"
  (let ((fd (epoll-event-data event))
        (mask (epoll-event-events event)))
    (cond
      ((logand mask +epollin+)
       (if (= fd server-fd)
           (accept-http-connection epfd fd)
           (handle-http-request epfd fd)))
      ((logand mask +epollout+)
       (send-http-response epfd fd))
      ((logand mask (logior +epollerr+ +epollhup+))
       (close-http-connection epfd fd)))))
```

### WebSocket Server

```lisp
(defun create-websocket-server (port)
  "WebSocket server with epoll event loop"
  ;; Similar structure to HTTP server
  ;; but with WebSocket protocol handling
  )
```

## Best Practices

### Resource Management

```lisp
;; Always clean up resources
(unwind-protect
    (progn
      (socket-connect fd address)
      (use-socket fd))
  (socket-close fd))

;; Use high-level wrappers when possible
(with-client-socket (fd :host "example.com" :port 80)
  (socket-send fd request)
  (socket-recv fd 4096))
```

### Error Recovery

```lisp
;; Handle temporary errors gracefully
(loop
  (handler-case
      (let ((client-fd (socket-accept server-fd)))
        (handle-client client-fd))
    (socket-error (e)
      (case (socket-error-code e)
        ((+eagain+ +ewouldblock+) 
         ;; No pending connections, continue
         )
        (+emfile+
         ;; Too many open files, wait and retry
         (sleep 0.1))
        (t (error e))))))
```

### Performance Monitoring

```lisp
(defun monitor-epoll-performance (epfd)
  "Monitor epoll event loop performance"
  (let ((start-time (get-internal-real-time))
        (event-count 0))
    (loop
      (let ((events (wait-for-events epfd 100 1000)))
        (incf event-count (length events))
        (when (zerop (mod event-count 1000))
          (let ((elapsed (/ (- (get-internal-real-time) start-time)
                           internal-time-units-per-second)))
            (format t "Events per second: ~,2F~%"
                    (/ event-count elapsed))))))))
```

## Dependencies

- **epsilon.core** - Core utilities and data structures
- **epsilon.foreign** - Foreign function interface
- **Linux kernel 2.6+** - Required for epoll support
- **SBCL** - Steel Bank Common Lisp

The Epsilon Linux package provides high-performance, Linux-specific networking capabilities essential for building scalable server applications with excellent resource utilization and throughput characteristics.
# epsilon.lib.net

Interim networking package providing cross-platform socket programming based on sb-bsd-sockets.

## Overview

epsilon.lib.net is an interim solution for network programming while the full epsilon.net async implementation is being developed. It provides a clean API wrapper around SBCL's sb-bsd-sockets for basic TCP/UDP networking.

## Design Goals

- **Cross-platform compatibility**: Works on macOS, Linux, and Windows
- **Clean API**: Simple, functional interface
- **Error handling**: Custom conditions for network errors
- **Resource management**: Automatic cleanup with context managers

## Exported Symbols

### Socket Creation

#### `make-tcp-socket` → socket
Creates a new TCP socket.

```lisp
(defparameter *socket* (net:make-tcp-socket))
```

#### `make-udp-socket` → socket  
Creates a new UDP socket.

```lisp
(defparameter *socket* (net:make-udp-socket))
```

### Server Operations

#### `make-listener` port &key address reuse-address backlog → socket
Creates a TCP listener socket bound to the specified port.

**Parameters:**
- `port` - Port number to bind to
- `address` - IP address to bind to (default: "0.0.0.0")
- `reuse-address` - Enable SO_REUSEADDR (default: t)
- `backlog` - Listen queue size (default: 5)

```lisp
;; Listen on all interfaces
(defparameter *listener* (net:make-listener 8080))

;; Listen on specific interface
(defparameter *listener* (net:make-listener 8080 :address "127.0.0.1"))
```

#### `accept-connection` listener &key timeout → socket
Accepts a connection from a listener socket.

**Parameters:**
- `listener` - Listener socket
- `timeout` - Timeout in seconds (not yet implemented)

```lisp
(defparameter *client* (net:accept-connection *listener*))
```

### Client Operations

#### `connect-tcp` host port &key timeout → socket
Connects to a TCP server.

**Parameters:**
- `host` - Hostname or IP address
- `port` - Port number
- `timeout` - Connection timeout in seconds (not yet implemented)

```lisp
(defparameter *socket* (net:connect-tcp "example.com" 80))
(defparameter *socket* (net:connect-tcp "127.0.0.1" 8080))
```

### Stream Operations

#### `socket-stream` socket &key input output buffering → stream
Creates a bidirectional stream from a socket.

**Parameters:**
- `socket` - Socket object
- `input` - Enable input (default: t)
- `output` - Enable output (default: t)  
- `buffering` - Buffering mode (default: :full)

```lisp
(defparameter *stream* (net:socket-stream *socket*))
(write-line "Hello, server!" *stream*)
(force-output *stream*)
(read-line *stream*)
```

### Socket Management

#### `close-socket` socket → nil
Closes a socket and releases resources.

```lisp
(net:close-socket *socket*)
```

### Address Utilities

#### `resolve-hostname` hostname → ip-vector
Resolves a hostname to an IPv4 address vector.

**Parameters:**
- `hostname` - Hostname string or IP address

**Returns:** 4-element vector representing IPv4 address

```lisp
(net:resolve-hostname "localhost")        ; → #(127 0 0 1)
(net:resolve-hostname "192.168.1.1")      ; → #(192 168 1 1)
(net:resolve-hostname "example.com")      ; → #(93 184 216 34)
```

#### `parse-address` address-string → host, port
Parses an address string in "host:port" format.

**Parameters:**
- `address-string` - String in "host:port" format

**Returns:** Two values: host string and port number

```lisp
(multiple-value-bind (host port) (net:parse-address "example.com:8080")
  (format t "Host: ~A, Port: ~A~%" host port))
; → Host: example.com, Port: 8080

(multiple-value-bind (host port) (net:parse-address "localhost")
  (format t "Host: ~A, Port: ~A~%" host port))
; → Host: localhost, Port: 80
```

### Socket Options

#### `set-socket-option` socket option value → nil
Sets a socket option.

**Supported Options:**
- `:reuse-address` - SO_REUSEADDR
- `:keep-alive` - SO_KEEPALIVE
- `:tcp-no-delay` - TCP_NODELAY

```lisp
(net:set-socket-option *socket* :reuse-address t)
(net:set-socket-option *socket* :tcp-no-delay t)
```

#### `get-socket-option` socket option → value
Gets a socket option value.

```lisp
(net:get-socket-option *socket* :reuse-address)  ; → T
```

### High-Level Utilities

#### `with-tcp-connection` (var host port &rest options) &body body
Establishes a TCP connection with automatic cleanup.

```lisp
(net:with-tcp-connection (socket "example.com" 80)
  (let ((stream (net:socket-stream socket)))
    (format stream "GET / HTTP/1.1~C~CHost: example.com~C~C~C~C"
            #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed)
    (force-output stream)
    (read-line stream)))
```

#### `with-tcp-listener` (var port &rest options) &body body  
Creates a TCP listener with automatic cleanup.

```lisp
(net:with-tcp-listener (listener 8080 :address "127.0.0.1")
  (loop
    (let ((client (net:accept-connection listener)))
      (when client
        (unwind-protect
            (let ((stream (net:socket-stream client)))
              (write-line "Hello from server!" stream)
              (force-output stream))
          (net:close-socket client))))))
```

## Conditions

### `network-error` (error)
Base condition for all network-related errors.

**Slots:**
- `message` - Error message string

### `connection-error` (network-error)
Signaled when connection operations fail.

### `timeout-error` (network-error)  
Signaled when operations exceed timeout limits.

## Example Usage

### Simple Echo Server

```lisp
(defun echo-server (port)
  "Simple echo server that repeats back what clients send"
  (net:with-tcp-listener (listener port :address "127.0.0.1")
    (format t "Echo server listening on port ~A~%" port)
    (loop
      (let ((client (net:accept-connection listener)))
        (when client
          (handler-case
              (let ((stream (net:socket-stream client)))
                (format t "Client connected~%")
                (loop for line = (read-line stream nil nil)
                      while line
                      do (format stream "Echo: ~A~%" line)
                         (force-output stream)
                         (format t "Echoed: ~A~%" line)))
            (error (e)
              (format t "Client error: ~A~%" e)))
          (net:close-socket client)
          (format t "Client disconnected~%"))))))
```

### HTTP GET Client

```lisp
(defun simple-http-get (host port path)
  "Simple HTTP GET request"
  (net:with-tcp-connection (socket host port)
    (let ((stream (net:socket-stream socket)))
      ;; Send HTTP request
      (format stream "GET ~A HTTP/1.1~C~CHost: ~A~C~CConnection: close~C~C~C~C"
              path #\Return #\Linefeed host #\Return #\Linefeed 
              #\Return #\Linefeed #\Return #\Linefeed)
      (force-output stream)
      
      ;; Read response
      (let ((response (make-string-output-stream)))
        (loop for line = (read-line stream nil nil)
              while line
              do (write-line line response))
        (get-output-stream-string response)))))
```

### JSON API Client

```lisp
(defun api-request (host port endpoint data)
  "Send JSON data to an API endpoint"
  (net:with-tcp-connection (socket host port)
    (let ((stream (net:socket-stream socket))
          (json-data (json:encode data))
          (content-length (length json-data)))
      
      ;; Send HTTP POST with JSON
      (format stream "POST ~A HTTP/1.1~C~C" endpoint #\Return #\Linefeed)
      (format stream "Host: ~A~C~C" host #\Return #\Linefeed)
      (format stream "Content-Type: application/json~C~C" #\Return #\Linefeed)
      (format stream "Content-Length: ~A~C~C" content-length #\Return #\Linefeed)
      (format stream "Connection: close~C~C" #\Return #\Linefeed)
      (format stream "~C~C" #\Return #\Linefeed)
      (write-string json-data stream)
      (force-output stream)
      
      ;; Read response
      (read-line stream))))
```

## Error Handling

```lisp
(handler-case
    (net:connect-tcp "unreachable-host.example" 80)
  (net:connection-error (e)
    (format t "Connection failed: ~A~%" (net:error-message e)))
  (net:network-error (e)
    (format t "Network error: ~A~%" (net:error-message e))))
```

## Platform Notes

### Cross-Platform Compatibility
- Works on macOS, Linux, and Windows via sb-bsd-sockets
- Hostname resolution uses system DNS
- IPv4 addresses supported, IPv6 support depends on SBCL

### Performance Characteristics
- **Blocking I/O**: All operations are synchronous
- **Single-threaded**: Each socket blocks the calling thread
- **Memory**: Minimal overhead per socket
- **Scalability**: Limited by thread count and blocking nature

## Migration Path

epsilon.lib.net is designed as an interim solution. When epsilon.net (async networking) becomes available:

1. **API compatibility**: Most functions will have direct equivalents
2. **Async upgrade**: Operations will become non-blocking
3. **Performance improvement**: Event-driven I/O will scale better
4. **Platform optimization**: Native event loops (kqueue/epoll/IOCP)

## See Also

- [epsilon.net](../../net/index.md) - Future async networking implementation
- [epsilon.http](../../http/index.md) - HTTP client/server built on networking
- [Examples](../../examples.md) - More networking examples and patterns
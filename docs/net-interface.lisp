;;;; This file defines the networking API that all platform packages
;;;; (epsilon.darwin, epsilon.linux, epsilon.windows) must implement.

(defpackage :epsilon.net
  (:use :cl)
  (:export
   ;; Address types
   #:address
   #:ipv4-address
   #:ipv6-address
   #:socket-address
   
   ;; Socket types
   #:tcp-listener
   #:tcp-stream
   #:udp-socket
   
   ;; TCP Listener operations
   #:tcp-bind
   #:tcp-accept
   #:tcp-incoming
   #:tcp-try-accept
   #:tcp-poll-accept
   #:tcp-local-addr
   
   ;; TCP Stream operations
   #:tcp-connect
   #:tcp-read
   #:tcp-write
   #:tcp-write-all
   #:tcp-flush
   #:tcp-try-read
   #:tcp-try-write
   #:tcp-poll-read
   #:tcp-poll-write
   #:tcp-peer-addr
   #:tcp-shutdown
   #:tcp-stream-reader
   #:tcp-stream-writer
   
   ;; UDP operations
   #:udp-bind
   #:udp-connect
   #:udp-send
   #:udp-recv
   #:udp-send-to
   #:udp-recv-from
   #:udp-local-addr
   
   ;; Address resolution
   #:make-socket-address
   #:resolve-address
   #:socket-address-ip
   #:socket-address-port
   #:parse-address
   
   ;; Socket options
   #:set-socket-option
   #:get-socket-option
   
   ;; Error conditions
   #:network-error
   #:connection-refused
   #:connection-reset
   #:connection-aborted
   #:timeout-error
   #:address-in-use
   #:would-block-error
   
   ;; Status checks
   #:tcp-connected-p))

(in-package :epsilon.net)

;;;; Interface Documentation
;;;;
;;;; All platform implementations must provide the following functions
;;;; with the specified signatures and semantics.

;;; ============================================================================
;;; TCP API
;;; ============================================================================

;;; TCP Listener - Server socket that accepts connections
;;; ----------------------------------------------------------------------------

;; (tcp-bind address) → tcp-listener
;;   Create a TCP listener bound to the specified address.
;;   The listener is immediately ready to accept connections.
;;   
;;   Parameters:
;;     address - A socket-address or string like "127.0.0.1:8080"
;;   
;;   Returns:
;;     A tcp-listener object
;;   
;;   Signals:
;;     address-in-use - If the address is already bound
;;     network-error - For other binding errors

;; (tcp-accept listener) → (values tcp-stream socket-address)
;;   Accept a new incoming connection. Blocks until a connection is available.
;;   
;;   Parameters:
;;     listener - A tcp-listener
;;   
;;   Returns:
;;     tcp-stream - The connection to the client
;;     socket-address - The client's address
;;   
;;   Signals:
;;     network-error - If accept fails

;; (tcp-incoming listener) → sequence
;;   Returns an iterator/sequence over incoming connections.
;;   Each element is a cons of (tcp-stream . socket-address).
;;   
;;   Parameters:
;;     listener - A tcp-listener
;;   
;;   Returns:
;;     A sequence that yields connections

;; (tcp-try-accept listener) → (or (values tcp-stream socket-address) :would-block)
;;   Try to accept a connection without blocking.
;;   
;;   Parameters:
;;     listener - A tcp-listener
;;   
;;   Returns:
;;     On success: (values tcp-stream socket-address)
;;     If would block: :would-block

;; (tcp-poll-accept listener waker) → (or (values tcp-stream socket-address) :pending)
;;   Poll for a connection (async operation).
;;   
;;   Parameters:
;;     listener - A tcp-listener
;;     waker - A waker/callback for async notification
;;   
;;   Returns:
;;     On success: (values tcp-stream socket-address)
;;     If pending: :pending (waker will be called when ready)

;; (tcp-local-addr listener) → socket-address
;;   Get the local address the listener is bound to.
;;   
;;   Parameters:
;;     listener - A tcp-listener
;;   
;;   Returns:
;;     The socket-address the listener is bound to

;;; TCP Stream - Connected TCP socket
;;; ----------------------------------------------------------------------------

;; (tcp-connect address) → tcp-stream
;;   Connect to a remote TCP server. Blocks until connected.
;;   
;;   Parameters:
;;     address - A socket-address or string like "example.com:80"
;;   
;;   Returns:
;;     A connected tcp-stream
;;   
;;   Signals:
;;     connection-refused - If connection is refused
;;     timeout-error - If connection times out
;;     network-error - For other connection errors

;; (tcp-read stream buffer &key (start 0) end) → bytes-read
;;   Read data from the stream into a buffer.
;;   
;;   Parameters:
;;     stream - A tcp-stream
;;     buffer - A byte array to read into
;;     start - Starting index in buffer (default 0)
;;     end - Ending index in buffer (default length of buffer)
;;   
;;   Returns:
;;     Number of bytes read (0 indicates EOF)
;;   
;;   Signals:
;;     connection-reset - If connection was reset
;;     network-error - For other read errors

;; (tcp-write stream data &key (start 0) end) → bytes-written
;;   Write data to the stream.
;;   
;;   Parameters:
;;     stream - A tcp-stream
;;     data - Byte array or string to write
;;     start - Starting index in data (default 0)
;;     end - Ending index in data (default length of data)
;;   
;;   Returns:
;;     Number of bytes written
;;   
;;   Signals:
;;     connection-reset - If connection was reset
;;     network-error - For other write errors

;; (tcp-write-all stream data)
;;   Write all data, retrying as needed until complete.
;;   
;;   Parameters:
;;     stream - A tcp-stream
;;     data - Byte array or string to write
;;   
;;   Signals:
;;     connection-reset - If connection was reset
;;     network-error - For other write errors

;; (tcp-flush stream)
;;   Flush any buffered data to the network.
;;   
;;   Parameters:
;;     stream - A tcp-stream
;;   
;;   Signals:
;;     network-error - If flush fails

;; (tcp-try-read stream buffer) → (or bytes-read :would-block)
;;   Try to read without blocking.
;;   
;;   Parameters:
;;     stream - A tcp-stream
;;     buffer - A byte array to read into
;;   
;;   Returns:
;;     Number of bytes read, or :would-block

;; (tcp-try-write stream data) → (or bytes-written :would-block)
;;   Try to write without blocking.
;;   
;;   Parameters:
;;     stream - A tcp-stream
;;     data - Data to write
;;   
;;   Returns:
;;     Number of bytes written, or :would-block

;; (tcp-poll-read stream waker) → (or bytes-available :pending)
;;   Poll for read readiness (async operation).
;;   
;;   Parameters:
;;     stream - A tcp-stream
;;     waker - A waker/callback for async notification
;;   
;;   Returns:
;;     Number of bytes available, or :pending

;; (tcp-poll-write stream waker) → (or :ready :pending)
;;   Poll for write readiness (async operation).
;;   
;;   Parameters:
;;     stream - A tcp-stream
;;     waker - A waker/callback for async notification
;;   
;;   Returns:
;;     :ready if can write, or :pending

;; (tcp-peer-addr stream) → socket-address
;;   Get the remote peer's address.
;;   
;;   Parameters:
;;     stream - A tcp-stream
;;   
;;   Returns:
;;     The peer's socket-address

;; (tcp-local-addr stream) → socket-address
;;   Get the local address of the socket.
;;   
;;   Parameters:
;;     stream - A tcp-stream
;;   
;;   Returns:
;;     The local socket-address

;; (tcp-shutdown stream how)
;;   Shutdown the TCP connection.
;;   
;;   Parameters:
;;     stream - A tcp-stream
;;     how - One of :read, :write, or :both
;;   
;;   Signals:
;;     network-error - If shutdown fails

;; (tcp-stream-reader stream) → input-stream
;;   Get a Lisp input stream for reading.
;;   
;;   Parameters:
;;     stream - A tcp-stream
;;   
;;   Returns:
;;     A Common Lisp input stream

;; (tcp-stream-writer stream) → output-stream
;;   Get a Lisp output stream for writing.
;;   
;;   Parameters:
;;     stream - A tcp-stream
;;   
;;   Returns:
;;     A Common Lisp output stream

;; (tcp-connected-p stream) → boolean
;;   Check if the stream is still connected.
;;   
;;   Parameters:
;;     stream - A tcp-stream
;;   
;;   Returns:
;;     T if connected, NIL otherwise

;;; ============================================================================
;;; UDP API
;;; ============================================================================

;; (udp-bind address) → udp-socket
;;   Create a UDP socket bound to an address.
;;   
;;   Parameters:
;;     address - A socket-address or string
;;   
;;   Returns:
;;     A udp-socket

;; (udp-connect socket address)
;;   Connect UDP socket to a default peer.
;;   
;;   Parameters:
;;     socket - A udp-socket
;;     address - The peer's address

;; (udp-send socket data address) → bytes-sent
;;   Send data to a specific address.
;;   
;;   Parameters:
;;     socket - A udp-socket
;;     data - Data to send
;;     address - Destination address
;;   
;;   Returns:
;;     Number of bytes sent

;; (udp-recv socket buffer) → (values bytes-read socket-address)
;;   Receive data from any sender.
;;   
;;   Parameters:
;;     socket - A udp-socket
;;     buffer - Buffer to receive into
;;   
;;   Returns:
;;     bytes-read - Number of bytes received
;;     socket-address - Sender's address

;; (udp-send-to socket data address) → bytes-sent
;;   Alias for udp-send for compatibility.

;; (udp-recv-from socket buffer) → (values bytes-read socket-address)  
;;   Alias for udp-recv for compatibility.

;; (udp-local-addr socket) → socket-address
;;   Get the local address of the UDP socket.
;;   
;;   Parameters:
;;     socket - A udp-socket
;;   
;;   Returns:
;;     The local socket-address

;;; ============================================================================
;;; Address Resolution
;;; ============================================================================

;; (make-socket-address host port) → socket-address
;;   Create a socket address from host and port.
;;   
;;   Parameters:
;;     host - IP address string or hostname
;;     port - Port number
;;   
;;   Returns:
;;     A socket-address object

;; (resolve-address address-spec) → (list socket-address...)
;;   Resolve an address specification to socket addresses.
;;   
;;   Parameters:
;;     address-spec - Can be:
;;       - String like "host:port" or "host"
;;       - List like (host port)
;;       - socket-address (returns as single-element list)
;;   
;;   Returns:
;;     List of socket-address objects

;; (socket-address-ip address) → ip-address
;;   Extract the IP address component.
;;   
;;   Parameters:
;;     address - A socket-address
;;   
;;   Returns:
;;     The IP address as a string

;; (socket-address-port address) → integer
;;   Extract the port number.
;;   
;;   Parameters:
;;     address - A socket-address
;;   
;;   Returns:
;;     The port number

;; (parse-address string) → (values host port)
;;   Parse an address string into components.
;;   
;;   Parameters:
;;     string - Address like "host:port"
;;   
;;   Returns:
;;     host - Host string
;;     port - Port number or NIL

;;; ============================================================================
;;; Socket Options
;;; ============================================================================

;; (set-socket-option socket option value)
;;   Set a socket option.
;;   
;;   Parameters:
;;     socket - Any socket type
;;     option - Option keyword (see below)
;;     value - Option value
;;   
;;   Valid options:
;;     :reuse-address - Allow address reuse (boolean)
;;     :keep-alive - Enable keep-alive (boolean)
;;     :broadcast - Enable broadcast (boolean)
;;     :linger - Set linger time (integer seconds or NIL)
;;     :recv-buffer - Receive buffer size (integer)
;;     :send-buffer - Send buffer size (integer)
;;     :recv-timeout - Receive timeout (integer milliseconds)
;;     :send-timeout - Send timeout (integer milliseconds)
;;     :nodelay - Disable Nagle's algorithm (boolean)

;; (get-socket-option socket option) → value
;;   Get a socket option value.
;;   
;;   Parameters:
;;     socket - Any socket type
;;     option - Option keyword
;;   
;;   Returns:
;;     The current option value

;;; ============================================================================
;;; Error Conditions
;;; ============================================================================

;; All network operations may signal these conditions:

(define-condition network-error (error)
  ((message :initarg :message :reader error-message))
  (:documentation "Base condition for network errors"))

(define-condition connection-refused (network-error)
  ()
  (:documentation "Connection was refused by the remote host"))

(define-condition connection-reset (network-error)
  ()
  (:documentation "Connection was reset by peer"))

(define-condition connection-aborted (network-error)
  ()
  (:documentation "Connection was aborted"))

(define-condition timeout-error (network-error)
  ()
  (:documentation "Operation timed out"))

(define-condition address-in-use (network-error)
  ()
  (:documentation "Address is already in use"))

(define-condition would-block-error (network-error)
  ()
  (:documentation "Operation would block (for non-blocking operations)"))

;;; ============================================================================
;;; Implementation Notes
;;; ============================================================================

;; 1. Platform implementations should use their native async mechanisms:
;;    - Darwin: kqueue
;;    - Linux: epoll  
;;    - Windows: IOCP
;;
;; 2. The tcp-poll-* and tcp-try-* functions enable async/non-blocking I/O
;;    without requiring a specific async runtime.
;;
;; 3. All blocking operations should be interruptible.
;;
;; 4. Socket options use keywords instead of numeric constants for portability.
;;
;; 5. Implementations should handle both IPv4 and IPv6 transparently.
;;
;; 6. The tcp-stream-reader and tcp-stream-writer functions should return
;;    gray streams or native CL streams that integrate with FORMAT, READ, etc.

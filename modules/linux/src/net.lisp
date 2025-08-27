;;;; Linux Networking Implementation - Refactored
;;;;
;;;; This module provides the epsilon.net interface for Linux systems
;;;; using the new modular structure with unified socket operations.

(defpackage #:epsilon.net
  (:use #:cl)
  (:local-nicknames
   (#:tcp #:epsilon.net.tcp)
   (#:udp #:epsilon.net.udp)
   (#:types #:epsilon.net.types)
   (#:errors #:epsilon.net.errors)
   (#:address #:epsilon.net.address))
  (:export
   ;; Address types
   #:socket-address
   #:socket-address-ip
   #:socket-address-port
   #:make-socket-address
   
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
   #:tcp-stream-handle
   #:tcp-connected-p
   
   ;; UDP operations
   #:udp-bind
   #:udp-connect
   #:udp-send
   #:udp-recv
   #:udp-send-to
   #:udp-recv-from
   #:udp-local-addr
   
   ;; Address resolution
   #:resolve-address
   #:parse-address
   
   ;; Socket options (placeholder)
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
   
   ;; High-level utilities
   #:with-tcp-server
   #:with-tcp-connection
   #:with-udp-socket))

(in-package :epsilon.net)

;;; ============================================================================
;;; Address Operations
;;; ============================================================================

(defun make-socket-address (ip port)
  "Create a socket address from IP string and port number"
  (address:make-socket-address ip port))

(defun parse-address (address-string)
  "Parse an address string like '127.0.0.1:8080' into socket-address"
  (address:parse-address address-string))

(defun resolve-address (hostname port)
  "Resolve hostname to socket addresses"
  (address:resolve-address hostname port))

;;; ============================================================================
;;; TCP Operations
;;; ============================================================================

(defun tcp-bind (address &key (backlog 128) (reuse-addr t))
  "Create a TCP listener bound to the specified address"
  (tcp:tcp-bind address :backlog backlog :reuse-addr reuse-addr))

(defun tcp-accept (listener &key (timeout nil))
  "Accept a connection, with optional timeout in seconds"
  (tcp:tcp-accept listener :timeout timeout))

(defun tcp-try-accept (listener)
  "Try to accept a connection without blocking"
  (tcp:tcp-try-accept listener))

(defun tcp-poll-accept (listener timeout-ms)
  "Poll for incoming connections with timeout"
  (tcp:tcp-poll-accept listener timeout-ms))

(defun tcp-local-addr (listener)
  "Get the local address of a TCP listener"
  (tcp:tcp-local-addr listener))

(defun tcp-incoming (listener)
  "Return a list of incoming connections"
  (tcp:tcp-incoming listener))

(defun tcp-connect (address &key (timeout nil))
  "Connect to a TCP server with optional timeout"
  (tcp:tcp-connect address :timeout timeout))

(defun tcp-read (stream buffer &key (start 0) (end (length buffer)) (timeout nil))
  "Read data from TCP stream into buffer with optional timeout"
  (tcp:tcp-read stream buffer :start start :end end :timeout timeout))

(defun tcp-write (stream data &key (start 0) (end nil) (timeout nil))
  "Write data to TCP stream with optional timeout"
  (tcp:tcp-write stream data :start start :end end :timeout timeout))

(defun tcp-write-all (stream data &key (start 0) (end nil))
  "Write all data to TCP stream"
  (tcp:tcp-write-all stream data :start start :end end))

(defun tcp-flush (stream)
  "Flush any buffered output"
  (tcp:tcp-flush stream))

(defun tcp-try-read (stream buffer &key (start 0) (end (length buffer)))
  "Try to read without blocking"
  (tcp:tcp-try-read stream buffer :start start :end end))

(defun tcp-try-write (stream data &key (start 0) (end nil))
  "Try to write without blocking"
  (tcp:tcp-try-write stream data :start start :end end))

(defun tcp-poll-read (stream timeout-ms)
  "Poll for readable data with timeout"
  (tcp:tcp-poll-read stream timeout-ms))

(defun tcp-poll-write (stream timeout-ms)
  "Poll for writability with timeout"
  (tcp:tcp-poll-write stream timeout-ms))

(defun tcp-peer-addr (stream)
  "Get peer address of TCP stream"
  (tcp:tcp-peer-addr stream))

(defun tcp-shutdown (stream &key (how :both))
  "Shutdown TCP stream"
  (tcp:tcp-shutdown stream :how how))

(defun tcp-stream-reader (stream)
  "Get input stream for TCP stream"
  (tcp:tcp-stream-reader stream))

(defun tcp-stream-writer (stream)
  "Get output stream for TCP stream"
  (tcp:tcp-stream-writer stream))

(defun tcp-stream-handle (stream)
  "Get the OS handle of a TCP stream"
  (types:tcp-stream-handle stream))

(defun tcp-connected-p (stream)
  "Check if TCP stream is connected"
  (tcp:tcp-connected-p stream))

;;; ============================================================================
;;; UDP Operations
;;; ============================================================================

(defun udp-bind (address)
  "Create a UDP socket bound to address"
  (udp:udp-bind address))

(defun udp-connect (socket address)
  "Connect UDP socket to remote address"
  (udp:udp-connect socket address))

(defun udp-send (socket data &key (start 0) (end nil))
  "Send data on connected UDP socket"
  (udp:udp-send socket data :start start :end end))

(defun udp-recv (socket buffer &key (start 0) (end (length buffer)))
  "Receive data on UDP socket"
  (udp:udp-recv socket buffer :start start :end end))

(defun udp-send-to (socket data address &key (start 0) (end nil))
  "Send data to specific address"
  (udp:udp-send-to socket data address :start start :end end))

(defun udp-recv-from (socket buffer &key (start 0) (end (length buffer)))
  "Receive data and sender address"
  (udp:udp-recv-from socket buffer :start start :end end))

(defun udp-local-addr (socket)
  "Get local address of UDP socket"
  (udp:udp-local-addr socket))

;;; ============================================================================
;;; Socket Options
;;; ============================================================================

(defun set-socket-option (socket-obj option value)
  "Set a socket option"
  (let ((handle (etypecase socket-obj
                  (types:tcp-listener (types:tcp-listener-handle socket-obj))
                  (types:tcp-stream (types:tcp-stream-handle socket-obj))
                  (types:udp-socket (types:udp-socket-handle socket-obj)))))
    (ecase option
      (:reuse-address 
       (epsilon.net.core:set-socket-option handle epsilon.net.constants:+sol-socket+ 
                                           epsilon.net.constants:+so-reuseaddr+ value))
      (:keep-alive 
       (epsilon.net.core:set-socket-option handle epsilon.net.constants:+sol-socket+ 
                                           epsilon.net.constants:+so-keepalive+ value))
      (:tcp-nodelay 
       (epsilon.net.core:set-socket-option handle epsilon.net.constants:+ipproto-tcp-level+ 
                                           epsilon.net.constants:+tcp-nodelay+ value))
      (:broadcast 
       (epsilon.net.core:set-socket-option handle epsilon.net.constants:+sol-socket+ 
                                           epsilon.net.constants:+so-broadcast+ value)))))

(defun get-socket-option (socket-obj option)
  "Get a socket option"
  (let ((handle (etypecase socket-obj
                  (types:tcp-listener (types:tcp-listener-handle socket-obj))
                  (types:tcp-stream (types:tcp-stream-handle socket-obj))
                  (types:udp-socket (types:udp-socket-handle socket-obj)))))
    (ecase option
      (:reuse-address 
       (not (zerop (epsilon.net.core:get-socket-option handle epsilon.net.constants:+sol-socket+ 
                                                       epsilon.net.constants:+so-reuseaddr+))))
      (:keep-alive 
       (not (zerop (epsilon.net.core:get-socket-option handle epsilon.net.constants:+sol-socket+ 
                                                       epsilon.net.constants:+so-keepalive+))))
      (:tcp-nodelay 
       (not (zerop (epsilon.net.core:get-socket-option handle epsilon.net.constants:+ipproto-tcp-level+ 
                                                       epsilon.net.constants:+tcp-nodelay+))))
      (:broadcast 
       (not (zerop (epsilon.net.core:get-socket-option handle epsilon.net.constants:+sol-socket+ 
                                                       epsilon.net.constants:+so-broadcast+))))
      (:recv-buffer 
       (epsilon.net.core:get-socket-option handle epsilon.net.constants:+sol-socket+ 
                                           epsilon.net.constants:+so-rcvbuf+))
      (:send-buffer 
       (epsilon.net.core:get-socket-option handle epsilon.net.constants:+sol-socket+ 
                                           epsilon.net.constants:+so-sndbuf+)))))

;;; ============================================================================
;;; High-Level Utilities
;;; ============================================================================

(defmacro with-tcp-server ((server address &key (backlog 128) (reuse-addr t)) &body body)
  "Execute body with a TCP server bound to address"
  `(tcp:with-tcp-server (,server ,address :backlog ,backlog :reuse-addr ,reuse-addr)
     ,@body))

(defmacro with-tcp-connection ((connection address &key (timeout nil)) &body body)
  "Execute body with a TCP connection to address"
  `(tcp:with-tcp-connection (,connection ,address :timeout ,timeout)
     ,@body))

(defmacro with-udp-socket ((socket address) &body body)
  "Execute body with a UDP socket bound to address"
  `(udp:with-udp-socket (,socket ,address)
     ,@body))

;;; ============================================================================
;;; Backward Compatibility Layer
;;; ============================================================================

;; This section provides compatibility with the existing test suite and
;; any code that depends on the old monolithic interface. These functions
;; delegate to the appropriate new modular implementations.
;; 
;; NOTE: New code should use the modular interfaces directly rather than
;; these compatibility functions.

;; Utility functions
(defun create-socket (family type protocol)
  "Internal: Create socket (for test compatibility)"
  (epsilon.net.core:create-socket family type protocol))

(defun make-sockaddr-in-into (addr ip port)
  "Internal: Create sockaddr_in (for test compatibility)" 
  (address:make-sockaddr-in-into addr ip port))

(defun parse-sockaddr-in (addr)
  "Internal: Parse sockaddr_in (for test compatibility)"
  (address:parse-sockaddr-in addr))

(defun get-local-address (fd)
  "Internal: Get local address (for test compatibility)"
  (epsilon.net.core:get-local-address fd))

(defun get-errno ()
  "Internal: Get errno (for test compatibility)"
  (errors:get-errno))

(defun errno-to-string (errno)
  "Internal: Convert errno to string (for test compatibility)"
  (errors:errno-to-string errno))

(defun check-error (result operation)
  "Internal: Check error (for test compatibility)"
  (errors:check-error result operation))

(defun set-nonblocking (fd)
  "Internal: Set nonblocking (for test compatibility)"
  (epsilon.net.core:set-nonblocking fd))

(defun set-socket-reuse-addr (fd enable)
  "Internal: Set SO_REUSEADDR (for test compatibility)"
  (epsilon.net.core:set-socket-reuse-addr fd enable))

(defun split-string (string delimiter)
  "Internal: Split string (for test compatibility)"
  (address:split-string string delimiter))

;; Constants for backward compatibility
(defconstant +af-inet+ epsilon.net.constants:+af-inet+
  "Address family constant for backward compatibility")
(defconstant +sock-stream+ epsilon.net.constants:+sock-stream+
  "Socket type constant for backward compatibility")
(defconstant +sock-dgram+ epsilon.net.constants:+sock-dgram+
  "Socket type constant for backward compatibility") 
(defconstant +ipproto-tcp+ epsilon.net.constants:+ipproto-tcp+
  "Protocol constant for backward compatibility")
(defconstant +ipproto-udp+ epsilon.net.constants:+ipproto-udp+
  "Protocol constant for backward compatibility")

;; FFI function wrappers for backward compatibility
(defun %close (fd)
  "Internal: Close file descriptor (for test compatibility)"
  (epsilon.net.constants:%close fd))

(defun %socket (domain type protocol)
  "Internal: Create socket (for test compatibility)"
  (epsilon.net.constants:%socket domain type protocol))

(defun %bind (sockfd addr addrlen)
  "Internal: Bind socket (for test compatibility)"
  (epsilon.net.constants:%bind sockfd addr addrlen))

(defun %listen (sockfd backlog)
  "Internal: Listen on socket (for test compatibility)"
  (epsilon.net.constants:%listen sockfd backlog))

(defun %accept (sockfd addr addrlen)
  "Internal: Accept connection (for test compatibility)"
  (epsilon.net.constants:%accept sockfd addr addrlen))

(defun %connect (sockfd addr addrlen)
  "Internal: Connect socket (for test compatibility)"
  (epsilon.net.constants:%connect sockfd addr addrlen))

(defun %send (sockfd buf len flags)
  "Internal: Send data (for test compatibility)"
  (epsilon.net.constants:%send sockfd buf len flags))

(defun %recv (sockfd buf len flags)
  "Internal: Receive data (for test compatibility)"
  (epsilon.net.constants:%recv sockfd buf len flags))

(defun %sendto (sockfd buf len flags dest-addr addrlen)
  "Internal: Send data to address (for test compatibility)"
  (epsilon.net.constants:%sendto sockfd buf len flags dest-addr addrlen))

(defun %recvfrom (sockfd buf len flags src-addr addrlen)
  "Internal: Receive data with source address (for test compatibility)"
  (epsilon.net.constants:%recvfrom sockfd buf len flags src-addr addrlen))

(defun %shutdown (sockfd how)
  "Internal: Shutdown socket (for test compatibility)"
  (epsilon.net.constants:%shutdown sockfd how))

(defun %setsockopt (sockfd level optname optval optlen)
  "Internal: Set socket option (for test compatibility)"
  (epsilon.net.constants:%setsockopt sockfd level optname optval optlen))

(defun %getsockopt (sockfd level optname optval optlen)
  "Internal: Get socket option (for test compatibility)"
  (epsilon.net.constants:%getsockopt sockfd level optname optval optlen))

(defun %getsockname (sockfd addr addrlen)
  "Internal: Get socket name (for test compatibility)"
  (epsilon.net.constants:%getsockname sockfd addr addrlen))

(defun %getpeername (sockfd addr addrlen)
  "Internal: Get peer name (for test compatibility)"
  (epsilon.net.constants:%getpeername sockfd addr addrlen))

(defun %fcntl (fd cmd arg)
  "Internal: File control (for test compatibility)"
  (epsilon.net.constants:%fcntl fd cmd arg))
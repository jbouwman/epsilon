;;;; Network Constants for Linux
;;;;
;;;; This module defines all network-related constants for Linux systems.

(defpackage epsilon.net.constants
  (:use cl)
  (:local-nicknames
   (lib epsilon.foreign))
  (:export
   ;; Address families
   #:+af-inet+
   #:+af-inet6+
   
   ;; Socket types  
   #:+sock-stream+
   #:+sock-dgram+
   
   ;; Protocols
   #:+ipproto-tcp+
   #:+ipproto-udp+
   
   ;; Socket options
   #:+sol-socket+
   #:+so-reuseaddr+
   #:+so-keepalive+
   #:+so-broadcast+
   #:+so-linger+
   #:+so-rcvbuf+
   #:+so-sndbuf+
   #:+so-rcvtimeo+
   #:+so-sndtimeo+
   
   ;; TCP options
   #:+ipproto-tcp-level+
   #:+tcp-nodelay+
   
   ;; Shutdown options
   #:+shut-rd+
   #:+shut-wr+
   #:+shut-rdwr+
   
   ;; fcntl commands
   #:+f-getfl+
   #:+f-setfl+
   #:+o-nonblock+
   
   ;; Errno values
   #:+eagain+
   #:+ewouldblock+
   #:+einprogress+
   #:+eaddrinuse+
   #:+econnrefused+
   #:+econnreset+
   #:+etimedout+
   #:+enotconn+
   
   ;; FFI Bindings
   #:%socket
   #:%bind
   #:%listen
   #:%accept
   #:%connect
   #:%send
   #:%recv
   #:%sendto
   #:%recvfrom
   #:%close
   #:%shutdown
   #:%setsockopt
   #:%getsockopt
   #:%getsockname
   #:%getpeername
   #:%fcntl))

(in-package epsilon.net.constants)

;;; ============================================================================
;;; Address Families
;;; ============================================================================

(defconstant +af-inet+ 2
  "IPv4 address family")

(defconstant +af-inet6+ 10
  "IPv6 address family")

;;; ============================================================================
;;; Socket Types
;;; ============================================================================

(defconstant +sock-stream+ 1
  "Stream socket (TCP)")

(defconstant +sock-dgram+ 2
  "Datagram socket (UDP)")

;;; ============================================================================
;;; Protocols
;;; ============================================================================

(defconstant +ipproto-tcp+ 6
  "TCP protocol")

(defconstant +ipproto-udp+ 17
  "UDP protocol")

;;; ============================================================================
;;; Socket Options
;;; ============================================================================

(defconstant +sol-socket+ 1
  "Socket level options")

(defconstant +so-reuseaddr+ 2
  "Allow reuse of local addresses")

(defconstant +so-keepalive+ 9
  "Keep connections alive")

(defconstant +so-broadcast+ 6
  "Allow broadcast messages")

(defconstant +so-linger+ 13
  "Linger on close if data present")

(defconstant +so-rcvbuf+ 8
  "Receive buffer size")

(defconstant +so-sndbuf+ 7
  "Send buffer size")

(defconstant +so-rcvtimeo+ 20
  "Receive timeout")

(defconstant +so-sndtimeo+ 21
  "Send timeout")

;;; ============================================================================
;;; TCP Options
;;; ============================================================================

(defconstant +ipproto-tcp-level+ 6
  "TCP protocol level for socket options")

(defconstant +tcp-nodelay+ 1
  "Disable Nagle's algorithm")

;;; ============================================================================
;;; Shutdown Options
;;; ============================================================================

(defconstant +shut-rd+ 0
  "Shutdown read half of connection")

(defconstant +shut-wr+ 1
  "Shutdown write half of connection")

(defconstant +shut-rdwr+ 2
  "Shutdown both halves of connection")

;;; ============================================================================
;;; File Control
;;; ============================================================================

(defconstant +f-getfl+ 3
  "Get file descriptor flags")

(defconstant +f-setfl+ 4
  "Set file descriptor flags")

(defconstant +o-nonblock+ #o4000
  "Non-blocking I/O flag")

;;; ============================================================================
;;; Error Numbers
;;; ============================================================================

(defconstant +eagain+ 11
  "Resource temporarily unavailable")

(defconstant +ewouldblock+ 11
  "Operation would block (same as EAGAIN on Linux)")

(defconstant +einprogress+ 115
  "Operation now in progress")

(defconstant +eaddrinuse+ 98
  "Address already in use")

(defconstant +econnrefused+ 111
  "Connection refused")

(defconstant +econnreset+ 104
  "Connection reset by peer")

(defconstant +etimedout+ 110
  "Connection timed out")

(defconstant +enotconn+ 107
  "Socket is not connected")

;;; ============================================================================
;;; FFI Bindings
;;; ============================================================================

(lib:defshared %socket "socket" "libc" :int 
  (domain :int) (type :int) (protocol :int)
  :documentation "Create socket")

(lib:defshared %bind "bind" "libc" :int
  (sockfd :int) (addr :pointer) (addrlen :unsigned-int)
  :documentation "Bind socket to address")

(lib:defshared %listen "listen" "libc" :int
  (sockfd :int) (backlog :int)
  :documentation "Listen for connections")

(lib:defshared %accept "accept" "libc" :int
  (sockfd :int) (addr :pointer) (addrlen :pointer)
  :documentation "Accept connection")

(lib:defshared %connect "connect" "libc" :int
  (sockfd :int) (addr :pointer) (addrlen :unsigned-int)
  :documentation "Connect socket")

(lib:defshared %send "send" "libc" :long
  (sockfd :int) (buf :pointer) (len :unsigned-long) (flags :int)
  :documentation "Send data on socket")

(lib:defshared %recv "recv" "libc" :long
  (sockfd :int) (buf :pointer) (len :unsigned-long) (flags :int)
  :documentation "Receive data from socket")

(lib:defshared %sendto "sendto" "libc" :long
  (sockfd :int) (buf :pointer) (len :unsigned-long) (flags :int)
  (dest-addr :pointer) (addrlen :unsigned-int)
  :documentation "Send data to specific address")

(lib:defshared %recvfrom "recvfrom" "libc" :long
  (sockfd :int) (buf :pointer) (len :unsigned-long) (flags :int)
  (src-addr :pointer) (addrlen :pointer)
  :documentation "Receive data and sender address")

(lib:defshared %close "close" "libc" :int (fd :int)
  :documentation "Close file descriptor")

(lib:defshared %shutdown "shutdown" "libc" :int
  (sockfd :int) (how :int)
  :documentation "Shutdown socket")

(lib:defshared %setsockopt "setsockopt" "libc" :int
  (sockfd :int) (level :int) (optname :int) (optval :pointer) (optlen :unsigned-int)
  :documentation "Set socket option")

(lib:defshared %getsockopt "getsockopt" "libc" :int
  (sockfd :int) (level :int) (optname :int) (optval :pointer) (optlen :pointer)
  :documentation "Get socket option")

(lib:defshared %getsockname "getsockname" "libc" :int
  (sockfd :int) (addr :pointer) (addrlen :pointer)
  :documentation "Get socket's own address")

(lib:defshared %getpeername "getpeername" "libc" :int
  (sockfd :int) (addr :pointer) (addrlen :pointer)
  :documentation "Get peer's address")

(lib:defshared %fcntl "fcntl" "libc" :int
  (fd :int) (cmd :int) (arg :int)
  :documentation "File control operations")
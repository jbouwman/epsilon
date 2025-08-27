;;;; Darwin/BSD network constants and FFI bindings

(defpackage epsilon.net.constants
  (:use cl)
  (:local-nicknames
   (lib epsilon.foreign))
  (:export
   ;; Address families
   +af-inet+
   +af-inet6+
   
   ;; Socket types
   +sock-stream+
   +sock-dgram+
   
   ;; Protocols
   +ipproto-tcp+
   +ipproto-udp+
   
   ;; Socket options
   +sol-socket+
   +so-reuseaddr+
   +so-keepalive+
   +so-broadcast+
   +so-linger+
   +so-rcvbuf+
   +so-sndbuf+
   +so-rcvtimeo+
   +so-sndtimeo+
   
   ;; TCP options
   +ipproto-tcp-level+
   +tcp-nodelay+
   
   ;; Shutdown options
   +shut-rd+
   +shut-wr+
   +shut-rdwr+
   
   ;; IPv6 constants
   +ipv6-addr-size+
   +sockaddr-in6-size+
   
   ;; File control
   +f-getfl+
   +f-setfl+
   +o-nonblock+
   
   ;; FFI functions
   %socket
   %bind
   %listen
   %accept
   %connect
   %send
   %recv
   %sendto
   %recvfrom
   %close
   %shutdown
   %setsockopt
   %getsockopt
   %getsockname
   %getpeername
   %fcntl))

(in-package epsilon.net.constants)

;;; ============================================================================
;;; Darwin Constants
;;; ============================================================================

;; Address families
(defconstant +af-inet+ 2)
(defconstant +af-inet6+ 30)

;; Socket types  
(defconstant +sock-stream+ 1)
(defconstant +sock-dgram+ 2)

;; Protocols
(defconstant +ipproto-tcp+ 6)
(defconstant +ipproto-udp+ 17)

;; Socket options
(defconstant +sol-socket+ #xffff)
(defconstant +so-reuseaddr+ #x0004)
(defconstant +so-keepalive+ #x0008)
(defconstant +so-broadcast+ #x0020)
(defconstant +so-linger+ #x0080)
(defconstant +so-rcvbuf+ #x1002)
(defconstant +so-sndbuf+ #x1001)
(defconstant +so-rcvtimeo+ #x1006)
(defconstant +so-sndtimeo+ #x1005)

;; TCP options
(defconstant +ipproto-tcp-level+ 6)
(defconstant +tcp-nodelay+ #x01)

;; Shutdown options
(defconstant +shut-rd+ 0)
(defconstant +shut-wr+ 1)
(defconstant +shut-rdwr+ 2)

;; IPv6 constants
(defconstant +ipv6-addr-size+ 16)
(defconstant +sockaddr-in6-size+ 28)

;; fcntl commands
(defconstant +f-getfl+ 3)
(defconstant +f-setfl+ 4)
(defconstant +o-nonblock+ #x0004)

;;; ============================================================================
;;; FFI Bindings
;;; ============================================================================

(lib:defshared %socket "socket" "libc" :int 
  ((domain :int) (type :int) (protocol :int))
  :documentation "Create socket")

(lib:defshared %bind "bind" "libc" :int
  ((sockfd :int) (addr :pointer) (addrlen :unsigned-int))
  :documentation "Bind socket to address")

(lib:defshared %listen "listen" "libc" :int
  ((sockfd :int) (backlog :int))
  :documentation "Listen for connections")

(lib:defshared %accept "accept" "libc" :int
  ((sockfd :int) (addr :pointer) (addrlen :pointer))
  :documentation "Accept connection")

(lib:defshared %connect "connect" "libc" :int
  ((sockfd :int) (addr :pointer) (addrlen :unsigned-int))
  :documentation "Connect socket")

(lib:defshared %send "send" "libc" :long
  ((sockfd :int) (buf :pointer) (len :unsigned-long) (flags :int))
  :documentation "Send data on socket")

(lib:defshared %recv "recv" "libc" :long
  ((sockfd :int) (buf :pointer) (len :unsigned-long) (flags :int))
  :documentation "Receive data from socket")

(lib:defshared %sendto "sendto" "libc" :long
  ((sockfd :int) (buf :pointer) (len :unsigned-long) (flags :int)
   (dest-addr :pointer) (addrlen :unsigned-int))
  :documentation "Send data to specific address")

(lib:defshared %recvfrom "recvfrom" "libc" :long
  ((sockfd :int) (buf :pointer) (len :unsigned-long) (flags :int)
   (src-addr :pointer) (addrlen :pointer))
  :documentation "Receive data and sender address")

(lib:defshared %close "close" "libc" :int ((fd :int))
  :documentation "Close file descriptor")

(lib:defshared %shutdown "shutdown" "libc" :int
  ((sockfd :int) (how :int))
  :documentation "Shutdown socket")

(lib:defshared %setsockopt "setsockopt" "libc" :int
  ((sockfd :int) (level :int) (optname :int) (optval :pointer) (optlen :unsigned-int))
  :documentation "Set socket option")

(lib:defshared %getsockopt "getsockopt" "libc" :int
  ((sockfd :int) (level :int) (optname :int) (optval :pointer) (optlen :pointer))
  :documentation "Get socket option")

(lib:defshared %getsockname "getsockname" "libc" :int
  ((sockfd :int) (addr :pointer) (addrlen :pointer))
  :documentation "Get socket's own address")

(lib:defshared %getpeername "getpeername" "libc" :int
  ((sockfd :int) (addr :pointer) (addrlen :pointer))
  :documentation "Get peer's address")

(lib:defshared %fcntl "fcntl" "libc" :int
  ((fd :int) (cmd :int) (arg :long))
  :documentation "File control operations")
;;;; Linux Networking Constants
;;;;
;;;; All Linux-specific networking constants in one place

(defpackage epsilon.net.constants
  (:use cl)
  (:export
   ;; Address families
   #:+af-unspec+
   #:+af-inet+
   #:+af-inet6+
   
   ;; Socket types
   #:+sock-stream+
   #:+sock-dgram+
   #:+sock-raw+
   
   ;; Protocols
   #:+ipproto-ip+
   #:+ipproto-tcp+
   #:+ipproto-udp+
   
   ;; Socket options levels
   #:+sol-socket+
   #:+ipproto-tcp-level+
   
   ;; Socket options
   #:+so-reuseaddr+
   #:+so-keepalive+
   #:+so-broadcast+
   #:+so-linger+
   #:+so-rcvbuf+
   #:+so-sndbuf+
   #:+so-rcvtimeo+
   #:+so-sndtimeo+
   #:+so-error+
   #:+so-type+
   
   ;; TCP options
   #:+tcp-nodelay+
   #:+tcp-keepidle+
   #:+tcp-keepintvl+
   #:+tcp-keepcnt+
   
   ;; Shutdown options
   #:+shut-rd+
   #:+shut-wr+
   #:+shut-rdwr+
   
   ;; fcntl commands
   #:+f-getfl+
   #:+f-setfl+
   #:+o-nonblock+
   
   ;; Error codes (errno values)
   #:+eperm+
   #:+enoent+
   #:+eintr+
   #:+eio+
   #:+ebadf+
   #:+eagain+
   #:+ewouldblock+
   #:+eacces+
   #:+efault+
   #:+enotblk+
   #:+ebusy+
   #:+eexist+
   #:+exdev+
   #:+enodev+
   #:+enotdir+
   #:+eisdir+
   #:+einval+
   #:+enfile+
   #:+emfile+
   #:+enotty+
   #:+etxtbsy+
   #:+efbig+
   #:+enospc+
   #:+espipe+
   #:+erofs+
   #:+emlink+
   #:+epipe+
   #:+edom+
   #:+erange+
   #:+edeadlk+
   #:+enametoolong+
   #:+enolck+
   #:+enosys+
   #:+enotempty+
   #:+eloop+
   #:+enomsg+
   #:+eidrm+
   #:+echrng+
   #:+el2nsync+
   #:+el3hlt+
   #:+el3rst+
   #:+elnrng+
   #:+eunatch+
   #:+enocsi+
   #:+el2hlt+
   #:+ebade+
   #:+ebadr+
   #:+exfull+
   #:+enoano+
   #:+ebadrqc+
   #:+ebadslt+
   #:+ebfont+
   #:+enostr+
   #:+enodata+
   #:+etime+
   #:+enosr+
   #:+enonet+
   #:+enopkg+
   #:+eremote+
   #:+enolink+
   #:+eadv+
   #:+esrmnt+
   #:+ecomm+
   #:+eproto+
   #:+emultihop+
   #:+edotdot+
   #:+ebadmsg+
   #:+eoverflow+
   #:+enotuniq+
   #:+ebadfd+
   #:+eremchg+
   #:+elibacc+
   #:+elibbad+
   #:+elibscn+
   #:+elibmax+
   #:+elibexec+
   #:+eilseq+
   #:+erestart+
   #:+estrpipe+
   #:+eusers+
   #:+enotsock+
   #:+edestaddrreq+
   #:+emsgsize+
   #:+eprototype+
   #:+enoprotoopt+
   #:+eprotonosupport+
   #:+esocktnosupport+
   #:+eopnotsupp+
   #:+epfnosupport+
   #:+eafnosupport+
   #:+eaddrinuse+
   #:+eaddrnotavail+
   #:+enetdown+
   #:+enetunreach+
   #:+enetreset+
   #:+econnaborted+
   #:+econnreset+
   #:+enobufs+
   #:+eisconn+
   #:+enotconn+
   #:+eshutdown+
   #:+etoomanyrefs+
   #:+etimedout+
   #:+econnrefused+
   #:+ehostdown+
   #:+ehostunreach+
   #:+ealready+
   #:+einprogress+
   #:+estale+
   #:+euclean+
   #:+enotnam+
   #:+enavail+
   #:+eisnam+
   #:+eremoteio+
   #:+edquot+
   
   ;; FFI function imports
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

(defconstant +af-unspec+ 0 "Unspecified address family")
(defconstant +af-inet+ 2 "IPv4 Internet protocols")
(defconstant +af-inet6+ 10 "IPv6 Internet protocols")

;;; ============================================================================
;;; Socket Types
;;; ============================================================================

(defconstant +sock-stream+ 1 "Sequenced, reliable, two-way connection-based byte streams")
(defconstant +sock-dgram+ 2 "Connectionless, unreliable datagrams")
(defconstant +sock-raw+ 3 "Raw network protocol access")

;;; ============================================================================
;;; Protocols
;;; ============================================================================

(defconstant +ipproto-ip+ 0 "Dummy protocol for IP")
(defconstant +ipproto-tcp+ 6 "Transmission Control Protocol")
(defconstant +ipproto-udp+ 17 "User Datagram Protocol")

;;; ============================================================================
;;; Socket Options
;;; ============================================================================

;; Socket level
(defconstant +sol-socket+ 1 "Socket level options")

;; SO_* socket options
(defconstant +so-reuseaddr+ 2 "Allow local address reuse")
(defconstant +so-type+ 3 "Get socket type")
(defconstant +so-error+ 4 "Get and clear error status")
(defconstant +so-broadcast+ 6 "Permit broadcast messages")
(defconstant +so-sndbuf+ 7 "Send buffer size")
(defconstant +so-rcvbuf+ 8 "Receive buffer size")
(defconstant +so-keepalive+ 9 "Keep connections alive")
(defconstant +so-linger+ 13 "Linger on close if unsent data")
(defconstant +so-rcvtimeo+ 20 "Receive timeout")
(defconstant +so-sndtimeo+ 21 "Send timeout")

;; TCP level
(defconstant +ipproto-tcp-level+ 6 "TCP protocol level")

;; TCP options
(defconstant +tcp-nodelay+ 1 "Don't delay sending to coalesce packets")
(defconstant +tcp-keepidle+ 4 "Start keepalives after this idle period")
(defconstant +tcp-keepintvl+ 5 "Interval between keepalives")
(defconstant +tcp-keepcnt+ 6 "Number of keepalives before death")

;;; ============================================================================
;;; Shutdown Options
;;; ============================================================================

(defconstant +shut-rd+ 0 "Shut down reading side")
(defconstant +shut-wr+ 1 "Shut down writing side")
(defconstant +shut-rdwr+ 2 "Shut down both sides")

;;; ============================================================================
;;; File Control
;;; ============================================================================

(defconstant +f-getfl+ 3 "Get file status flags")
(defconstant +f-setfl+ 4 "Set file status flags")
(defconstant +o-nonblock+ #o4000 "Non-blocking I/O")

;;; ============================================================================
;;; Error Codes (errno)
;;; ============================================================================

(defconstant +eperm+ 1 "Operation not permitted")
(defconstant +enoent+ 2 "No such file or directory")
(defconstant +eintr+ 4 "Interrupted system call")
(defconstant +eio+ 5 "I/O error")
(defconstant +ebadf+ 9 "Bad file descriptor")
(defconstant +eagain+ 11 "Try again")
(defconstant +ewouldblock+ 11 "Operation would block")
(defconstant +eacces+ 13 "Permission denied")
(defconstant +efault+ 14 "Bad address")
(defconstant +enotblk+ 15 "Block device required")
(defconstant +ebusy+ 16 "Device or resource busy")
(defconstant +eexist+ 17 "File exists")
(defconstant +exdev+ 18 "Cross-device link")
(defconstant +enodev+ 19 "No such device")
(defconstant +enotdir+ 20 "Not a directory")
(defconstant +eisdir+ 21 "Is a directory")
(defconstant +einval+ 22 "Invalid argument")
(defconstant +enfile+ 23 "File table overflow")
(defconstant +emfile+ 24 "Too many open files")
(defconstant +enotty+ 25 "Not a typewriter")
(defconstant +etxtbsy+ 26 "Text file busy")
(defconstant +efbig+ 27 "File too large")
(defconstant +enospc+ 28 "No space left on device")
(defconstant +espipe+ 29 "Illegal seek")
(defconstant +erofs+ 30 "Read-only file system")
(defconstant +emlink+ 31 "Too many links")
(defconstant +epipe+ 32 "Broken pipe")
(defconstant +edom+ 33 "Math argument out of domain")
(defconstant +erange+ 34 "Math result not representable")
(defconstant +edeadlk+ 35 "Resource deadlock would occur")
(defconstant +enametoolong+ 36 "File name too long")
(defconstant +enolck+ 37 "No record locks available")
(defconstant +enosys+ 38 "Function not implemented")
(defconstant +enotempty+ 39 "Directory not empty")
(defconstant +eloop+ 40 "Too many symbolic links encountered")
(defconstant +enomsg+ 42 "No message of desired type")
(defconstant +eidrm+ 43 "Identifier removed")
(defconstant +echrng+ 44 "Channel number out of range")
(defconstant +el2nsync+ 45 "Level 2 not synchronized")
(defconstant +el3hlt+ 46 "Level 3 halted")
(defconstant +el3rst+ 47 "Level 3 reset")
(defconstant +elnrng+ 48 "Link number out of range")
(defconstant +eunatch+ 49 "Protocol driver not attached")
(defconstant +enocsi+ 50 "No CSI structure available")
(defconstant +el2hlt+ 51 "Level 2 halted")
(defconstant +ebade+ 52 "Invalid exchange")
(defconstant +ebadr+ 53 "Invalid request descriptor")
(defconstant +exfull+ 54 "Exchange full")
(defconstant +enoano+ 55 "No anode")
(defconstant +ebadrqc+ 56 "Invalid request code")
(defconstant +ebadslt+ 57 "Invalid slot")
(defconstant +ebfont+ 59 "Bad font file format")
(defconstant +enostr+ 60 "Device not a stream")
(defconstant +enodata+ 61 "No data available")
(defconstant +etime+ 62 "Timer expired")
(defconstant +enosr+ 63 "Out of streams resources")
(defconstant +enonet+ 64 "Machine is not on the network")
(defconstant +enopkg+ 65 "Package not installed")
(defconstant +eremote+ 66 "Object is remote")
(defconstant +enolink+ 67 "Link has been severed")
(defconstant +eadv+ 68 "Advertise error")
(defconstant +esrmnt+ 69 "Srmount error")
(defconstant +ecomm+ 70 "Communication error on send")
(defconstant +eproto+ 71 "Protocol error")
(defconstant +emultihop+ 72 "Multihop attempted")
(defconstant +edotdot+ 73 "RFS specific error")
(defconstant +ebadmsg+ 74 "Not a data message")
(defconstant +eoverflow+ 75 "Value too large for defined data type")
(defconstant +enotuniq+ 76 "Name not unique on network")
(defconstant +ebadfd+ 77 "File descriptor in bad state")
(defconstant +eremchg+ 78 "Remote address changed")
(defconstant +elibacc+ 79 "Cannot access needed shared library")
(defconstant +elibbad+ 80 "Accessing corrupted shared library")
(defconstant +elibscn+ 81 ".lib section in a.out corrupted")
(defconstant +elibmax+ 82 "Attempting to link in too many shared libraries")
(defconstant +elibexec+ 83 "Cannot exec a shared library directly")
(defconstant +eilseq+ 84 "Illegal byte sequence")
(defconstant +erestart+ 85 "Interrupted system call should be restarted")
(defconstant +estrpipe+ 86 "Streams pipe error")
(defconstant +eusers+ 87 "Too many users")
(defconstant +enotsock+ 88 "Socket operation on non-socket")
(defconstant +edestaddrreq+ 89 "Destination address required")
(defconstant +emsgsize+ 90 "Message too long")
(defconstant +eprototype+ 91 "Protocol wrong type for socket")
(defconstant +enoprotoopt+ 92 "Protocol not available")
(defconstant +eprotonosupport+ 93 "Protocol not supported")
(defconstant +esocktnosupport+ 94 "Socket type not supported")
(defconstant +eopnotsupp+ 95 "Operation not supported on transport endpoint")
(defconstant +epfnosupport+ 96 "Protocol family not supported")
(defconstant +eafnosupport+ 97 "Address family not supported by protocol")
(defconstant +eaddrinuse+ 98 "Address already in use")
(defconstant +eaddrnotavail+ 99 "Cannot assign requested address")
(defconstant +enetdown+ 100 "Network is down")
(defconstant +enetunreach+ 101 "Network is unreachable")
(defconstant +enetreset+ 102 "Network dropped connection because of reset")
(defconstant +econnaborted+ 103 "Software caused connection abort")
(defconstant +econnreset+ 104 "Connection reset by peer")
(defconstant +enobufs+ 105 "No buffer space available")
(defconstant +eisconn+ 106 "Transport endpoint is already connected")
(defconstant +enotconn+ 107 "Transport endpoint is not connected")
(defconstant +eshutdown+ 108 "Cannot send after transport endpoint shutdown")
(defconstant +etoomanyrefs+ 109 "Too many references: cannot splice")
(defconstant +etimedout+ 110 "Connection timed out")
(defconstant +econnrefused+ 111 "Connection refused")
(defconstant +ehostdown+ 112 "Host is down")
(defconstant +ehostunreach+ 113 "No route to host")
(defconstant +ealready+ 114 "Operation already in progress")
(defconstant +einprogress+ 115 "Operation now in progress")
(defconstant +estale+ 116 "Stale file handle")
(defconstant +euclean+ 117 "Structure needs cleaning")
(defconstant +enotnam+ 118 "Not a XENIX named type file")
(defconstant +enavail+ 119 "No XENIX semaphores available")
(defconstant +eisnam+ 120 "Is a named type file")
(defconstant +eremoteio+ 121 "Remote I/O error")
(defconstant +edquot+ 122 "Quota exceeded")

;;; ============================================================================
;;; FFI Bindings
;;; ============================================================================

(epsilon.foreign:defshared %socket "socket" "libc" :int 
  (domain :int) (type :int) (protocol :int)
  :documentation "Create socket")

(epsilon.foreign:defshared %bind "bind" "libc" :int
  (sockfd :int) (addr :pointer) (addrlen :unsigned-int)
  :documentation "Bind socket to address")

(epsilon.foreign:defshared %listen "listen" "libc" :int
  (sockfd :int) (backlog :int)
  :documentation "Listen for connections")

(epsilon.foreign:defshared %accept "accept" "libc" :int
  (sockfd :int) (addr :pointer) (addrlen :pointer)
  :documentation "Accept connection")

(epsilon.foreign:defshared %connect "connect" "libc" :int
  (sockfd :int) (addr :pointer) (addrlen :unsigned-int)
  :documentation "Connect socket")

(epsilon.foreign:defshared %send "send" "libc" :long
  (sockfd :int) (buf :pointer) (len :unsigned-long) (flags :int)
  :documentation "Send data on socket")

(epsilon.foreign:defshared %recv "recv" "libc" :long
  (sockfd :int) (buf :pointer) (len :unsigned-long) (flags :int)
  :documentation "Receive data from socket")

(epsilon.foreign:defshared %sendto "sendto" "libc" :long
  (sockfd :int) (buf :pointer) (len :unsigned-long) (flags :int)
  (dest-addr :pointer) (addrlen :unsigned-int)
  :documentation "Send data to specific address")

(epsilon.foreign:defshared %recvfrom "recvfrom" "libc" :long
  (sockfd :int) (buf :pointer) (len :unsigned-long) (flags :int)
  (src-addr :pointer) (addrlen :pointer)
  :documentation "Receive data and sender address")

(epsilon.foreign:defshared %close "close" "libc" :int (fd :int)
  :documentation "Close file descriptor")

(epsilon.foreign:defshared %shutdown "shutdown" "libc" :int
  (sockfd :int) (how :int)
  :documentation "Shutdown socket")

(epsilon.foreign:defshared %setsockopt "setsockopt" "libc" :int
  (sockfd :int) (level :int) (optname :int) (optval :pointer) (optlen :unsigned-int)
  :documentation "Set socket option")

(epsilon.foreign:defshared %getsockopt "getsockopt" "libc" :int
  (sockfd :int) (level :int) (optname :int) (optval :pointer) (optlen :pointer)
  :documentation "Get socket option")

(epsilon.foreign:defshared %getsockname "getsockname" "libc" :int
  (sockfd :int) (addr :pointer) (addrlen :pointer)
  :documentation "Get socket's own address")

(epsilon.foreign:defshared %getpeername "getpeername" "libc" :int
  (sockfd :int) (addr :pointer) (addrlen :pointer)
  :documentation "Get peer's address")

(epsilon.foreign:defshared %fcntl "fcntl" "libc" :int
  (fd :int) (cmd :int) (arg :int)
  :documentation "File control operations")
;;;; core.lisp - Linux network implementation using epsilon.foreign

(defpackage :epsilon.net.core
  (:use :cl)
  (:local-nicknames
   (lib epsilon.foreign)
   (seq epsilon.sequence))
  (:export
   ;; Socket operations
   #:socket-create
   #:socket-bind
   #:socket-listen
   #:socket-accept
   #:socket-connect
   #:socket-send
   #:socket-recv
   #:socket-close
   
   ;; Address creation
   #:make-inet-address
   #:make-inet6-address
   
   ;; Socket options
   #:set-socket-option
   #:get-socket-option
   
   ;; High-level API
   #:with-socket
   #:with-server-socket
   #:with-client-socket
   
   ;; Constants
   #:+AF-INET+
   #:+AF-INET6+
   #:+AF-UNIX+
   #:+SOCK-STREAM+
   #:+SOCK-DGRAM+
   #:+SOCK-RAW+
   #:+IPPROTO-TCP+
   #:+IPPROTO-UDP+
   #:+SOL-SOCKET+
   #:+SO-REUSEADDR+
   #:+SO-KEEPALIVE+
   #:+SO-BROADCAST+
   #:+SO-RCVBUF+
   #:+SO-SNDBUF+))

(in-package :epsilon.net.core)

;;;; Constants

;; Address families
(defconstant +AF-UNIX+  1)   ; Unix domain sockets
(defconstant +AF-INET+  2)   ; Internet IP Protocol
(defconstant +AF-INET6+ 10)  ; IP version 6

;; Socket types
(defconstant +SOCK-STREAM+ 1) ; TCP
(defconstant +SOCK-DGRAM+  2) ; UDP  
(defconstant +SOCK-RAW+    3) ; Raw protocol

;; Protocol numbers
(defconstant +IPPROTO-TCP+ 6)
(defconstant +IPPROTO-UDP+ 17)

;; Socket option levels
(defconstant +SOL-SOCKET+ 1)

;; Socket options
(defconstant +SO-REUSEADDR+ 2)
(defconstant +SO-KEEPALIVE+ 9)
(defconstant +SO-BROADCAST+ 6)
(defconstant +SO-RCVBUF+ 8)
(defconstant +SO-SNDBUF+ 7)

;;;; FFI Function Definitions using epsilon.foreign

(lib:defshared %socket "socket" "libc" :int
  (domain :int) (type :int) (protocol :int)
  :documentation "Create a socket")

(lib:defshared %bind "bind" "libc" :int
  (sockfd :int) (addr :pointer) (addrlen :unsigned-int)
  :documentation "Bind socket to address")

(lib:defshared %listen "listen" "libc" :int
  (sockfd :int) (backlog :int)
  :documentation "Listen for connections")

(lib:defshared %accept "accept" "libc" :int
  (sockfd :int) (addr :pointer) (addrlen :pointer)
  :documentation "Accept a connection")

(lib:defshared %connect "connect" "libc" :int
  (sockfd :int) (addr :pointer) (addrlen :unsigned-int)
  :documentation "Connect to a socket")

(lib:defshared %send "send" "libc" :long
  (sockfd :int) (buf :pointer) (len :unsigned-long) (flags :int)
  :documentation "Send data on socket")

(lib:defshared %recv "recv" "libc" :long
  (sockfd :int) (buf :pointer) (len :unsigned-long) (flags :int)
  :documentation "Receive data from socket")

(lib:defshared %close "close" "libc" :int
  (fd :int)
  :documentation "Close socket")

(lib:defshared %setsockopt "setsockopt" "libc" :int
  (sockfd :int) (level :int) (optname :int) (optval :pointer) (optlen :unsigned-int)
  :documentation "Set socket option")

(lib:defshared %getsockopt "getsockopt" "libc" :int
  (sockfd :int) (level :int) (optname :int) (optval :pointer) (optlen :pointer)
  :documentation "Get socket option")

;; Network byte order functions
(lib:defshared %htons "htons" "libc" :unsigned-short
  (hostshort :unsigned-short)
  :documentation "Host to network short")

(lib:defshared %htonl "htonl" "libc" :unsigned-int
  (hostlong :unsigned-int)
  :documentation "Host to network long")

(lib:defshared %ntohs "ntohs" "libc" :unsigned-short
  (netshort :unsigned-short)
  :documentation "Network to host short")

(lib:defshared %ntohl "ntohl" "libc" :unsigned-int
  (netlong :unsigned-int)
  :documentation "Network to host long")

;;;; Address Structure Helpers

;; struct sockaddr_in {
;;     sa_family_t    sin_family; /* address family: AF_INET */
;;     in_port_t      sin_port;   /* port in network byte order */
;;     struct in_addr sin_addr;   /* internet address */
;;     char           sin_zero[8]; /* pad to size of struct sockaddr */
;; };

(defun pack-sockaddr-in (buffer family port address)
  "Pack sockaddr_in structure into buffer"
  ;; sin_family (2 bytes)
  (setf (sb-sys:sap-ref-16 buffer 0) family)
  ;; sin_port (2 bytes) - network byte order
  (setf (sb-sys:sap-ref-16 buffer 2) (%htons port))
  ;; sin_addr (4 bytes) - already in network byte order
  (setf (sb-sys:sap-ref-32 buffer 4) address)
  ;; sin_zero (8 bytes) - zero padding
  (loop for i from 8 below 16
        do (setf (sb-sys:sap-ref-8 buffer i) 0)))

(defun make-inet-address (ip-string port)
  "Create an IPv4 socket address"
  (let ((addr (lib:foreign-alloc 16))) ; sockaddr_in size
    ;; Parse IP address
    (let ((parts (loop for part in (seq:realize (epsilon.string:split #\. ip-string))
                       collect (parse-integer part))))
      (unless (= (length parts) 4)
        (error "Invalid IPv4 address: ~A" ip-string))
      (let ((ip-value (logior (ash (first parts) 24)
                              (ash (second parts) 16)
                              (ash (third parts) 8)
                              (fourth parts))))
        ;; Pack the structure
        (pack-sockaddr-in addr +AF-INET+ port (%htonl ip-value))
        addr))))

(defun make-inet6-address (ip-string port)
  "Create an IPv6 socket address (stub)"
  ;; TODO: Implement IPv6 address creation
  (error "IPv6 not yet implemented"))

;;;; Socket Operations

(defun socket-create (domain type protocol)
  "Create a new socket"
  (let ((sockfd (%socket domain type protocol)))
    (when (< sockfd 0)
      (error "Failed to create socket"))
    sockfd))

(defun socket-bind (socket address address-len)
  "Bind socket to address"
  (let ((result (%bind socket address address-len)))
    (when (< result 0)
      (error "Failed to bind socket"))
    result))

(defun socket-listen (socket backlog)
  "Listen for connections on socket"
  (let ((result (%listen socket backlog)))
    (when (< result 0)
      (error "Failed to listen on socket"))
    result))

(defun socket-accept (socket)
  "Accept a connection on socket"
  (lib:with-foreign-memory ((addr 16)    ; sockaddr_in
                            (addrlen 4))  ; socklen_t
    (setf (sb-sys:sap-ref-32 addrlen 0) 16)
    (let ((newsock (%accept socket addr addrlen)))
      (when (< newsock 0)
        (error "Failed to accept connection"))
      newsock)))

(defun socket-connect (socket address address-len)
  "Connect socket to address"
  (let ((result (%connect socket address address-len)))
    (when (< result 0)
      (error "Failed to connect socket"))
    result))

(defun socket-send (socket data &key (flags 0))
  "Send data on socket"
  (etypecase data
    (string
     ;; Simple ASCII conversion for now
     (let ((bytes (make-array (length data) :element-type '(unsigned-byte 8))))
       (loop for i from 0 below (length data)
             do (setf (aref bytes i) (char-code (char data i))))
       (socket-send socket bytes :flags flags)))
    ((simple-array (unsigned-byte 8) (*))
     (lib:with-foreign-memory ((buffer (length data)))
       (loop for i from 0 below (length data)
             do (setf (sb-sys:sap-ref-8 buffer i) (aref data i)))
       (let ((sent (%send socket buffer (length data) flags)))
         (when (< sent 0)
           (error "Failed to send data"))
         sent)))))

(defun socket-recv (socket size &key (flags 0))
  "Receive data from socket"
  (lib:with-foreign-memory ((buffer size))
    (let ((received (%recv socket buffer size flags)))
      (cond
        ((< received 0)
         (error "Failed to receive data"))
        ((= received 0)
         nil) ; Connection closed
        (t
         ;; Convert to byte array
         (let ((data (make-array received :element-type '(unsigned-byte 8))))
           (loop for i from 0 below received
                 do (setf (aref data i) (sb-sys:sap-ref-8 buffer i)))
           data))))))

(defun socket-close (socket)
  "Close a socket"
  (%close socket))

(defun set-socket-option (socket level option value)
  "Set a socket option"
  (lib:with-foreign-memory ((optval 4))
    (setf (sb-sys:sap-ref-32 optval 0) value)
    (let ((result (%setsockopt socket level option optval 4)))
      (when (< result 0)
        (error "Failed to set socket option"))
      result)))

(defun get-socket-option (socket level option)
  "Get a socket option"
  (lib:with-foreign-memory ((optval 4)
                            (optlen 4))
    (setf (sb-sys:sap-ref-32 optlen 0) 4)
    (let ((result (%getsockopt socket level option optval optlen)))
      (when (< result 0)
        (error "Failed to get socket option"))
      (sb-sys:sap-ref-32 optval 0))))

;;;; High-level API

(defmacro with-socket ((socket-var domain type protocol) &body body)
  "Create and automatically close a socket"
  `(let ((,socket-var (socket-create ,domain ,type ,protocol)))
     (unwind-protect
          (progn ,@body)
       (socket-close ,socket-var))))

(defmacro with-server-socket ((socket-var port &key (backlog 5) (reuse-addr t)) 
                              &body body)
  "Create a TCP server socket bound to port"
  `(with-socket (,socket-var +AF-INET+ +SOCK-STREAM+ 0)
     (when ,reuse-addr
       (set-socket-option ,socket-var +SOL-SOCKET+ +SO-REUSEADDR+ 1))
     (let ((addr (make-inet-address "0.0.0.0" ,port)))
       (unwind-protect
            (progn
              (socket-bind ,socket-var addr 16)
              (socket-listen ,socket-var ,backlog)
              ,@body)
         (lib:foreign-free addr)))))

(defmacro with-client-socket ((socket-var host port) &body body)
  "Create a TCP client socket connected to host:port"
  `(with-socket (,socket-var +AF-INET+ +SOCK-STREAM+ 0)
     (let ((addr (make-inet-address ,host ,port)))
       (unwind-protect
            (progn
              (socket-connect ,socket-var addr 16)
              ,@body)
         (lib:foreign-free addr)))))
;;;; This module provides networking primitives for Linux systems
;;;; using epoll for event notification.

(defpackage #:epsilon.net
  (:use #:cl)
  (:local-nicknames
   (#:epoll #:epsilon.sys.epoll))
  (:shadow #:listen #:close)
  (:export
   ;; Core types & conditions
   #:socket #:listener #:connection
   #:network-error #:connection-error #:timeout-error
   
   ;; High-level operations
   #:with-connection #:with-listener #:with-timeout
   
   ;; Async
   #:async-connect #:async-accept #:async-read #:async-write
   #:future #:await #:select
   
   ;; Address handling
   #:address #:resolve #:parse-address
   #:ipv4 #:ipv6 #:hostname
   
   ;; Options & configuration
   #:socket-option #:set-socket-option #:get-socket-option
   #:keep-alive #:no-delay #:reuse-addr #:recv-buffer #:send-buffer
   
   ;; Socket stream access
   #:socket-stream #:socket-from-stream
   #:socket-peer-address
   
   ;; TLS/SSL
   #:secure-context #:make-secure-context
   #:secure-connection #:secure-listener
   
   ;; Socket operations
   #:socket-listen #:socket-connect #:socket-accept #:socket-close))

(in-package :epsilon.net)

(require :sb-bsd-sockets)

;;; Conditions

(define-condition network-error (error) 
  ((message :initarg :message :reader error-message)))

(define-condition connection-error (network-error)
  ())

(define-condition timeout-error (network-error)
  ())

;;; Core Types

(defclass socket ()
  ((handle :initarg :handle :reader socket-handle)
   (stream :initarg :stream :initform nil :accessor socket-stream-slot)
   (options :initform (make-hash-table) :reader socket-options)))

(defclass listener (socket) 
  ((backlog :initarg :backlog :reader listener-backlog)
   (address :initarg :address :reader listener-address)
   (port :initarg :port :reader listener-port)
   (epoll :initarg :epoll :reader listener-epoll)))

(defclass connection (socket)
  ((remote-address :initarg :remote-address :accessor connection-remote-address)
   (remote-port :initarg :remote-port :accessor connection-remote-port)))

;;; Socket Constants (Linux-specific)

;; Address families
(defconstant +af-inet+ 2)
(defconstant +af-inet6+ 10)

;; Socket types
(defconstant +sock-stream+ 1)
(defconstant +sock-dgram+ 2)

;; Protocols
(defconstant +ipproto-tcp+ 6)
(defconstant +ipproto-udp+ 17)

;;; Socket FFI Bindings using direct SBCL alien functions

(sb-alien:define-alien-routine ("socket" %socket) sb-alien:int
  (domain sb-alien:int) (type sb-alien:int) (protocol sb-alien:int))

(sb-alien:define-alien-routine ("bind" %bind) sb-alien:int
  (sockfd sb-alien:int) (addr sb-alien:system-area-pointer) (addrlen sb-alien:unsigned-int))

(sb-alien:define-alien-routine ("listen" %listen) sb-alien:int
  (sockfd sb-alien:int) (backlog sb-alien:int))

(sb-alien:define-alien-routine ("accept" %accept) sb-alien:int
  (sockfd sb-alien:int) (addr sb-alien:system-area-pointer) (addrlen sb-alien:system-area-pointer))

(sb-alien:define-alien-routine ("connect" %connect) sb-alien:int
  (sockfd sb-alien:int) (addr sb-alien:system-area-pointer) (addrlen sb-alien:unsigned-int))

(sb-alien:define-alien-routine ("send" %send) sb-alien:long
  (sockfd sb-alien:int) (buf sb-alien:system-area-pointer) (len sb-alien:unsigned-long) (flags sb-alien:int))

(sb-alien:define-alien-routine ("recv" %recv) sb-alien:long
  (sockfd sb-alien:int) (buf sb-alien:system-area-pointer) (len sb-alien:unsigned-long) (flags sb-alien:int))

(sb-alien:define-alien-routine ("close" %net-close) sb-alien:int
  (fd sb-alien:int))

;;; Address Handling (Linux sockaddr_in)

;; struct sockaddr_in {
;;     sa_family_t    sin_family; /* address family: AF_INET */
;;     in_port_t      sin_port;   /* port in network byte order */
;;     struct in_addr sin_addr;   /* internet address */
;;     char           sin_zero[8]; /* pad to sizeof(struct sockaddr) */
;; };

(defun make-sockaddr-in (ip-address port)
  "Create sockaddr_in structure for IPv4 (Linux version)"
  (sb-alien:with-alien ((addr (sb-alien:array sb-alien:char 16)))
    (let ((sap (sb-alien:alien-sap addr)))
      ;; sin_family (2 bytes)
      (setf (sb-sys:sap-ref-16 sap 0) +af-inet+)
      ;; sin_port (2 bytes, network byte order)
      (setf (sb-sys:sap-ref-16 sap 2)
            (logior (ash (logand port #xff) 8)
                    (ash (logand port #xff00) -8)))
      ;; sin_addr (4 bytes, network byte order)
      (let ((ip-parts (mapcar #'parse-integer 
                              (split-string ip-address #\.))))
        (setf (sb-sys:sap-ref-32 sap 4)
              (logior (ash (first ip-parts) 24)
                      (ash (second ip-parts) 16)
                      (ash (third ip-parts) 8)
                      (fourth ip-parts))))
      ;; sin_zero (8 bytes of zeros)
      (loop for i from 8 to 15
            do (setf (sb-sys:sap-ref-8 sap i) 0))
      sap)))

(defun split-string (string delimiter)
  "Split string by delimiter"
  (let ((parts '())
        (start 0))
    (loop for pos = (position delimiter string :start start)
          while pos
          do (push (subseq string start pos) parts)
             (setf start (1+ pos))
          finally (push (subseq string start) parts))
    (nreverse parts)))

;;; Socket Helper Functions

(defun create-tcp-socket ()
  "Create TCP socket"
  (let ((sock (%socket +af-inet+ +sock-stream+ +ipproto-tcp+)))
    (when (= sock -1)
      (error "Failed to create TCP socket"))
    sock))

(defun create-udp-socket ()
  "Create UDP socket"
  (let ((sock (%socket +af-inet+ +sock-dgram+ +ipproto-udp+)))
    (when (= sock -1)
      (error "Failed to create UDP socket"))
    sock))

(defun close-socket (socket-fd)
  "Close socket"
  (%net-close socket-fd))

;;; Core Socket Operations using epoll

(defun socket-listen (address port &key (reuse-address t) (backlog 5) type)
  "Create a listening socket on the given address and port using epoll"
  (declare (ignore type reuse-address)) ;; TODO: implement socket options
  (handler-case
      (let* ((socket-fd (create-tcp-socket))
             (epfd (epoll:epoll-create1))
             (host (if (or (string= address "0.0.0.0") (string= address "*"))
                       "0.0.0.0"
                       address)))
        
        ;; Bind socket
        (sb-alien:with-alien ((addr (sb-alien:array sb-alien:char 16)))
          (let ((sockaddr (make-sockaddr-in host port)))
            (loop for i from 0 to 15
                  do (setf (sb-alien:deref addr i) (sb-alien:deref sockaddr i)))
            (when (= (%bind socket-fd addr 16) -1)
              (error "Failed to bind socket"))))
        
        ;; Listen
        (when (= (%listen socket-fd backlog) -1)
          (error "Failed to listen on socket"))
        
        ;; Add to epoll for accept events
        (epoll:add-event epfd socket-fd epoll:+epollin+)
        
        ;; Create and return listener object
        (make-instance 'listener
                       :handle socket-fd
                       :backlog backlog
                       :address address
                       :port port
                       :epoll epfd))
    (error (e)
      (error 'network-error :message (format nil "Error creating listener: ~A" e)))))

(defun socket-connect (host port &key (timeout 30) type)
  "Connect to a remote host and port using epoll"
  (declare (ignore type timeout)) ;; TODO: implement timeout
  (handler-case
      (let ((socket-fd (create-tcp-socket)))
        
        ;; Connect
        (sb-alien:with-alien ((addr (sb-alien:array sb-alien:char 16)))
          (let ((sockaddr (make-sockaddr-in host port)))
            (loop for i from 0 to 15
                  do (setf (sb-alien:deref addr i) (sb-alien:deref sockaddr i)))
            (when (= (%connect socket-fd addr 16) -1)
              (error "Failed to connect"))))
        
        ;; Create and return connection object
        (make-instance 'connection
                       :handle socket-fd
                       :remote-address host
                       :remote-port port))
    (error (e)
      (error 'network-error :message (format nil "Error connecting to ~A:~A - ~A" host port e)))))

(defun socket-accept (listener)
  "Accept a connection from a listener using epoll"
  (handler-case
      (let ((epfd (listener-epoll listener)))
        ;; Wait for accept event
        (let ((events (epoll:wait-for-events epfd 1 0))) ; Non-blocking check
          (when events
            (sb-alien:with-alien ((addr (sb-alien:array sb-alien:char 16))
                                  (addrlen sb-alien:int))
              (setf addrlen 16)
              (let ((client-fd (%accept (socket-handle listener) 
                                        (sb-alien:alien-sap addr) 
                                        (sb-alien:addr addrlen))))
                (when (= client-fd -1)
                  (error "Failed to accept connection"))
                
                ;; Extract remote address info (simplified)
                (make-instance 'connection
                               :handle client-fd
                               :remote-address "unknown" ; TODO: extract from addr
                               :remote-port 0))))))           ; TODO: extract from addr
    (error (e)
      (error 'network-error :message (format nil "Error accepting connection: ~A" e)))))

(defun socket-close (socket)
  "Close a socket"
  (handler-case
      (progn
        ;; Close the stream if it exists
        (when (socket-stream-slot socket)
          (close (socket-stream-slot socket))
          (setf (socket-stream-slot socket) nil))
        
        ;; Close epoll if it's a listener
        (when (and (typep socket 'listener) (listener-epoll socket))
          (epoll:epoll-close (listener-epoll socket)))
        
        ;; Close the socket
        (close-socket (socket-handle socket)))
    (error (e)
      (error 'network-error :message (format nil "Error closing socket: ~A" e)))))

;;; Context Managers

(defmacro with-connection ((var address port &rest options) &body body)
  "Establish connection with automatic cleanup"
  `(let ((,var (socket-connect ,address ,port ,@options)))
     (unwind-protect
          (progn ,@body)
       (when ,var
         (socket-close ,var)))))

(defmacro with-listener ((var address port &rest options) &body body)
  "Create listener with automatic cleanup"
  `(let ((,var (socket-listen ,address ,port ,@options)))
     (unwind-protect
          (progn ,@body)
       (when ,var
         (socket-close ,var)))))

(defmacro with-timeout ((seconds) &body body)
  "Execute body with timeout"
  (let ((deadline (gensym "DEADLINE-"))
        (now (gensym "NOW-"))
        (result (gensym "RESULT-")))
    `(let ((,deadline (+ (get-internal-real-time) 
                        (* ,seconds internal-time-units-per-second))))
       (block timeout-block
         (tagbody
          retry
            (let ((,now (get-internal-real-time)))
              (when (> ,now ,deadline)
                (error 'timeout-error :message "Operation timed out"))
              (let ((,result
                     (handler-case 
                         (progn ,@body)
                       (timeout-error () 
                         (go retry)))))
                (return-from timeout-block ,result))))))))

;;; Stream Creation and Access

(defun socket-stream (socket)
  "Get a bidirectional stream for the socket"
  (or (socket-stream-slot socket)
      (setf (socket-stream-slot socket)
            (sb-bsd-sockets:socket-make-stream 
             (socket-handle socket)
             :input t :output t :buffering :full))))

(defun socket-from-stream (stream)
  "Get the socket object associated with a stream"
  (declare (ignore stream))
  (error "Not implemented - socket-from-stream function"))

(defun socket-peer-address (socket)
  "Get the peer address of a socket"
  (values (connection-remote-address socket)
          (connection-remote-port socket)))

;;; Stub implementations for compatibility

(defun get-socket-option (socket option-name)
  "Get socket option value"
  (declare (ignore socket option-name))
  (error "Not implemented - get-socket-option"))

(defun set-socket-option (socket option-name value)
  "Set socket option value"
  (declare (ignore socket option-name value))
  (error "Not implemented - set-socket-option"))

(defun socket-option (socket option-name &optional (value nil value-provided))
  "Get or set a socket option"
  (if value-provided
      (set-socket-option socket option-name value)
      (get-socket-option socket option-name)))

;;; Address Handling

(defclass address ()
  ((host :initarg :host :reader address-host)
   (port :initarg :port :reader address-port)))

(defun resolve (hostname &key (family :any))
  "Resolve hostname to address(es)"
  (declare (ignore family))
  (error "Not implemented - resolve"))

(defun parse-address (string)
  "Parse address from string form"
  (let* ((colon-pos (position #\: string))
         (host (if colon-pos (subseq string 0 colon-pos) string))
         (port (if colon-pos 
                   (parse-integer (subseq string (1+ colon-pos)))
                   80)))
    (make-instance 'address :host host :port port)))

;;; TLS/SSL Support

(defclass secure-context ()
  ((cert-file :initarg :cert-file)
   (key-file :initarg :key-file)
   (verify :initarg :verify :initform t)))

(defun make-secure-context (&key cert-file key-file (verify t))
  "Create TLS/SSL context"
  (make-instance 'secure-context
                 :cert-file cert-file
                 :key-file key-file
                 :verify verify))

(defclass secure-connection (connection)
  ((context :initarg :context :reader connection-context)))

(defclass secure-listener (listener)
  ((context :initarg :context :reader listener-context)))

;;; Async Operations (stubs for now)

(defun async-connect (host port &key callback)
  "Asynchronously connect to host:port"
  (declare (ignore host port callback))
  (error "Not implemented - async-connect"))

(defun async-accept (listener &key callback)
  "Asynchronously accept connection"
  (declare (ignore listener callback))
  (error "Not implemented - async-accept"))

(defun async-read (socket buffer &key callback)
  "Asynchronously read from socket"
  (declare (ignore socket buffer callback))
  (error "Not implemented - async-read"))

(defun async-write (socket buffer &key callback)
  "Asynchronously write to socket"
  (declare (ignore socket buffer callback))
  (error "Not implemented - async-write"))

(defun future (&rest args)
  "Create a future"
  (declare (ignore args))
  (error "Not implemented - future"))

(defun await (future)
  "Wait for future completion"
  (declare (ignore future))
  (error "Not implemented - await"))

(defun select (&rest futures)
  "Wait for any future to complete"
  (declare (ignore futures))
  (error "Not implemented - select"))

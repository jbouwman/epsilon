;;;; Linux Networking Implementation
;;;; 
;;;; This module implements the epsilon.net interface for Linux systems
;;;; using epoll for event notification and async I/O.

(defpackage #:epsilon.net
  (:use #:cl)
  (:local-nicknames
   (#:epoll #:epsilon.sys.epoll)
   (#:lib #:epsilon.foreign))
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
   #:tcp-stream-handle
   
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

;;; ============================================================================
;;; Linux Constants
;;; ============================================================================

;; Address families
(defconstant +af-inet+ 2)
(defconstant +af-inet6+ 10)

;; Socket types  
(defconstant +sock-stream+ 1)
(defconstant +sock-dgram+ 2)

;; Protocols
(defconstant +ipproto-tcp+ 6)
(defconstant +ipproto-udp+ 17)

;; Socket options
(defconstant +sol-socket+ 1)
(defconstant +so-reuseaddr+ 2)
(defconstant +so-keepalive+ 9)
(defconstant +so-broadcast+ 6)
(defconstant +so-linger+ 13)
(defconstant +so-rcvbuf+ 8)
(defconstant +so-sndbuf+ 7)
(defconstant +so-rcvtimeo+ 20)
(defconstant +so-sndtimeo+ 21)

;; TCP options
(defconstant +ipproto-tcp-level+ 6)
(defconstant +tcp-nodelay+ 1)

;; Shutdown options
(defconstant +shut-rd+ 0)
(defconstant +shut-wr+ 1)
(defconstant +shut-rdwr+ 2)

;; fcntl commands  
(defconstant +f-getfl+ 3)
(defconstant +f-setfl+ 4)
(defconstant +o-nonblock+ #o4000)

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

;;; ============================================================================
;;; Error Conditions
;;; ============================================================================

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
  (:documentation "Operation would block"))

;;; ============================================================================
;;; Error Handling Utilities
;;; ============================================================================

(defun get-errno ()
  "Get the current errno value"
  (handler-case
      (let ((errno-ptr (sb-alien:alien-funcall 
                        (sb-alien:extern-alien "__errno_location" 
                                               (function (* sb-alien:int))))))
        (sb-alien:deref errno-ptr 0))
    (error ()
	   ;; If we can't get errno, return -1
	   -1)))

(defun errno-to-string (errno)
  "Convert errno to human-readable string"
  (case errno
	(1 "EPERM - Operation not permitted")
	(2 "ENOENT - No such file or directory")
	(9 "EBADF - Bad file descriptor")
	(11 "EAGAIN/EWOULDBLOCK - Resource temporarily unavailable")
	(13 "EACCES - Permission denied")
	(22 "EINVAL - Invalid argument")
	(24 "EMFILE - Too many open files")
	(98 "EADDRINUSE - Address already in use")
	(99 "EADDRNOTAVAIL - Cannot assign requested address")
	(104 "ECONNRESET - Connection reset by peer")
	(107 "ENOTCONN - Socket is not connected")
	(110 "ETIMEDOUT - Operation timed out")
	(111 "ECONNREFUSED - Connection refused")
	(t (format nil "Unknown error (~D)" errno))))

(defun check-error (result operation)
  "Check system call result and signal appropriate error"
  (when (< result 0)
    (let ((errno (get-errno)))
      (case errno
            (11 (error 'would-block-error :message (format nil "~A would block" operation)))
            (98 (error 'address-in-use :message (format nil "~A: ~A" operation (errno-to-string errno))))
            (104 (error 'connection-reset :message (format nil "~A: ~A" operation (errno-to-string errno))))
            (110 (error 'timeout-error :message (format nil "~A: ~A" operation (errno-to-string errno))))
            (111 (error 'connection-refused :message (format nil "~A: ~A" operation (errno-to-string errno))))
            (t (error 'network-error :message (format nil "~A failed: ~A" operation (errno-to-string errno))))))))

;;; ============================================================================
;;; Core Types
;;; ============================================================================

(defclass socket-address ()
  ((ip :initarg :ip :reader socket-address-ip
       :documentation "IP address as string")
   (port :initarg :port :reader socket-address-port
         :type integer
         :documentation "Port number"))
  (:documentation "Socket address (IP + port)"))

(defclass tcp-listener ()
  ((handle :initarg :handle :reader tcp-listener-handle
           :documentation "OS socket file descriptor")
   (local-address :initarg :local-address :reader tcp-listener-local-address
                  :type socket-address
                  :documentation "Local bind address")
   (epoll :initarg :epoll :reader tcp-listener-epoll
          :documentation "Epoll instance for async operations")
   (backlog :initarg :backlog :reader tcp-listener-backlog
            :initform 128
            :documentation "Connection backlog"))
  (:documentation "TCP server socket"))

(defclass tcp-stream ()
  ((handle :initarg :handle :reader tcp-stream-handle
           :documentation "OS socket file descriptor")
   (local-address :initarg :local-address :reader tcp-stream-local-address
                  :type socket-address
                  :documentation "Local socket address")
   (peer-address :initarg :peer-address :reader tcp-stream-peer-address
                 :type socket-address
                 :documentation "Remote peer address")
   (input-stream :initform nil :accessor tcp-stream-input
                 :documentation "Lisp input stream")
   (output-stream :initform nil :accessor tcp-stream-output
                  :documentation "Lisp output stream")
   (connected-p :initarg :connected-p :initform t :accessor tcp-stream-connected-p
                :documentation "Connection status")
   (epoll :initform nil :accessor tcp-stream-epoll
          :documentation "Epoll instance for async operations"))
  (:documentation "TCP connection stream"))

(defclass udp-socket ()
  ((handle :initarg :handle :reader udp-socket-handle
           :documentation "OS socket file descriptor")
   (local-address :initarg :local-address :reader udp-socket-local-address
                  :type socket-address
                  :documentation "Local bind address")
   (connected-peer :initform nil :accessor udp-socket-connected-peer
                   :type (or null socket-address)
                   :documentation "Connected peer address if any"))
  (:documentation "UDP socket"))

;;; ============================================================================
;;; Address Utilities
;;; ============================================================================

(defun make-sockaddr-in-into (addr ip-address port)
  "Fill sockaddr_in structure for IPv4 (Linux version) into provided buffer"
  ;; addr is a SAP from epsilon.foreign, use sb-sys:sap-ref-* functions
  (let ((sap (if (typep addr 'sb-sys:system-area-pointer)
                 addr
               (sb-alien:alien-sap addr))))
    ;; sin_family (2 bytes)
    (setf (sb-sys:sap-ref-16 sap 0) +af-inet+)
    ;; sin_port (network byte order)
    (setf (sb-sys:sap-ref-16 sap 2) 
          (logior (ash (logand port #xff) 8)
                  (ash (logand port #xff00) -8)))
    ;; sin_addr (convert IP string to network byte order)
    (let ((ip-parts (mapcar #'parse-integer 
                            (split-string ip-address #\.))))
      (setf (sb-sys:sap-ref-32 sap 4)
            (logior (ash (first ip-parts) 24)
                    (ash (second ip-parts) 16)
                    (ash (third ip-parts) 8)
                    (fourth ip-parts))))
    ;; sin_zero (8 bytes of zeros)
    (loop for i from 8 to 15
          do (setf (sb-sys:sap-ref-8 sap i) 0))))

(defun parse-sockaddr-in (addr)
  "Parse sockaddr_in structure to extract IP and port"
  ;; addr could be a SAP or alien value, normalize to SAP
  (let* ((sap (if (typep addr 'sb-sys:system-area-pointer)
                  addr
                (sb-alien:alien-sap addr)))
         (port-bytes (sb-sys:sap-ref-16 sap 2))
         (port (logior (ash (logand port-bytes #xff) 8)
                       (ash (logand port-bytes #xff00) -8)))
         (ip-bytes (sb-sys:sap-ref-32 sap 4))
         (ip (format nil "~D.~D.~D.~D"
                     (ldb (byte 8 24) ip-bytes)
                     (ldb (byte 8 16) ip-bytes)
                     (ldb (byte 8 8) ip-bytes)
                     (ldb (byte 8 0) ip-bytes))))
    (make-instance 'socket-address :ip ip :port port)))

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

(defun set-nonblocking (fd)
  "Set a file descriptor to non-blocking mode"
  (let ((flags (%fcntl fd +f-getfl+ 0)))
    (when (< flags 0)
      (error "Failed to get file descriptor flags"))
    (%fcntl fd +f-setfl+ (logior flags +o-nonblock+))))

;;; ============================================================================
;;; Address Creation and Resolution
;;; ============================================================================

(defun make-socket-address (ip port)
  "Create a socket address from IP string and port number"
  (make-instance 'socket-address :ip ip :port port))

(defun parse-address (address-string)
  "Parse an address string like '127.0.0.1:8080' into socket-address"
  (let ((colon-pos (position #\: address-string :from-end t)))
    (if colon-pos
        (make-socket-address
         (subseq address-string 0 colon-pos)
         (parse-integer (subseq address-string (1+ colon-pos))))
      (error "Invalid address format: ~A" address-string))))

(defun resolve-address (hostname-or-address port)
  "Resolve hostname to socket addresses"
  (cond
   ;; If it looks like an IP address, use it directly
   ((and (stringp hostname-or-address)
         (every (lambda (c) (or (digit-char-p c) (char= c #\.)))
                hostname-or-address))
    (list (make-socket-address hostname-or-address port)))
   
   ;; If it's "localhost", resolve to 127.0.0.1
   ((string= hostname-or-address "localhost")
    (list (make-socket-address "127.0.0.1" port)))
   
   ;; Otherwise try DNS resolution (simplified)
   (t
    (handler-case
        (list (make-socket-address "127.0.0.1" port)) ; Fallback for now
      (error (e)
             (error 'network-error :message (format nil "Failed to resolve ~A: ~A" hostname-or-address e)))))))

;;; ============================================================================
;;; Socket Utilities
;;; ============================================================================

(defun create-socket (domain type protocol)
  "Create a socket with error handling"
  (let ((fd (%socket domain type protocol)))
    (check-error fd "socket creation")
    fd))

(defun set-socket-reuse-addr (fd enable)
  "Enable or disable SO_REUSEADDR on socket"
  (lib:with-foreign-memory ((optval :int :count 1))
			   (setf (sb-sys:sap-ref-32 optval 0) (if enable 1 0))
			   (let ((result (%setsockopt fd +sol-socket+ +so-reuseaddr+ optval 4)))
			     (check-error result "setsockopt SO_REUSEADDR"))))

(defun socket-to-stream (socket direction)
  "Convert socket to Lisp stream"
  (sb-sys:make-fd-stream socket
                         :input (member direction '(:input :io))
                         :output (member direction '(:output :io))
                         :element-type '(unsigned-byte 8)
                         :buffering :none))

;;; ============================================================================
;;; Address Helpers
;;; ============================================================================

(defun get-local-address (socket-fd)
  "Get the local address of a socket"
  (lib:with-foreign-memory ((sockaddr :char :count 16)
                            (addrlen :int :count 1))
			   (setf (sb-sys:sap-ref-32 addrlen 0) 16)
			   (let ((result (%getsockname socket-fd sockaddr addrlen)))
			     (check-error result "getsockname")
			     (parse-sockaddr-in sockaddr))))

(defun get-peer-address (socket-fd)
  "Get the peer address of a socket"
  (lib:with-foreign-memory ((sockaddr :char :count 16)
                            (addrlen :int :count 1))
			   (setf (sb-sys:sap-ref-32 addrlen 0) 16)
			   (let ((result (%getpeername socket-fd sockaddr addrlen)))
			     (check-error result "getpeername")
			     (parse-sockaddr-in sockaddr))))

;;; ============================================================================
;;; TCP Listener Operations
;;; ============================================================================

(defun tcp-bind (address &key (backlog 128) (reuse-addr t))
  "Create a TCP listener bound to the specified address"
  (let* ((sock-addr (etypecase address
			       (socket-address address)
			       (string (parse-address address))))
         (socket-fd (create-socket +af-inet+ +sock-stream+ +ipproto-tcp+))
         (epoll-instance (epoll:epoll-create1)))
    
    (when reuse-addr
      (set-socket-reuse-addr socket-fd t))
    
    (handler-case
        (progn
          ;; Bind socket to address
          (lib:with-foreign-memory ((sockaddr :char :count 16))
				   (make-sockaddr-in-into sockaddr 
							  (socket-address-ip sock-addr)
							  (socket-address-port sock-addr))
				   (let ((result (%bind socket-fd sockaddr 16)))
				     (check-error result "bind")))
          
          ;; Listen for connections
          (let ((result (%listen socket-fd backlog)))
            (check-error result "listen"))
          
          ;; Set to non-blocking mode
          (set-nonblocking socket-fd)
          
          ;; Register with epoll for incoming connections
          (epoll:epoll-add epoll-instance socket-fd '(:in))
          
          ;; Get actual bound address (in case port was 0)
          (let ((actual-addr (get-local-address socket-fd)))
            (make-instance 'tcp-listener
                           :handle socket-fd
                           :local-address actual-addr
                           :epoll epoll-instance
                           :backlog backlog)))
      (error (e)
             (%close socket-fd)
             (epoll:epoll-close epoll-instance)
             (error e)))))

(defun tcp-accept (listener &key (timeout nil))
  "Accept a connection, with optional timeout in seconds"
  (let ((timeout-ms (if timeout (round (* timeout 1000)) -1)))
    (loop
     (handler-case
         (lib:with-foreign-memory ((peer-sockaddr :char :count 16)
                                   (addrlen :int :count 1))
				  (setf (sb-sys:sap-ref-32 addrlen 0) 16)
				  (let ((client-fd (%accept (tcp-listener-handle listener) 
							    peer-sockaddr addrlen)))
				    (cond
				      ((>= client-fd 0)
				       ;; Success - got a connection
				       (set-nonblocking client-fd)
				       (let ((local-addr (get-local-address client-fd))
					     (peer-addr (parse-sockaddr-in peer-sockaddr)))
					 (return (make-instance 'tcp-stream
								:handle client-fd
								:local-address local-addr
								:peer-address peer-addr
								:connected-p t))))
				      (t
				       ;; Check what error occurred
				       (check-error client-fd "accept")))))
       (would-block-error ()
			  ;; Wait for socket to be ready with timeout
			  (let ((events (epoll:wait-for-events (tcp-listener-epoll listener) 1 timeout-ms)))
			    (when (null events)
			      ;; Timeout occurred
			      (if timeout
				  (error 'timeout-error :message "Accept timeout")
				;; No timeout specified, continue waiting
				nil))))
       (error (e)
              (error 'network-error :message (format nil "Accept failed: ~A" e)))))))

(defun tcp-try-accept (listener)
  "Try to accept a connection without blocking"
  (handler-case
      (lib:with-foreign-memory ((peer-sockaddr :char :count 16)
                                (addrlen :int :count 1))
			       (setf (sb-sys:sap-ref-32 addrlen 0) 16)
			       (let ((client-fd (%accept (tcp-listener-handle listener) 
							 peer-sockaddr addrlen)))
				 (when (>= client-fd 0)
				   (set-nonblocking client-fd)
				   (let ((local-addr (get-local-address client-fd))
					 (peer-addr (parse-sockaddr-in peer-sockaddr)))
				     (make-instance 'tcp-stream
						    :handle client-fd
						    :local-address local-addr
						    :peer-address peer-addr
						    :connected-p t)))))
    (would-block-error ()
		       nil)
    (error (e)
	   (error 'network-error :message (format nil "Accept failed: ~A" e)))))

(defun tcp-poll-accept (listener timeout-ms)
  "Poll for incoming connections with timeout"
  (let ((events (epoll:wait-for-events (tcp-listener-epoll listener) 1 timeout-ms)))
    (when events
      (tcp-try-accept listener))))

(defun tcp-local-addr (listener)
  "Get the local address of a TCP listener"
  (tcp-listener-local-address listener))

(defun tcp-incoming (listener)
  "Return a lazy sequence of incoming connections"
  ;; This would ideally return a lazy sequence, but for now returns a list
  (loop for conn = (tcp-poll-accept listener 0)
        while conn
        collect conn))

;;; ============================================================================
;;; TCP Stream Epoll Helpers
;;; ============================================================================

(defun ensure-tcp-stream-epoll (stream)
  "Ensure TCP stream has an epoll instance for async operations"
  (unless (tcp-stream-epoll stream)
    (let ((epoll-instance (epoll:epoll-create1)))
      (setf (tcp-stream-epoll stream) epoll-instance)
      ;; Add socket to epoll for read events by default
      (epoll:epoll-add epoll-instance (tcp-stream-handle stream) '(:in))))
  (tcp-stream-epoll stream))

(defun tcp-stream-wait-for-read (stream timeout-ms)
  "Wait for stream to be ready for reading"
  (let ((epoll-instance (ensure-tcp-stream-epoll stream)))
    (let ((events (epoll:wait-for-events epoll-instance 1 timeout-ms)))
      (when events
        (let ((event (first events)))
          (epoll:epoll-event-readable-p event))))))

(defun tcp-stream-wait-for-write (stream timeout-ms)
  "Wait for stream to be ready for writing"
  (let ((epoll-instance (ensure-tcp-stream-epoll stream)))
    ;; Modify epoll to monitor write events
    (epoll:epoll-modify epoll-instance (tcp-stream-handle stream) '(:out))
    (let ((events (epoll:wait-for-events epoll-instance 1 timeout-ms)))
      ;; Switch back to read monitoring
      (epoll:epoll-modify epoll-instance (tcp-stream-handle stream) '(:in))
      (when events
        (let ((event (first events)))
          (epoll:epoll-event-writable-p event))))))

;;; ============================================================================
;;; TCP Stream Operations
;;; ============================================================================

(defun tcp-connect (address &key (timeout nil))
  "Connect to a TCP server with optional timeout"
  (let* ((sock-addr (etypecase address
			       (socket-address address)
			       (string (parse-address address))))
         (socket-fd (create-socket +af-inet+ +sock-stream+ +ipproto-tcp+)))
    
    (handler-case
        (progn
          ;; Set to non-blocking mode before connecting for async support
          (set-nonblocking socket-fd)
          
          ;; Try to connect
          (lib:with-foreign-memory ((sockaddr :char :count 16))
				   (make-sockaddr-in-into sockaddr 
							  (socket-address-ip sock-addr)
							  (socket-address-port sock-addr))
				   (let ((result (%connect socket-fd sockaddr 16)))
				     (when (< result 0)
				       ;; Check if it's a would-block error (async connection in progress)
				       (let ((errno (get-errno)))
					 (if (= errno 115) ; EINPROGRESS - connection in progress
					     (when timeout
					       ;; Wait for connection to complete with timeout
					       (let ((epoll-instance (epoll:epoll-create1))
						     (timeout-ms (round (* timeout 1000))))
						 (unwind-protect
						     (progn
						       (epoll:epoll-add epoll-instance socket-fd '(:out))
						       (let ((events (epoll:wait-for-events epoll-instance 1 timeout-ms)))
							 (unless events
							   (error 'timeout-error :message "Connection timeout"))))
						   (epoll:epoll-close epoll-instance))))
					   ;; Other error, check and signal appropriately
					   (check-error result "connect"))))))
          
          ;; Get local and peer addresses
          (let ((local-addr (get-local-address socket-fd))
                (peer-addr (get-peer-address socket-fd)))
            (make-instance 'tcp-stream
                           :handle socket-fd
                           :local-address local-addr
                           :peer-address peer-addr
                           :connected-p t)))
      (error (e)
             (%close socket-fd)
             (error e)))))

(defun tcp-stream-reader (stream)
  "Get or create input stream for TCP stream"
  (or (tcp-stream-input stream)
      (setf (tcp-stream-input stream)
            (socket-to-stream (tcp-stream-handle stream) :input))))

(defun tcp-stream-writer (stream)
  "Get or create output stream for TCP stream"
  (or (tcp-stream-output stream)
      (setf (tcp-stream-output stream)
            (socket-to-stream (tcp-stream-handle stream) :output))))

(defun tcp-read (stream buffer &key (start 0) (end (length buffer)) (timeout nil))
  "Read data from TCP stream into buffer with optional timeout"
  (lib:with-foreign-memory ((buf :char :count (- end start)))
			   (let ((result (%recv (tcp-stream-handle stream) buf (- end start) 0)))
			     (cond
			      ((> result 0)
			       ;; Copy data from foreign buffer to Lisp buffer
			       (loop for i from 0 below result
				     do (setf (aref buffer (+ start i))
					      (sb-sys:sap-ref-8 buf i)))
			       result)
			      ((= result 0)
			       ;; Connection closed
			       (setf (tcp-stream-connected-p stream) nil)
			       0)
			      (t
			       ;; Error occurred
			       (let ((errno (get-errno)))
				 (if (= errno 11) ; EWOULDBLOCK
				     (if timeout
					 ;; Wait for data with timeout
					 (let ((timeout-ms (round (* timeout 1000))))
					   (if (tcp-stream-wait-for-read stream timeout-ms)
					       ;; Retry read after epoll indicates readiness
					       (tcp-read stream buffer :start start :end end :timeout nil)
					     0)) ; Timeout occurred
				       0) ; No data available, no timeout specified
				   (progn
				     (setf (tcp-stream-connected-p stream) nil)
				     (check-error result "recv")
				     0))))))))

(defun tcp-write (stream data &key (start 0) (end nil) (timeout nil))
  "Write data to TCP stream with optional timeout"
  (let* ((buffer (etypecase data
			    (string (sb-ext:string-to-octets data))
			    (vector data)
			    (list (coerce data 'vector))))
         (actual-end (or end (length buffer))))
    (lib:with-foreign-memory ((buf :char :count (- actual-end start)))
			     ;; Copy data to foreign buffer
			     (loop for i from start below actual-end
				   for j from 0
				   do (setf (sb-sys:sap-ref-8 buf j) (aref buffer i)))
			     
			     (let ((result (%send (tcp-stream-handle stream) buf (- actual-end start) 0)))
			       (cond
				((>= result 0) result)
				(t
				 (let ((errno (get-errno)))
				   (if (= errno 11) ; EWOULDBLOCK
				       (if timeout
					   ;; Wait for socket to be writable with timeout
					   (let ((timeout-ms (round (* timeout 1000))))
					     (if (tcp-stream-wait-for-write stream timeout-ms)
						 ;; Retry write after epoll indicates writability
						 (tcp-write stream data :start start :end end :timeout nil)
					       0)) ; Timeout occurred
					 0) ; No bytes written, would block, no timeout specified
				     (progn
				       (setf (tcp-stream-connected-p stream) nil)
				       (check-error result "send")
				       0)))))))))

(defun tcp-write-all (stream data &key (start 0) (end nil))
  "Write all data to TCP stream"
  (let ((actual-end (or end (length data))))
    (loop with pos = start
          while (< pos actual-end)
          for bytes-written = (tcp-write stream data :start pos :end actual-end)
          do (incf pos bytes-written)
          (when (zerop bytes-written)
            ;; If no bytes were written, avoid infinite loop
            (sleep 0.001))
          finally (return (- actual-end start)))))

(defun tcp-flush (stream)
  "Flush any buffered output"
  ;; With direct socket operations, flushing is automatic
  (declare (ignore stream))
  t)

(defun tcp-try-read (stream buffer &key (start 0) (end (length buffer)))
  "Try to read without blocking"
  ;; Non-blocking read
  (tcp-read stream buffer :start start :end end))

(defun tcp-try-write (stream data &key (start 0) (end nil))
  "Try to write without blocking"
  ;; Non-blocking write
  (tcp-write stream data :start start :end end))

(defun tcp-poll-read (stream timeout-ms)
  "Poll for readable data with timeout"
  (tcp-stream-wait-for-read stream timeout-ms))

(defun tcp-poll-write (stream timeout-ms)
  "Poll for writability with timeout"
  (tcp-stream-wait-for-write stream timeout-ms))

(defun tcp-peer-addr (stream)
  "Get peer address of TCP stream"
  (tcp-stream-peer-address stream))

(defun tcp-shutdown (stream &key (how :both))
  "Shutdown TCP stream"
  (let ((shutdown-how (ecase how
                             (:read +shut-rd+)
                             (:write +shut-wr+)
                             (:both +shut-rdwr+))))
    (let ((result (%shutdown (tcp-stream-handle stream) shutdown-how)))
      (check-error result "shutdown"))
    ;; Clean up epoll instance if it exists
    (when (tcp-stream-epoll stream)
      (epoll:epoll-close (tcp-stream-epoll stream))
      (setf (tcp-stream-epoll stream) nil))
    (setf (tcp-stream-connected-p stream) nil)))

(defun tcp-connected-p (stream)
  "Check if TCP stream is connected"
  (tcp-stream-connected-p stream))

;;; ============================================================================
;;; UDP Operations
;;; ============================================================================

(defun udp-bind (address)
  "Create a UDP socket bound to address"
  (let* ((sock-addr (etypecase address
			       (socket-address address)
			       (string (parse-address address))))
         (socket-fd (create-socket +af-inet+ +sock-dgram+ +ipproto-udp+)))
    
    (handler-case
        (progn
          (set-socket-reuse-addr socket-fd t)
          
          ;; Bind socket to address
          (lib:with-foreign-memory ((sockaddr :char :count 16))
				   (make-sockaddr-in-into sockaddr 
							  (socket-address-ip sock-addr)
							  (socket-address-port sock-addr))
				   (let ((result (%bind socket-fd sockaddr 16)))
				     (check-error result "UDP bind")))
          
          (set-nonblocking socket-fd)
          
          ;; Get actual bound address (in case port was 0)
          (let ((actual-addr (get-local-address socket-fd)))
            (make-instance 'udp-socket
                           :handle socket-fd
                           :local-address actual-addr)))
      (error (e)
             (%close socket-fd)
             (error e)))))

(defun udp-connect (socket address)
  "Connect UDP socket to remote address"
  (let ((sock-addr (etypecase address
			      (socket-address address)
			      (string (parse-address address)))))
    (handler-case
        (lib:with-foreign-memory ((sockaddr :char :count 16))
				 (make-sockaddr-in-into sockaddr 
							(socket-address-ip sock-addr)
							(socket-address-port sock-addr))
				 (let ((result (%connect (udp-socket-handle socket) sockaddr 16)))
				   (check-error result "UDP connect")
				   (setf (udp-socket-connected-peer socket) sock-addr)))
      (error (e)
             (error e)))))

(defun udp-send (socket data &key (start 0) (end nil))
  "Send data on connected UDP socket"
  (let* ((buffer (etypecase data
			    (string (sb-ext:string-to-octets data))
			    (vector data)
			    (list (coerce data 'vector))))
         (actual-end (or end (length buffer))))
    (lib:with-foreign-memory ((buf :char :count (- actual-end start)))
			     ;; Copy data to foreign buffer
			     (loop for i from start below actual-end
				   for j from 0
				   do (setf (sb-sys:sap-ref-8 buf j) (aref buffer i)))
			     
			     (let ((result (%send (udp-socket-handle socket) buf (- actual-end start) 0)))
			       (check-error result "UDP send")
			       result))))

(defun udp-recv (socket buffer &key (start 0) (end (length buffer)))
  "Receive data on UDP socket"
  (lib:with-foreign-memory ((buf :char :count (- end start)))
			   (let ((result (%recv (udp-socket-handle socket) buf (- end start) 0)))
			     (cond
			      ((> result 0)
			       ;; Copy data from foreign buffer to Lisp buffer
			       (loop for i from 0 below result
				     do (setf (aref buffer (+ start i))
					      (sb-sys:sap-ref-8 buf i)))
			       result)
			      ((= result 0)
			       0) ; No data
			      (t
			       (let ((errno (get-errno)))
				 (if (= errno 11) ; EWOULDBLOCK
				     0
				   (check-error result "UDP recv"))
				 0))))))

(defun udp-send-to (socket data address &key (start 0) (end nil))
  "Send data to specific address"
  (let* ((sock-addr (etypecase address
			       (socket-address address)
			       (string (parse-address address))))
         (buffer (etypecase data
			    (string (sb-ext:string-to-octets data))
			    (vector data)
			    (list (coerce data 'vector))))
         (actual-end (or end (length buffer))))
    (lib:with-foreign-memory ((buf :char :count (- actual-end start))
			      (sockaddr :char :count 16))
			     ;; Copy data to foreign buffer
			     (loop for i from start below actual-end
				   for j from 0
				   do (setf (sb-sys:sap-ref-8 buf j) (aref buffer i)))
			     
			     ;; Create destination address
			     (make-sockaddr-in-into sockaddr 
						    (socket-address-ip sock-addr)
						    (socket-address-port sock-addr))
			     
			     (let ((result (%sendto (udp-socket-handle socket) buf (- actual-end start) 0
						    sockaddr 16)))
			       (check-error result "UDP sendto")
			       result))))

(defun udp-recv-from (socket buffer &key (start 0) (end (length buffer)))
  "Receive data and sender address"
  (lib:with-foreign-memory ((buf :char :count (- end start))
                            (sockaddr :char :count 16)
                            (addrlen :int :count 1))
			   (setf (sb-sys:sap-ref-32 addrlen 0) 16)
			   (let ((result (%recvfrom (udp-socket-handle socket) buf (- end start) 0
						    sockaddr addrlen)))
			     (cond
			      ((> result 0)
			       ;; Copy data from foreign buffer to Lisp buffer
			       (loop for i from 0 below result
				     do (setf (aref buffer (+ start i))
					      (sb-sys:sap-ref-8 buf i)))
			       (values result (parse-sockaddr-in sockaddr)))
			      ((= result 0)
			       (values 0 nil))
			      (t
			       (let ((errno (get-errno)))
				 (if (= errno 11) ; EWOULDBLOCK
				     (values 0 nil)
				   (progn
				     (check-error result "UDP recvfrom")
				     (values 0 nil)))))))))

(defun udp-local-addr (socket)
  "Get local address of UDP socket"
  (udp-socket-local-address socket))

;;; ============================================================================
;;; Socket Options
;;; ============================================================================

(defun set-socket-option (socket-obj option value)
  "Set a socket option"
  (let ((handle (etypecase socket-obj
			   (tcp-listener (tcp-listener-handle socket-obj))
			   (tcp-stream (tcp-stream-handle socket-obj))
			   (udp-socket (udp-socket-handle socket-obj)))))
    (lib:with-foreign-memory ((optval :int :count 1))
			     (setf (sb-sys:sap-ref-32 optval 0) (if value 1 0))
			     (let ((result (ecase option
						  (:reuse-address (%setsockopt handle +sol-socket+ +so-reuseaddr+ optval 4))
						  (:keep-alive (%setsockopt handle +sol-socket+ +so-keepalive+ optval 4))
						  (:tcp-nodelay (%setsockopt handle +ipproto-tcp-level+ +tcp-nodelay+ optval 4)))))
			       (check-error result "setsockopt")))))

(defun get-socket-option (socket-obj option)
  "Get a socket option"
  (let ((handle (etypecase socket-obj
			   (tcp-listener (tcp-listener-handle socket-obj))
			   (tcp-stream (tcp-stream-handle socket-obj))
			   (udp-socket (udp-socket-handle socket-obj)))))
    (lib:with-foreign-memory ((optval :int :count 1)
			      (optlen :int :count 1))
			     (setf (sb-sys:sap-ref-32 optlen 0) 4)
			     (let ((result (ecase option
						  (:reuse-address (%getsockopt handle +sol-socket+ +so-reuseaddr+ optval optlen))
						  (:keep-alive (%getsockopt handle +sol-socket+ +so-keepalive+ optval optlen))
						  (:recv-buffer (%getsockopt handle +sol-socket+ +so-rcvbuf+ optval optlen))
						  (:send-buffer (%getsockopt handle +sol-socket+ +so-sndbuf+ optval optlen))
						  (:tcp-nodelay (%getsockopt handle +ipproto-tcp-level+ +tcp-nodelay+ optval optlen)))))
			       (check-error result "getsockopt")
			       (case option
				     ((:reuse-address :keep-alive :tcp-nodelay) 
				      (not (zerop (sb-sys:sap-ref-32 optval 0))))
				     ((:recv-buffer :send-buffer)
				      (sb-sys:sap-ref-32 optval 0)))))))

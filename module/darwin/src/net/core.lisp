(defpackage epsilon.net
  (:use cl)
  (:local-nicknames
   (kqueue epsilon.kqueue)
   (lib epsilon.sys.lib))
  (:shadow
   listen
   close)
  (:export

   ;; Core types & conditions
   socket listener connection
   network-error connection-error timeout-error
   
   ;; High-level operations
   with-connection with-listener with-timeout
   
   ;; Async
   async-connect async-accept async-read async-write
   future await select
   
   ;; Address handling
   address resolve parse-address
   ipv4 ipv6 hostname
   
   ;; Options & configuration
   socket-option set-socket-option get-socket-option
   keep-alive no-delay reuse-addr recv-buffer send-buffer
   
   ;; Socket stream access
   socket-stream socket-from-stream
   socket-peer-address
   
   ;; Socket operations
   socket-listen socket-connect socket-accept socket-close))

(in-package epsilon.net)

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
   (kqueue :initarg :kqueue :reader listener-kqueue)))

(defclass connection (socket)
  ((remote-address :initarg :remote-address :accessor connection-remote-address)
   (remote-port :initarg :remote-port :accessor connection-remote-port)))

;;; Socket Constants (Darwin-specific)

;; Address families
(defconstant +af-inet+ 2)
(defconstant +af-inet6+ 30)

;; Socket types  
(defconstant +sock-stream+ 1)
(defconstant +sock-dgram+ 2)

;; Protocols
(defconstant +ipproto-tcp+ 6)
(defconstant +ipproto-udp+ 17)

;;; Socket FFI Bindings

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

(lib:defshared %close "close" "libc" :int (fd :int)
  :documentation "Close file descriptor")

;;; Address Handling (Darwin sockaddr_in)

;; struct sockaddr_in {
;;     uint8_t        sin_len;
;;     sa_family_t    sin_family;
;;     in_port_t      sin_port;
;;     struct in_addr sin_addr;
;;     char           sin_zero[8];
;; };

(defun make-sockaddr-in (ip-address port)
  "Create sockaddr_in structure for IPv4 (Darwin version)"
  (lib:with-foreign-memory ((addr :char :count 16))
    ;; sin_len
    (setf (sb-alien:deref addr 0) 16)
    ;; sin_family 
    (setf (sb-alien:deref addr 1) +af-inet+)
    ;; sin_port (network byte order)
    (setf (sb-alien:sap-ref-16 (sb-alien:alien-sap addr) 2) 
          (logior (ash (logand port #xff) 8)
                  (ash (logand port #xff00) -8)))
    ;; sin_addr (convert IP string to network byte order)
    (let ((ip-parts (mapcar #'parse-integer 
                            (split-string ip-address #\.))))
      (setf (sb-alien:sap-ref-32 (sb-alien:alien-sap addr) 4)
            (logior (ash (first ip-parts) 24)
                    (ash (second ip-parts) 16)
                    (ash (third ip-parts) 8)
                    (fourth ip-parts))))
    ;; sin_zero (8 bytes of zeros)
    (loop for i from 8 to 15
          do (setf (sb-alien:deref addr i) 0))
    addr))

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
  (%close socket-fd))

;;; Core Socket Operations using kqueue

(defun socket-listen (address port &key (reuse-address t) (backlog 5) type)
  "Create a listening socket on the given address and port using kqueue"
  (declare (ignore type reuse-address)) ;; TODO: implement socket options
  (handler-case
      (let* ((socket-fd (create-tcp-socket))
             (kq (kqueue:kqueue))
             (host (if (or (string= address "0.0.0.0") (string= address "*"))
                       "0.0.0.0"
                       address)))
        
        ;; Bind socket
        (lib:with-foreign-memory ((addr :char :count 16))
          (let ((sockaddr (make-sockaddr-in host port)))
            (loop for i from 0 to 15
                  do (setf (sb-alien:deref addr i) (sb-alien:deref sockaddr i)))
            (when (= (%bind socket-fd addr 16) -1)
              (error "Failed to bind socket"))))
        
        ;; Listen
        (when (= (%listen socket-fd backlog) -1)
          (error "Failed to listen on socket"))
        
        ;; Add to kqueue for accept events
        (kqueue:watch-socket-read kq socket-fd)
        
        ;; Create and return listener object
        (make-instance 'listener
                       :handle socket-fd
                       :backlog backlog
                       :address address
                       :port port
                       :kqueue kq))
    (error (e)
      (error 'network-error :message (format nil "Error creating listener: ~A" e)))))

(defun socket-connect (host port &key (timeout 30) type)
  "Connect to a remote host and port using kqueue"
  (declare (ignore type timeout)) ;; TODO: implement timeout
  (handler-case
      (let ((socket-fd (create-tcp-socket)))
        
        ;; Connect
        (lib:with-foreign-memory ((addr :char :count 16))
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
  "Accept a connection from a listener using kqueue"
  (handler-case
      (let ((kq (listener-kqueue listener)))
        ;; Wait for accept event
        (let ((events (kqueue:wait-for-events kq 1 0))) ; Non-blocking check
          (when events
            (lib:with-foreign-memory ((addr :char :count 16)
                                      (addrlen :int :count 1))
              (setf (sb-alien:deref addrlen 0) 16)
              (let ((client-fd (%accept (socket-handle listener) addr addrlen)))
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
        
        ;; Close kqueue if it's a listener
        (when (and (typep socket 'listener) (listener-kqueue socket))
          (kqueue:kqueue-close (listener-kqueue socket)))
        
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

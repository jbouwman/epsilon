;;;; Darwin/macOS Networking Implementation
;;;; 
;;;; This module implements the epsilon.net interface for macOS/BSD systems
;;;; using kqueue for event notification and async I/O.

(defpackage #:epsilon.net
  (:use #:cl)
  (:local-nicknames
   (#:kqueue #:epsilon.kqueue)
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
   (kqueue :initarg :kqueue :reader tcp-listener-kqueue
           :documentation "Kqueue instance for async operations")
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
                :documentation "Connection status"))
  (:documentation "TCP connection"))

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
  (fd :int) (cmd :int) (arg :long)
  :documentation "File control operations")

;; fcntl commands
(defconstant +f-getfl+ 3)
(defconstant +f-setfl+ 4)
(defconstant +o-nonblock+ #x0004)

;;; ============================================================================
;;; Error Handling Utilities
;;; ============================================================================

(defun get-errno ()
  "Get the current errno value"
  (handler-case
      (let ((errno-ptr (sb-alien:alien-funcall 
                        (sb-alien:extern-alien "__error" 
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
    (13 "EACCES - Permission denied")
    (22 "EINVAL - Invalid argument")
    (24 "EMFILE - Too many open files")
    (35 "EAGAIN/EWOULDBLOCK - Resource temporarily unavailable")
    (48 "EADDRINUSE - Address already in use")
    (49 "EADDRNOTAVAIL - Cannot assign requested address")
    (54 "ECONNRESET - Connection reset by peer")
    (57 "ENOTCONN - Socket is not connected")
    (60 "ETIMEDOUT - Operation timed out")
    (61 "ECONNREFUSED - Connection refused")
    (t (format nil "Unknown error (~D)" errno))))

;;; ============================================================================
;;; Address Utilities
;;; ============================================================================

(defun make-sockaddr-in-into (addr ip-address port)
  "Fill sockaddr_in structure for IPv4 (Darwin version) into provided buffer"
  ;; addr is a SAP from epsilon.foreign, use sb-sys:sap-ref-* functions
  (let ((sap (if (typep addr 'sb-sys:system-area-pointer)
                 addr
                 (sb-alien:alien-sap addr))))
    ;; sin_len
    (setf (sb-sys:sap-ref-8 sap 0) 16)
    ;; sin_family 
    (setf (sb-sys:sap-ref-8 sap 1) +af-inet+)
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

(defun make-sockaddr-in (ip-address port)
  "Create sockaddr_in structure for IPv4 (Darwin version) - deprecated, use make-sockaddr-in-into"
  (lib:with-foreign-memory ((addr :char :count 16))
    (make-sockaddr-in-into addr ip-address port)
    ;; This is still problematic - returning pointer to freed memory
    ;; Only kept for compatibility, should be removed
    addr))

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

;; FIXME use library functions

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
;;; TCP Implementation - Part 1: Basic Operations
;;; ============================================================================

(defun tcp-bind (address)
  "Create a TCP listener bound to the specified address"
  (let ((sock-addr (etypecase address
                     (socket-address address)
                     (string (parse-address address)))))
    (handler-case
        (let* ((socket-fd (%socket +af-inet+ +sock-stream+ +ipproto-tcp+))
               (kq (kqueue:kqueue)))
          (when (< socket-fd 0)
            (error 'network-error :message "Failed to create TCP socket"))
          
          ;; Set reuse address by default
          (lib:with-foreign-memory ((optval :int :count 1))
            (setf (sb-sys:sap-ref-32 optval 0) 1)
            (%setsockopt socket-fd +sol-socket+ +so-reuseaddr+ optval 4))
          
          ;; Bind socket
          (lib:with-foreign-memory ((addr :char :count 16))
            (make-sockaddr-in-into addr (socket-address-ip sock-addr) (socket-address-port sock-addr))
            (let ((result (%bind socket-fd addr 16)))
              (when (< result 0)
                (error 'network-error 
                       :message (format nil "Failed to bind socket to ~A:~A" 
                                        (socket-address-ip sock-addr) 
                                        (socket-address-port sock-addr))))))
          
          ;; Listen
          (when (< (%listen socket-fd 128) 0)
            (error 'network-error :message "Failed to listen on socket"))
          
          ;; Add to kqueue for accept events
          (kqueue:add-event kq socket-fd kqueue:+evfilt-read+)
          
          ;; Get actual bound address (in case port was 0)
          (lib:with-foreign-memory ((addr :char :count 16)
                                    (addrlen :int :count 1))
            (setf (sb-sys:sap-ref-32 addrlen 0) 16)
            (%getsockname socket-fd addr addrlen)
            (let ((local-addr (parse-sockaddr-in addr)))
              (make-instance 'tcp-listener
                             :handle socket-fd
                             :local-address local-addr
                             :kqueue kq
                             :backlog 128))))
      (error (e)
        (error 'network-error :message (format nil "TCP bind failed: ~A" e))))))

(defun tcp-accept (listener)
  "Accept a new incoming connection. Blocks until a connection is available."
  (handler-case
      (loop
       ;; Wait for accept event
       (let ((events (kqueue:wait-for-events (tcp-listener-kqueue listener) 1 nil)))
         (when events
           (lib:with-foreign-memory ((addr :char :count 16)
                                     (addrlen :int :count 1))
             (setf (sb-sys:sap-ref-32 addrlen 0) 16)
             (let ((client-fd (%accept (tcp-listener-handle listener) addr addrlen)))
               (cond
                 ((>= client-fd 0)
                  ;; Success - create the stream
                  (let ((peer-addr (parse-sockaddr-in addr)))
                    ;; Get local address of accepted socket
                    (lib:with-foreign-memory ((local-addr :char :count 16)
                                              (local-addrlen :int :count 1))
                      (setf (sb-sys:sap-ref-32 local-addrlen 0) 16)
                      (%getsockname client-fd local-addr local-addrlen)
                      (let ((local-sock-addr (parse-sockaddr-in local-addr)))
                        (return (values 
                                 (make-instance 'tcp-stream
                                                :handle client-fd
                                                :local-address local-sock-addr
                                                :peer-address peer-addr
                                                :connected-p t)
                                 peer-addr))))))
                 (t
                  ;; accept failed - get errno for specific error
                  (let ((errno-val (get-errno)))
                    (error 'network-error 
                           :message (format nil "Accept failed with errno ~D: ~A" 
                                            errno-val
                                            (errno-to-string errno-val)))))))))))
    (network-error (e)
      ;; Re-raise network errors as-is
      (error e))
    (error (e)
      ;; Wrap other errors with more context
      (error 'network-error :message (format nil "TCP accept failed: ~A (~A)" e (type-of e))))))

(defun tcp-try-accept (listener)
  "Try to accept a connection without blocking"
  (handler-case
      (let ((events (kqueue:wait-for-events (tcp-listener-kqueue listener) 1 0)))
        (if events
            (lib:with-foreign-memory ((addr :char :count 16)
                                      (addrlen :int :count 1))
              (setf (sb-sys:sap-ref-32 addrlen 0) 16)
              (let ((client-fd (%accept (tcp-listener-handle listener) addr addrlen)))
                (if (>= client-fd 0)
                    (let ((peer-addr (parse-sockaddr-in addr)))
                      (lib:with-foreign-memory ((local-addr :char :count 16)
                                                (local-addrlen :int :count 1))
                        (setf (sb-sys:sap-ref-32 local-addrlen 0) 16)
                        (%getsockname client-fd local-addr local-addrlen)
                        (let ((local-sock-addr (parse-sockaddr-in local-addr)))
                          (values 
                           (make-instance 'tcp-stream
                                          :handle client-fd
                                          :local-address local-sock-addr
                                          :peer-address peer-addr
                                          :connected-p t)
                           peer-addr))))
                    :would-block)))
            :would-block))
    (error (e)
      (error 'network-error :message (format nil "Try-accept failed: ~A" e)))))

(defun tcp-poll-accept (listener waker)
  "Poll for a connection (async operation)"
  ;; For now, just try to accept and return immediately
  ;; TODO: Integrate with waker callback system
  (declare (ignore waker))
  (let ((result (tcp-try-accept listener)))
    (if (eq result :would-block)
        :pending
        result)))

(defun tcp-incoming (listener)
  "Returns an iterator/sequence over incoming connections"
  ;; Simple implementation - returns a list of accepted connections
  ;; In production, this would be a lazy sequence
  (list))

(defun tcp-local-addr (listener)
  "Get the local address the listener is bound to"
  (tcp-listener-local-address listener))

;;; ============================================================================
;;; TCP Stream Operations
;;; ============================================================================

(defun tcp-connect (address)
  "Connect to a remote TCP server. Blocks until connected."
  (let ((sock-addr (etypecase address
                     (socket-address address)
                     (string (parse-address address)))))
        (let ((socket-fd (%socket +af-inet+ +sock-stream+ +ipproto-tcp+)))
          (when (< socket-fd 0)
            (let ((errno-val (get-errno)))
              (error 'network-error 
                     :message (format nil "Failed to create TCP socket - errno ~D: ~A" 
                                      errno-val (errno-to-string errno-val)))))
          
          ;; Connect
          (lib:with-foreign-memory ((addr :char :count 16))
            (make-sockaddr-in-into addr (socket-address-ip sock-addr) (socket-address-port sock-addr))
            (let ((result (%connect socket-fd addr 16)))
              (when (< result 0)
                (let ((errno-val (get-errno)))
                  (%close socket-fd)  ; Clean up socket on failure
                  (case errno-val
                    (61 (error 'connection-refused 
                               :message (format nil "Connection refused to ~A:~D" 
                                                (socket-address-ip sock-addr)
                                                (socket-address-port sock-addr))))
                    (t (error 'network-error 
                              :message (format nil "Connect failed to ~A:~D - errno ~D: ~A" 
                                               (socket-address-ip sock-addr)
                                               (socket-address-port sock-addr)
                                               errno-val 
                                               (errno-to-string errno-val))))))))
            
            ;; Get local address
            (lib:with-foreign-memory ((local-addr :char :count 16)
                                      (local-addrlen :int :count 1))
              (setf (sb-sys:sap-ref-32 local-addrlen 0) 16)
              (%getsockname socket-fd local-addr local-addrlen)
              (let ((local-sock-addr (parse-sockaddr-in local-addr)))
                (make-instance 'tcp-stream
                               :handle socket-fd
                               :local-address local-sock-addr
                               :peer-address sock-addr
                               :connected-p t)))))))

(defun tcp-read (stream buffer &key (start 0) end)
  "Read data from the stream into a buffer"
  (let* ((end (or end (length buffer)))
         (count (- end start)))
    (handler-case
        (lib:with-foreign-memory ((buf :char :count count))
          (let ((bytes-read (%recv (tcp-stream-handle stream) buf count 0)))
            (cond
              ((> bytes-read 0)
               ;; Copy data to buffer
               (loop for i from 0 below bytes-read
                     do (setf (aref buffer (+ start i))
                              (sb-sys:sap-ref-8 buf i)))
               bytes-read)
              ((= bytes-read 0)
               ;; EOF - connection closed
               (setf (tcp-stream-connected-p stream) nil)
               0)
              (t
               (error 'connection-reset :message "Read error")))))
      (error (e)
        (error 'network-error :message (format nil "Read failed: ~A" e))))))

(defun tcp-write (stream data &key (start 0) end)
  "Write data to the stream"
  (let* ((data-bytes (etypecase data
                       (string (sb-ext:string-to-octets data))
                       (vector data)))
         (end (or end (length data-bytes)))
         (count (- end start)))
    (handler-case
        (lib:with-foreign-memory ((buf :char :count count))
          ;; Copy data to foreign buffer
          (loop for i from 0 below count
                do (setf (sb-sys:sap-ref-8 buf i)
                         (aref data-bytes (+ start i))))
          (let ((bytes-written (%send (tcp-stream-handle stream) buf count 0)))
            (if (>= bytes-written 0)
                bytes-written
                (error 'connection-reset :message "Write error"))))
      (error (e)
        (error 'network-error :message (format nil "Write failed: ~A" e))))))

(defun tcp-write-all (stream data)
  "Write all data, retrying as needed until complete"
  (let* ((data-bytes (etypecase data
                       (string (sb-ext:string-to-octets data))
                       (vector data)))
         (total-bytes (length data-bytes))
         (bytes-written 0))
    (loop while (< bytes-written total-bytes)
          do (incf bytes-written
                   (tcp-write stream data-bytes
                              :start bytes-written
                              :end total-bytes)))))

(defun tcp-flush (stream)
  "Flush any buffered data to the network"
  ;; For raw sockets, there's no buffering at this level
  ;; If using Lisp streams, flush those
  (when (tcp-stream-output stream)
    (finish-output (tcp-stream-output stream))))

(defun tcp-try-read (stream buffer)
  "Try to read without blocking"
  (handler-case
      (progn
        ;; Set socket to non-blocking
        (set-nonblocking (tcp-stream-handle stream))
        (lib:with-foreign-memory ((buf :char :count (length buffer)))
          (let ((bytes-read (%recv (tcp-stream-handle stream) buf (length buffer) 0)))
            (cond
              ((> bytes-read 0)
               ;; Copy data to buffer
               (loop for i from 0 below bytes-read
                     do (setf (aref buffer i)
                              (sb-sys:sap-ref-8 buf i)))
               bytes-read)
              ((= bytes-read 0)
               0)
              (t :would-block)))))
    (error () :would-block)))

(defun tcp-try-write (stream data)
  "Try to write without blocking"
  (handler-case
      (progn
        ;; Set socket to non-blocking
        (set-nonblocking (tcp-stream-handle stream))
        (let* ((data-bytes (etypecase data
                             (string (sb-ext:string-to-octets data))
                             (vector data)))
               (count (length data-bytes)))
          (lib:with-foreign-memory ((buf :char :count count))
            ;; Copy data to foreign buffer
            (loop for i from 0 below count
                  do (setf (sb-sys:sap-ref-8 buf i)
                           (aref data-bytes i)))
            (let ((bytes-written (%send (tcp-stream-handle stream) buf count 0)))
              (if (>= bytes-written 0)
                  bytes-written
                  :would-block)))))
    (error () :would-block)))

(defun tcp-poll-read (stream waker)
  "Poll for read readiness"
  ;; For now, just check if data is available
  ;; TODO: Integrate with waker callback system
  (declare (ignore waker))
  (let ((buffer (make-array 1 :element-type '(unsigned-byte 8))))
    (let ((result (tcp-try-read stream buffer)))
      (if (eq result :would-block)
          :pending
          result))))

(defun tcp-poll-write (stream waker)
  "Poll for write readiness"
  ;; For now, assume write is always ready
  ;; TODO: Integrate with waker callback system
  (declare (ignore stream waker))
  :ready)

(defun tcp-peer-addr (stream)
  "Get the remote peer's address"
  (tcp-stream-peer-address stream))

(defun tcp-local-addr-stream (stream)
  "Get the local address of the socket"
  (tcp-stream-local-address stream))

(defun tcp-shutdown (stream how)
  "Shutdown the TCP connection"
  (handler-case
      (let ((shutdown-how (ecase how
                            (:read +shut-rd+)
                            (:write +shut-wr+)
                            (:both +shut-rdwr+))))
        (%shutdown (tcp-stream-handle stream) shutdown-how)
        (when (member how '(:both :read))
          (setf (tcp-stream-connected-p stream) nil)))
    (error (e)
      (error 'network-error :message (format nil "Shutdown failed: ~A" e)))))

(defun tcp-stream-reader (stream)
  "Get a Lisp input stream for reading"
  (or (tcp-stream-input stream)
      (setf (tcp-stream-input stream)
            (sb-sys:make-fd-stream (tcp-stream-handle stream)
                                   :input t
                                   :buffering :full))))

(defun tcp-stream-writer (stream)
  "Get a Lisp output stream for writing"
  (or (tcp-stream-output stream)
      (setf (tcp-stream-output stream)
            (sb-sys:make-fd-stream (tcp-stream-handle stream)
                                   :output t
                                   :buffering :full))))

(defun tcp-connected-p (stream)
  "Check if the stream is still connected"
  (tcp-stream-connected-p stream))

;;; ============================================================================
;;; UDP Implementation
;;; ============================================================================

(defun udp-bind (address)
  "Create a UDP socket bound to an address"
  (let ((sock-addr (etypecase address
                     (socket-address address)
                     (string (parse-address address)))))
    (handler-case
        (let ((socket-fd (%socket +af-inet+ +sock-dgram+ +ipproto-udp+)))
          (when (< socket-fd 0)
            (error 'network-error :message "Failed to create UDP socket"))
          
          ;; Bind socket
          (lib:with-foreign-memory ((addr :char :count 16))
            (make-sockaddr-in-into addr (socket-address-ip sock-addr) (socket-address-port sock-addr))
            (when (< (%bind socket-fd addr 16) 0)
              (error 'address-in-use :message "Failed to bind UDP socket")))
          
          ;; Get actual bound address
          (lib:with-foreign-memory ((addr :char :count 16)
                                    (addrlen :int :count 1))
            (setf (sb-sys:sap-ref-32 addrlen 0) 16)
            (%getsockname socket-fd addr addrlen)
            (let ((local-addr (parse-sockaddr-in addr)))
              (make-instance 'udp-socket
                             :handle socket-fd
                             :local-address local-addr))))
      (error (e)
        (error 'network-error :message (format nil "UDP bind failed: ~A" e))))))

(defun udp-connect (socket address)
  "Connect UDP socket to a default peer"
  (let ((sock-addr (etypecase address
                     (socket-address address)
                     (string (parse-address address)))))
    (handler-case
        (progn
          (lib:with-foreign-memory ((addr :char :count 16))
            (make-sockaddr-in-into addr (socket-address-ip sock-addr) (socket-address-port sock-addr))
            (when (< (%connect (udp-socket-handle socket) addr 16) 0)
              (error 'network-error :message "UDP connect failed")))
          (setf (udp-socket-connected-peer socket) sock-addr))
      (error (e)
        (error 'network-error :message (format nil "UDP connect failed: ~A" e))))))

(defun udp-send (socket data address)
  "Send data to a specific address"
  (let* ((sock-addr (etypecase address
                      (socket-address address)
                      (string (parse-address address))))
         (data-bytes (etypecase data
                       (string (sb-ext:string-to-octets data))
                       (vector data)))
         (count (length data-bytes)))
    (handler-case
        (lib:with-foreign-memory ((buf :char :count count)
                                  (addr :char :count 16))
          ;; Copy data to foreign buffer
          (loop for i from 0 below count
                do (setf (sb-sys:sap-ref-8 buf i)
                         (aref data-bytes i)))
          ;; Set up address
          (make-sockaddr-in-into addr (socket-address-ip sock-addr) (socket-address-port sock-addr))
          (let ((bytes-sent (%sendto (udp-socket-handle socket) buf count 0 addr 16)))
            (if (>= bytes-sent 0)
                bytes-sent
                (error 'network-error :message "UDP send failed"))))
      (error (e)
        (error 'network-error :message (format nil "UDP send failed: ~A" e))))))

(defun udp-recv (socket buffer)
  "Receive data from any sender"
  (handler-case
      (lib:with-foreign-memory ((buf :char :count (length buffer))
                                (addr :char :count 16)
                                (addrlen :int :count 1))
        (setf (sb-sys:sap-ref-32 addrlen 0) 16)
        (let ((bytes-read (%recvfrom (udp-socket-handle socket)
                                     buf (length buffer) 0
                                     addr addrlen)))
          (when (> bytes-read 0)
            ;; Copy data to buffer
            (loop for i from 0 below bytes-read
                  do (setf (aref buffer i)
                           (sb-sys:sap-ref-8 buf i)))
            (let ((sender-addr (parse-sockaddr-in addr)))
              (values bytes-read sender-addr)))))
    (error (e)
      (error 'network-error :message (format nil "UDP recv failed: ~A" e)))))

(defun udp-send-to (socket data address)
  "Alias for udp-send for compatibility"
  (udp-send socket data address))

(defun udp-recv-from (socket buffer)
  "Alias for udp-recv for compatibility"
  (udp-recv socket buffer))

(defun udp-local-addr (socket)
  "Get the local address of the UDP socket"
  (udp-socket-local-address socket))

;;; ============================================================================
;;; Address Resolution
;;; ============================================================================

(defun make-socket-address (host port)
  "Create a socket address from host and port"
  (make-instance 'socket-address :ip host :port port))

(defun resolve-address (address-spec)
  "Resolve an address specification to socket addresses"
  (etypecase address-spec
    (socket-address (list address-spec))
    (string
     (let ((addr (parse-address address-spec)))
       (list addr)))
    (list
     (list (make-socket-address (first address-spec) (second address-spec))))))

(defun parse-address (string)
  "Parse an address string into components"
  (let* ((colon-pos (position #\: string :from-end t))
         (host (if colon-pos
                   (subseq string 0 colon-pos)
                   string))
         (port (if colon-pos
                   (parse-integer (subseq string (1+ colon-pos)))
                   80)))
    (make-socket-address host port)))

;;; ============================================================================
;;; Socket Options
;;; ============================================================================

(defun option-to-constants (option)
  "Convert keyword option to level and optname constants"
  (case option
    (:reuse-address (values +sol-socket+ +so-reuseaddr+))
    (:keep-alive (values +sol-socket+ +so-keepalive+))
    (:broadcast (values +sol-socket+ +so-broadcast+))
    (:linger (values +sol-socket+ +so-linger+))
    (:recv-buffer (values +sol-socket+ +so-rcvbuf+))
    (:send-buffer (values +sol-socket+ +so-sndbuf+))
    (:recv-timeout (values +sol-socket+ +so-rcvtimeo+))
    (:send-timeout (values +sol-socket+ +so-sndtimeo+))
    (:nodelay (values +ipproto-tcp-level+ +tcp-nodelay+))
    (otherwise (error "Unknown socket option: ~A" option))))

(defun get-socket-handle (socket)
  "Get the file descriptor from any socket type"
  (etypecase socket
    (tcp-listener (tcp-listener-handle socket))
    (tcp-stream (tcp-stream-handle socket))
    (udp-socket (udp-socket-handle socket))))

(defun set-socket-option (socket option value)
  "Set a socket option"
  (multiple-value-bind (level optname) (option-to-constants option)
    (let ((fd (get-socket-handle socket)))
      (handler-case
          (case option
            ((:reuse-address :keep-alive :broadcast :nodelay)
             ;; Boolean options
             (lib:with-foreign-memory ((optval :int :count 1))
               (setf (sb-sys:sap-ref-32 optval 0) (if value 1 0))
               (%setsockopt fd level optname optval 4)))
            ((:recv-buffer :send-buffer)
             ;; Integer size options
             (lib:with-foreign-memory ((optval :int :count 1))
               (setf (sb-sys:sap-ref-32 optval 0) value)
               (%setsockopt fd level optname optval 4)))
            ((:recv-timeout :send-timeout)
             ;; Timeout options (milliseconds to timeval)
             (lib:with-foreign-memory ((optval :char :count 16))
               ;; struct timeval { long tv_sec; long tv_usec; }
               (let ((seconds (floor value 1000))
                     (microseconds (* (mod value 1000) 1000)))
                 (setf (sb-sys:sap-ref-64 (sb-alien:alien-sap optval) 0) seconds)
                 (setf (sb-sys:sap-ref-64 (sb-alien:alien-sap optval) 8) microseconds))
               (%setsockopt fd level optname optval 16)))
            (:linger
             ;; Linger option
             (lib:with-foreign-memory ((optval :char :count 8))
               ;; struct linger { int l_onoff; int l_linger; }
               (if value
                   (progn
                     (setf (sb-sys:sap-ref-32 (sb-alien:alien-sap optval) 0) 1)
                     (setf (sb-sys:sap-ref-32 (sb-alien:alien-sap optval) 4) value))
                   (setf (sb-sys:sap-ref-32 (sb-alien:alien-sap optval) 0) 0))
               (%setsockopt fd level optname optval 8))))
        (error (e)
          (error 'network-error :message (format nil "Failed to set socket option: ~A" e)))))))

(defun get-socket-option (socket option)
  "Get a socket option value"
  (multiple-value-bind (level optname) (option-to-constants option)
    (let ((fd (get-socket-handle socket)))
      (handler-case
          (case option
            ((:reuse-address :keep-alive :broadcast :nodelay)
             ;; Boolean options
             (lib:with-foreign-memory ((optval :int :count 1)
                                       (optlen :int :count 1))
               (setf (sb-sys:sap-ref-32 optlen 0) 4)
               (%getsockopt fd level optname optval optlen)
               (not (zerop (sb-sys:sap-ref-32 optval 0)))))
            ((:recv-buffer :send-buffer)
             ;; Integer size options
             (lib:with-foreign-memory ((optval :int :count 1)
                                       (optlen :int :count 1))
               (setf (sb-sys:sap-ref-32 optlen 0) 4)
               (%getsockopt fd level optname optval optlen)
               (sb-sys:sap-ref-32 optval 0)))
            ((:recv-timeout :send-timeout)
             ;; Timeout options (timeval to milliseconds)
             (lib:with-foreign-memory ((optval :char :count 16)
                                       (optlen :int :count 1))
               (setf (sb-sys:sap-ref-32 optlen 0) 16)
               (%getsockopt fd level optname optval optlen)
               (let ((seconds (sb-sys:sap-ref-64 (sb-alien:alien-sap optval) 0))
                     (microseconds (sb-sys:sap-ref-64 (sb-alien:alien-sap optval) 8)))
                 (+ (* seconds 1000) (floor microseconds 1000)))))
            (:linger
             ;; Linger option
             (lib:with-foreign-memory ((optval :char :count 8)
                                       (optlen :int :count 1))
               (setf (sb-sys:sap-ref-32 optlen 0) 8)
               (%getsockopt fd level optname optval optlen)
               (let ((onoff (sb-sys:sap-ref-32 (sb-alien:alien-sap optval) 0)))
                 (if (zerop onoff)
                     nil
                     (sb-sys:sap-ref-32 (sb-alien:alien-sap optval) 4)))))
            (error (e)
             (error 'network-error :message (format nil "Failed to get socket option: ~A" e))))))))

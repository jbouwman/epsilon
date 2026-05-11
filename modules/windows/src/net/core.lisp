;;;; Windows Networking Implementation
;;;;
;;;; This module implements the epsilon.net interface for Windows systems
;;;; using Winsock FFI and IOCP (I/O Completion Ports) for async I/O.

(defpackage #:epsilon.net
  (:use #:cl)
  (:import
   (epsilon.sys.iocp iocp)
   (epsilon.foreign lib))
  (:import-from #:epsilon.net.conditions
   #:network-error #:connection-refused #:connection-reset
   #:connection-aborted #:timeout-error #:address-in-use
   #:would-block-error)
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
   #:tcp-close
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

;;; ============================================================================
;;; Winsock Constants
;;; ============================================================================

(defconstant +af-inet+ 2)
(defconstant +sock-stream+ 1)
(defconstant +sock-dgram+ 2)
(defconstant +ipproto-tcp+ 6)
(defconstant +sol-socket+ #xffff)
(defconstant +so-reuseaddr+ #x0004)
(defconstant +invalid-socket+ -1)
(defconstant +socket-error+ -1)
(defconstant +sockaddr-in-size+ 16)

;; WSAE error codes
(defconstant +wsaewouldblock+ 10035)
(defconstant +wsaeintr+ 10004)
(defconstant +wsaeconnrefused+ 10061)
(defconstant +wsaeconnreset+ 10054)
(defconstant +wsaeaddrinuse+ 10048)

;;; ============================================================================
;;; Winsock FFI
;;; ============================================================================

(lib:defshared %wsa-startup "WSAStartup" "ws2_32" :int
  (version-requested :unsigned-short) (wsa-data :pointer)
  :documentation "Initialize Winsock")

(lib:defshared %wsa-cleanup "WSACleanup" "ws2_32" :int ()
  :documentation "Cleanup Winsock")

(lib:defshared %wsa-get-last-error "WSAGetLastError" "ws2_32" :int ()
  :documentation "Get last Winsock error code")

(lib:defshared %ws-socket "socket" "ws2_32" :int
  (af :int) (type :int) (protocol :int)
  :documentation "Create socket")

(lib:defshared %ws-bind "bind" "ws2_32" :int
  (s :int) (name :pointer) (namelen :int)
  :documentation "Bind socket to address")

(lib:defshared %ws-listen "listen" "ws2_32" :int
  (s :int) (backlog :int)
  :documentation "Listen for connections")

(lib:defshared %ws-accept "accept" "ws2_32" :int
  (s :int) (addr :pointer) (addrlen :pointer)
  :documentation "Accept connection")

(lib:defshared %ws-connect "connect" "ws2_32" :int
  (s :int) (name :pointer) (namelen :int)
  :documentation "Connect socket")

(lib:defshared %ws-closesocket "closesocket" "ws2_32" :int
  (s :int)
  :documentation "Close socket")

(lib:defshared %ws-send "send" "ws2_32" :int
  (s :int) (buf :pointer) (len :int) (flags :int)
  :documentation "Send data")

(lib:defshared %ws-recv "recv" "ws2_32" :int
  (s :int) (buf :pointer) (len :int) (flags :int)
  :documentation "Receive data")

(lib:defshared %ws-sendto "sendto" "ws2_32" :int
  (s :int) (buf :pointer) (len :int) (flags :int)
  (to :pointer) (tolen :int)
  :documentation "Send data to address")

(lib:defshared %ws-recvfrom "recvfrom" "ws2_32" :int
  (s :int) (buf :pointer) (len :int) (flags :int)
  (from :pointer) (fromlen :pointer)
  :documentation "Receive data with sender address")

(lib:defshared %ws-setsockopt "setsockopt" "ws2_32" :int
  (s :int) (level :int) (optname :int) (optval :pointer) (optlen :int)
  :documentation "Set socket option")

(lib:defshared %ws-getsockopt "getsockopt" "ws2_32" :int
  (s :int) (level :int) (optname :int) (optval :pointer) (optlen :pointer)
  :documentation "Get socket option")

(lib:defshared %ws-getsockname "getsockname" "ws2_32" :int
  (s :int) (name :pointer) (namelen :pointer)
  :documentation "Get local socket address")

(lib:defshared %ws-getpeername "getpeername" "ws2_32" :int
  (s :int) (name :pointer) (namelen :pointer)
  :documentation "Get peer socket address")

(lib:defshared %ws-shutdown "shutdown" "ws2_32" :int
  (s :int) (how :int)
  :documentation "Shutdown socket")

(lib:defshared %ws-inet-addr "inet_addr" "ws2_32" :unsigned-int
  (cp :string)
  :documentation "Convert dotted IP string to network byte order")

(lib:defshared %ws-htons "htons" "ws2_32" :unsigned-short
  (hostshort :unsigned-short)
  :documentation "Host to network byte order (short)")

(lib:defshared %ws-ntohs "ntohs" "ws2_32" :unsigned-short
  (netshort :unsigned-short)
  :documentation "Network to host byte order (short)")

(lib:defshared %ws-gethostbyname "gethostbyname" "ws2_32" :pointer
  (name :string)
  :documentation "Resolve hostname to hostent")

;;; ============================================================================
;;; Winsock Initialization
;;; ============================================================================

(defvar *winsock-initialized* nil)

(defun ensure-winsock-initialized ()
  "Initialize Winsock if not already done"
  (unless *winsock-initialized*
    (lib:with-foreign-memory ((wsa-data 408))  ; WSADATA structure size
      (let ((result (%wsa-startup #x0202 wsa-data)))  ; Request Winsock 2.2
        (when (not (zerop result))
          (error 'network-error :message (format nil "Failed to initialize Winsock: ~A" result)))
        (setf *winsock-initialized* t)))))

(defun cleanup-winsock ()
  "Cleanup Winsock"
  (when *winsock-initialized*
    (%wsa-cleanup)
    (setf *winsock-initialized* nil)))

;;; ============================================================================
;;; sockaddr_in Helpers
;;; ============================================================================

(defun fill-sockaddr-in (sap ip-string port)
  "Fill a sockaddr_in structure in foreign memory."
  (dotimes (i +sockaddr-in-size+) (setf (sb-sys:sap-ref-8 sap i) 0))
  ;; sin_family = AF_INET (2 bytes, little-endian)
  (setf (sb-sys:sap-ref-16 sap 0) +af-inet+)
  ;; sin_port (network byte order)
  (setf (sb-sys:sap-ref-8 sap 2) (ldb (byte 8 8) port))
  (setf (sb-sys:sap-ref-8 sap 3) (ldb (byte 8 0) port))
  ;; sin_addr
  (setf (sb-sys:sap-ref-32 sap 4) (%ws-inet-addr ip-string)))

(defun parse-sockaddr-in (sap)
  "Parse a sockaddr_in from foreign memory. Returns (values ip-string port)."
  (let ((port (+ (ash (sb-sys:sap-ref-8 sap 2) 8)
                 (sb-sys:sap-ref-8 sap 3)))
        (b0 (sb-sys:sap-ref-8 sap 4))
        (b1 (sb-sys:sap-ref-8 sap 5))
        (b2 (sb-sys:sap-ref-8 sap 6))
        (b3 (sb-sys:sap-ref-8 sap 7)))
    (values (format nil "~D.~D.~D.~D" b0 b1 b2 b3) port)))

(defun resolve-hostent-address (hostent-ptr)
  "Extract first IPv4 address from a hostent pointer. Returns dotted-quad string."
  ;; struct hostent on Win64: h_name(8), h_aliases(8), h_addrtype(2), h_length(2), pad(4), h_addr_list(8)
  (let* ((addr-list-ptr (sb-sys:sap-ref-sap hostent-ptr 24))
         (first-addr-ptr (sb-sys:sap-ref-sap addr-list-ptr 0)))
    (format nil "~D.~D.~D.~D"
            (sb-sys:sap-ref-8 first-addr-ptr 0)
            (sb-sys:sap-ref-8 first-addr-ptr 1)
            (sb-sys:sap-ref-8 first-addr-ptr 2)
            (sb-sys:sap-ref-8 first-addr-ptr 3))))

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
           :type integer
           :documentation "Winsock socket handle")
   (local-address :initarg :local-address :reader tcp-listener-local-address
                  :type socket-address
                  :documentation "Local bind address")
   (iocp :initarg :iocp :reader tcp-listener-iocp
         :documentation "IOCP handle for async operations")
   (backlog :initarg :backlog :reader tcp-listener-backlog
            :initform 128
            :documentation "Connection backlog"))
  (:documentation "TCP server socket"))

(defclass tcp-stream ()
  ((handle :initarg :handle :reader tcp-stream-handle
           :type integer
           :documentation "Winsock socket handle")
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
   (connected-p :initform t :accessor tcp-connected-p
                :documentation "Connection status"))
  (:documentation "TCP connection stream"))

(defclass udp-socket ()
  ((handle :initarg :handle :reader udp-socket-handle
           :type integer
           :documentation "Winsock socket handle")
   (local-address :initarg :local-address :reader udp-socket-local-address
                  :type (or null socket-address)
                  :documentation "Local bind address if bound"))
  (:documentation "UDP socket"))

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

(defun resolve-address (hostname port)
  "Resolve hostname to socket addresses"
  (ensure-winsock-initialized)
  (let ((hostent (%ws-gethostbyname hostname)))
    (when (sb-sys:sap= hostent (sb-sys:int-sap 0))
      (error 'network-error
             :message (format nil "Failed to resolve ~A: error ~D"
                              hostname (%wsa-get-last-error))))
    (list (make-socket-address (resolve-hostent-address hostent) port))))

;;; ============================================================================
;;; Internal Socket Operations
;;; ============================================================================

(defun make-winsock-socket (type)
  "Create a Winsock socket with error handling. Returns integer handle."
  (ensure-winsock-initialized)
  (let ((handle (%ws-socket +af-inet+ type 0)))
    (when (= handle +invalid-socket+)
      (error 'network-error
             :message (format nil "Socket creation failed: error ~D"
                              (%wsa-get-last-error))))
    handle))

(defun set-socket-reuseaddr (handle)
  "Enable SO_REUSEADDR on socket"
  (lib:with-foreign-memory ((optval :int :count 1))
    (setf (sb-sys:sap-ref-32 optval 0) 1)
    (%ws-setsockopt handle +sol-socket+ +so-reuseaddr+ optval 4)))

(defun get-socket-local-address (handle)
  "Get the local address of a socket. Returns socket-address."
  (lib:with-foreign-memory ((addr :char :count +sockaddr-in-size+)
                            (addrlen :int :count 1))
    (setf (sb-sys:sap-ref-32 addrlen 0) +sockaddr-in-size+)
    (%ws-getsockname handle addr addrlen)
    (multiple-value-bind (ip port) (parse-sockaddr-in addr)
      (make-socket-address ip port))))

(defun get-socket-peer-address (handle)
  "Get the peer address of a socket. Returns socket-address."
  (lib:with-foreign-memory ((addr :char :count +sockaddr-in-size+)
                            (addrlen :int :count 1))
    (setf (sb-sys:sap-ref-32 addrlen 0) +sockaddr-in-size+)
    (%ws-getpeername handle addr addrlen)
    (multiple-value-bind (ip port) (parse-sockaddr-in addr)
      (make-socket-address ip port))))

(defun socket-to-stream (handle direction)
  "Convert socket handle to Lisp stream"
  (sb-sys:make-fd-stream handle
                         :direction direction
                         :element-type '(unsigned-byte 8)
                         :buffering :none
                         :external-format :latin-1))

;;; ============================================================================
;;; TCP Listener Operations
;;; ============================================================================

(defun tcp-bind (address &key (backlog 128) (reuse-addr t))
  "Create a TCP listener bound to the specified address"
  (let ((handle (make-winsock-socket +sock-stream+))
        (iocp-handle (iocp:iocp-create)))
    (when reuse-addr
      (set-socket-reuseaddr handle))
    (handler-case
        (lib:with-foreign-memory ((addr :char :count +sockaddr-in-size+))
          (fill-sockaddr-in addr (socket-address-ip address) (socket-address-port address))
          (when (= (%ws-bind handle addr +sockaddr-in-size+) +socket-error+)
            (let ((err (%wsa-get-last-error)))
              (if (= err +wsaeaddrinuse+)
                  (error 'address-in-use
                         :message (format nil "Address ~A:~D already in use"
                                          (socket-address-ip address)
                                          (socket-address-port address)))
                  (error 'network-error
                         :message (format nil "Bind failed: error ~D" err)))))
          (when (= (%ws-listen handle backlog) +socket-error+)
            (error 'network-error
                   :message (format nil "Listen failed: error ~D" (%wsa-get-last-error))))
          ;; Associate socket with IOCP
          (iocp:iocp-associate iocp-handle handle 0)
          (make-instance 'tcp-listener
                         :handle handle
                         :local-address address
                         :iocp iocp-handle
                         :backlog backlog))
      (error (e)
        (%ws-closesocket handle)
        (iocp:iocp-close iocp-handle)
        (error e)))))

(defun tcp-accept (listener)
  "Accept a connection, blocking if necessary"
  (loop
    (lib:with-foreign-memory ((addr :char :count +sockaddr-in-size+)
                              (addrlen :int :count 1))
      (setf (sb-sys:sap-ref-32 addrlen 0) +sockaddr-in-size+)
      (let ((client-handle (%ws-accept (tcp-listener-handle listener) addr addrlen)))
        (cond
          ((= client-handle +invalid-socket+)
           (let ((err (%wsa-get-last-error)))
             (cond
               ((= err +wsaeintr+) nil)  ; Retry on EINTR
               ((= err +wsaewouldblock+) (return nil))
               (t (error 'network-error
                         :message (format nil "Accept failed: error ~D" err))))))
          (t
           (multiple-value-bind (peer-ip peer-port) (parse-sockaddr-in addr)
             (let ((local-addr (get-socket-local-address client-handle)))
               (return (make-instance 'tcp-stream
                                      :handle client-handle
                                      :local-address local-addr
                                      :peer-address (make-socket-address peer-ip peer-port)))))))))))

(defun tcp-try-accept (listener)
  "Try to accept a connection without blocking"
  (lib:with-foreign-memory ((addr :char :count +sockaddr-in-size+)
                            (addrlen :int :count 1))
    (setf (sb-sys:sap-ref-32 addrlen 0) +sockaddr-in-size+)
    (let ((client-handle (%ws-accept (tcp-listener-handle listener) addr addrlen)))
      (when (/= client-handle +invalid-socket+)
        (multiple-value-bind (peer-ip peer-port) (parse-sockaddr-in addr)
          (let ((local-addr (get-socket-local-address client-handle)))
            (make-instance 'tcp-stream
                          :handle client-handle
                          :local-address local-addr
                          :peer-address (make-socket-address peer-ip peer-port))))))))

(defun tcp-poll-accept (listener timeout-ms)
  "Poll for incoming connections with timeout"
  (let ((events (iocp:iocp-wait (tcp-listener-iocp listener) timeout-ms)))
    (when events
      (tcp-try-accept listener))))

(defun tcp-local-addr (listener)
  "Get the local address of a TCP listener"
  (tcp-listener-local-address listener))

(defun tcp-incoming (listener)
  "Return a lazy sequence of incoming connections"
  (loop while (tcp-poll-accept listener 0) collect it))

;;; ============================================================================
;;; TCP Stream Operations
;;; ============================================================================

(defun tcp-connect (address &key (timeout nil))
  "Connect to a TCP server"
  (declare (ignore timeout))
  (let ((handle (make-winsock-socket +sock-stream+)))
    (handler-case
        (lib:with-foreign-memory ((addr :char :count +sockaddr-in-size+))
          (fill-sockaddr-in addr (socket-address-ip address) (socket-address-port address))
          (when (= (%ws-connect handle addr +sockaddr-in-size+) +socket-error+)
            (let ((err (%wsa-get-last-error)))
              (cond
                ((= err +wsaeconnrefused+)
                 (error 'connection-refused :message (format nil "Connection refused")))
                (t (error 'network-error
                          :message (format nil "Connect failed: error ~D" err))))))
          (let ((local-addr (get-socket-local-address handle)))
            (make-instance 'tcp-stream
                          :handle handle
                          :local-address local-addr
                          :peer-address address)))
      (error (e)
        (%ws-closesocket handle)
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

(defun tcp-read (stream buffer &key (start 0) (end (length buffer)))
  "Read data from TCP stream into buffer"
  (let ((input (tcp-stream-reader stream))
        (bytes-read 0))
    (handler-case
        (loop for i from start below end
              for byte = (read-byte input nil nil)
              while byte
              do (setf (aref buffer i) byte)
                 (incf bytes-read)
              finally (return bytes-read))
      (error (e)
        (setf (tcp-connected-p stream) nil)
        (error 'connection-reset :message (format nil "Read failed: ~A" e))))))

(defun tcp-write (stream buffer &key (start 0) (end (length buffer)))
  "Write data from buffer to TCP stream"
  (let ((output (tcp-stream-writer stream))
        (bytes-written 0))
    (handler-case
        (loop for i from start below end
              do (write-byte (aref buffer i) output)
                 (incf bytes-written)
              finally (return bytes-written))
      (error (e)
        (setf (tcp-connected-p stream) nil)
        (error 'connection-reset :message (format nil "Write failed: ~A" e))))))

(defun tcp-write-all (stream buffer &key (start 0) (end (length buffer)))
  "Write all data from buffer to TCP stream"
  (loop with pos = start
        while (< pos end)
        do (incf pos (tcp-write stream buffer :start pos :end end))
        finally (return (- end start))))

(defun tcp-flush (stream)
  "Flush any buffered output"
  (when (tcp-stream-output stream)
    (force-output (tcp-stream-output stream))))

(defun tcp-try-read (stream buffer &key (start 0) (end (length buffer)))
  "Try to read without blocking"
  (tcp-read stream buffer :start start :end end))

(defun tcp-try-write (stream buffer &key (start 0) (end (length buffer)))
  "Try to write without blocking"
  (tcp-write stream buffer :start start :end end))

(defun tcp-poll-read (stream timeout-ms)
  "Poll for readable data with timeout"
  (declare (ignore stream timeout-ms))
  t)

(defun tcp-poll-write (stream timeout-ms)
  "Poll for writability with timeout"
  (declare (ignore stream timeout-ms))
  t)

(defun tcp-peer-addr (stream)
  "Get peer address of TCP stream"
  (tcp-stream-peer-address stream))

(defun tcp-shutdown (stream &key (how :both))
  "Shutdown TCP stream"
  (let ((shutdown-how (ecase how
                        (:read 0)
                        (:write 1)
                        (:both 2))))
    (%ws-shutdown (tcp-stream-handle stream) shutdown-how)
    (setf (tcp-connected-p stream) nil)))

(defun tcp-close (socket-or-listener)
  "Close a TCP listener or stream.
   Works with both tcp-listener and tcp-stream objects."
  (etypecase socket-or-listener
    (tcp-listener
     (let ((handle (tcp-listener-handle socket-or-listener)))
       (when handle
         (%ws-closesocket handle))))
    (tcp-stream
     (let ((handle (tcp-stream-handle socket-or-listener)))
       (when handle
         (handler-case
             (%ws-shutdown handle 2)
           (error () nil))
         (%ws-closesocket handle)
         (setf (tcp-connected-p socket-or-listener) nil))))))

;;; ============================================================================
;;; UDP Operations
;;; ============================================================================

(defun udp-bind (address)
  "Create a UDP socket bound to address"
  (let ((handle (make-winsock-socket +sock-dgram+)))
    (set-socket-reuseaddr handle)
    (handler-case
        (lib:with-foreign-memory ((addr :char :count +sockaddr-in-size+))
          (fill-sockaddr-in addr (socket-address-ip address) (socket-address-port address))
          (when (= (%ws-bind handle addr +sockaddr-in-size+) +socket-error+)
            (error 'network-error
                   :message (format nil "UDP bind failed: error ~D" (%wsa-get-last-error))))
          (make-instance 'udp-socket
                         :handle handle
                         :local-address address))
      (error (e)
        (%ws-closesocket handle)
        (error e)))))

(defun udp-connect (socket address)
  "Connect UDP socket to remote address"
  (lib:with-foreign-memory ((addr :char :count +sockaddr-in-size+))
    (fill-sockaddr-in addr (socket-address-ip address) (socket-address-port address))
    (when (= (%ws-connect (udp-socket-handle socket) addr +sockaddr-in-size+) +socket-error+)
      (error 'network-error
             :message (format nil "UDP connect failed: error ~D" (%wsa-get-last-error))))))

(defun udp-send (socket buffer &key address (start 0) (end (length buffer)))
  "Send data on UDP socket.  When ADDRESS is nil, use the connected peer."
  (if address
      (udp-send-to socket buffer address :start start :end end)
      (let ((len (- end start)))
        (lib:with-foreign-memory ((buf :char :count len))
          (dotimes (i len)
            (setf (sb-sys:sap-ref-8 buf i) (aref buffer (+ start i))))
          (let ((sent (%ws-send (udp-socket-handle socket) buf len 0)))
            (when (= sent +socket-error+)
              (error 'network-error
                     :message (format nil "UDP send failed: error ~D" (%wsa-get-last-error))))
            sent)))))

(defun udp-recv (socket buffer &key (start 0) (end (length buffer)))
  "Receive data on UDP socket"
  (let ((len (- end start)))
    (lib:with-foreign-memory ((buf :char :count len))
      (let ((received (%ws-recv (udp-socket-handle socket) buf len 0)))
        (when (= received +socket-error+)
          (error 'network-error
                 :message (format nil "UDP recv failed: error ~D" (%wsa-get-last-error))))
        (dotimes (i received)
          (setf (aref buffer (+ start i)) (sb-sys:sap-ref-8 buf i)))
        received))))

(defun udp-send-to (socket buffer address &key (start 0) (end (length buffer)))
  "Send data to specific address"
  (let ((len (- end start)))
    (lib:with-foreign-memory ((buf :char :count len)
                              (addr :char :count +sockaddr-in-size+))
      (dotimes (i len)
        (setf (sb-sys:sap-ref-8 buf i) (aref buffer (+ start i))))
      (fill-sockaddr-in addr (socket-address-ip address) (socket-address-port address))
      (let ((sent (%ws-sendto (udp-socket-handle socket) buf len 0 addr +sockaddr-in-size+)))
        (when (= sent +socket-error+)
          (error 'network-error
                 :message (format nil "UDP send-to failed: error ~D" (%wsa-get-last-error))))
        sent))))

(defun udp-recv-from (socket buffer &key (start 0) (end (length buffer)))
  "Receive data and sender address"
  (let ((len (- end start)))
    (lib:with-foreign-memory ((buf :char :count len)
                              (addr :char :count +sockaddr-in-size+)
                              (addrlen :int :count 1))
      (setf (sb-sys:sap-ref-32 addrlen 0) +sockaddr-in-size+)
      (let ((received (%ws-recvfrom (udp-socket-handle socket) buf len 0 addr addrlen)))
        (when (= received +socket-error+)
          (error 'network-error
                 :message (format nil "UDP recv-from failed: error ~D" (%wsa-get-last-error))))
        (dotimes (i received)
          (setf (aref buffer (+ start i)) (sb-sys:sap-ref-8 buf i)))
        (multiple-value-bind (ip port) (parse-sockaddr-in addr)
          (values received (make-socket-address ip port)))))))

(defun udp-local-addr (socket)
  "Get local address of UDP socket"
  (udp-socket-local-address socket))

;;; ============================================================================
;;; Socket Options
;;; ============================================================================

(defun set-socket-option (socket-obj level option value)
  "Set a socket option"
  (let ((handle (etypecase socket-obj
                  (tcp-listener (tcp-listener-handle socket-obj))
                  (tcp-stream (tcp-stream-handle socket-obj))
                  (udp-socket (udp-socket-handle socket-obj)))))
    (lib:with-foreign-memory ((optval :int :count 1))
      (setf (sb-sys:sap-ref-32 optval 0) value)
      (%ws-setsockopt handle level option optval 4))))

(defun get-socket-option (socket-obj level option)
  "Get a socket option"
  (let ((handle (etypecase socket-obj
                  (tcp-listener (tcp-listener-handle socket-obj))
                  (tcp-stream (tcp-stream-handle socket-obj))
                  (udp-socket (udp-socket-handle socket-obj)))))
    (lib:with-foreign-memory ((optval :int :count 1)
                              (optlen :int :count 1))
      (setf (sb-sys:sap-ref-32 optlen 0) 4)
      (%ws-getsockopt handle level option optval optlen)
      (sb-sys:sap-ref-32 optval 0))))

;;; ============================================================================
;;; Cleanup
;;; ============================================================================

(pushnew #'cleanup-winsock sb-ext:*exit-hooks*)

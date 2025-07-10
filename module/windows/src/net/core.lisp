(defpackage #:epsilon.net
  (:use #:cl)
  (:local-nicknames
   (#:iocp #:epsilon.sys.iocp)
   (#:lib #:epsilon.sys.lib))
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
   (iocp :initarg :iocp :reader listener-iocp)))

(defclass connection (socket)
  ((remote-address :initarg :remote-address :accessor connection-remote-address)
   (remote-port :initarg :remote-port :accessor connection-remote-port)))

(defclass socket-stream-wrapper ()
  ((socket :initarg :socket :reader stream-socket)
   (read-buffer :initform (make-array 4096 :element-type '(unsigned-byte 8)) 
                :reader stream-read-buffer)
   (write-buffer :initform (make-array 4096 :element-type '(unsigned-byte 8)) 
                 :reader stream-write-buffer)))

;;; Socket Constants (Windows-specific)

;; Address families
(defconstant +af-inet+ 2)
(defconstant +af-inet6+ 23)

;; Socket types
(defconstant +sock-stream+ 1)
(defconstant +sock-dgram+ 2)

;; Protocols
(defconstant +ipproto-tcp+ 6)
(defconstant +ipproto-udp+ 17)

;;; Winsock Initialization

(lib:defshared %wsa-startup "WSAStartup" "ws2_32" :int
  (version-requested :unsigned-short) (wsa-data :pointer)
  :documentation "Initialize Winsock")

(lib:defshared %wsa-cleanup "WSACleanup" "ws2_32" :int ()
  :documentation "Cleanup Winsock")

(defvar *winsock-initialized* nil)

(defun ensure-winsock-initialized ()
  "Initialize Winsock if not already done"
  (unless *winsock-initialized*
    (lib:with-foreign-memory ((wsa-data :char :count 408))  ; WSADATA structure
      (let ((result (%wsa-startup #x0202 wsa-data)))  ; Request Winsock 2.2
        (when (not (zerop result))
          (error "Failed to initialize Winsock: ~A" result))
        (setf *winsock-initialized* t)))))

(defun cleanup-winsock ()
  "Cleanup Winsock"
  (when *winsock-initialized*
    (%wsa-cleanup)
    (setf *winsock-initialized* nil)))

;;; Socket FFI Bindings

(lib:defshared %socket "socket" "ws2_32" :pointer
  (af :int) (type :int) (protocol :int)
  :documentation "Create socket")

(lib:defshared %bind "bind" "ws2_32" :int
  (socket :pointer) (addr :pointer) (namelen :int)
  :documentation "Bind socket to address")

(lib:defshared %listen "listen" "ws2_32" :int
  (socket :pointer) (backlog :int)
  :documentation "Listen for connections")

(lib:defshared %accept "accept" "ws2_32" :pointer
  (socket :pointer) (addr :pointer) (addrlen :pointer)
  :documentation "Accept connection")

(lib:defshared %connect "connect" "ws2_32" :int
  (socket :pointer) (addr :pointer) (namelen :int)
  :documentation "Connect socket")

(lib:defshared %send "send" "ws2_32" :int
  (socket :pointer) (buf :pointer) (len :int) (flags :int)
  :documentation "Send data on socket")

(lib:defshared %recv "recv" "ws2_32" :int
  (socket :pointer) (buf :pointer) (len :int) (flags :int)
  :documentation "Receive data from socket")

(lib:defshared %closesocket "closesocket" "ws2_32" :int
  (socket :pointer)
  :documentation "Close socket")

;;; Address Handling (Windows sockaddr_in)

;; struct sockaddr_in {
;;   short   sin_family;
;;   u_short sin_port;
;;   struct  in_addr sin_addr;
;;   char    sin_zero[8];
;; };

(defun make-sockaddr-in (ip-address port)
  "Create sockaddr_in structure for IPv4 (Windows version)"
  (lib:with-foreign-memory ((addr :char :count 16))
    ;; sin_family (2 bytes)
    (setf (sb-alien:sap-ref-16 (sb-alien:alien-sap addr) 0) +af-inet+)
    ;; sin_port (2 bytes, network byte order)
    (setf (sb-alien:sap-ref-16 (sb-alien:alien-sap addr) 2)
          (logior (ash (logand port #xff) 8)
                  (ash (logand port #xff00) -8)))
    ;; sin_addr (4 bytes, network byte order)
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
  (ensure-winsock-initialized)
  (let ((sock (%socket +af-inet+ +sock-stream+ +ipproto-tcp+)))
    (when (= (sb-alien:alien-sap sock) iocp:+invalid-handle-value+)
      (error "Failed to create TCP socket"))
    sock))

(defun create-udp-socket ()
  "Create UDP socket"
  (ensure-winsock-initialized)
  (let ((sock (%socket +af-inet+ +sock-dgram+ +ipproto-udp+)))
    (when (= (sb-alien:alien-sap sock) iocp:+invalid-handle-value+)
      (error "Failed to create UDP socket"))
    sock))

(defun close-socket (socket-handle)
  "Close socket"
  (%closesocket socket-handle))

;;; Core Socket Operations using IOCP

(defun socket-listen (address port &key (reuse-address t) (backlog 5) type)
  "Create a listening socket on the given address and port using IOCP"
  (declare (ignore type reuse-address)) ;; TODO: implement socket options
  (handler-case
      (let* ((socket-handle (create-tcp-socket))
             (iocp-handle (iocp:create-io-completion-port))
             (host (if (or (string= address "0.0.0.0") (string= address "*"))
                       "0.0.0.0"
                       address)))
        
        ;; Bind socket
        (lib:with-foreign-memory ((addr :char :count 16))
          (let ((sockaddr (make-sockaddr-in host port)))
            (loop for i from 0 to 15
                  do (setf (sb-alien:deref addr i) (sb-alien:deref sockaddr i)))
            (when (not (zerop (%bind socket-handle addr 16)))
              (error "Failed to bind socket"))))
        
        ;; Listen
        (when (not (zerop (%listen socket-handle backlog)))
          (error "Failed to listen on socket"))
        
        ;; Associate with IOCP
        (iocp:associate-socket iocp-handle socket-handle 
                               (iocp:completion-key-to-pointer
                                (iocp:make-completion-key :socket socket-handle 
                                                          :operation :accept)))
        
        ;; Create and return listener object
        (make-instance 'listener
                       :handle socket-handle
                       :backlog backlog
                       :address address
                       :port port
                       :iocp iocp-handle))
    (error (e)
      (error 'network-error :message (format nil "Error creating listener: ~A" e)))))

(defun socket-connect (host port &key (timeout 30) type)
  "Connect to a remote host and port using IOCP"
  (declare (ignore type timeout)) ;; TODO: implement timeout
  (handler-case
      (let ((socket-handle (create-tcp-socket)))
        
        ;; Connect
        (lib:with-foreign-memory ((addr :char :count 16))
          (let ((sockaddr (make-sockaddr-in host port)))
            (loop for i from 0 to 15
                  do (setf (sb-alien:deref addr i) (sb-alien:deref sockaddr i)))
            (when (not (zerop (%connect socket-handle addr 16)))
              (error "Failed to connect"))))
        
        ;; Create and return connection object
        (make-instance 'connection
                       :handle socket-handle
                       :remote-address host
                       :remote-port port))
    (error (e)
      (error 'network-error :message (format nil "Error connecting to ~A:~A - ~A" host port e)))))

(defun socket-accept (listener)
  "Accept a connection from a listener using IOCP"
  (handler-case
      (let ((iocp-handle (listener-iocp listener)))
        ;; Wait for accept completion
        (multiple-value-bind (result bytes-transferred completion-key overlapped)
            (iocp:wait-for-completion iocp-handle 0) ; Non-blocking check
          (when result
            (lib:with-foreign-memory ((addr :char :count 16)
                                      (addrlen :pointer :count 1))
              (setf (sb-alien:deref addrlen 0) 16)
              (let ((client-socket (%accept (socket-handle listener) addr addrlen)))
                (when (= (sb-alien:alien-sap client-socket) iocp:+invalid-handle-value+)
                  (error "Failed to accept connection"))
                
                ;; Extract remote address info (simplified)
                (make-instance 'connection
                               :handle client-socket
                               :remote-address "unknown" ; TODO: extract from addr
                               :remote-port 0))))))      ; TODO: extract from addr
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
        
        ;; Close IOCP if it's a listener
        (when (and (typep socket 'listener) (listener-iocp socket))
          (iocp:close-handle (listener-iocp socket)))
        
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
            ;; Create a basic socket stream wrapper for Windows
            (make-instance 'socket-stream-wrapper :socket socket))))

(defun socket-from-stream (stream)
  "Get the socket object associated with a stream"
  (if (typep stream 'socket-stream-wrapper)
      (stream-socket stream)
      (error "Stream is not a socket stream wrapper")))

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

;;; Async Operations using IOCP

(defun async-connect (host port &key callback)
  "Asynchronously connect to host:port using IOCP"
  (handler-case
      (let* ((socket-handle (create-tcp-socket))
             (iocp-handle (iocp:create-io-completion-port))
             (overlapped (iocp:make-overlapped)))
        
        ;; Associate socket with IOCP
        (iocp:associate-socket iocp-handle socket-handle 
                               (iocp:completion-key-to-pointer
                                (iocp:make-completion-key :socket socket-handle 
                                                          :operation :connect)))
        
        ;; Create sockaddr structure for connection
        (lib:with-foreign-memory ((addr :char :count 16)
                                  (overlapped-buf :char :count (iocp:overlapped-size)))
          (let ((sockaddr (make-sockaddr-in host port)))
            ;; Copy sockaddr
            (loop for i from 0 to 15
                  do (setf (sb-alien:deref addr i) (sb-alien:deref sockaddr i)))
            
            ;; Pack overlapped structure
            (iocp:pack-overlapped overlapped overlapped-buf 0)
            
            ;; Initiate async connect
            (let ((result (iocp:async-connect-socket socket-handle addr overlapped-buf)))
              (if callback
                  ;; Non-blocking with callback
                  (progn
                    (funcall callback 
                             (if (zerop result)
                                 (make-instance 'connection
                                                :handle socket-handle
                                                :remote-address host
                                                :remote-port port)
                                 (error 'network-error :message "Async connect failed")))
                    nil)
                  ;; Blocking wait for completion
                  (multiple-value-bind (wait-result bytes completion-key overlapped-ptr)
                      (iocp:wait-for-completion iocp-handle 30000) ; 30 second timeout
                    (if (iocp:completion-success-p wait-result)
                        (make-instance 'connection
                                       :handle socket-handle
                                       :remote-address host
                                       :remote-port port)
                        (error 'network-error :message "Connect operation failed or timed out")))))))
    (error (e)
      (error 'network-error :message (format nil "Error in async-connect: ~A" e)))))

(defun async-accept (listener &key callback)
  "Asynchronously accept connection using IOCP"
  (handler-case
      (let* ((iocp-handle (listener-iocp listener))
             (overlapped (iocp:make-overlapped)))
        
        (lib:with-foreign-memory ((overlapped-buf :char :count (iocp:overlapped-size)))
          ;; Pack overlapped structure
          (iocp:pack-overlapped overlapped overlapped-buf 0)
          
          ;; Initiate async accept
          (multiple-value-bind (accept-socket addr)
              (iocp:async-accept-socket (socket-handle listener) overlapped-buf)
            (if callback
                ;; Non-blocking with callback
                (if accept-socket
                    (funcall callback 
                             (make-instance 'connection
                                            :handle accept-socket
                                            :remote-address "unknown" ; TODO: extract from addr
                                            :remote-port 0))         ; TODO: extract from addr
                    (funcall callback nil))
                ;; Blocking wait for completion
                (if accept-socket
                    (make-instance 'connection
                                   :handle accept-socket
                                   :remote-address "unknown" ; TODO: extract from addr
                                   :remote-port 0)           ; TODO: extract from addr
                    (multiple-value-bind (wait-result bytes completion-key overlapped-ptr)
                        (iocp:wait-for-completion iocp-handle 0) ; Non-blocking check
                      (if (and (iocp:completion-success-p wait-result) accept-socket)
                          (make-instance 'connection
                                         :handle accept-socket
                                         :remote-address "unknown"
                                         :remote-port 0)
                          nil)))))))
    (error (e)
      (error 'network-error :message (format nil "Error in async-accept: ~A" e)))))

(defun async-read (socket buffer &key callback)
  "Asynchronously read from socket using IOCP"
  (handler-case
      (let* ((socket-handle (socket-handle socket))
             (iocp-handle (iocp:create-io-completion-port))
             (overlapped (iocp:make-overlapped)))
        
        ;; Associate socket with IOCP if not already done
        (iocp:associate-socket iocp-handle socket-handle 
                               (iocp:completion-key-to-pointer
                                (iocp:make-completion-key :socket socket-handle 
                                                          :operation :read)))
        
        (lib:with-foreign-memory ((overlapped-buf :char :count (iocp:overlapped-size))
                                  (read-buffer :char :count (length buffer)))
          ;; Pack overlapped structure
          (iocp:pack-overlapped overlapped overlapped-buf 0)
          
          ;; Initiate async read
          (multiple-value-bind (result bytes-read)
              (iocp:async-read-socket socket-handle read-buffer overlapped-buf)
            (if callback
                ;; Non-blocking with callback
                (if (or (zerop result) (= result iocp:+error-io-pending+))
                    (progn
                      ;; Operation is pending or completed immediately
                      (multiple-value-bind (wait-result bytes completion-key overlapped-ptr)
                          (iocp:wait-for-completion iocp-handle 0) ; Non-blocking check
                        (when (iocp:completion-success-p wait-result)
                          ;; Copy data from foreign buffer to Lisp buffer
                          (loop for i from 0 below (min bytes (length buffer))
                                do (setf (aref buffer i) (sb-alien:deref read-buffer i)))
                          (funcall callback buffer bytes)))
                      nil)
                    (error 'network-error :message "Read operation failed"))
                ;; Blocking wait for completion
                (if (or (zerop result) (= result iocp:+error-io-pending+))
                    (multiple-value-bind (wait-result bytes completion-key overlapped-ptr)
                        (iocp:wait-for-completion iocp-handle 30000) ; 30 second timeout
                      (if (iocp:completion-success-p wait-result)
                          (progn
                            ;; Copy data from foreign buffer to Lisp buffer
                            (loop for i from 0 below (min bytes (length buffer))
                                  do (setf (aref buffer i) (sb-alien:deref read-buffer i)))
                            bytes)
                          (error 'network-error :message "Read operation failed or timed out")))
                    (error 'network-error :message "Read operation failed immediately"))))))
    (error (e)
      (error 'network-error :message (format nil "Error in async-read: ~A" e)))))

(defun async-write (socket buffer &key callback)
  "Asynchronously write to socket using IOCP"
  (handler-case
      (let* ((socket-handle (socket-handle socket))
             (iocp-handle (iocp:create-io-completion-port))
             (overlapped (iocp:make-overlapped)))
        
        ;; Associate socket with IOCP if not already done
        (iocp:associate-socket iocp-handle socket-handle 
                               (iocp:completion-key-to-pointer
                                (iocp:make-completion-key :socket socket-handle 
                                                          :operation :write)))
        
        (lib:with-foreign-memory ((overlapped-buf :char :count (iocp:overlapped-size))
                                  (write-buffer :char :count (length buffer)))
          ;; Copy data from Lisp buffer to foreign buffer
          (loop for i from 0 below (length buffer)
                do (setf (sb-alien:deref write-buffer i) (aref buffer i)))
          
          ;; Pack overlapped structure
          (iocp:pack-overlapped overlapped overlapped-buf 0)
          
          ;; Initiate async write
          (multiple-value-bind (result bytes-written)
              (iocp:async-write-socket socket-handle write-buffer overlapped-buf)
            (if callback
                ;; Non-blocking with callback
                (if (or (zerop result) (= result iocp:+error-io-pending+))
                    (progn
                      ;; Operation is pending or completed immediately
                      (multiple-value-bind (wait-result bytes completion-key overlapped-ptr)
                          (iocp:wait-for-completion iocp-handle 0) ; Non-blocking check
                        (when (iocp:completion-success-p wait-result)
                          (funcall callback bytes)))
                      nil)
                    (error 'network-error :message "Write operation failed"))
                ;; Blocking wait for completion
                (if (or (zerop result) (= result iocp:+error-io-pending+))
                    (multiple-value-bind (wait-result bytes completion-key overlapped-ptr)
                        (iocp:wait-for-completion iocp-handle 30000) ; 30 second timeout
                      (if (iocp:completion-success-p wait-result)
                          bytes
                          (error 'network-error :message "Write operation failed or timed out")))
                    (error 'network-error :message "Write operation failed immediately"))))))
    (error (e)
      (error 'network-error :message (format nil "Error in async-write: ~A" e)))))

(defclass future ()
  ((operation :initarg :operation :reader future-operation)
   (completed :initform nil :accessor future-completed-p)
   (result :initform nil :accessor future-result)
   (error :initform nil :accessor future-error)
   (iocp :initarg :iocp :reader future-iocp)
   (completion-key :initarg :completion-key :reader future-completion-key)))

(defun future (operation &key iocp completion-key)
  "Create a future for an async operation"
  (make-instance 'future
                 :operation operation
                 :iocp iocp
                 :completion-key completion-key))

(defun await (future &optional (timeout 30000))
  "Wait for future completion with optional timeout"
  (if (future-completed-p future)
      (if (future-error future)
          (error (future-error future))
          (future-result future))
      (handler-case
          (multiple-value-bind (wait-result bytes completion-key overlapped-ptr)
              (iocp:wait-for-completion (future-iocp future) timeout)
            (if (iocp:completion-success-p wait-result)
                (progn
                  (setf (future-completed-p future) t)
                  (setf (future-result future) bytes)
                  (future-result future))
                (progn
                  (setf (future-completed-p future) t)
                  (setf (future-error future) 
                        (make-condition 'timeout-error :message "Future timed out"))
                  (error (future-error future)))))
        (error (e)
          (setf (future-completed-p future) t)
          (setf (future-error future) e)
          (error e)))))

(defun select (&rest futures)
  "Wait for any future to complete"
  (if (null futures)
      nil
      (loop for future in futures
            for i from 0
            do (when (future-completed-p future)
                 (return-from select (values future i)))
            finally
               ;; None completed, wait for first one
               (loop for future in futures
                     for i from 0
                     do (handler-case
                            (progn
                              (await future 0) ; Non-blocking check
                              (return-from select (values future i)))
                          (timeout-error ()
                            ;; Continue to next future
                            nil))
                  ;; If none ready immediately, wait for any with small timeout
                  (loop for future in futures
                        for i from 0
                        do (handler-case
                               (progn
                                 (await future 100) ; 100ms timeout
                                 (return-from select (values future i)))
                             (timeout-error ()
                               ;; Continue to next future
                               nil)))
                  ;; Return nil if none complete within timeout
                  nil)))

;;; Cleanup Hook

(pushnew #'cleanup-winsock sb-ext:*exit-hooks*)
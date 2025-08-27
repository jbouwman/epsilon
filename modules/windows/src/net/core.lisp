;;;; Windows Networking Implementation
;;;; 
;;;; This module implements the epsilon.net interface for Windows systems
;;;; using IOCP (I/O Completion Ports) for event notification and async I/O.

(defpackage #:epsilon.net
  (:use #:cl)
  (:local-nicknames
   (#:iocp #:epsilon.sys.iocp)
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

(require :sb-bsd-sockets)

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
;;; Winsock Initialization
;;; ============================================================================

(lib:defshared %wsa-startup "WSAStartup" "ws2_32" :int
  ((version-requested :unsigned-short) (wsa-data :pointer))
  :documentation "Initialize Winsock")

(lib:defshared %wsa-cleanup "WSACleanup" "ws2_32" :int ()
  :documentation "Cleanup Winsock")

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
           :documentation "OS socket handle")
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
           :documentation "OS socket handle")
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
           :documentation "OS socket handle")
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
  (handler-case
      (let ((host-ent (sb-bsd-sockets:host-ent-address
                       (sb-bsd-sockets:get-host-by-name hostname))))
        (list (make-socket-address
               (format nil "~{~A~^.~}"
                      (coerce host-ent 'list))
               port)))
    (error (e)
      (error 'network-error :message (format nil "Failed to resolve ~A: ~A" hostname e)))))

;;; ============================================================================
;;; Internal Socket Operations
;;; ============================================================================

(defun make-socket (domain type &optional protocol)
  "Create a socket with error handling"
  (ensure-winsock-initialized)
  (handler-case
      (sb-bsd-sockets:socket domain type (or protocol 0))
    (error (e)
      (error 'network-error :message (format nil "Socket creation failed: ~A" e)))))

(defun set-socket-nonblocking (socket)
  "Set socket to non-blocking mode (stub for Windows)"
  ;; Windows non-blocking sockets are handled differently
  ;; For now, we'll use blocking sockets and handle async via IOCP
  (declare (ignore socket))
  nil)

(defun set-socket-reuseaddr (socket)
  "Enable SO_REUSEADDR on socket"
  (sb-bsd-sockets:sockopt-reuse-address socket t))

(defun socket-to-stream (socket direction)
  "Convert socket to Lisp stream"
  (sb-sys:make-fd-stream socket
                         :direction direction
                         :element-type '(unsigned-byte 8)
                         :buffering :none
                         :external-format :latin-1))

;;; ============================================================================
;;; TCP Listener Operations
;;; ============================================================================

(defun tcp-bind (address &key (backlog 128) (reuse-addr t))
  "Create a TCP listener bound to the specified address"
  (let* ((socket (make-socket sb-bsd-sockets:af-inet sb-bsd-sockets:sock-stream))
         (addr (sb-bsd-sockets:make-inet-address (socket-address-ip address)))
         (iocp-handle (iocp:iocp-create)))
    
    (when reuse-addr
      (set-socket-reuseaddr socket))
    
    (handler-case
        (progn
          (sb-bsd-sockets:socket-bind socket addr (socket-address-port address))
          (sb-bsd-sockets:socket-listen socket backlog)
          
          ;; Associate socket with IOCP
          (iocp:iocp-associate iocp-handle socket 0)
          
          (make-instance 'tcp-listener
                         :handle socket
                         :local-address address
                         :iocp iocp-handle
                         :backlog backlog))
      (error (e)
        (sb-bsd-sockets:socket-close socket)
        (iocp:iocp-close iocp-handle)
        (cond
          ((search "Address already in use" (format nil "~A" e))
           (error 'address-in-use :message (format nil "~A" e)))
          (t
           (error 'network-error :message (format nil "Bind failed: ~A" e))))))))

(defun tcp-accept (listener)
  "Accept a connection, blocking if necessary"
  (loop
    (handler-case
        (multiple-value-bind (client-socket peer-addr)
            (sb-bsd-sockets:socket-accept (tcp-listener-handle listener))
          (when client-socket
            (let* ((peer-ip (format nil "~{~A~^.~}"
                                   (coerce (sb-bsd-sockets:get-host-by-address peer-addr) 'list)))
                   (peer-port (sb-bsd-sockets:socket-peername client-socket))
                   (local-port (sb-bsd-sockets:socket-name client-socket)))
              (return (make-instance 'tcp-stream
                                    :handle client-socket
                                    :local-address (make-socket-address "0.0.0.0" local-port)
                                    :peer-address (make-socket-address peer-ip peer-port))))))
      (sb-bsd-sockets:interrupted-error ()
        ;; Retry on EINTR
        nil)
      (error (e)
        (error 'network-error :message (format nil "Accept failed: ~A" e))))))

(defun tcp-try-accept (listener)
  "Try to accept a connection without blocking"
  (handler-case
      (multiple-value-bind (client-socket peer-addr)
          (sb-bsd-sockets:socket-accept (tcp-listener-handle listener))
        (when client-socket
          (let* ((peer-ip (format nil "~{~A~^.~}"
                                 (coerce (sb-bsd-sockets:get-host-by-address peer-addr) 'list)))
                 (peer-port (sb-bsd-sockets:socket-peername client-socket))
                 (local-port (sb-bsd-sockets:socket-name client-socket)))
            (make-instance 'tcp-stream
                          :handle client-socket
                          :local-address (make-socket-address "0.0.0.0" local-port)
                          :peer-address (make-socket-address peer-ip peer-port)))))
    (sb-bsd-sockets:would-block-error ()
      nil)
    (error (e)
      (error 'network-error :message (format nil "Accept failed: ~A" e)))))

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
  ;; This would ideally return a lazy sequence, but for now returns a list
  (loop while (tcp-poll-accept listener 0) collect it))

;;; ============================================================================
;;; TCP Stream Operations
;;; ============================================================================

(defun tcp-connect (address &key (timeout nil))
  "Connect to a TCP server"
  (declare (ignore timeout)) ; TODO: Implement connection timeout
  (let* ((socket (make-socket sb-bsd-sockets:af-inet sb-bsd-sockets:sock-stream))
         (addr (sb-bsd-sockets:make-inet-address (socket-address-ip address))))
    
    (handler-case
        (progn
          (sb-bsd-sockets:socket-connect socket addr (socket-address-port address))
          
          (let ((local-port (sb-bsd-sockets:socket-name socket)))
            (make-instance 'tcp-stream
                          :handle socket
                          :local-address (make-socket-address "0.0.0.0" local-port)
                          :peer-address address)))
      (error (e)
        (sb-bsd-sockets:socket-close socket)
        (cond
          ((search "Connection refused" (format nil "~A" e))
           (error 'connection-refused :message (format nil "~A" e)))
          ((search "Connection reset" (format nil "~A" e))
           (error 'connection-reset :message (format nil "~A" e)))
          (t
           (error 'network-error :message (format nil "Connect failed: ~A" e))))))))

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
  ;; For now, same as tcp-read since Windows handles async differently
  (tcp-read stream buffer :start start :end end))

(defun tcp-try-write (stream buffer &key (start 0) (end (length buffer)))
  "Try to write without blocking"
  ;; For now, same as tcp-write since Windows handles async differently
  (tcp-write stream buffer :start start :end end))

(defun tcp-poll-read (stream timeout-ms)
  "Poll for readable data with timeout"
  (declare (ignore stream timeout-ms))
  ;; TODO: Implement with IOCP
  t)

(defun tcp-poll-write (stream timeout-ms)
  "Poll for writability with timeout"
  (declare (ignore stream timeout-ms))
  ;; TODO: Implement with IOCP
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
    (sb-posix:shutdown (tcp-stream-handle stream) shutdown-how)
    (setf (tcp-connected-p stream) nil)))

;;; ============================================================================
;;; UDP Operations
;;; ============================================================================

(defun udp-bind (address)
  "Create a UDP socket bound to address"
  (let* ((socket (make-socket sb-bsd-sockets:af-inet sb-bsd-sockets:sock-dgram))
         (addr (sb-bsd-sockets:make-inet-address (socket-address-ip address))))
    
    (handler-case
        (progn
          (set-socket-reuseaddr socket)
          (sb-bsd-sockets:socket-bind socket addr (socket-address-port address))
          
          (make-instance 'udp-socket
                         :handle socket
                         :local-address address))
      (error (e)
        (sb-bsd-sockets:socket-close socket)
        (error 'network-error :message (format nil "UDP bind failed: ~A" e))))))

(defun udp-connect (socket address)
  "Connect UDP socket to remote address"
  (let ((addr (sb-bsd-sockets:make-inet-address (socket-address-ip address))))
    (handler-case
        (sb-bsd-sockets:socket-connect (udp-socket-handle socket)
                                       addr
                                       (socket-address-port address))
      (error (e)
        (error 'network-error :message (format nil "UDP connect failed: ~A" e))))))

(defun udp-send (socket buffer &key (start 0) (end (length buffer)))
  "Send data on connected UDP socket"
  (handler-case
      (sb-bsd-sockets:socket-send (udp-socket-handle socket)
                                  buffer
                                  (- end start)
                                  :start start)
    (error (e)
      (error 'network-error :message (format nil "UDP send failed: ~A" e)))))

(defun udp-recv (socket buffer &key (start 0) (end (length buffer)))
  "Receive data on UDP socket"
  (handler-case
      (multiple-value-bind (buffer size)
          (sb-bsd-sockets:socket-receive (udp-socket-handle socket)
                                         buffer
                                         (- end start)
                                         :start start)
        (declare (ignore buffer))
        size)
    (error (e)
      (error 'network-error :message (format nil "UDP recv failed: ~A" e)))))

(defun udp-send-to (socket buffer address &key (start 0) (end (length buffer)))
  "Send data to specific address"
  (let ((addr (sb-bsd-sockets:make-inet-address (socket-address-ip address))))
    (handler-case
        (sb-bsd-sockets:socket-send (udp-socket-handle socket)
                                    buffer
                                    (- end start)
                                    :address addr
                                    :port (socket-address-port address)
                                    :start start)
      (error (e)
        (error 'network-error :message (format nil "UDP send-to failed: ~A" e))))))

(defun udp-recv-from (socket buffer &key (start 0) (end (length buffer)))
  "Receive data and sender address"
  (handler-case
      (multiple-value-bind (buffer size addr port)
          (sb-bsd-sockets:socket-receive (udp-socket-handle socket)
                                         buffer
                                         (- end start)
                                         :start start)
        (declare (ignore buffer))
        (values size
                (make-socket-address
                 (format nil "~{~A~^.~}" (coerce addr 'list))
                 port)))
    (error (e)
      (error 'network-error :message (format nil "UDP recv-from failed: ~A" e)))))

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
    (sb-bsd-sockets:setsockopt handle level option value)))

(defun get-socket-option (socket-obj level option)
  "Get a socket option"
  (let ((handle (etypecase socket-obj
                  (tcp-listener (tcp-listener-handle socket-obj))
                  (tcp-stream (tcp-stream-handle socket-obj))
                  (udp-socket (udp-socket-handle socket-obj)))))
    (sb-bsd-sockets:getsockopt handle level option)))

;;; ============================================================================
;;; Cleanup
;;; ============================================================================

(pushnew #'cleanup-winsock sb-ext:*exit-hooks*)
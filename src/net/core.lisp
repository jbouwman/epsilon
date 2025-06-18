(defpackage #:epsilon.net.core
  (:use #:cl)
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

(in-package :epsilon.net.core)

;;; Conditions

(define-condition network-error (error) 
  ((message :initarg :message :reader error-message)))

(define-condition connection-error (network-error)
  ())

(define-condition timeout-error (network-error)
  ())

;;; Core Types
;;; 
;;; These are lightweight wrappers around sb-bsd-sockets
;;; to maintain API compatibility while using SBCL's socket implementation directly

(defclass socket ()
  ((handle :initarg :handle :reader socket-handle)
   (stream :initarg :stream :initform nil :accessor socket-stream-slot)
   (options :initform (make-hash-table) :reader socket-options)))

(defclass listener (socket) 
  ((backlog :initarg :backlog :reader listener-backlog)
   (address :initarg :address :reader listener-address)
   (port :initarg :port :reader listener-port)))

(defclass connection (socket)
  ((remote-address :initarg :remote-address :accessor connection-remote-address)
   (remote-port :initarg :remote-port :accessor connection-remote-port)))

;;; Option Handling

;; FIXME
#++
(defun get-socket-option (socket option-name)
  "Get socket option value."
  (error "todo implement"))

;; FIXME
#++
(defun set-socket-option (socket option-name value)
  "Set socket option value."
  (error "todo implement"))

;;; Stream Creation and Access

(defun socket-stream (socket)
  "Get a bidirectional stream for the socket."
  (or (socket-stream-slot socket)
      (setf (socket-stream-slot socket)
            (error "unimplemented"))))

(defun socket-from-stream (stream)
  "Get the socket object associated with a stream."
  (declare (ignore stream))
  (error "Not implemented - socket-from-stream function"))

(defun socket-peer-address (socket)
  "Get the peer address of a socket."
  (multiple-value-bind (address port) 
      (error "unimplemented")
    (values address port)))

;;; Core Socket Operations

(defun socket-listen (address port &key (reuse-address t) (backlog 5) type)
  "Create a listening socket on the given address and port."
  (declare (ignore type)) ;; we only support TCP for now
  (handler-case
      (let* ((socket (error "unimplemented"))
             (host (if (or (string= address "0.0.0.0") (string= address "*"))
                       "0.0.0.0"
                       address)))
        ;; Set socket options
        ;;(setf (sb-bsd-sockets:sockopt-reuse-address socket) reuse-address)
        
        ;; Bind socket
        ;;(sb-bsd-sockets:socket-bind socket host port)
        
        ;; Listen
        ;;(sb-bsd-sockets:socket-listen socket backlog)
        
        ;; Create and return listener object
        (make-instance 'listener
                       :handle socket
                       :backlog backlog
                       :address address
                       :port port))
    (error (e)
      (error 'network-error :message (format nil "Error creating listener: ~A" e)))))

(defun socket-connect (host port &key (timeout 30) type)
  "Connect to a remote host and port."
  (declare (ignore type)) ;; we only support TCP for now
  (handler-case
      (let ((socket (error "unimplemented")))
        ;; Set non-blocking mode for timeout support
        ;;(setf (sb-bsd-sockets:non-blocking-mode socket) t)
        
        ;; Connect with timeout
        ;; (sb-bsd-sockets:socket-connect socket host port)
        
        ;; Revert to blocking mode
        ;; (setf (sb-bsd-sockets:non-blocking-mode socket) nil)
        
        ;; Create and return connection object
        (make-instance 'connection
                       :handle socket
                       :remote-address host
                       :remote-port port))
    (error (e)
      (error 'network-error :message (format nil "Error connecting to ~A:~A - ~A" host port e)))))

(defun socket-accept (listener)
  "Accept a connection from a listener."
  (handler-case
      (multiple-value-bind (client-socket remote-host remote-port)
          (error "unimplemented")
        (make-instance 'connection
                       :handle client-socket
                       :remote-address remote-host
                       :remote-port remote-port))
    (error (e)
      (error 'network-error :message (format nil "Error accepting connection: ~A" e)))))

(defun socket-close (socket)
  "Close a socket."
  (handler-case
      (progn
        ;; Close the stream if it exists
        (when (socket-stream-slot socket)
          (close (socket-stream-slot socket))
          (setf (socket-stream-slot socket) nil))
        
        ;; Close the socket
        (error "unimplemented"))
    (error (e)
      (error 'network-error :message (format nil "Error closing socket: ~A" e)))))

;;; Socket option mapping - simple wrapper around sb-bsd-sockets options

(defun socket-option (socket option-name &optional (value nil value-provided))
  "Get or set a socket option."
  (if value-provided
      (set-socket-option socket option-name value)
      (get-socket-option socket option-name)))

;;; Context Managers

(defmacro with-connection ((var address port &rest options) &body body)
  "Establish connection with automatic cleanup."
  `(let ((,var (socket-connect ,address ,port ,@options)))
     (unwind-protect
          (progn ,@body)
       (when ,var
         (socket-close ,var)))))

(defmacro with-listener ((var address port &rest options) &body body)
  "Create listener with automatic cleanup."
  `(let ((,var (socket-listen ,address ,port ,@options)))
     (unwind-protect
          (progn ,@body)
       (when ,var
         (socket-close ,var)))))

(defmacro with-timeout ((seconds) &body body)
  "Execute body with timeout."
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

;;; TLS/SSL Support

(defclass secure-context ()
  ((cert-file :initarg :cert-file)
   (key-file :initarg :key-file)
   (verify :initarg :verify :initform t)))

(defun make-secure-context (&key cert-file key-file (verify t))
  "Create TLS/SSL context."
  (make-instance 'secure-context
                 :cert-file cert-file
                 :key-file key-file
                 :verify verify))

(defclass secure-connection (connection)
  ((context :initarg :context :reader connection-context)))

(defclass secure-listener (listener)
  ((context :initarg :context :reader listener-context)))

;;; Address Handling

(defclass address ()
  ((host :initarg :host :reader address-host)
   (port :initarg :port :reader address-port)))

(defun resolve (hostname &key (family :any))
  "Resolve hostname to address(es)."
  (error "unimplemented"))

(defun parse-address (string)
  "Parse address from string form."
  (let* ((colon-pos (position #\: string))
         (host (if colon-pos (subseq string 0 colon-pos) string))
         (port (if colon-pos 
                   (parse-integer (subseq string (1+ colon-pos)))
                   80)))
    (make-instance 'address :host host :port port)))

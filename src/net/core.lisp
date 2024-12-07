(defpackage #:epsilon.net
  (:use #:cl)
  (:shadow #:listen #:close)
  (:export

   ;; Core types & conditions
   #:socket #:listener #:connection
   #:network-error #:connection-error #:timeout-error
   
   ;; Main
   #:connect #:listen #:accept #:close
   #:read-bytes #:write-bytes #:flush
   
   ;; High-level operations
   #:with-connection #:with-listener #:with-timeout
   
   ;; Async
   #:async-connect #:async-accept #:async-read #:async-write
   #:future #:await #:select
   
   ;; Address handling
   #:address #:resolve #:parse-address
   #:ipv4 #:ipv6 #:hostname
   
   ;; Options & configuration
   #:socket-option #:get-option #:set-option
   #:keep-alive #:no-delay #:reuse-addr #:recv-buffer #:send-buffer
   
   ;; TLS/SSL
   #:secure-context #:make-secure-context
   #:secure-connection #:secure-listener))

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
   (options :initform (make-hash-table) :reader socket-options)))

(defclass listener (socket) 
  ((backlog :initarg :backlog :reader listener-backlog)))

(defclass connection (socket)
  ((input-buffer :initform (make-array 4096))
   (output-buffer :initform (make-array 4096))))

;;; Main Interface

(defun connect (address &key timeout (type :tcp))
  "Connect to remote address. Returns connection object.")

(defun listen (address &key backlog (reuse t) (type :tcp))
  "Create listening socket. Returns listener object."

  (let* ((local (and host (not (pathnamep host))
                     (car (get-hosts-by-name (host-to-hostname host)))))
         (ipv6 (and local (= 16 (length local))))
         (sock-type (cond
                      (ipv6 'sb-bsd-sockets:inet6-socket)
                      (t 'sb-bsd-sockets:inet-socket)))
         (reuseaddress (if reuse-address-supplied-p reuse-address reuseaddress))
         (sock (make-instance sock-type
                              :type :stream
                              :protocol :tcp))
         (bind-args (list #+sbcl (if (and local (not (eq host *wildcard-host*)))
                                     local
                                     (hbo-to-vector-quad sb-bsd-sockets-internal::inaddr-any))
                          #+(or ecl mkcl clasp) (host-to-vector-quad host)
                          port)))
    (handler-case
        (with-mapped-conditions (nil host)
          (setf (sb-bsd-sockets:sockopt-reuse-address sock) reuseaddress)
          (apply #'sb-bsd-sockets:socket-bind (cons sock bind-args))
          (sb-bsd-sockets:socket-listen sock backlog)
          (make-stream-server-socket sock))
      (t (c)
        ;; Make sure we don't leak filedescriptors
        (sb-bsd-sockets:socket-close sock)
        (error c)))))


  
  

  )

(defun accept (listener &key timeout)
  "Accept connection from listener. Returns connection object.")

(defun close (socket)
  "Close socket, listener, or connection.")

(defun read-bytes (connection count &key timeout)
  "Read exactly count bytes or signal error.")

(defun write-bytes (connection bytes &key start end timeout)
  "Write bytes to connection.")

(defun flush (connection &key timeout)
  "Ensure all buffered data is written.")

;;; Context Managers

(defmacro with-connection ((var address &rest options) &body body)
  "Establish connection with automatic cleanup.")

(defmacro with-listener ((var address &rest options) &body body)
  "Create listener with automatic cleanup.")

(defmacro with-timeout ((seconds) &body body)
  "Execute body with timeout.")

;;; Async Interface

(deftype future ()
  "Represents pending async operation.")

(defun async-connect (address &key timeout)
  "Begin async connection. Returns future.")

(defun async-accept (listener)
  "Begin async accept. Returns future.")

(defun async-read (connection count)
  "Begin async read. Returns future.")

(defun async-write (connection bytes &key start end)
  "Begin async write. Returns future.")

(defun await (future &key timeout)
  "Wait for future to complete.")

(defun select (futures &key timeout)
  "Wait for any future to complete.")

;;; Address Handling

(defclass address ()
  ((host :initarg :host :reader address-host)
   (port :initarg :port :reader address-port)))

(defun resolve (hostname &key (family :any))
  "Resolve hostname to address(es).")

(defun parse-address (string)
  "Parse address from string form.")

;;; Socket Options

(defgeneric get-option (socket option)
  (:documentation "Get socket option value."))

(defgeneric set-option (socket option value)
  (:documentation "Set socket option value."))

;;; TLS/SSL Support

(defclass secure-context ()
  ((cert-file :initarg :cert-file)
   (key-file :initarg :key-file)
   (verify :initarg :verify :initform t)))

(defun make-secure-context (&key cert-file key-file (verify t))
  "Create TLS/SSL context.")

(defclass secure-connection (connection)
  ((context :initarg :context :reader connection-context)))

(defclass secure-listener (listener)
  ((context :initarg :context :reader listener-context)))

;;; Internal Utilities

(defun %make-socket (family type)
  "Create raw socket.")

(defun %set-nonblocking (socket)
  "Set socket non-blocking mode.")

(defun %handle-would-block (socket op timeout)
  "Handle EAGAIN/EWOULDBLOCK condition.")

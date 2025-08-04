;;;; Platform-independent networking abstraction
;;;;
;;;; This module provides a unified interface for networking operations
;;;; across different platforms (Linux, Darwin, Windows)

(defpackage :epsilon.net
  (:use :cl)
  (:shadow #:listen #:close)
  (:export
   ;; Socket operations
   #:socket
   #:socket-type
   #:socket-connected-p
   #:connect
   #:bind
   #:listen
   #:accept
   #:close
   #:shutdown
   
   ;; Socket options
   #:set-socket-option
   #:get-socket-option
   #:socket-option
   
   ;; I/O operations
   #:send
   #:recv
   #:sendto
   #:recvfrom
   
   ;; Stream interface
   #:socket-stream
   #:make-socket-stream
   
   ;; Address resolution
   #:resolve-hostname
   #:make-socket-address
   #:net-address-host
   #:net-address-port
   
   ;; Socket types
   #:+socket-type-stream+
   #:+socket-type-dgram+
   
   ;; Socket options
   #:+socket-option-reuseaddr+
   #:+socket-option-keepalive+
   #:+socket-option-nodelay+
   
   ;; Conditions
   #:network-error
   #:connection-refused
   #:connection-timeout
   #:address-in-use))

(in-package :epsilon.net)

;;; Constants

(defconstant +socket-type-stream+ :stream)
(defconstant +socket-type-dgram+ :datagram)

(defconstant +socket-option-reuseaddr+ :reuse-address)
(defconstant +socket-option-keepalive+ :keep-alive)
(defconstant +socket-option-nodelay+ :no-delay)

;;; Conditions

(define-condition network-error (error)
  ((message :initarg :message :reader network-error-message))
  (:report (lambda (condition stream)
             (format stream "Network error: ~A" 
                     (network-error-message condition)))))

(define-condition connection-refused (network-error) ())
(define-condition connection-timeout (network-error) ())
(define-condition address-in-use (network-error) ())

;;; Socket address structure

(defstruct net-address
  host
  port)

;;; Main socket class

(defclass socket ()
  ((handle :initarg :handle :accessor socket-handle)
   (type :initarg :type :accessor socket-type)
   (connected-p :initarg :connected-p :initform nil :accessor socket-connected-p)))

;;; Platform detection and implementation loading

(defparameter *platform*
  #+darwin :darwin
  #+linux :linux
  #+windows :windows
  #-(or darwin linux windows) :unknown)

;;; Generic implementation using SBCL sockets

(defun socket (&key (type +socket-type-stream+) (protocol :tcp))
  "Create a new socket"
  (let ((sbcl-type (ecase type
                     (:stream :stream)
                     (:datagram :datagram))))
    (make-instance 'socket
                   :handle (make-instance 'sb-bsd-sockets:inet-socket
                                          :type sbcl-type
                                          :protocol protocol)
                   :type type)))

(defun connect (socket address)
  "Connect socket to remote address"
  (let ((sbcl-socket (socket-handle socket))
        (host (net-address-host address))
        (port (net-address-port address)))
    (handler-case
        (progn
          (sb-bsd-sockets:socket-connect 
           sbcl-socket
           (sb-bsd-sockets:host-ent-address
            (sb-bsd-sockets:get-host-by-name host))
           port)
          (setf (socket-connected-p socket) t)
          socket)
      (sb-bsd-sockets:connection-refused-error ()
        (error 'connection-refused :message (format nil "Connection refused to ~A:~A" host port))))))

(defun bind (socket address)
  "Bind socket to local address"
  (let ((sbcl-socket (socket-handle socket))
        (host (net-address-host address))
        (port (net-address-port address)))
    (handler-case
        (sb-bsd-sockets:socket-bind 
         sbcl-socket
         (if (string= host "0.0.0.0")
             (sb-bsd-sockets:make-inet-address "0.0.0.0")
             (sb-bsd-sockets:make-inet-address host))
         port)
      (sb-bsd-sockets:address-in-use-error ()
        (error 'address-in-use :message (format nil "Address ~A:~A already in use" host port))))))

(defun listen (socket &optional (backlog 5))
  "Put socket in listening state"
  (sb-bsd-sockets:socket-listen (socket-handle socket) backlog))

(defun accept (socket)
  "Accept incoming connection"
  (let ((client-handle (sb-bsd-sockets:socket-accept (socket-handle socket))))
    (make-instance 'socket
                   :handle client-handle
                   :type (socket-type socket)
                   :connected-p t)))

(defun close (socket)
  "Close socket"
  (when (socket-handle socket)
    (sb-bsd-sockets:socket-close (socket-handle socket))
    (setf (socket-handle socket) nil
          (socket-connected-p socket) nil)))

(defun send (socket data &key (start 0) (end (length data)))
  "Send data through socket"
  (sb-bsd-sockets:socket-send (socket-handle socket) 
                               data 
                               (- end start)
                               :external-format :iso-8859-1))

(defun recv (socket buffer &key (start 0) (end (length buffer)))
  "Receive data from socket"
  (sb-bsd-sockets:socket-receive (socket-handle socket)
                                  buffer
                                  (- end start)))

(defun socket-stream (socket)
  "Get stream interface for socket"
  (sb-bsd-sockets:socket-make-stream (socket-handle socket)
                                      :input t 
                                      :output t
                                      :element-type 'character
                                      :external-format :utf-8))

(defun make-socket-stream (socket &key (element-type 'character))
  "Create a stream from socket"
  (sb-bsd-sockets:socket-make-stream (socket-handle socket)
                                      :input t 
                                      :output t
                                      :element-type element-type))

(defun set-socket-option (socket option value)
  "Set socket option"
  (let ((sbcl-socket (socket-handle socket)))
    (case option
      (:reuse-address
       (setf (sb-bsd-sockets:sockopt-reuse-address sbcl-socket) value))
      (:keep-alive
       (setf (sb-bsd-sockets:sockopt-keep-alive sbcl-socket) value))
      (:no-delay
       (when (eq (socket-type socket) +socket-type-stream+)
         (setf (sb-bsd-sockets:sockopt-tcp-nodelay sbcl-socket) value)))
      (t (error "Unknown socket option: ~A" option)))))

(defun resolve-hostname (hostname)
  "Resolve hostname to IP address"
  (handler-case
      (sb-bsd-sockets:host-ent-address
       (sb-bsd-sockets:get-host-by-name hostname))
    (error (e)
      (error 'network-error :message (format nil "Failed to resolve hostname ~A: ~A" hostname e)))))

(defun make-socket-address (host port)
  "Create a socket address"
  (make-net-address :host host :port port))
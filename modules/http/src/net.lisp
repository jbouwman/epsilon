;;;; Simple Network Layer for HTTP
;;;;
;;;; Provides basic TCP socket functionality using SBCL's built-in sockets
;;;; This is a temporary implementation to get HTTP working without 
;;;; platform-specific dependencies.

(defpackage :epsilon.http.net
  (:use :cl)
  (:local-nicknames (#:log #:epsilon.log))
  (:export
   #:socket-connect
   #:socket-listen
   #:socket-accept
   #:socket-close
   #:socket-stream))

(in-package :epsilon.http.net)

(defun socket-connect (host port)
  "Create a TCP connection to host:port"
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket 
                               :type :stream 
                               :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket 
                                   (sb-bsd-sockets:host-ent-address
                                    (sb-bsd-sockets:get-host-by-name host))
                                   port)
    socket))

(defun socket-listen (address port)
  "Create a listening socket on address:port"
  (log:info "Creating listening socket on ~A:~A" address port)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket 
                               :type :stream 
                               :protocol :tcp)))
    (log:debug "Socket created: ~A" socket)
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (log:debug "Binding socket...")
    (sb-bsd-sockets:socket-bind socket 
                                (sb-bsd-sockets:make-inet-address address)
                                port)
    (log:debug "Socket bound, calling listen...")
    (sb-bsd-sockets:socket-listen socket 5)
    (log:info "Socket listening on ~A:~A" address port)
    socket))

(defun socket-accept (listening-socket)
  "Accept a connection on listening socket"
  (log:debug "socket-accept called, waiting for connection...")
  (let ((connection (sb-bsd-sockets:socket-accept listening-socket)))
    (log:info "Connection accepted: ~A" connection)
    connection))

(defun socket-close (socket)
  "Close a socket"
  (sb-bsd-sockets:socket-close socket))

(defun socket-stream (socket)
  "Get stream for socket I/O"
  (log:debug "Creating stream for socket: ~A" socket)
  (let ((stream (sb-bsd-sockets:socket-make-stream socket
                                                   :input t 
                                                   :output t
                                                   :element-type 'character
                                                   :timeout 10)))
    (log:debug "Stream created: ~A" stream)
    stream))
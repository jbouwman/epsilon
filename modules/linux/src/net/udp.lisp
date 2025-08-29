;;;; UDP Operations Wrapper
;;;;
;;;; This module provides the high-level UDP API by delegating to the sockets module.

(defpackage epsilon.net.udp
  (:use cl)
  (:local-nicknames
   (sockets epsilon.net.sockets)
   (types epsilon.net.types))
  (:export
   ;; UDP operations
   #:udp-bind
   #:udp-connect
   #:udp-send
   #:udp-recv
   #:udp-send-to
   #:udp-recv-from
   #:udp-local-addr
   #:udp-poll-send
   #:udp-poll-recv
   
   ;; Macros
   #:with-udp-socket))

(in-package epsilon.net.udp)

;;; ============================================================================
;;; UDP Operations
;;; ============================================================================

(defun udp-bind (address)
  "Create a UDP socket bound to address"
  (sockets:udp-bind address))

(defun udp-connect (socket address)
  "Connect UDP socket to remote address"
  (sockets:udp-connect socket address))

(defun udp-send (socket data &key (start 0) (end nil))
  "Send data on connected UDP socket"
  (sockets:udp-send socket data :start start :end end))

(defun udp-recv (socket buffer &key (start 0) (end nil))
  "Receive data on UDP socket"
  (sockets:udp-recv socket buffer :start start :end end))

(defun udp-send-to (socket data address &key (start 0) (end nil))
  "Send data to specific address"
  (sockets:udp-send-to socket data address :start start :end end))

(defun udp-recv-from (socket buffer &key (start 0) (end nil))
  "Receive data and sender address"
  (sockets:udp-recv-from socket buffer :start start :end end))

(defun udp-local-addr (socket)
  "Get local address of UDP socket"
  (sockets:udp-local-addr socket))

(defun udp-poll-send (socket timeout-ms)
  "Poll for send readiness with timeout"
  (sockets:udp-poll-send socket timeout-ms))

(defun udp-poll-recv (socket timeout-ms)
  "Poll for receive readiness with timeout"
  (sockets:udp-poll-recv socket timeout-ms))

;;; ============================================================================
;;; Convenience Macros
;;; ============================================================================

(defmacro with-udp-socket ((socket address) &body body)
  "Execute body with a UDP socket bound to address"
  `(let ((,socket (udp-bind ,address)))
     (unwind-protect
          (progn ,@body)
       (when ,socket
         (handler-case
             (epsilon.net.constants:%close (types:udp-socket-handle ,socket))
           (error ()
             ;; Ignore errors during cleanup
             nil))))))
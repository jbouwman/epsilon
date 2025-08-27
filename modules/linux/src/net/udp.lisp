;;;; UDP High-Level Operations for Linux
;;;;
;;;; UDP-specific high-level operations using unified socket utilities

(defpackage epsilon.net.udp
  (:use cl)
  (:local-nicknames
   (sockets epsilon.net.sockets)
   (types epsilon.net.types)
   (errors epsilon.net.errors)
   (address epsilon.net.address))
  (:export
   ;; Re-export socket operations for UDP
   #:udp-bind
   #:udp-connect
   #:udp-send
   #:udp-recv
   #:udp-send-to
   #:udp-recv-from
   #:udp-local-addr
   #:udp-try-send
   #:udp-try-recv
   #:udp-poll-send
   #:udp-poll-recv
   
   ;; UDP-specific high-level utilities
   #:with-udp-socket
   #:udp-echo-server
   #:udp-broadcast
   #:udp-multicast-join
   #:udp-multicast-leave))

(in-package epsilon.net.udp)

;;; ============================================================================
;;; Re-exports from socket operations
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

(defun udp-recv (socket buffer &key (start 0) (end (length buffer)))
  "Receive data on UDP socket"
  (sockets:udp-recv socket buffer :start start :end end))

(defun udp-send-to (socket data address &key (start 0) (end nil))
  "Send data to specific address"
  (sockets:udp-send-to socket data address :start start :end end))

(defun udp-recv-from (socket buffer &key (start 0) (end (length buffer)))
  "Receive data and sender address"
  (sockets:udp-recv-from socket buffer :start start :end end))

(defun udp-local-addr (socket)
  "Get local address of UDP socket"
  (sockets:udp-local-addr socket))

(defun udp-try-send (socket data &key (start 0) (end nil))
  "Try to send without blocking"
  (sockets:udp-try-send socket data :start start :end end))

(defun udp-try-recv (socket buffer &key (start 0) (end (length buffer)))
  "Try to receive without blocking"
  (sockets:udp-try-recv socket buffer :start start :end end))

(defun udp-poll-send (socket timeout-ms)
  "Poll for send readiness with timeout"
  (sockets:udp-poll-send socket timeout-ms))

(defun udp-poll-recv (socket timeout-ms)
  "Poll for receive readiness with timeout"
  (sockets:udp-poll-recv socket timeout-ms))

;;; ============================================================================
;;; UDP-Specific High-Level Utilities
;;; ============================================================================

(defmacro with-udp-socket ((socket address) &body body)
  "Execute body with a UDP socket bound to address"
  `(let ((,socket (udp-bind ,address)))
     (unwind-protect
          (progn ,@body)
       (ignore-errors
         (sockets:close-socket (types:udp-socket-handle ,socket))))))

(defun udp-echo-server (socket &key (max-packet-size 1400))
  "Run a simple UDP echo server"
  (let ((buffer (make-array max-packet-size :element-type '(unsigned-byte 8))))
    (loop
     (multiple-value-bind (bytes-read sender-addr)
         (udp-recv-from socket buffer)
       (when (and (plusp bytes-read) sender-addr)
         ;; Echo back to sender
         (handler-case
             (udp-send-to socket buffer sender-addr :end bytes-read)
           (error (e)
             (warn "UDP echo server send error: ~A" e))))))))

(defun udp-broadcast (socket data port &key (broadcast-addr "255.255.255.255"))
  "Send broadcast packet"
  ;; First need to enable broadcast on socket
  (handler-case
      (progn
        ;; This would need SO_BROADCAST socket option
        ;; (core:set-socket-option (types:udp-socket-handle socket) 
        ;;                         const:+sol-socket+ const:+so-broadcast+ t)
        (let ((addr (address:make-socket-address broadcast-addr port)))
          (udp-send-to socket data addr)))
    (error (e)
      (error 'errors:network-error 
             :message (format nil "UDP broadcast failed: ~A" e)))))

(defun udp-multicast-join (socket group-address interface-address)
  "Join a multicast group"
  ;; This would need to set IP_ADD_MEMBERSHIP socket option
  ;; Implementation would require more complex sockopt handling
  (declare (ignore socket group-address interface-address))
  (error 'errors:network-error 
         :message "Multicast join not implemented - requires IP_ADD_MEMBERSHIP"))

(defun udp-multicast-leave (socket group-address interface-address)
  "Leave a multicast group"
  ;; This would need to set IP_DROP_MEMBERSHIP socket option
  (declare (ignore socket group-address interface-address))
  (error 'errors:network-error 
         :message "Multicast leave not implemented - requires IP_DROP_MEMBERSHIP"))
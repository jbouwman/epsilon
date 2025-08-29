;;;; Network Types for Linux
;;;;
;;;; This module defines the core network types used throughout the networking stack.

(defpackage epsilon.net.types
  (:use cl)
  (:export
   ;; Address types
   #:socket-address
   #:socket-address-ip
   #:socket-address-port
   #:make-socket-address
   
   ;; Socket types
   #:tcp-listener
   #:tcp-listener-handle
   #:tcp-listener-local-address
   #:tcp-listener-backlog
   
   #:tcp-stream
   #:tcp-stream-handle
   #:tcp-stream-local-address
   #:tcp-stream-peer-address
   #:tcp-stream-connected-p
   #:tcp-stream-input
   #:tcp-stream-output
   
   #:udp-socket
   #:udp-socket-handle
   #:udp-socket-local-address
   #:udp-socket-connected-peer))

(in-package epsilon.net.types)

;;; ============================================================================
;;; Address Types
;;; ============================================================================

(defclass socket-address ()
  ((ip :initarg :ip :reader socket-address-ip
       :documentation "IP address as string")
   (port :initarg :port :reader socket-address-port
         :type integer
         :documentation "Port number"))
  (:documentation "Socket address (IP + port)"))

(defmethod print-object ((addr socket-address) stream)
  (print-unreadable-object (addr stream :type t)
    (format stream "~A:~D" 
            (socket-address-ip addr)
            (socket-address-port addr))))

(defun make-socket-address (ip port)
  "Create a socket address from IP string and port number"
  (make-instance 'socket-address :ip ip :port port))

;;; ============================================================================
;;; TCP Types
;;; ============================================================================

(defclass tcp-listener ()
  ((handle :initarg :handle :reader tcp-listener-handle
           :documentation "OS socket file descriptor")
   (local-address :initarg :local-address :reader tcp-listener-local-address
                  :type socket-address
                  :documentation "Local bind address")
   (backlog :initarg :backlog :reader tcp-listener-backlog
            :initform 128
            :documentation "Connection backlog"))
  (:documentation "TCP server socket"))

(defmethod print-object ((listener tcp-listener) stream)
  (print-unreadable-object (listener stream :type t)
    (format stream "listening on ~A" 
            (tcp-listener-local-address listener))))

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
  (:documentation "TCP connection stream"))

(defmethod print-object ((stream tcp-stream) stream-obj)
  (print-unreadable-object (stream stream-obj :type t)
    (format stream-obj "~A -> ~A (~A)" 
            (tcp-stream-local-address stream)
            (tcp-stream-peer-address stream)
            (if (tcp-stream-connected-p stream) "connected" "disconnected"))))

;;; ============================================================================
;;; UDP Types
;;; ============================================================================

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

(defmethod print-object ((socket udp-socket) stream)
  (print-unreadable-object (socket stream :type t)
    (format stream "bound to ~A~@[ connected to ~A~]" 
            (udp-socket-local-address socket)
            (udp-socket-connected-peer socket))))
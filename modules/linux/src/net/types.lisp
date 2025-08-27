;;;; Core Network Types for Linux
;;;;
;;;; Type definitions for networking objects

(defpackage epsilon.net.types
  (:use cl)
  (:export
   ;; Address types
   #:socket-address
   #:socket-address-p
   #:socket-address-ip
   #:socket-address-port
   #:make-socket-address
   
   ;; Socket types
   #:tcp-listener
   #:tcp-listener-p
   #:tcp-listener-handle
   #:tcp-listener-local-address
   #:tcp-listener-epoll
   #:tcp-listener-backlog
   #:make-tcp-listener
   
   #:tcp-stream
   #:tcp-stream-p
   #:tcp-stream-handle
   #:tcp-stream-local-address
   #:tcp-stream-peer-address
   #:tcp-stream-input
   #:tcp-stream-output
   #:tcp-stream-connected-p
   #:tcp-stream-epoll
   #:make-tcp-stream
   
   #:udp-socket
   #:udp-socket-p
   #:udp-socket-handle
   #:udp-socket-local-address
   #:udp-socket-connected-peer
   #:udp-socket-epoll
   #:make-udp-socket))

(in-package epsilon.net.types)

;;; ============================================================================
;;; Address Types
;;; ============================================================================

(defclass socket-address ()
  ((ip :initarg :ip 
       :reader socket-address-ip
       :type string
       :documentation "IP address as string")
   (port :initarg :port 
         :reader socket-address-port
         :type (integer 0 65535)
         :documentation "Port number"))
  (:documentation "Socket address (IP + port)"))

(defun make-socket-address (ip port)
  "Create a socket address from IP string and port number"
  (make-instance 'socket-address :ip ip :port port))

(defmethod print-object ((addr socket-address) stream)
  (print-unreadable-object (addr stream :type t)
    (format stream "~A:~D" 
            (socket-address-ip addr) 
            (socket-address-port addr))))

;;; ============================================================================
;;; TCP Types
;;; ============================================================================

(defclass tcp-listener ()
  ((handle :initarg :handle 
           :reader tcp-listener-handle
           :type integer
           :documentation "OS socket file descriptor")
   (local-address :initarg :local-address 
                  :reader tcp-listener-local-address
                  :type socket-address
                  :documentation "Local bind address")
   (epoll :initarg :epoll 
          :accessor tcp-listener-epoll
          :initform nil
          :documentation "Epoll instance for async operations")
   (backlog :initarg :backlog 
            :reader tcp-listener-backlog
            :initform 128
            :type integer
            :documentation "Connection backlog"))
  (:documentation "TCP server socket"))

(defun make-tcp-listener (&key handle local-address epoll (backlog 128))
  "Create a TCP listener"
  (make-instance 'tcp-listener
                 :handle handle
                 :local-address local-address
                 :epoll epoll
                 :backlog backlog))

(defmethod print-object ((listener tcp-listener) stream)
  (print-unreadable-object (listener stream :type t)
    (format stream "fd=~D ~A" 
            (tcp-listener-handle listener)
            (tcp-listener-local-address listener))))

(defclass tcp-stream ()
  ((handle :initarg :handle 
           :reader tcp-stream-handle
           :type integer
           :documentation "OS socket file descriptor")
   (local-address :initarg :local-address 
                  :reader tcp-stream-local-address
                  :type socket-address
                  :documentation "Local socket address")
   (peer-address :initarg :peer-address 
                 :reader tcp-stream-peer-address
                 :type socket-address
                 :documentation "Remote peer address")
   (input-stream :initform nil 
                 :accessor tcp-stream-input
                 :documentation "Lisp input stream")
   (output-stream :initform nil 
                  :accessor tcp-stream-output
                  :documentation "Lisp output stream")
   (connected-p :initarg :connected-p 
                :initform t 
                :accessor tcp-stream-connected-p
                :type boolean
                :documentation "Connection status")
   (epoll :initform nil 
          :accessor tcp-stream-epoll
          :documentation "Epoll instance for async operations"))
  (:documentation "TCP connection stream"))

(defun make-tcp-stream (&key handle local-address peer-address (connected-p t) epoll)
  "Create a TCP stream"
  (make-instance 'tcp-stream
                 :handle handle
                 :local-address local-address
                 :peer-address peer-address
                 :connected-p connected-p
                 :epoll epoll))

(defmethod print-object ((stream tcp-stream) stream-obj)
  (print-unreadable-object (stream stream-obj :type t)
    (format stream-obj "fd=~D ~A->~A~:[~; DISCONNECTED~]" 
            (tcp-stream-handle stream)
            (tcp-stream-local-address stream)
            (tcp-stream-peer-address stream)
            (not (tcp-stream-connected-p stream)))))

;;; ============================================================================
;;; UDP Types
;;; ============================================================================

(defclass udp-socket ()
  ((handle :initarg :handle 
           :reader udp-socket-handle
           :type integer
           :documentation "OS socket file descriptor")
   (local-address :initarg :local-address 
                  :reader udp-socket-local-address
                  :type socket-address
                  :documentation "Local bind address")
   (connected-peer :initform nil 
                   :accessor udp-socket-connected-peer
                   :type (or null socket-address)
                   :documentation "Connected peer address if any")
   (epoll :initform nil
          :accessor udp-socket-epoll
          :documentation "Epoll instance for async operations"))
  (:documentation "UDP socket"))

(defun make-udp-socket (&key handle local-address connected-peer epoll)
  "Create a UDP socket"
  (make-instance 'udp-socket
                 :handle handle
                 :local-address local-address
                 :connected-peer connected-peer
                 :epoll epoll))

(defmethod print-object ((socket udp-socket) stream)
  (print-unreadable-object (socket stream :type t)
    (format stream "fd=~D ~A~@[ connected->~A~]" 
            (udp-socket-handle socket)
            (udp-socket-local-address socket)
            (udp-socket-connected-peer socket))))
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
   #:socket-address-family
   #:make-socket-address

   ;; Socket types
   #:tcp-listener
   #:tcp-listener-handle
   #:tcp-listener-local-address
   #:tcp-listener-backlog
   #:tcp-listener-accept-epfd
   #:tcp-listener-shutdown-read-fd
   #:tcp-listener-shutdown-write-fd

   #:tcp-stream
   #:tcp-stream-handle
   #:tcp-stream-local-address
   #:tcp-stream-peer-address
   #:tcp-stream-connected-p
   #:tcp-stream-input
   #:tcp-stream-output
   #:tcp-stream-byte-input
   #:tcp-stream-byte-output

   #:udp-socket
   #:udp-socket-handle
   #:udp-socket-local-address
   #:udp-socket-connected-peer

   #:unix-socket-address
   #:unix-socket-address-path
   #:unix-socket-stream
   #:unix-socket-stream-handle
   #:unix-socket-stream-connected-p
   #:unix-socket-stream-io-stream
   #:unix-socket-listener
   #:unix-socket-listener-handle
   #:unix-socket-listener-path

   #:unix-dgram-socket
   #:unix-dgram-socket-handle
   #:unix-dgram-socket-path))

;;; ============================================================================
;;; Address Types
;;; ============================================================================

(defclass socket-address ()
  ((ip :initarg :ip :reader socket-address-ip
       :documentation "IP address as string")
   (port :initarg :port :reader socket-address-port
         :type integer
         :documentation "Port number")
   (family :initarg :family :reader socket-address-family
           :initform :ipv4
           :documentation "Address family: :ipv4 or :ipv6"))
  (:documentation "Socket address (IP + port + family)"))

(defmethod print-object ((addr socket-address) stream)
  (print-unreadable-object (addr stream :type t)
    (if (eq (socket-address-family addr) :ipv6)
        (format stream "[~A]:~D" (socket-address-ip addr) (socket-address-port addr))
        (format stream "~A:~D" (socket-address-ip addr) (socket-address-port addr)))))

(defun make-socket-address (ip port &key (family nil))
  "Create a socket address from IP string and port number.
   FAMILY is auto-detected from IP if not specified."
  (make-instance 'socket-address :ip ip :port port
                 :family (or family
                             (if (and (stringp ip) (position #\: ip))
                                 :ipv6
                                 :ipv4))))

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
            :documentation "Connection backlog")
   (accept-epfd :initarg :accept-epfd :reader tcp-listener-accept-epfd
                :initform nil
                :documentation "Private epoll fd for accept waiting")
   (shutdown-read-fd :initarg :shutdown-read-fd :reader tcp-listener-shutdown-read-fd
                     :initform nil
                     :documentation "Read end of shutdown notification pipe")
   (shutdown-write-fd :initarg :shutdown-write-fd :reader tcp-listener-shutdown-write-fd
                      :initform nil
                      :documentation "Write end of shutdown notification pipe"))
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
                :documentation "Connection status")
   (byte-input :initform nil :accessor tcp-stream-byte-input
               :documentation "Byte input stream")
   (byte-output :initform nil :accessor tcp-stream-byte-output
                :documentation "Byte output stream"))
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

;;; ============================================================================
;;; Unix Domain Socket Types
;;; ============================================================================

(defclass unix-socket-address ()
  ((path :initarg :path :reader unix-socket-address-path
         :type string
         :documentation "Filesystem path for the Unix socket"))
  (:documentation "Unix domain socket address"))

(defmethod print-object ((addr unix-socket-address) stream)
  (print-unreadable-object (addr stream :type t)
    (format stream "~A" (unix-socket-address-path addr))))

(defclass unix-socket-stream ()
  ((handle :initarg :handle :accessor unix-socket-stream-handle
           :documentation "OS socket file descriptor")
   (io-stream :initform nil :accessor unix-socket-stream-io-stream
              :documentation "Bidirectional (unsigned-byte 8) stream")
   (connected-p :initarg :connected-p :initform t :accessor unix-socket-stream-connected-p
                :documentation "Connection status"))
  (:documentation "Unix domain socket connection"))

(defmethod print-object ((stream unix-socket-stream) stream-obj)
  (print-unreadable-object (stream stream-obj :type t)
    (format stream-obj "fd=~D (~A)"
            (unix-socket-stream-handle stream)
            (if (unix-socket-stream-connected-p stream) "connected" "disconnected"))))

(defclass unix-socket-listener ()
  ((handle :initarg :handle :accessor unix-socket-listener-handle
           :documentation "OS socket file descriptor")
   (path :initarg :path :reader unix-socket-listener-path
         :type string
         :documentation "Filesystem path this listener is bound to"))
  (:documentation "Unix domain socket listener"))

(defclass unix-dgram-socket ()
  ((handle :initarg :handle :accessor unix-dgram-socket-handle
           :documentation "OS socket file descriptor")
   (path :initarg :path :initform nil :accessor unix-dgram-socket-path
         :type (or string null)
         :documentation "Filesystem path this socket is bound to (nil for connected-only)"))
  (:documentation "Unix domain datagram socket"))

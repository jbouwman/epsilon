;;;; Core network type definitions

(defpackage epsilon.net.types
  (:use cl)
  (:export
   ;; Address types
   address
   ipv4-address
   ipv4-address-octets
   ipv6-address
   ipv6-address-words
   socket-address
   socket-address-ip
   socket-address-port
   socket-address-family
   
   ;; Socket types
   tcp-listener
   tcp-listener-handle
   tcp-listener-local-address
   tcp-listener-kqueue
   tcp-listener-backlog
   
   tcp-stream
   tcp-stream-handle
   tcp-stream-local-address
   tcp-stream-peer-address
   tcp-stream-input
   tcp-stream-output
   tcp-stream-connected-p
   
   udp-socket
   udp-socket-handle
   udp-socket-local-address
   udp-socket-connected-peer))

(in-package epsilon.net.types)

;;; ============================================================================
;;; Core Types
;;; ============================================================================

(defclass address () ()
  (:documentation "Base class for IP addresses"))

(defclass ipv4-address (address)
  ((octets :initarg :octets :reader ipv4-address-octets
           :type (vector (unsigned-byte 8) 4)
           :documentation "Four octets of IPv4 address"))
  (:documentation "IPv4 address"))

(defclass ipv6-address (address)
  ((words :initarg :words :reader ipv6-address-words
          :type (vector (unsigned-byte 16) 8)
          :documentation "Eight 16-bit words of IPv6 address"))
  (:documentation "IPv6 address"))

(defclass socket-address ()
  ((ip :initarg :ip :reader socket-address-ip
       :documentation "IP address as string")
   (port :initarg :port :reader socket-address-port
         :type integer
         :documentation "Port number")
   (family :initarg :family :reader socket-address-family
           :initform :ipv4
           :documentation "Address family: :ipv4 or :ipv6"))
  (:documentation "Socket address (IP + port)"))

(defclass tcp-listener ()
  ((handle :initarg :handle :reader tcp-listener-handle
           :documentation "OS socket file descriptor")
   (local-address :initarg :local-address :reader tcp-listener-local-address
                  :type socket-address
                  :documentation "Local bind address")
   (kqueue :initarg :kqueue :reader tcp-listener-kqueue
           :documentation "Kqueue instance for async operations")
   (backlog :initarg :backlog :reader tcp-listener-backlog
            :initform 128
            :documentation "Connection backlog"))
  (:documentation "TCP server socket"))

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
  (:documentation "TCP connection"))

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
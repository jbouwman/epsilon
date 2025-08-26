;;;; Core network types and error conditions
;;;;
;;;; This file consolidates the fundamental network types and error conditions
;;;; from the previous types.lisp and errors.lisp files.

(defpackage epsilon.net.core
  (:use cl)
  (:export
   ;; Address types
   #:address
   #:ipv4-address
   #:ipv4-address-octets
   #:ipv6-address
   #:ipv6-address-words
   #:socket-address
   #:socket-address-ip
   #:socket-address-port
   #:socket-address-family
   
   ;; Socket types
   #:tcp-listener
   #:tcp-listener-handle
   #:tcp-listener-local-address
   #:tcp-listener-kqueue
   #:tcp-listener-backlog
   
   #:tcp-stream
   #:tcp-stream-handle
   #:tcp-stream-local-address
   #:tcp-stream-peer-address
   #:tcp-stream-input
   #:tcp-stream-output
   #:tcp-stream-connected-p
   
   #:udp-socket
   #:udp-socket-handle
   #:udp-socket-local-address
   #:udp-socket-connected-peer
   
   ;; Error conditions
   #:network-error
   #:connection-refused
   #:connection-reset
   #:connection-aborted
   #:timeout-error
   #:address-in-use
   #:would-block-error
   
   ;; Error utilities
   #:get-errno
   #:errno-to-string))

(in-package epsilon.net.core)

;;; ============================================================================
;;; Address Types
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

;;; ============================================================================
;;; Socket Types
;;; ============================================================================

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
                  :documentation "Local address")
   (peer-address :initarg :peer-address :reader tcp-stream-peer-address
                 :type socket-address
                 :documentation "Remote peer address")
   (input :initarg :input :accessor tcp-stream-input
          :initform nil
          :documentation "Input stream for reading")
   (output :initarg :output :accessor tcp-stream-output
           :initform nil
           :documentation "Output stream for writing")
   (connected-p :initarg :connected-p :accessor tcp-stream-connected-p
                :initform t
                :documentation "Connection status"))
  (:documentation "TCP stream socket"))

(defclass udp-socket ()
  ((handle :initarg :handle :reader udp-socket-handle
           :documentation "OS socket file descriptor")
   (local-address :initarg :local-address :reader udp-socket-local-address
                  :type socket-address
                  :documentation "Local bind address")
   (connected-peer :initarg :connected-peer :accessor udp-socket-connected-peer
                   :initform nil
                   :documentation "Connected peer for connected UDP"))
  (:documentation "UDP socket"))

;;; ============================================================================
;;; Error Conditions
;;; ============================================================================

(define-condition network-error (error)
  ((message :initarg :message :reader error-message))
  (:documentation "Base condition for network errors")
  (:report (lambda (condition stream)
             (format stream "Network error: ~A" 
                     (if (slot-boundp condition 'message)
                         (error-message condition)
                         "Unknown error")))))

(define-condition connection-refused (network-error)
  ()
  (:documentation "Connection was refused by the remote host")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Connection refused"))))

(define-condition connection-reset (network-error)
  ()
  (:documentation "Connection was reset by peer")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Connection reset by peer"))))

(define-condition connection-aborted (network-error)
  ()
  (:documentation "Connection was aborted")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Connection aborted"))))

(define-condition timeout-error (network-error)
  ()
  (:documentation "Operation timed out")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Operation timed out"))))

(define-condition address-in-use (network-error)
  ()
  (:documentation "Address is already in use")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Address already in use"))))

(define-condition would-block-error (network-error)
  ()
  (:documentation "Operation would block")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Operation would block"))))

;;; ============================================================================
;;; Error Handling Utilities
;;; ============================================================================

(defun get-errno ()
  "Get the current errno value"
  (handler-case
      (let ((errno-ptr (sb-alien:alien-funcall 
                        (sb-alien:extern-alien "__error" 
                                               (function (* sb-alien:int))))))
        (sb-alien:deref errno-ptr 0))
    (error ()
      ;; If we can't get errno, return -1
      -1)))

(defun errno-to-string (errno)
  "Convert errno to human-readable string"
  (case errno
    (1 "EPERM - Operation not permitted")
    (2 "ENOENT - No such file or directory")
    (9 "EBADF - Bad file descriptor")
    (13 "EACCES - Permission denied")
    (22 "EINVAL - Invalid argument")
    (24 "EMFILE - Too many open files")
    (35 "EAGAIN/EWOULDBLOCK - Resource temporarily unavailable")
    (36 "EINPROGRESS - Operation now in progress")
    (48 "EADDRINUSE - Address already in use")
    (49 "EADDRNOTAVAIL - Cannot assign requested address")
    (54 "ECONNRESET - Connection reset by peer")
    (57 "ENOTCONN - Socket is not connected")
    (60 "ETIMEDOUT - Operation timed out")
    (61 "ECONNREFUSED - Connection refused")
    (t (format nil "Unknown error (~D)" errno))))
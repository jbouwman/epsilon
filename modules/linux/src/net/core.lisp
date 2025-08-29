;;;; Core Socket Utilities for Linux
;;;;
;;;; This module provides core socket operations and utilities.

(defpackage epsilon.net.core
  (:use cl)
  (:local-nicknames
   (const epsilon.net.constants)
   (errors epsilon.net.errors)
   (types epsilon.net.types)
   (address epsilon.net.address)
   (lib epsilon.foreign))
  (:export
   ;; Socket creation and management
   #:create-socket
   #:close-socket
   #:set-nonblocking
   #:set-socket-reuse-addr
   
   ;; Socket options
   #:set-socket-option
   #:get-socket-option
   
   ;; Address operations
   #:get-local-address
   #:get-peer-address
   
   ;; Stream conversion
   #:socket-to-stream))

(in-package epsilon.net.core)

;;; ============================================================================
;;; Socket Creation and Management
;;; ============================================================================

(defun create-socket (domain type protocol)
  "Create a socket with error handling"
  (let ((fd (const:%socket domain type protocol)))
    (errors:check-error fd "socket creation")
    fd))

(defun close-socket (fd)
  "Close a socket file descriptor"
  (const:%close fd))

(defun set-nonblocking (fd)
  "Set a file descriptor to non-blocking mode"
  (let ((flags (const:%fcntl fd const:+f-getfl+ 0)))
    (when (< flags 0)
      (error "Failed to get file descriptor flags for fd ~A" fd))
    (let ((result (const:%fcntl fd const:+f-setfl+ 
                                (logior flags const:+o-nonblock+))))
      (when (< result 0)
        (error "Failed to set non-blocking mode for fd ~A" fd))
      fd)))

(defun set-socket-reuse-addr (fd enable)
  "Enable or disable SO_REUSEADDR on socket"
  (lib:with-foreign-memory ((optval :int :count 1))
    (setf (sb-sys:sap-ref-32 optval 0) (if enable 1 0))
    (let ((result (const:%setsockopt fd const:+sol-socket+ 
                                     const:+so-reuseaddr+ optval 4)))
      (errors:check-error result "setsockopt SO_REUSEADDR"))))

;;; ============================================================================
;;; Socket Options
;;; ============================================================================

(defun set-socket-option (handle level optname value)
  "Set a raw socket option"
  (lib:with-foreign-memory ((optval :int :count 1))
    (setf (sb-sys:sap-ref-32 optval 0) (if value 1 0))
    (let ((result (const:%setsockopt handle level optname optval 4)))
      (errors:check-error result "setsockopt"))))

(defun get-socket-option (handle level optname)
  "Get a raw socket option"
  (lib:with-foreign-memory ((optval :int :count 1)
                            (optlen :int :count 1))
    (setf (sb-sys:sap-ref-32 optlen 0) 4)
    (let ((result (const:%getsockopt handle level optname optval optlen)))
      (errors:check-error result "getsockopt")
      (sb-sys:sap-ref-32 optval 0))))

;;; ============================================================================
;;; Address Operations
;;; ============================================================================

(defun get-local-address (socket-fd)
  "Get the local address of a socket"
  (lib:with-foreign-memory ((sockaddr :char :count 16)
                            (addrlen :int :count 1))
    (setf (sb-sys:sap-ref-32 addrlen 0) 16)
    (let ((result (const:%getsockname socket-fd sockaddr addrlen)))
      (errors:check-error result "getsockname")
      (address:parse-sockaddr-in sockaddr))))

(defun get-peer-address (socket-fd)
  "Get the peer address of a socket"
  (lib:with-foreign-memory ((sockaddr :char :count 16)
                            (addrlen :int :count 1))
    (setf (sb-sys:sap-ref-32 addrlen 0) 16)
    (let ((result (const:%getpeername socket-fd sockaddr addrlen)))
      (errors:check-error result "getpeername")
      (address:parse-sockaddr-in sockaddr))))

;;; ============================================================================
;;; Stream Conversion
;;; ============================================================================

(defun socket-to-stream (socket direction)
  "Convert socket to Lisp stream"
  (sb-sys:make-fd-stream socket
                         :input (member direction '(:input :io))
                         :output (member direction '(:output :io))
                         :element-type '(unsigned-byte 8)
                         :buffering :none))
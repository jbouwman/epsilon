;;;; Core Socket Utilities for Linux
;;;;
;;;; Common socket operations and utilities

(defpackage epsilon.net.core
  (:use cl)
  (:local-nicknames
   (const epsilon.net.constants)
   (errors epsilon.net.errors)
   (types epsilon.net.types)
   (address epsilon.net.address)
   (lib epsilon.foreign))
  (:export
   ;; Socket creation
   #:create-socket
   #:close-socket
   
   ;; Socket configuration
   #:set-nonblocking
   #:set-socket-reuse-addr
   #:set-socket-option
   #:get-socket-option
   
   ;; Socket operations
   #:bind-socket
   #:listen-socket
   #:get-local-address
   #:get-peer-address
   
   ;; Stream conversion
   #:socket-to-stream
   
   ;; Data utilities
   #:normalize-data
   #:buffer-to-octets))

(in-package epsilon.net.core)

;;; ============================================================================
;;; Socket Creation and Cleanup
;;; ============================================================================

(defun create-socket (family type protocol)
  "Create a socket with error handling"
  (let ((fd (const:%socket family type protocol)))
    (errors:check-error fd "socket creation")
    fd))

(defun close-socket (fd)
  "Close a socket file descriptor"
  (when (and (integerp fd) (>= fd 0))
    (const:%close fd)))

;;; ============================================================================
;;; Socket Configuration
;;; ============================================================================

(defun set-nonblocking (fd)
  "Set a file descriptor to non-blocking mode"
  (let ((flags (const:%fcntl fd const:+f-getfl+ 0)))
    (when (< flags 0)
      (error 'errors:network-error 
             :message "Failed to get file descriptor flags"))
    (let ((result (const:%fcntl fd const:+f-setfl+ 
                                (logior flags const:+o-nonblock+))))
      (when (< result 0)
        (error 'errors:network-error 
               :message "Failed to set non-blocking mode"))
      fd)))

(defun set-socket-reuse-addr (fd enable)
  "Enable or disable SO_REUSEADDR on socket"
  (lib:with-foreign-memory ((optval :int :count 1))
    (setf (sb-sys:sap-ref-32 optval 0) (if enable 1 0))
    (let ((result (const:%setsockopt fd const:+sol-socket+ 
                                     const:+so-reuseaddr+ optval 4)))
      (errors:check-error result "setsockopt SO_REUSEADDR"))))

(defun set-socket-option (fd level option value)
  "Set a socket option"
  (lib:with-foreign-memory ((optval :int :count 1))
    (setf (sb-sys:sap-ref-32 optval 0) 
          (etypecase value
            (boolean (if value 1 0))
            (integer value)))
    (let ((result (const:%setsockopt fd level option optval 4)))
      (errors:check-error result "setsockopt"))))

(defun get-socket-option (fd level option)
  "Get a socket option"
  (lib:with-foreign-memory ((optval :int :count 1)
                            (optlen :int :count 1))
    (setf (sb-sys:sap-ref-32 optlen 0) 4)
    (let ((result (const:%getsockopt fd level option optval optlen)))
      (errors:check-error result "getsockopt")
      (sb-sys:sap-ref-32 optval 0))))

;;; ============================================================================
;;; Socket Operations
;;; ============================================================================

(defun bind-socket (fd address)
  "Bind a socket to an address"
  (let ((sock-addr (address:normalize-address address)))
    (lib:with-foreign-memory ((addr :char :count 16))
      (address:make-sockaddr-in-into addr 
                                     (types:socket-address-ip sock-addr)
                                     (types:socket-address-port sock-addr))
      (let ((result (const:%bind fd addr 16)))
        (errors:check-error result "bind")))))

(defun listen-socket (fd backlog)
  "Put socket in listening state"
  (let ((result (const:%listen fd backlog)))
    (errors:check-error result "listen")))

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

;;; ============================================================================
;;; Data Utilities
;;; ============================================================================

(defun normalize-data (data)
  "Convert various data types to octet vector"
  (etypecase data
    (string (sb-ext:string-to-octets data))
    ((vector (unsigned-byte 8)) data)
    (vector (coerce data '(vector (unsigned-byte 8))))
    (list (coerce data '(vector (unsigned-byte 8))))))

(defun buffer-to-octets (buffer &key (start 0) (end (length buffer)))
  "Extract octets from buffer"
  (subseq buffer start end))
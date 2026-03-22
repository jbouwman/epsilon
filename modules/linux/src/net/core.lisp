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
   #:socket-to-stream)
  (:enter t))

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

(defun option-to-constants (option)
  "Map a keyword option to (level optname) values"
  (case option
    (:reuse-address (values const:+sol-socket+ const:+so-reuseaddr+))
    (:keep-alive    (values const:+sol-socket+ const:+so-keepalive+))
    (:broadcast     (values const:+sol-socket+ const:+so-broadcast+))
    (:error         (values const:+sol-socket+ const:+so-error+))
    (:linger        (values const:+sol-socket+ const:+so-linger+))
    (:recv-buffer   (values const:+sol-socket+ const:+so-rcvbuf+))
    (:send-buffer   (values const:+sol-socket+ const:+so-sndbuf+))
    (:recv-timeout  (values const:+sol-socket+ const:+so-rcvtimeo+))
    (:send-timeout  (values const:+sol-socket+ const:+so-sndtimeo+))
    ((:nodelay :no-delay) (values const:+ipproto-tcp+ const:+tcp-nodelay+))
    (t (error "Unknown socket option: ~A" option))))

(defun get-socket-handle (socket)
  "Extract the file descriptor from a socket object"
  (etypecase socket
    (types:tcp-listener (types:tcp-listener-handle socket))
    (types:tcp-stream (types:tcp-stream-handle socket))
    (types:udp-socket (types:udp-socket-handle socket))
    (integer socket)))

(defun set-socket-option (socket option value)
  "Set a socket option"
  (multiple-value-bind (level optname) (option-to-constants option)
    (let ((fd (get-socket-handle socket)))
      (case option
        ((:reuse-address :keep-alive :broadcast :nodelay :no-delay)
         ;; Boolean options
         (lib:with-foreign-memory ((optval :int :count 1))
           (setf (sb-sys:sap-ref-32 optval 0) (if value 1 0))
           (let ((result (const:%setsockopt fd level optname optval 4)))
             (errors:check-error result "setsockopt"))))
        ((:recv-buffer :send-buffer)
         ;; Integer size options
         (lib:with-foreign-memory ((optval :int :count 1))
           (setf (sb-sys:sap-ref-32 optval 0) value)
           (let ((result (const:%setsockopt fd level optname optval 4)))
             (errors:check-error result "setsockopt"))))
        ((:recv-timeout :send-timeout)
         ;; Timeout options (value in seconds, convert to timeval)
         (lib:with-foreign-memory ((optval :char :count 16))
           ;; struct timeval { long tv_sec; long tv_usec; }
           (let ((seconds (floor value))
                 (microseconds (* (mod value 1) 1000000)))
             (setf (sb-sys:sap-ref-64 optval 0) seconds)
             (setf (sb-sys:sap-ref-64 optval 8) microseconds))
           (let ((result (const:%setsockopt fd level optname optval 16)))
             (errors:check-error result "setsockopt"))))
        (:linger
         ;; Linger option
         (lib:with-foreign-memory ((optval :char :count 8))
           ;; struct linger { int l_onoff; int l_linger; }
           (if value
               (progn
                 (setf (sb-sys:sap-ref-32 optval 0) 1)
                 (setf (sb-sys:sap-ref-32 optval 4) value))
               (setf (sb-sys:sap-ref-32 optval 0) 0))
           (let ((result (const:%setsockopt fd level optname optval 8)))
             (errors:check-error result "setsockopt"))))))))

(defun get-socket-option (socket option)
  "Get a socket option"
  (multiple-value-bind (level optname) (option-to-constants option)
    (let ((fd (get-socket-handle socket)))
      (case option
        ((:reuse-address :keep-alive :broadcast :nodelay :no-delay)
         ;; Boolean options
         (lib:with-foreign-memory ((optval :int :count 1)
                                   (optlen :int :count 1))
           (setf (sb-sys:sap-ref-32 optlen 0) 4)
           (let ((result (const:%getsockopt fd level optname optval optlen)))
             (errors:check-error result "getsockopt")
             (not (zerop (sb-sys:sap-ref-32 optval 0))))))
        ((:error :recv-buffer :send-buffer)
         ;; Integer options
         (lib:with-foreign-memory ((optval :int :count 1)
                                   (optlen :int :count 1))
           (setf (sb-sys:sap-ref-32 optlen 0) 4)
           (let ((result (const:%getsockopt fd level optname optval optlen)))
             (errors:check-error result "getsockopt")
             (sb-sys:sap-ref-32 optval 0))))
        ((:recv-timeout :send-timeout)
         ;; Timeout options (timeval to seconds)
         (lib:with-foreign-memory ((optval :char :count 16)
                                   (optlen :int :count 1))
           (setf (sb-sys:sap-ref-32 optlen 0) 16)
           (let ((result (const:%getsockopt fd level optname optval optlen)))
             (errors:check-error result "getsockopt")
             (let ((seconds (sb-sys:sap-ref-64 optval 0))
                   (microseconds (sb-sys:sap-ref-64 optval 8)))
               (+ (* seconds 1000) (floor microseconds 1000))))))
        (:linger
         ;; Linger option
         (lib:with-foreign-memory ((optval :char :count 8)
                                   (optlen :int :count 1))
           (setf (sb-sys:sap-ref-32 optlen 0) 8)
           (let ((result (const:%getsockopt fd level optname optval optlen)))
             (errors:check-error result "getsockopt")
             (if (zerop (sb-sys:sap-ref-32 optval 0))
                 nil
                 (sb-sys:sap-ref-32 optval 4)))))))))

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
      ;; Parse the sockaddr directly and create the correct type
      (let* ((port-bytes (sb-sys:sap-ref-16 sockaddr 2))
             (port (logior (ash (logand port-bytes #xff) 8)
                           (ash (logand port-bytes #xff00) -8)))
             (ip (format nil "~D.~D.~D.~D"
                        (sb-sys:sap-ref-8 sockaddr 4)
                        (sb-sys:sap-ref-8 sockaddr 5)
                        (sb-sys:sap-ref-8 sockaddr 6)
                        (sb-sys:sap-ref-8 sockaddr 7))))
        (types:make-socket-address ip port)))))

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

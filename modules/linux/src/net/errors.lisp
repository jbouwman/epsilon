;;;; Linux Networking Error Handling
;;;;
;;;; Error conditions and error handling utilities

(defpackage epsilon.net.errors
  (:use cl)
  (:local-nicknames
   (const epsilon.net.constants))
  (:export
   ;; Error conditions
   #:network-error
   #:connection-refused
   #:connection-reset
   #:connection-aborted
   #:timeout-error
   #:address-in-use
   #:address-not-available
   #:would-block-error
   #:already-connected
   #:not-connected
   #:shutdown-error
   #:host-unreachable
   #:network-unreachable
   #:operation-in-progress
   
   ;; Error utilities
   #:get-errno
   #:errno-to-string
   #:errno-to-condition
   #:check-error
   #:with-error-handling))

(in-package epsilon.net.errors)

;;; ============================================================================
;;; Error Conditions
;;; ============================================================================

(define-condition network-error (error)
  ((message :initarg :message :reader error-message)
   (errno :initarg :errno :initform nil :reader error-errno))
  (:report (lambda (condition stream)
             (format stream "Network error: ~A~@[ (errno: ~D)~]"
                     (error-message condition)
                     (error-errno condition))))
  (:documentation "Base condition for network errors"))

(define-condition connection-refused (network-error)
  ()
  (:documentation "Connection was refused by the remote host"))

(define-condition connection-reset (network-error)
  ()
  (:documentation "Connection was reset by peer"))

(define-condition connection-aborted (network-error)
  ()
  (:documentation "Connection was aborted"))

(define-condition timeout-error (network-error)
  ()
  (:documentation "Operation timed out"))

(define-condition address-in-use (network-error)
  ()
  (:documentation "Address is already in use"))

(define-condition address-not-available (network-error)
  ()
  (:documentation "Cannot assign requested address"))

(define-condition would-block-error (network-error)
  ()
  (:documentation "Operation would block"))

(define-condition already-connected (network-error)
  ()
  (:documentation "Socket is already connected"))

(define-condition not-connected (network-error)
  ()
  (:documentation "Socket is not connected"))

(define-condition shutdown-error (network-error)
  ()
  (:documentation "Cannot send after socket shutdown"))

(define-condition host-unreachable (network-error)
  ()
  (:documentation "No route to host"))

(define-condition network-unreachable (network-error)
  ()
  (:documentation "Network is unreachable"))

(define-condition operation-in-progress (network-error)
  ()
  (:documentation "Operation already in progress"))

;;; ============================================================================
;;; Error Utilities
;;; ============================================================================

(defun get-errno ()
  "Get the current errno value"
  (handler-case
      (let ((errno-ptr (sb-alien:alien-funcall 
                        (sb-alien:extern-alien "__errno_location" 
                                               (function (* sb-alien:int))))))
        (sb-alien:deref errno-ptr 0))
    (error ()
      ;; If we can't get errno, return -1
      -1)))

(defun errno-to-string (errno)
  "Convert errno to human-readable string"
  (case errno
    (#.const:+eperm+ "EPERM - Operation not permitted")
    (#.const:+enoent+ "ENOENT - No such file or directory")
    (#.const:+eintr+ "EINTR - Interrupted system call")
    (#.const:+eio+ "EIO - I/O error")
    (#.const:+ebadf+ "EBADF - Bad file descriptor")
    (#.const:+eagain+ "EAGAIN/EWOULDBLOCK - Resource temporarily unavailable")
    (#.const:+eacces+ "EACCES - Permission denied")
    (#.const:+efault+ "EFAULT - Bad address")
    (#.const:+ebusy+ "EBUSY - Device or resource busy")
    (#.const:+eexist+ "EEXIST - File exists")
    (#.const:+einval+ "EINVAL - Invalid argument")
    (#.const:+enfile+ "ENFILE - File table overflow")
    (#.const:+emfile+ "EMFILE - Too many open files")
    (#.const:+enospc+ "ENOSPC - No space left on device")
    (#.const:+epipe+ "EPIPE - Broken pipe")
    (#.const:+enotsock+ "ENOTSOCK - Socket operation on non-socket")
    (#.const:+edestaddrreq+ "EDESTADDRREQ - Destination address required")
    (#.const:+emsgsize+ "EMSGSIZE - Message too long")
    (#.const:+eprototype+ "EPROTOTYPE - Protocol wrong type for socket")
    (#.const:+enoprotoopt+ "ENOPROTOOPT - Protocol not available")
    (#.const:+eprotonosupport+ "EPROTONOSUPPORT - Protocol not supported")
    (#.const:+esocktnosupport+ "ESOCKTNOSUPPORT - Socket type not supported")
    (#.const:+eopnotsupp+ "EOPNOTSUPP - Operation not supported")
    (#.const:+epfnosupport+ "EPFNOSUPPORT - Protocol family not supported")
    (#.const:+eafnosupport+ "EAFNOSUPPORT - Address family not supported")
    (#.const:+eaddrinuse+ "EADDRINUSE - Address already in use")
    (#.const:+eaddrnotavail+ "EADDRNOTAVAIL - Cannot assign requested address")
    (#.const:+enetdown+ "ENETDOWN - Network is down")
    (#.const:+enetunreach+ "ENETUNREACH - Network is unreachable")
    (#.const:+enetreset+ "ENETRESET - Network dropped connection on reset")
    (#.const:+econnaborted+ "ECONNABORTED - Software caused connection abort")
    (#.const:+econnreset+ "ECONNRESET - Connection reset by peer")
    (#.const:+enobufs+ "ENOBUFS - No buffer space available")
    (#.const:+eisconn+ "EISCONN - Socket is already connected")
    (#.const:+enotconn+ "ENOTCONN - Socket is not connected")
    (#.const:+eshutdown+ "ESHUTDOWN - Cannot send after shutdown")
    (#.const:+etimedout+ "ETIMEDOUT - Operation timed out")
    (#.const:+econnrefused+ "ECONNREFUSED - Connection refused")
    (#.const:+ehostdown+ "EHOSTDOWN - Host is down")
    (#.const:+ehostunreach+ "EHOSTUNREACH - No route to host")
    (#.const:+ealready+ "EALREADY - Operation already in progress")
    (#.const:+einprogress+ "EINPROGRESS - Operation now in progress")
    (t (format nil "Unknown error (~D)" errno))))

(defun errno-to-condition (errno)
  "Map errno to appropriate condition type"
  (case errno
    (#.const:+eagain+ 'would-block-error)
    (#.const:+ewouldblock+ 'would-block-error)
    (#.const:+eaddrinuse+ 'address-in-use)
    (#.const:+eaddrnotavail+ 'address-not-available)
    (#.const:+econnreset+ 'connection-reset)
    (#.const:+econnaborted+ 'connection-aborted)
    (#.const:+etimedout+ 'timeout-error)
    (#.const:+econnrefused+ 'connection-refused)
    (#.const:+eisconn+ 'already-connected)
    (#.const:+enotconn+ 'not-connected)
    (#.const:+eshutdown+ 'shutdown-error)
    (#.const:+ehostunreach+ 'host-unreachable)
    (#.const:+enetunreach+ 'network-unreachable)
    (#.const:+ealready+ 'operation-in-progress)
    (#.const:+einprogress+ 'operation-in-progress)
    (t 'network-error)))

(defun check-error (result operation)
  "Check system call result and signal appropriate error"
  (when (< result 0)
    (let* ((errno (get-errno))
           (condition-type (errno-to-condition errno))
           (message (format nil "~A failed: ~A" operation (errno-to-string errno))))
      (error condition-type :message message :errno errno))))

(defmacro with-error-handling ((result operation) &body body)
  "Execute body with automatic error checking"
  `(let ((,result (progn ,@body)))
     (check-error ,result ,operation)
     ,result))
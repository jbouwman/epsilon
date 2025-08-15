;;;; Network error conditions and error handling utilities

(defpackage epsilon.net.errors
  (:use cl)
  (:export
   ;; Error conditions
   network-error
   connection-refused
   connection-reset
   connection-aborted
   timeout-error
   address-in-use
   would-block-error
   
   ;; Error utilities
   get-errno
   errno-to-string))

(in-package epsilon.net.errors)

;;; ============================================================================
;;; Error Conditions
;;; ============================================================================

(define-condition network-error (error)
  ((message :initarg :message :reader error-message))
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

(define-condition would-block-error (network-error)
  ()
  (:documentation "Operation would block"))

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
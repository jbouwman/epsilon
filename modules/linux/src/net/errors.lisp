;;;; Network Error Handling for Linux
;;;;
;;;; This module provides error handling utilities and conditions for network operations.

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
   #:would-block-error
   
   ;; Error utilities
   #:get-errno
   #:errno-to-string
   #:errno-to-condition
   #:check-error))

(in-package epsilon.net.errors)

;;; ============================================================================
;;; Error Conditions
;;; ============================================================================

(define-condition network-error (error)
  ((message :initarg :message :reader error-message))
  (:documentation "Base condition for network errors")
  (:report (lambda (condition stream)
             (format stream "Network error: ~A" (error-message condition)))))

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
    (1 "EPERM - Operation not permitted")
    (2 "ENOENT - No such file or directory")
    (9 "EBADF - Bad file descriptor")
    (11 "EAGAIN/EWOULDBLOCK - Resource temporarily unavailable")
    (13 "EACCES - Permission denied")
    (22 "EINVAL - Invalid argument")
    (24 "EMFILE - Too many open files")
    (32 "EPIPE - Broken pipe")
    (98 "EADDRINUSE - Address already in use")
    (99 "EADDRNOTAVAIL - Cannot assign requested address")
    (103 "ECONNABORTED - Software caused connection abort")
    (104 "ECONNRESET - Connection reset by peer")
    (107 "ENOTCONN - Socket is not connected")
    (110 "ETIMEDOUT - Operation timed out")
    (111 "ECONNREFUSED - Connection refused")
    (113 "EHOSTUNREACH - No route to host")
    (114 "EALREADY - Operation already in progress")
    (115 "EINPROGRESS - Operation now in progress")
    (t (format nil "Unknown error (~D)" errno))))

(defun errno-to-condition (errno)
  "Convert errno to appropriate condition class"
  (case errno
    ((11) 'would-block-error)  ; EAGAIN/EWOULDBLOCK
    ((98) 'address-in-use)
    ((103) 'connection-aborted)
    ((104) 'connection-reset)
    ((110) 'timeout-error)
    ((111) 'connection-refused)
    (t 'network-error)))

(defun check-error (result operation)
  "Check system call result and signal appropriate error"
  (when (< result 0)
    (let* ((errno (get-errno))
           (condition-class (errno-to-condition errno))
           (message (format nil "~A: ~A" operation (errno-to-string errno))))
      (error condition-class :message message))))
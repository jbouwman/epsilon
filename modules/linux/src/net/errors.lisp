;;;; Network Error Handling for Linux
;;;;
;;;; This module provides error handling utilities and conditions for network operations.

(defpackage epsilon.net.errors
  (:use cl)
  (:local-nicknames
   (const epsilon.net.constants))
  (:import-from #:epsilon.net.conditions
   #:network-error #:error-message #:connection-refused #:connection-reset
   #:connection-aborted #:timeout-error #:address-in-use
   #:would-block-error)
  (:export
   ;; Error conditions (from epsilon.net.conditions)
   #:network-error
   #:error-message
   #:connection-refused
   #:connection-reset
   #:connection-aborted
   #:timeout-error
   #:address-in-use
   #:would-block-error

   ;; Extended error conditions
   #:dns-resolution-error
   #:dns-error-hostname
   #:host-unreachable
   #:unreachable-host

   ;; Error utilities
   #:get-errno
   #:errno-to-string
   #:errno-to-condition
   #:check-error)
  (:enter t))

;;; ============================================================================
;;; Print Methods for Condition Reporting
;;; ============================================================================
;;;
;;; These print-object methods ensure that when conditions are printed with
;;; ~A format directive (princ), the actual error message is shown instead
;;; of SBCL's default "Condition <TYPE> was signalled."

(defmethod print-object ((condition network-error) stream)
  "Print network-error with its message for clear error reporting."
  (if *print-escape*
      (print-unreadable-object (condition stream :type t :identity t)
        (format stream "~A" (error-message condition)))
      (format stream "Network error: ~A" (error-message condition))))

;;; ============================================================================
;;; Error Utilities
;;; ============================================================================

;;; ============================================================================
;;; Extended Error Conditions
;;; ============================================================================

(define-condition dns-resolution-error (network-error)
  ((hostname :initarg :hostname :reader dns-error-hostname))
  (:documentation "DNS resolution failed")
  (:report (lambda (condition stream)
             (format stream "DNS resolution failed for ~A: ~A"
                     (dns-error-hostname condition)
                     (if (slot-boundp condition 'message)
                         (error-message condition)
                         "Unknown error")))))

(define-condition host-unreachable (network-error)
  ((host :initarg :host :reader unreachable-host))
  (:documentation "Host is unreachable")
  (:report (lambda (condition stream)
             (format stream "Host unreachable: ~A - ~A"
                     (unreachable-host condition)
                     (if (slot-boundp condition 'message)
                         (error-message condition)
                         "No route to host")))))

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

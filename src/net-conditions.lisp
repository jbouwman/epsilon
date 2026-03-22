;;;; Shared network error conditions
;;;;
;;;; Base condition hierarchy for network errors, identical across
;;;; darwin, linux, and windows platforms. Each platform provides
;;;; its own errno-to-condition mapping.

(defpackage :epsilon.net.conditions
  (:use :cl)
  (:export
   #:network-error
   #:error-message
   #:connection-refused
   #:connection-reset
   #:connection-aborted
   #:timeout-error
   #:address-in-use
   #:would-block-error))

(in-package :epsilon.net.conditions)

(define-condition network-error (error)
  ((message :initarg :message :reader error-message :initform nil))
  (:report (lambda (c s)
             (let ((msg (error-message c)))
               (if msg
                   (format s "Network error: ~A" msg)
                   (format s "Network error"))))))

(define-condition connection-refused (network-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Connection refused"))))

(define-condition connection-reset (network-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Connection reset by peer"))))

(define-condition connection-aborted (network-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Connection aborted"))))

(define-condition timeout-error (network-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Connection timed out"))))

(define-condition address-in-use (network-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Address already in use"))))

(define-condition would-block-error (network-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Operation would block"))))

;;;; UDP Operations Wrapper
;;;;
;;;; This module provides the high-level UDP API by delegating to the sockets module.

(defpackage epsilon.net.udp
  (:use cl)
  (:local-nicknames
   (sockets epsilon.net.sockets)
   (types epsilon.net.types))
  ;; Re-export all UDP functions directly from sockets module
  (:import-from epsilon.net.sockets
   #:udp-bind
   #:udp-connect
   #:udp-send
   #:udp-recv
   #:udp-send-to
   #:udp-recv-from
   #:udp-local-addr
   #:udp-poll-send
   #:udp-poll-recv)
  (:export
   ;; Re-exported from sockets
   #:udp-bind
   #:udp-connect
   #:udp-send
   #:udp-recv
   #:udp-send-to
   #:udp-recv-from
   #:udp-local-addr
   #:udp-poll-send
   #:udp-poll-recv
   
   ;; Macros
   #:with-udp-socket))

(in-package epsilon.net.udp)

;;; ============================================================================
;;; Convenience Macros
;;; ============================================================================

(defmacro with-udp-socket ((socket address) &body body)
  "Execute body with a UDP socket bound to address"
  `(let ((,socket (udp-bind ,address)))
     (unwind-protect
          (progn ,@body)
       (when ,socket
         (handler-case
             (epsilon.net.constants:%close (types:udp-socket-handle ,socket))
           (error ()
             ;; Ignore errors during cleanup
             nil))))))
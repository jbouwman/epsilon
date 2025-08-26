;;;; Compatibility shim for epsilon.net.udp
;;;;
;;;; This package now re-exports everything from epsilon.net.sockets
;;;; to maintain backwards compatibility while UDP has been consolidated.

(defpackage epsilon.net.udp
  (:use cl)
  (:import-from epsilon.net.sockets
   ;; UDP operations
   udp-bind
   udp-connect
   udp-send
   udp-recv
   udp-send-to
   udp-recv-from
   udp-local-addr
   udp-try-send
   udp-try-recv
   udp-poll-send
   udp-poll-recv)
  (:export
   ;; UDP operations
   udp-bind
   udp-connect
   udp-send
   udp-recv
   udp-send-to
   udp-recv-from
   udp-local-addr
   udp-try-send
   udp-try-recv
   udp-poll-send
   udp-poll-recv))

(in-package epsilon.net.udp)


;;;; Compatibility shim for epsilon.net.types
;;;;
;;;; This package now re-exports everything from epsilon.net.core
;;;; to maintain backwards compatibility while types have been consolidated.

(defpackage epsilon.net.types
  (:use cl)
  (:import-from epsilon.net.core
   ;; Address types
   address
   ipv4-address
   ipv4-address-octets
   ipv6-address
   ipv6-address-words
   socket-address
   socket-address-ip
   socket-address-port
   socket-address-family
   
   ;; Socket types
   tcp-listener
   tcp-listener-handle
   tcp-listener-local-address
   tcp-listener-kqueue
   tcp-listener-backlog
   
   tcp-stream
   tcp-stream-handle
   tcp-stream-local-address
   tcp-stream-peer-address
   tcp-stream-input
   tcp-stream-output
   tcp-stream-connected-p
   
   udp-socket
   udp-socket-handle
   udp-socket-local-address
   udp-socket-connected-peer)
  (:export
   ;; Address types
   address
   ipv4-address
   ipv4-address-octets
   ipv6-address
   ipv6-address-words
   socket-address
   socket-address-ip
   socket-address-port
   socket-address-family
   
   ;; Socket types
   tcp-listener
   tcp-listener-handle
   tcp-listener-local-address
   tcp-listener-kqueue
   tcp-listener-backlog
   
   tcp-stream
   tcp-stream-handle
   tcp-stream-local-address
   tcp-stream-peer-address
   tcp-stream-input
   tcp-stream-output
   tcp-stream-connected-p
   
   udp-socket
   udp-socket-handle
   udp-socket-local-address
   udp-socket-connected-peer))

(in-package epsilon.net.types)
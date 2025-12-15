;;;; Main epsilon.net package - re-exports all public APIs

(defpackage epsilon.net
  (:use cl)
  (:import-from epsilon.net.core
   network-error connection-refused connection-reset connection-aborted
   timeout-error address-in-use would-block-error
   address ipv4-address ipv6-address socket-address
   tcp-listener tcp-stream udp-socket
   socket-address-ip socket-address-port socket-address-family
   tcp-stream-handle)
  (:import-from epsilon.net.address
   make-socket-address resolve-address parse-address)
  (:import-from epsilon.net.socket-options
   set-socket-option get-socket-option)
  (:import-from epsilon.net.sockets
   tcp-bind tcp-accept tcp-incoming tcp-try-accept tcp-poll-accept tcp-local-addr tcp-close
   tcp-connect tcp-read tcp-write tcp-write-all tcp-flush
   tcp-try-read tcp-try-write tcp-poll-read tcp-poll-write
   tcp-peer-addr tcp-shutdown tcp-stream-reader tcp-stream-writer tcp-connected-p
   udp-bind udp-connect udp-send udp-recv udp-send-to udp-recv-from
   udp-local-addr udp-try-send udp-try-recv udp-poll-send udp-poll-recv)
  (:export
   ;; Address types
   address
   ipv4-address
   ipv6-address
   socket-address
   
   ;; Socket types
   tcp-listener
   tcp-stream
   udp-socket
   
   ;; TCP Listener operations
   tcp-bind
   tcp-accept
   tcp-incoming
   tcp-try-accept
   tcp-poll-accept
   tcp-local-addr
   tcp-close
   
   ;; TCP Stream operations
   tcp-connect
   tcp-read
   tcp-write
   tcp-write-all
   tcp-flush
   tcp-try-read
   tcp-try-write
   tcp-poll-read
   tcp-poll-write
   tcp-peer-addr
   tcp-shutdown
   tcp-stream-reader
   tcp-stream-writer
   tcp-stream-handle
   
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
   udp-poll-recv
   
   ;; Address resolution
   make-socket-address
   resolve-address
   socket-address-ip
   socket-address-port
   socket-address-family
   parse-address
   
   ;; Socket options
   set-socket-option
   get-socket-option
   
   ;; Error conditions
   network-error
   connection-refused
   connection-reset
   connection-aborted
   timeout-error
   address-in-use
   would-block-error
   
   ;; Status checks
   tcp-connected-p))

(in-package epsilon.net)
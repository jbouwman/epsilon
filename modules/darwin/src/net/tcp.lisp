;;;; Compatibility shim for epsilon.net.tcp
;;;;
;;;; This package now re-exports everything from epsilon.net.sockets
;;;; to maintain backwards compatibility while TCP has been consolidated.

(defpackage epsilon.net.tcp
  (:use cl)
  (:import-from epsilon.net.sockets
   ;; TCP Listener operations
   tcp-bind
   tcp-accept
   tcp-incoming
   tcp-try-accept
   tcp-poll-accept
   tcp-local-addr
   
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
   tcp-connected-p)
  (:export
   ;; TCP Listener operations
   tcp-bind
   tcp-accept
   tcp-incoming
   tcp-try-accept
   tcp-poll-accept
   tcp-local-addr
   
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
   tcp-connected-p))

(in-package epsilon.net.tcp)

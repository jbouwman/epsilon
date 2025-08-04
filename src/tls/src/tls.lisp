;;;; TLS Package Definition and Core Structures
;;;; 
;;;; This file provides the package definition and core data structures
;;;; for the epsilon.tls module. The actual implementations are in tls-impl.lisp

(defpackage :epsilon.tls
  (:use :cl)
  (:local-nicknames
   (#:map #:epsilon.map)
   (#:stream #:epsilon.stream))
  (:export
   ;; TLS context management
   #:create-tls-context
   #:make-tls-context
   #:tls-context-p
   #:tls-context-server-p
   #:tls-context-cert-file
   #:tls-context-key-file
   #:tls-context-verify-mode
   #:load-cert-file
   #:load-key-file
   #:set-verify-mode
   
   ;; TLS connection handling
   #:tls-connect
   #:tls-accept
   #:tls-close
   #:tls-read
   #:tls-write
   #:tls-stream
   #:tls-connection-p
   #:tls-connection-socket
   #:tls-connection-connected-p
   #:tls-connection-handshake-complete-p
   
   ;; Constants
   #:+tls-verify-none+
   #:+tls-verify-peer+
   
   ;; Utilities
   #:tls-handshake
   #:tls-version
   #:tls-cipher
   #:get-peer-certificate))

(in-package :epsilon.tls)

;;;; TLS Constants

(defconstant +tls-verify-none+ 0)
(defconstant +tls-verify-peer+ 1)

;;;; TLS Context Structure (for stub fallback)

(defstruct tls-context
  "TLS context for managing certificates and settings"
  (server-p nil :type boolean)
  (cert-file nil :type (or null string))
  (key-file nil :type (or null string))
  (verify-mode +tls-verify-peer+ :type integer)
  (cipher-list nil :type (or null string)))

;;;; TLS Connection Structure (for stub fallback)

(defstruct tls-connection
  "TLS connection wrapper around a socket"
  (socket nil)
  (context nil :type (or null tls-context))
  (connected-p nil :type boolean)
  (handshake-complete-p nil :type boolean))

;;;; NOTE: All function implementations are in tls-impl.lisp
;;;; This file only provides package definition and data structures
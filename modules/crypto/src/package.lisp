;;;; Package Definition for epsilon.crypto
;;;;
;;;; This file defines the main package with core types and constants

(defpackage :epsilon.crypto
  (:use :cl)
  (:local-nicknames
   (#:map #:epsilon.map)
   (#:stream #:epsilon.stream))
  (:export
   ;;;; TLS/SSL functionality
   ;; TLS context management
   #:create-tls-context
   #:make-tls-context
   #:tls-context-p
   #:make-tls-connection
   #:tls-connection-p
   #:tls-context-server-p
   #:tls-context-cert-file
   #:tls-context-key-file
   #:tls-context-verify-mode
   #:tls-context-handle
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
   #:tls-connection-socket
   #:tls-connection-connected-p
   #:tls-connection-handshake-complete-p
   #:tls-connection-ssl-handle
   
   ;; TLS Constants
   #:+tls-verify-none+
   #:+tls-verify-peer+
   
   ;; TLS Utilities
   #:tls-handshake
   #:tls-version
   #:tls-cipher
   #:get-peer-certificate
   
   ;;;; Public Key Cryptography
   ;; Key types and structures
   #:crypto-key
   #:make-crypto-key
   #:crypto-key-p
   #:crypto-key-type
   #:crypto-key-bits
   #:crypto-key-public-p
   #:crypto-key-private-p
   #:crypto-key-handle
   
   ;; Key generation
   #:generate-rsa-key
   #:generate-ec-key
   #:generate-ed25519-key
   
   ;; Key management
   #:load-public-key
   #:load-private-key
   #:save-public-key
   #:save-private-key
   #:export-public-key
   #:import-public-key
   #:derive-public-key
   
   ;; Key formats
   #:key-to-pem
   #:key-from-pem
   #:key-to-der
   #:key-from-der
   
   ;; Digital signatures
   #:sign
   #:verify
   #:sign-message
   #:verify-message
   
   ;; Encryption/Decryption
   #:encrypt
   #:decrypt
   #:seal
   #:open-sealed
   
   ;; Message digests
   #:digest
   #:digest-algorithm
   #:+digest-sha256+
   #:+digest-sha384+
   #:+digest-sha512+
   #:+digest-sha3-256+
   #:+digest-sha3-512+
   
   ;; X.509 Certificates
   #:x509-certificate
   #:make-x509-certificate
   #:x509-certificate-p
   #:x509-certificate-subject
   #:x509-certificate-issuer
   #:x509-certificate-serial
   #:x509-certificate-not-before
   #:x509-certificate-not-after
   #:x509-certificate-handle
   #:create-certificate
   #:load-certificate
   #:save-certificate
   #:certificate-public-key
   #:verify-certificate
   
   ;; Certificate Signing Requests
   #:create-csr
   #:sign-csr
   #:load-csr
   #:save-csr
   
   ;; Random number generation
   #:crypto-random-bytes
   #:crypto-random-integer
   
   ;; Error handling
   #:crypto-error
   #:crypto-error-code
   #:crypto-error-string
   #:get-crypto-errors
   
   ;; Integration functions
   #:tls-context-set-key
   #:tls-context-set-certificate
   #:tls-get-peer-public-key
   #:load-key-and-cert-pair
   #:save-key-and-cert-pair
   
   ;; OpenSSL implementation
   #:openssl-context
   #:make-openssl-context
   #:openssl-context-p
   #:openssl-context-handle
   #:openssl-context-server-p
   #:openssl-context-cert-file
   #:openssl-context-key-file
   #:openssl-context-verify-mode
   #:openssl-connection
   #:make-openssl-connection
   #:openssl-connection-p
   #:openssl-connection-ssl
   #:openssl-connection-socket
   #:openssl-connection-context
   #:openssl-connection-connected-p
   #:openssl-connect
   #:openssl-accept
   #:openssl-close
   #:openssl-read
   #:openssl-write
   #:openssl-stream
   #:create-openssl-context
   
   ;; Test mocking (for TLS)
   #:with-mock-tls
   #:enable-mock-mode
   #:disable-mock-mode
   #:simulate-tls-handshake
   #:mock-tls-read
   #:mock-tls-write
   #:create-mock-connection
   #:mock-tls-connection-p
   #:mock-tls-connection-handshake-complete-p
   
   ;; Constants
   #:+ssl-filetype-pem+
   #:+ssl-filetype-asn1+
   #:+ssl-verify-none+
   #:+ssl-verify-peer+
   #:+ssl-verify-fail-if-no-peer-cert+
   #:+ssl-verify-client-once+
   #:+ssl-error-none+
   #:+ssl-error-ssl+
   #:+ssl-error-want-read+
   #:+ssl-error-want-write+
   #:+ssl-error-want-x509-lookup+
   #:+ssl-error-syscall+
   #:+ssl-error-zero-return+
   #:+ssl-error-want-connect+
   #:+ssl-error-want-accept+
   #:+evp-pkey-rsa+
   #:+evp-pkey-ec+
   #:+evp-pkey-ed25519+
   #:+evp-pkey-x25519+
   #:+rsa-f4+))

(in-package :epsilon.crypto)

;;;; Constants

;; TLS/SSL Constants
(defconstant +tls-verify-none+ 0)
(defconstant +tls-verify-peer+ 1)

(defconstant +ssl-filetype-pem+ 1)
(defconstant +ssl-filetype-asn1+ 2)

(defconstant +ssl-verify-none+ 0)
(defconstant +ssl-verify-peer+ 1)
(defconstant +ssl-verify-fail-if-no-peer-cert+ 2)
(defconstant +ssl-verify-client-once+ 4)

(defconstant +ssl-error-none+ 0)
(defconstant +ssl-error-ssl+ 1)
(defconstant +ssl-error-want-read+ 2)
(defconstant +ssl-error-want-write+ 3)
(defconstant +ssl-error-want-x509-lookup+ 4)
(defconstant +ssl-error-syscall+ 5)
(defconstant +ssl-error-zero-return+ 6)
(defconstant +ssl-error-want-connect+ 7)
(defconstant +ssl-error-want-accept+ 8)

;; Crypto Constants
(defconstant +evp-pkey-rsa+ 6)
(defconstant +evp-pkey-ec+ 408)
(defconstant +evp-pkey-ed25519+ 1087)
(defconstant +evp-pkey-x25519+ 1034)

(defconstant +rsa-f4+ 65537)

(defparameter +digest-sha256+ "SHA256")
(defparameter +digest-sha384+ "SHA384")
(defparameter +digest-sha512+ "SHA512")
(defparameter +digest-sha3-256+ "SHA3-256")
(defparameter +digest-sha3-512+ "SHA3-512")

;;;; Data Structures

;; TLS Context Structure
(defstruct tls-context
  "TLS context for managing certificates and settings"
  (server-p nil :type boolean)
  (cert-file nil :type (or null string))
  (key-file nil :type (or null string))
  (verify-mode +tls-verify-peer+ :type integer)
  (cipher-list nil :type (or null string))
  (handle nil :type (or null sb-sys:system-area-pointer)))

;; TLS Connection Structure
(defstruct tls-connection
  "TLS connection wrapper around a socket"
  (socket nil)
  (context nil :type (or null tls-context))
  (connected-p nil :type boolean)
  (handshake-complete-p nil :type boolean)
  (ssl-handle nil :type (or null sb-sys:system-area-pointer)))

;; Crypto Key Structure
(defstruct crypto-key
  "Represents a cryptographic key (public or private)"
  (handle nil :type (or null sb-sys:system-area-pointer))
  (type nil :type (or null keyword))
  (bits 0 :type integer)
  (public-p nil :type boolean)
  (private-p nil :type boolean))

;; X.509 Certificate Structure
(defstruct x509-certificate
  "X.509 certificate structure"
  (handle nil :type (or null sb-sys:system-area-pointer))
  (subject nil :type (or null string))
  (issuer nil :type (or null string))
  (serial nil :type (or null string))
  (not-before nil :type (or null integer))
  (not-after nil :type (or null integer)))

;; OpenSSL Implementation Structures
(defstruct openssl-context
  "OpenSSL-specific TLS context"
  (handle nil :type (or null sb-sys:system-area-pointer))
  (server-p nil :type boolean)
  (cert-file nil :type (or null string))
  (key-file nil :type (or null string))
  (verify-mode 0 :type integer))

(defstruct openssl-connection
  "OpenSSL-specific TLS connection"
  (ssl nil :type (or null sb-sys:system-area-pointer))
  (socket nil)
  (context nil :type (or null openssl-context))
  (connected-p nil :type boolean))

;;;; Error Handling

(define-condition crypto-error (error)
  ((code :initarg :code :reader crypto-error-code)
   (message :initarg :message :reader crypto-error-string))
  (:report (lambda (condition stream)
             (format stream "Crypto error ~A: ~A"
                     (crypto-error-code condition)
                     (crypto-error-string condition)))))
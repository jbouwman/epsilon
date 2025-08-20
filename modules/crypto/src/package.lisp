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
  "TLS context for managing certificates, verification settings, and cipher configuration.
   
   A TLS context encapsulates the configuration needed for TLS/SSL connections,
   including certificates, private keys, peer verification requirements, and
   allowed cipher suites. Contexts can be configured for either client or server use.
   
   Security Notes:
   - Always use +TLS-VERIFY-PEER+ in production for proper certificate validation
   - Consider cipher-list restrictions to enforce strong cryptographic algorithms
   - Private key files should have restricted file permissions (0600)"
  (server-p nil 
            :type boolean
            :documentation "Whether this context is configured for server-side TLS")
  (cert-file nil 
             :type (or null string)
             :documentation "Path to X.509 certificate file in PEM format")
  (key-file nil 
            :type (or null string)
            :documentation "Path to private key file corresponding to cert-file")
  (verify-mode +tls-verify-peer+ 
               :type integer
               :documentation "Certificate verification mode (see +TLS-VERIFY-*+ constants)")
  (cipher-list nil 
               :type (or null string)
               :documentation "OpenSSL cipher list string to restrict allowed algorithms")
  (handle nil 
          :type (or null sb-sys:system-area-pointer)
          :documentation "Internal OpenSSL SSL_CTX pointer (implementation detail)"))

;; TLS Connection Structure
(defstruct tls-connection
  "Active TLS connection state wrapping an underlying network socket.
   
   Represents an established or in-progress TLS connection. The connection
   tracks the underlying socket, associated TLS context, handshake state,
   and internal OpenSSL connection handle.
   
   Lifecycle:
   1. Create connection with socket and context
   2. Perform TLS handshake (sets handshake-complete-p to t)
   3. Exchange application data
   4. Close connection (invalidates ssl-handle)"
  (socket nil 
          :type t
          :documentation "Underlying network socket (from epsilon.net)")
  (context nil 
           :type (or null tls-context)
           :documentation "TLS context containing certificates and configuration")
  (connected-p nil 
               :type boolean
               :documentation "Whether the underlying socket connection is active")
  (handshake-complete-p nil 
                        :type boolean
                        :documentation "Whether the TLS handshake has completed successfully")
  (ssl-handle nil 
              :type (or null sb-sys:system-area-pointer)
              :documentation "Internal OpenSSL SSL pointer (implementation detail)"))

;; Crypto Key Structure
(defstruct crypto-key
  "Cryptographic key for public key operations (RSA, EC, Ed25519).
   
   Represents a public key, private key, or key pair for cryptographic operations
   including digital signatures, encryption/decryption, and key exchange. Keys
   may be generated, loaded from files, or derived from other keys.
   
   Supported key types:
   - :RSA - RSA keys (2048, 3072, 4096 bits recommended)
   - :EC - Elliptic Curve keys (P-256, P-384, P-521, secp256k1)
   - :ED25519 - Edwards curve keys (signing only, 32 bytes)
   - :X25519 - Montgomery curve keys (ECDH only, 32 bytes)
   
   Security Notes:
   - Private key material is stored in OpenSSL's secure memory when possible
   - Keys should be freed promptly after use to minimize exposure
   - Use appropriate key sizes: RSA ≥2048 bits, EC curves ≥256 bits"
  (handle nil 
          :type (or null sb-sys:system-area-pointer)
          :documentation "Internal OpenSSL EVP_PKEY pointer (implementation detail)")
  (type nil 
        :type (or null keyword)
        :documentation "Key type: :RSA, :EC, :ED25519, or :X25519")
  (bits 0 
        :type (integer 0 *)
        :documentation "Key size in bits (RSA) or curve size (EC)")
  (public-p nil 
            :type boolean
            :documentation "Whether this key contains public key material")
  (private-p nil 
             :type boolean
             :documentation "Whether this key contains private key material"))

;; X.509 Certificate Structure
(defstruct x509-certificate
  "X.509 digital certificate for public key infrastructure (PKI).
   
   Represents an X.509 certificate containing a public key, identity information,
   validity period, and digital signature from a Certificate Authority (CA).
   Certificates are used for authentication, encryption, and establishing trust
   in TLS connections and other cryptographic protocols.
   
   Certificate Components:
   - Subject: Entity the certificate identifies (CN, O, OU, C fields)
   - Issuer: Certificate Authority that signed this certificate  
   - Serial: Unique identifier within the issuer's namespace
   - Validity: Time period during which certificate is valid
   - Public Key: Cryptographic public key for the subject
   - Signature: CA's digital signature over certificate contents
   
   Security Notes:
   - Always verify certificate chain back to trusted root CA
   - Check validity dates and revocation status before trusting
   - Validate subject name matches expected identity"
  (handle nil 
          :type (or null sb-sys:system-area-pointer)
          :documentation "Internal OpenSSL X509 pointer (implementation detail)")
  (subject nil 
           :type (or null string)
           :documentation "Certificate subject DN (e.g., '/CN=example.com/O=Example Corp')")
  (issuer nil 
          :type (or null string)
          :documentation "Issuing CA's subject DN")
  (serial nil 
          :type (or null string)
          :documentation "Certificate serial number as string")
  (not-before nil 
              :type (or null integer)
              :documentation "Certificate validity start time (Unix timestamp)")
  (not-after nil 
             :type (or null integer)
             :documentation "Certificate validity end time (Unix timestamp)"))

;; OpenSSL Implementation Structures
(defstruct openssl-context
  "Low-level OpenSSL SSL_CTX wrapper for advanced TLS configuration.
   
   Provides direct access to OpenSSL's SSL context functionality for scenarios
   requiring fine-grained control over TLS parameters, certificate chain
   handling, or advanced SSL features not exposed by the high-level API.
   
   Use the high-level TLS-CONTEXT structure for most applications.
   This structure is intended for advanced use cases requiring specific
   OpenSSL features or compatibility with existing OpenSSL-based code."
  (handle nil 
          :type (or null sb-sys:system-area-pointer)
          :documentation "OpenSSL SSL_CTX pointer")
  (server-p nil 
            :type boolean
            :documentation "Whether this context is for server-side connections")
  (cert-file nil 
             :type (or null string)
             :documentation "Path to certificate file")
  (key-file nil 
            :type (or null string)
            :documentation "Path to private key file")
  (verify-mode 0 
               :type (integer 0 *)
               :documentation "SSL verification mode bitmask"))

(defstruct openssl-connection
  "Low-level OpenSSL SSL connection wrapper.
   
   Provides direct access to OpenSSL's SSL connection object for advanced
   operations. Use the high-level TLS-CONNECTION structure for most applications."
  (ssl nil 
       :type (or null sb-sys:system-area-pointer)
       :documentation "OpenSSL SSL pointer")
  (socket nil
          :type t
          :documentation "Underlying network socket")
  (context nil 
           :type (or null openssl-context)
           :documentation "Associated OpenSSL context")
  (connected-p nil 
               :type boolean
               :documentation "Whether the SSL connection is established"))

;;;; Error Handling

(define-condition crypto-error (error)
  "Cryptographic error condition signaled when cryptographic operations fail.
   
   This condition is used for all cryptographic errors including:
   - Key generation failures
   - Certificate operations failures
   - TLS/SSL connection errors
   - Signature/verification failures
   - Encryption/decryption errors
   
   The error includes both a numeric code (from OpenSSL) and a human-readable message."
  ((code :initarg :code :reader crypto-error-code 
         :type integer
         :documentation "Numeric error code from underlying cryptographic library")
   (message :initarg :message :reader crypto-error-string
            :type string
            :documentation "Human-readable error description"))
  (:report (lambda (condition stream)
             (format stream "Crypto error ~A: ~A"
                     (crypto-error-code condition)
                     (crypto-error-string condition)))))

;;;; Utility Functions

(defun get-crypto-errors ()
  "Retrieve and clear all pending cryptographic errors from OpenSSL.
   
   Returns a list of error descriptions as strings. This function is useful
   for debugging cryptographic operations that may have accumulated multiple
   errors in the OpenSSL error stack.
   
   Returns:
     List of strings describing any pending OpenSSL errors.
   
   Security Note:
     Error messages may contain sensitive information. Avoid logging them
     in production environments where logs might be accessible to unauthorized users."
  (declare (values list))
  (loop with errors = '()
        for error-code = (sb-alien:alien-funcall 
                          (sb-alien:extern-alien "ERR_get_error" 
                                                (function sb-alien:unsigned-long)))
        while (not (zerop error-code))
        do (let ((error-string (sb-alien:alien-funcall
                                (sb-alien:extern-alien "ERR_error_string"
                                                      (function sb-alien:c-string 
                                                               sb-alien:unsigned-long
                                                               sb-sys:system-area-pointer))
                                error-code
                                (sb-sys:int-sap 0))))
             (push error-string errors))
        finally (return (reverse errors))))
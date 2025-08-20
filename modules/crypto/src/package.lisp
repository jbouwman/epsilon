;;;; Package Definition for epsilon.crypto
;;;;
;;;; This file defines the main package with core types and constants

(defpackage :epsilon.crypto
  (:use :cl)
  (:local-nicknames
   (#:map #:epsilon.map)
   (#:stream #:epsilon.stream))
  (:import-from :epsilon.crypto.ffi
                #:crypto-error
                #:crypto-error-code
                #:crypto-error-string)
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
   
   ;; Key Derivation Functions (KDF)
   #:pbkdf2
   #:hkdf
   #:scrypt
   
   ;; BLAKE2 Hash Functions
   #:blake2b
   #:blake2s
   
   ;; Authenticated Encryption (AEAD)
   #:aes-gcm-encrypt
   #:aes-gcm-decrypt
   #:chacha20-poly1305-encrypt
   #:chacha20-poly1305-decrypt
   
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
  (server-p nil :type boolean)
  (cert-file nil :type (or null string))
  (key-file nil :type (or null string))
  (verify-mode +tls-verify-peer+ :type integer)
  (cipher-list nil :type (or null string))
  (handle nil :type (or null sb-sys:system-area-pointer)))

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
  (socket nil :type t)
  (context nil :type (or null tls-context))
  (connected-p nil :type boolean)
  (handshake-complete-p nil :type boolean)
  (ssl-handle nil :type (or null sb-sys:system-area-pointer)))

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
  (handle nil :type (or null sb-sys:system-area-pointer))
  (type nil :type (or null keyword))
  (bits 0 :type (integer 0 *))
  (public-p nil :type boolean)
  (private-p nil :type boolean))

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
  (handle nil :type (or null sb-sys:system-area-pointer))
  (subject nil :type (or null string))
  (issuer nil :type (or null string))
  (serial nil :type (or null string))
  (not-before nil :type (or null integer))
  (not-after nil :type (or null integer)))

;; OpenSSL Implementation Structures
(defstruct openssl-context
  "Low-level OpenSSL SSL_CTX wrapper for advanced TLS configuration.
   
   Provides direct access to OpenSSL's SSL context functionality for scenarios
   requiring fine-grained control over TLS parameters, certificate chain
   handling, or advanced SSL features not exposed by the high-level API.
   
   Use the high-level TLS-CONTEXT structure for most applications.
   This structure is intended for advanced use cases requiring specific
   OpenSSL features or compatibility with existing OpenSSL-based code."
  (handle nil :type (or null sb-sys:system-area-pointer))
  (server-p nil :type boolean)
  (cert-file nil :type (or null string))
  (key-file nil :type (or null string))
  (verify-mode 0 :type (integer 0 *)))

(defstruct openssl-connection
  "Low-level OpenSSL SSL connection wrapper.
   
   Provides direct access to OpenSSL's SSL connection object for advanced
   operations. Use the high-level TLS-CONNECTION structure for most applications."
  (ssl nil :type (or null sb-sys:system-area-pointer))
  (socket nil :type t)
  (context nil :type (or null openssl-context))
  (connected-p nil :type boolean))

;;;; Error Handling

;; crypto-error is imported from epsilon.crypto.ffi in the defpackage

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

;;;; Random Number Generation (Public API)

(defun crypto-random-bytes (n)
  "Generate cryptographically secure random bytes.
   
   Uses OpenSSL's RAND_bytes function to generate cryptographically
   secure random data suitable for keys, IVs, nonces, and other
   security-critical values.
   
   Parameters:
     n (integer): Number of random bytes to generate
   
   Returns:
     Byte vector of length n containing random data
   
   Security Notes:
     - Uses system entropy sources (e.g., /dev/urandom)
     - Automatically reseeds from system entropy
     - Thread-safe and fork-safe
     - Suitable for all cryptographic purposes
   
   Example - Generate a 256-bit key:
     (crypto-random-bytes 32)  ; Returns 32 random bytes
   
   Example - Generate a random IV:
     (crypto-random-bytes 16)  ; For AES block size
   
   Errors:
     Signals CRYPTO-ERROR if random generation fails"
  (declare (type (integer 1 *) n))
  (let ((buffer (make-array n :element-type '(unsigned-byte 8))))
    (sb-sys:with-pinned-objects (buffer)
      (let ((result (epsilon.crypto.ffi:%rand-bytes (sb-sys:vector-sap buffer) n)))
        (when (zerop result)
          (error 'crypto-error :code (epsilon.crypto.ffi:%err-get-error)
                 :message "Failed to generate random bytes"))))
    buffer))

(defun crypto-random-integer (max)
  "Generate random integer from 0 to max-1.
   
   Uses cryptographically secure random number generation
   to produce uniformly distributed integers.
   
   Parameters:
     max (integer): Upper bound (exclusive)
   
   Returns:
     Random integer in range [0, max)
   
   Example:
     (crypto-random-integer 100)  ; Returns 0-99"
  (declare (type (integer 1 *) max))
  ;; Special case for max=1
  (when (= max 1)
    (return-from crypto-random-integer 0))
  (let* ((bytes-needed (max 1 (ceiling (log max 256))))
         (bytes (crypto-random-bytes bytes-needed))
         (value 0))
    (loop for i from 0 below bytes-needed
          do (setf value (+ (* value 256) (aref bytes i))))
    (mod value max)))
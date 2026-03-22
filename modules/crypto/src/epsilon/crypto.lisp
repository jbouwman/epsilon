;;;; Epsilon Crypto Module - Main Entry Point
;;;;
;;;; This package provides the unified cryptography API. Pure crypto functions
;;;; (hashing, HMAC, KDF, AEAD, random) are backed by epsilon.ssl (pure Lisp).
;;;; TLS operations delegate directly to epsilon.crypto.native, which bridges
;;;; to the pure-Lisp TLS 1.3 implementation.
;;;;
;;;; PKI operations (key generation, signing, PEM import/export) are backed by
;;;; the native pure-Lisp implementation (epsilon.crypto.native).
;;;;
;;;; Example:
;;;;   (import (epsilon.crypto crypto))
;;;;   (crypto:sha256 data)
;;;;   (crypto:hmac-sha256 key data)
;;;;   (let ((ctx (crypto:make-client-context)))
;;;;     (crypto:tls-connect socket ctx :hostname "example.com"))

(defpackage epsilon.crypto
  (:use :cl)
  (:require (epsilon.symbol sym)
            (epsilon.crypto.errors err)
            (epsilon.ssl ssl)
            (epsilon.crypto.native native)
            (epsilon.crypto.age age)
            (epsilon.crypto.jwt jwt)
            (epsilon.crypto.jwk jwk)
            (epsilon.crypto.totp totp)
            (epsilon.digest digest-mod)
            (epsilon.base-encode b64))
  (:export ;; Error conditions
           crypto-error
           crypto-error-p
           crypto-error-code
           crypto-error-message
           tls-error
           tls-error-p
           certificate-error
           certificate-error-p
           key-error
           key-error-p
           signal-crypto-error
           ;; Random (via epsilon.ssl)
           random-bytes
           random-integer
           crypto-random-bytes
           crypto-random-hex
           crypto-random-base64
           ;; Hashing (via epsilon.ssl)
           sha256
           sha256-hex
           sha384
           sha384-hex
           sha512
           sha512-hex
           sha3-256
           sha3-384
           sha3-512
           md5
           sha1
           blake2b
           blake2s
           ;; Multi-algorithm digest
           digest
           ;; HMAC (via epsilon.ssl)
           hmac
           hmac-sha256
           hmac-sha256-verify
           hmac-sha384
           hmac-sha512
           ;; HKDF (via epsilon.ssl)
           hkdf
           hkdf-extract
           hkdf-expand
           ;; PBKDF2 (via epsilon.ssl)
           pbkdf2
           pbkdf2-sha256
           verify-pbkdf2-password
           ;; Scrypt (via epsilon.ssl)
           scrypt
           ;; AEAD (via epsilon.ssl)
           aes-gcm-encrypt
           aes-gcm-decrypt
           chacha20-poly1305-encrypt
           chacha20-poly1305-decrypt
           ;; X25519 (via epsilon.ssl)
           x25519
           x25519-base
           ;; TLS Context
           make-client-context
           make-server-context
           create-tls-context
           context-set-certificate
           context-set-private-key
           context-check-private-key
           context-set-certificate-chain
           context-set-ca-file
           context-set-ca-path
           context-set-verify-mode
           context-set-verify-depth
           context-set-min-version
           context-set-max-version
           context-set-ciphersuites
           context-set-cipher-list
           context-set-session-cache-mode
           context-set-client-ca-list
           context-set-alpn-protocols
           free-tls-context
           +tls-1.0+
           +tls-1.1+
           +tls-1.2+
           +tls-1.3+
           +verify-none+
           +verify-peer+
           +verify-fail-if-no-peer-cert+
           +verify-client-once+
           +session-cache-off+
           +session-cache-client+
           +session-cache-server+
           +session-cache-both+
           ;; TLS Connection
           tls-connection-socket
           tls-connection-p
           tls-connection-connected-p
           tls-connection-handshake-complete-p
           tls-connect
           tls-accept
           tls-read
           tls-write
           tls-shutdown
           tls-close
           connection-peer-certificate
           connection-cipher
           connection-version
           connection-alpn-protocol
           connection-pending
           verify-peer-certificate
           get-verify-result
           +ssl-error-none+
           +ssl-error-ssl+
           +ssl-error-want-read+
           +ssl-error-want-write+
           +ssl-error-syscall+
           +ssl-error-zero-return+
           ;; TLS Stream
           tls-stream-p
           make-tls-stream
           tls-stream-connection
           tls-stream-peer-certificate
           tls-stream-cipher
           tls-stream-version
           tls-stream-alpn-protocol
           tls-connect-stream
           tls-accept-stream
           wrap-socket-with-tls
           tls-read-line
           tls-write-line
           tls-write-string
           ;; ALPN
           +alpn-http/1.0+
           +alpn-http/1.1+
           +alpn-h2+
           +alpn-h2c+
           +alpn-grpc+
           connection-set-alpn-protocols
           connection-selected-alpn
           make-alpn-buffer
           parse-alpn-buffer
           ;; PKI
           native-key
           native-key-p
           native-key-type
           native-key-private-p
           native-key-material
           generate-ed25519-key
           generate-ec-p256-key
           generate-rsa-key
           key-to-pem
           key-from-pem
           sign-message
           verify-message
           ;; mTLS
           make-mtls-server-context
           make-mtls-client-context
           match-certificate-hostname
           ;; JWT
           jwt-encode
           jwt-decode
           jwt-decode-unsafe
           jwt-error
           jwt-error-p
           jwt-error-message
           jwt-expired-error
           jwt-expired-error-p
           jwt-invalid-signature-error
           jwt-invalid-signature-error-p
           jwt-malformed-error
           jwt-malformed-error-p
           ;; JWK (RFC 7517)
           key-to-jwk
           key-from-jwk
           keys-to-jwks
           keys-from-jwks
           jwk-to-json
           jwks-to-json
           ;; TOTP (RFC 6238)
           generate-totp-secret
           compute-totp
           verify-totp
           totp-provisioning-uri
           ;; Digest protocol (BLAKE3, xxHash, CRC-32)
           hasher-update hasher-finalize hasher-reset hasher-copy
           hasher-algorithm hasher-output-length hasher-block-length
           hasher-p cryptographic-hasher-p keyed-hasher-p
           hash-bytes hash-reader make-hashing-reader make-hashing-writer
           blake3 blake3-keyed blake3-derive-key blake3-xof
           make-blake3-hasher
           xxhash64 xxhash128 xxhash64-bytes xxhash128-bytes
           make-xxhash64-hasher make-xxhash128-hasher
           crc32 crc32-int make-crc32-hasher
           hashing-reader hashing-writer hash-reader-blake3
           make-hasher available-algorithms bytes-to-hex hex-to-bytes
           +blake3-output-length+ +xxhash64-output-length+
           +md5-output-length+ +sha1-output-length+ +sha256-output-length+
           +sha384-output-length+ +sha512-output-length+ +crc32-output-length+
           ;; Convenience wrappers
           generate-x25519-key
           x25519-private-key-bytes
           x25519-public-key-bytes
           generate-aes-key
           generate-nonce
           hex-digest
           digest-file
           +sha256+ +sha384+ +sha512+ +sha1+ +md5+
           ;; Age decryption
           bech32-decode
           decode-age-identity
           parse-age-file
           age-header
           age-header-p
           age-header-recipients
           age-header-mac
           age-header-payload
           age-recipient
           age-recipient-type
           age-recipient-args
           age-recipient-body
           decrypt-age-file
           decrypt-age-x25519
           age-error
           age-error-message)
  (:enter t))

;;;; ============================================================================
;;;; Error Conditions (from epsilon.crypto.errors)
;;;; ============================================================================

(sym:reexport :epsilon.crypto.errors
  '(crypto-error-p crypto-error-code crypto-error-message
    tls-error-p certificate-error-p key-error-p
    signal-crypto-error))

(deftype crypto-error () 'err:crypto-error)
(deftype tls-error () 'err:tls-error)
(deftype certificate-error () 'err:certificate-error)
(deftype key-error () 'err:key-error)

;;;; ============================================================================
;;;; Pure Crypto Functions (from epsilon.ssl)
;;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; Random Number Generation
;;; ---------------------------------------------------------------------------

(setf (fdefinition 'random-bytes) #'ssl:random-bytes)
(setf (fdefinition 'random-integer) #'ssl:random-integer)

(defun crypto-random-bytes (n)
  "Generate N cryptographically secure random bytes.
Delegates to epsilon.ssl:random-bytes."
  (ssl:random-bytes n))

(defun crypto-random-hex (size)
  "Generate SIZE random bytes as a hex string."
  (let ((bytes (ssl:random-bytes size)))
    (with-output-to-string (s)
      (loop for b across bytes
            do (format s "~2,'0x" b)))))

(defun crypto-random-base64 (size)
  "Generate SIZE random bytes as a base64 string."
  (b64:base64-encode (ssl:random-bytes size)))

;;; ---------------------------------------------------------------------------
;;; Hashing
;;; ---------------------------------------------------------------------------

(setf (fdefinition 'sha256) #'ssl:sha256)
(setf (fdefinition 'sha256-hex) #'ssl:sha256-hex)
(setf (fdefinition 'sha384) #'ssl:sha384)
(setf (fdefinition 'sha384-hex) #'ssl:sha384-hex)
(setf (fdefinition 'sha512) #'ssl:sha512)
(setf (fdefinition 'sha512-hex) #'ssl:sha512-hex)
(setf (fdefinition 'sha3-256) #'ssl:sha3-256)
(setf (fdefinition 'sha3-384) #'ssl:sha3-384)
(setf (fdefinition 'sha3-512) #'ssl:sha3-512)
(setf (fdefinition 'md5) #'ssl:md5)
(setf (fdefinition 'sha1) #'ssl:sha1)
(setf (fdefinition 'blake2b) #'ssl:blake2b)
(setf (fdefinition 'blake2s) #'ssl:blake2s)

(defun digest (algorithm data)
  "Compute a message digest using the specified algorithm.
ALGORITHM is a keyword: :sha256, :sha384, :sha512, :sha3-256, :sha3-384,
:sha3-512, :md5, :sha1, :blake2b, :blake2s.
DATA is a byte vector or string."
  (let ((bytes (if (stringp data)
                   (sb-ext:string-to-octets data :external-format :utf-8)
                   data)))
    (ecase algorithm
      (:sha256 (ssl:sha256 bytes))
      (:sha384 (ssl:sha384 bytes))
      (:sha512 (ssl:sha512 bytes))
      (:sha3-256 (ssl:sha3-256 bytes))
      (:sha3-384 (ssl:sha3-384 bytes))
      (:sha3-512 (ssl:sha3-512 bytes))
      (:md5 (ssl:md5 bytes))
      (:sha1 (ssl:sha1 bytes))
      (:blake2b (ssl:blake2b bytes))
      (:blake2s (ssl:blake2s bytes)))))

;;; ---------------------------------------------------------------------------
;;; HMAC
;;; ---------------------------------------------------------------------------

(setf (fdefinition 'hmac) #'ssl:hmac)
(setf (fdefinition 'hmac-sha256) #'ssl:hmac-sha256)
(setf (fdefinition 'hmac-sha256-verify) #'ssl:hmac-sha256-verify)
(setf (fdefinition 'hmac-sha384) #'ssl:hmac-sha384)
(setf (fdefinition 'hmac-sha512) #'ssl:hmac-sha512)

;;; ---------------------------------------------------------------------------
;;; HKDF
;;; ---------------------------------------------------------------------------

(setf (fdefinition 'hkdf) #'ssl:hkdf)
(setf (fdefinition 'hkdf-extract) #'ssl:hkdf-extract)
(setf (fdefinition 'hkdf-expand) #'ssl:hkdf-expand)

;;; ---------------------------------------------------------------------------
;;; PBKDF2
;;; ---------------------------------------------------------------------------

(setf (fdefinition 'pbkdf2) #'ssl:pbkdf2)

(defun pbkdf2-sha256 (password salt &key (iterations 200000) (key-length 32))
  "PBKDF2 with SHA-256. PASSWORD and SALT can be strings or byte vectors."
  (let ((pw-bytes (etypecase password
                    ((simple-array (unsigned-byte 8) (*)) password)
                    (string (sb-ext:string-to-octets password :external-format :utf-8))))
        (salt-bytes (etypecase salt
                      ((simple-array (unsigned-byte 8) (*)) salt)
                      (string (sb-ext:string-to-octets salt :external-format :utf-8)))))
    (ssl:pbkdf2 :sha256 pw-bytes salt-bytes iterations key-length)))

(defun verify-pbkdf2-password (password salt stored-hash &key (iterations 200000))
  "Verify a password against a stored PBKDF2-SHA256 hash using constant-time comparison."
  (let ((computed (pbkdf2-sha256 password salt
                                 :iterations iterations
                                 :key-length (length stored-hash))))
    (and (= (length computed) (length stored-hash))
         (loop for a across computed
               for b across stored-hash
               with result = 0
               do (setf result (logior result (logxor a b)))
               finally (return (zerop result))))))

;;; ---------------------------------------------------------------------------
;;; Scrypt
;;; ---------------------------------------------------------------------------

(setf (fdefinition 'scrypt) #'ssl:scrypt)

;;; ---------------------------------------------------------------------------
;;; AEAD
;;; ---------------------------------------------------------------------------

(setf (fdefinition 'aes-gcm-encrypt) #'ssl:aes-gcm-encrypt)
(setf (fdefinition 'aes-gcm-decrypt) #'ssl:aes-gcm-decrypt)
(setf (fdefinition 'chacha20-poly1305-encrypt) #'ssl:chacha20-poly1305-encrypt)
(setf (fdefinition 'chacha20-poly1305-decrypt) #'ssl:chacha20-poly1305-decrypt)

;;; ---------------------------------------------------------------------------
;;; X25519
;;; ---------------------------------------------------------------------------

(setf (fdefinition 'x25519) #'ssl:x25519)
(setf (fdefinition 'x25519-base) #'ssl:x25519-base)

;;; ---------------------------------------------------------------------------
;;; Convenience Wrappers
;;; ---------------------------------------------------------------------------

(defstruct (x25519-keypair (:constructor %make-x25519-keypair (private public)))
  (private nil :type (simple-array (unsigned-byte 8) (32)))
  (public nil :type (simple-array (unsigned-byte 8) (32))))

(defun generate-x25519-key ()
  "Generate an X25519 key pair."
  (let* ((priv (ssl:random-bytes 32))
         (pub (ssl:x25519-base priv)))
    (%make-x25519-keypair priv pub)))

(defun x25519-private-key-bytes (keypair)
  "Get the 32-byte private key from a keypair."
  (x25519-keypair-private keypair))

(defun x25519-public-key-bytes (keypair)
  "Get the 32-byte public key from a keypair."
  (x25519-keypair-public keypair))

(defun generate-aes-key (&key (bits 256))
  "Generate a random AES key of BITS length (128 or 256)."
  (ssl:random-bytes (/ bits 8)))

(defun generate-nonce (&key (size 12))
  "Generate a random nonce of SIZE bytes."
  (ssl:random-bytes size))

(defun hex-digest (data algorithm)
  "Compute hex-encoded digest of DATA using ALGORITHM."
  (let ((hash (digest algorithm data)))
    (with-output-to-string (s)
      (loop for b across hash do (format s "~(~2,'0x~)" b)))))

(defun digest-file (pathname algorithm)
  "Compute a message digest of the file at PATHNAME using ALGORITHM."
  (let ((bytes (with-open-file (stream pathname :element-type '(unsigned-byte 8))
                (let ((buf (make-array (file-length stream) :element-type '(unsigned-byte 8))))
                  (read-sequence buf stream)
                  buf))))
    (digest algorithm bytes)))

(define-symbol-macro +sha256+ :sha256)
(define-symbol-macro +sha384+ :sha384)
(define-symbol-macro +sha512+ :sha512)
(define-symbol-macro +sha1+ :sha1)
(define-symbol-macro +md5+ :md5)

;;;; ============================================================================
;;;; Age Decryption
;;;; ============================================================================

(sym:reexport :epsilon.crypto.age
  '(bech32-decode decode-age-identity
    parse-age-file age-header-p age-header-recipients age-header-mac age-header-payload
    age-recipient-type age-recipient-args age-recipient-body
    decrypt-age-file decrypt-age-x25519
    age-error-message))

(deftype age-header () 'age:age-header)
(deftype age-recipient () 'age:age-recipient)
(deftype age-error () 'age:age-error)

;;;; ============================================================================
;;;; JWT (JSON Web Tokens)
;;;; ============================================================================

(sym:reexport :epsilon.crypto.jwt
  '(jwt-encode jwt-decode jwt-decode-unsafe
    jwt-error-p jwt-error-message
    jwt-expired-error-p jwt-invalid-signature-error-p jwt-malformed-error-p))

(deftype jwt-error () 'jwt:jwt-error)
(deftype jwt-expired-error () 'jwt:jwt-expired-error)
(deftype jwt-invalid-signature-error () 'jwt:jwt-invalid-signature-error)
(deftype jwt-malformed-error () 'jwt:jwt-malformed-error)

;;;; ============================================================================
;;;; Digest Module (BLAKE3, xxHash3, CRC-32, IO integration)
;;;; ============================================================================

(sym:reexport :epsilon.digest
  '(;; Protocol
    hasher-update hasher-finalize hasher-reset hasher-copy
    hasher-algorithm hasher-output-length hasher-block-length
    hasher-p cryptographic-hasher-p keyed-hasher-p
    hash-bytes hash-reader make-hashing-reader make-hashing-writer
    ;; BLAKE3
    blake3 blake3-keyed blake3-derive-key blake3-xof
    make-blake3-hasher
    ;; xxHash3
    xxhash64 xxhash128 xxhash64-bytes xxhash128-bytes
    make-xxhash64-hasher make-xxhash128-hasher
    ;; CRC-32
    crc32 crc32-int make-crc32-hasher
    ;; IO integration
    hashing-reader hashing-writer hash-reader-blake3
    ;; Utilities
    make-hasher available-algorithms bytes-to-hex hex-to-bytes
    ;; Constants
    +blake3-output-length+ +xxhash64-output-length+
    +md5-output-length+ +sha1-output-length+ +sha256-output-length+
    +sha384-output-length+ +sha512-output-length+ +crc32-output-length+))

;;;; ============================================================================
;;;; TLS Constants (from native bridge)
;;;; ============================================================================

(define-symbol-macro +tls-1.0+ native:+tls-1.0+)
(define-symbol-macro +tls-1.1+ native:+tls-1.1+)
(define-symbol-macro +tls-1.2+ native:+tls-1.2+)
(define-symbol-macro +tls-1.3+ native:+tls-1.3+)
(define-symbol-macro +verify-none+ native:+verify-none+)
(define-symbol-macro +verify-peer+ native:+verify-peer+)
(define-symbol-macro +verify-fail-if-no-peer-cert+ native:+verify-fail-if-no-peer-cert+)
(define-symbol-macro +verify-client-once+ native:+verify-client-once+)
(define-symbol-macro +session-cache-off+ native:+session-cache-off+)
(define-symbol-macro +session-cache-client+ native:+session-cache-client+)
(define-symbol-macro +session-cache-server+ native:+session-cache-server+)
(define-symbol-macro +session-cache-both+ native:+session-cache-both+)
(define-symbol-macro +ssl-error-none+ native:+ssl-error-none+)
(define-symbol-macro +ssl-error-ssl+ native:+ssl-error-ssl+)
(define-symbol-macro +ssl-error-want-read+ native:+ssl-error-want-read+)
(define-symbol-macro +ssl-error-want-write+ native:+ssl-error-want-write+)
(define-symbol-macro +ssl-error-syscall+ native:+ssl-error-syscall+)
(define-symbol-macro +ssl-error-zero-return+ native:+ssl-error-zero-return+)
(define-symbol-macro +alpn-http/1.0+ native:+alpn-http/1.0+)
(define-symbol-macro +alpn-http/1.1+ native:+alpn-http/1.1+)
(define-symbol-macro +alpn-h2+ native:+alpn-h2+)
(define-symbol-macro +alpn-h2c+ native:+alpn-h2c+)
(define-symbol-macro +alpn-grpc+ native:+alpn-grpc+)

;;;; ============================================================================
;;;; TLS Functions (delegating to epsilon.crypto.native)
;;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; TLS Context
;;; ---------------------------------------------------------------------------

(defun make-client-context (&rest args)
  "Create a TLS client context."
  (apply #'native:make-client-context args))

(defun make-server-context (&rest args)
  "Create a TLS server context."
  (apply #'native:make-server-context args))

(defun create-tls-context (&rest args)
  "Create a TLS context. Delegates to make-client-context."
  (apply #'make-client-context args))

(setf (fdefinition 'context-set-certificate) #'native:context-set-certificate)
(setf (fdefinition 'context-set-private-key) #'native:context-set-private-key)
(setf (fdefinition 'context-check-private-key) #'native:context-check-private-key)
(setf (fdefinition 'context-set-certificate-chain) #'native:context-set-certificate-chain)
(setf (fdefinition 'context-set-ca-file) #'native:context-set-ca-file)
(setf (fdefinition 'context-set-ca-path) #'native:context-set-ca-path)
(setf (fdefinition 'context-set-verify-mode) #'native:context-set-verify-mode)
(setf (fdefinition 'context-set-verify-depth) #'native:context-set-verify-depth)
(setf (fdefinition 'context-set-min-version) #'native:context-set-min-version)
(setf (fdefinition 'context-set-max-version) #'native:context-set-max-version)
(setf (fdefinition 'context-set-ciphersuites) #'native:context-set-ciphersuites)
(setf (fdefinition 'context-set-cipher-list) #'native:context-set-cipher-list)
(setf (fdefinition 'context-set-session-cache-mode) #'native:context-set-session-cache-mode)
(setf (fdefinition 'context-set-client-ca-list) #'native:context-set-client-ca-list)
(setf (fdefinition 'context-set-alpn-protocols) #'native:context-set-alpn-protocols)
(setf (fdefinition 'free-tls-context) #'native:free-tls-context)

;;; ---------------------------------------------------------------------------
;;; TLS Connection
;;; ---------------------------------------------------------------------------

(defun tls-connect (socket context &rest args)
  "Establish a TLS connection as a client."
  (apply #'native:tls-connect socket context args))

(defun tls-accept (socket context)
  "Accept a TLS connection as a server."
  (native:tls-accept socket context))

(defun tls-read (connection buffer &rest args)
  "Read data from a TLS connection."
  (apply #'native:tls-read connection buffer args))

(defun tls-write (connection buffer &rest args)
  "Write data to a TLS connection."
  (apply #'native:tls-write connection buffer args))

(setf (fdefinition 'tls-shutdown) #'native:tls-shutdown)
(setf (fdefinition 'tls-close) #'native:tls-close)
(setf (fdefinition 'connection-peer-certificate) #'native:connection-peer-certificate)
(setf (fdefinition 'connection-cipher) #'native:connection-cipher)
(setf (fdefinition 'connection-version) #'native:connection-version)
(setf (fdefinition 'connection-alpn-protocol) #'native:connection-alpn-protocol)
(setf (fdefinition 'connection-pending) #'native:connection-pending)
(setf (fdefinition 'verify-peer-certificate) #'native:verify-peer-certificate)
(setf (fdefinition 'get-verify-result) #'native:get-verify-result)
(setf (fdefinition 'tls-connection-socket) #'native:tls-connection-socket)
(setf (fdefinition 'tls-connection-p) #'native:tls-connection-p)
(setf (fdefinition 'tls-connection-connected-p) #'native:tls-connection-connected-p)
(setf (fdefinition 'tls-connection-handshake-complete-p) #'native:tls-connection-handshake-complete-p)

;;; ---------------------------------------------------------------------------
;;; TLS Stream
;;; ---------------------------------------------------------------------------

(defun make-tls-stream (&rest args)
  (apply #'native:make-tls-stream args))

(setf (fdefinition 'tls-stream-p) #'native:tls-stream-p)

(defun tls-connect-stream (socket context &rest args)
  (apply #'native:tls-connect-stream socket context args))

(defun tls-accept-stream (socket context)
  (native:tls-accept-stream socket context))

(defun wrap-socket-with-tls (socket &rest args)
  (apply #'native:wrap-socket-with-tls socket args))

(defun tls-read-line (tls-strm &rest args)
  (apply #'native:tls-read-line tls-strm args))

(setf (fdefinition 'tls-write-line) #'native:tls-write-line)
(setf (fdefinition 'tls-write-string) #'native:tls-write-string)

(defun tls-stream-connection (tls-strm)
  "Get the connection from a TLS stream."
  (native:tls-stream-connection tls-strm))

(defun tls-stream-peer-certificate (tls-strm)
  (when (native:tls-stream-connection tls-strm)
    (native:connection-peer-certificate (native:tls-stream-connection tls-strm))))

(defun tls-stream-cipher (tls-strm)
  (when (native:tls-stream-connection tls-strm)
    (native:connection-cipher (native:tls-stream-connection tls-strm))))

(defun tls-stream-version (tls-strm)
  (when (native:tls-stream-connection tls-strm)
    (native:connection-version (native:tls-stream-connection tls-strm))))

(defun tls-stream-alpn-protocol (tls-strm)
  (when (native:tls-stream-connection tls-strm)
    (native:connection-alpn-protocol (native:tls-stream-connection tls-strm))))

;;; ---------------------------------------------------------------------------
;;; ALPN helpers (from native bridge)
;;; ---------------------------------------------------------------------------

(sym:reexport :epsilon.crypto.native
  '(connection-set-alpn-protocols connection-selected-alpn
    make-alpn-buffer parse-alpn-buffer))

;;; ---------------------------------------------------------------------------
;;; PKI Functions
;;; ---------------------------------------------------------------------------

(deftype native-key () 'native:native-key)
(setf (fdefinition 'native-key-p) #'native:native-key-p)
(setf (fdefinition 'native-key-type) #'native:native-key-type)
(setf (fdefinition 'native-key-private-p) #'native:native-key-private-p)
(setf (fdefinition 'native-key-material) #'native:native-key-material)
(setf (fdefinition 'generate-ed25519-key) #'native:generate-ed25519-key)
(setf (fdefinition 'generate-ec-p256-key) #'native:generate-ec-p256-key)
(setf (fdefinition 'generate-rsa-key) #'native:generate-rsa-key)
(setf (fdefinition 'key-to-pem) #'native:key-to-pem)

(defun key-from-pem (pem-string &rest args)
  "Import a key from PEM format string."
  (apply #'native:key-from-pem pem-string args))

(defun sign-message (key data &rest args)
  "Sign data with a private key."
  (apply #'native:sign-message key data args))

(defun verify-message (key data signature &rest args)
  "Verify a signature with a public key."
  (apply #'native:verify-message key data signature args))

;;; ---------------------------------------------------------------------------
;;; mTLS Functions
;;; ---------------------------------------------------------------------------

(defun make-mtls-server-context (&rest args)
  (apply #'native:make-mtls-server-context args))

(defun make-mtls-client-context (&rest args)
  (apply #'native:make-mtls-client-context args))

(setf (fdefinition 'match-certificate-hostname) #'native:match-certificate-hostname)

;;;; ============================================================================
;;;; JWK (JSON Web Key - RFC 7517)
;;;; ============================================================================

(sym:reexport :epsilon.crypto.jwk
  '(key-to-jwk key-from-jwk keys-to-jwks keys-from-jwks
    jwk-to-json jwks-to-json))

;;;; ============================================================================
;;;; TOTP (Time-Based One-Time Password - RFC 6238)
;;;; ============================================================================

(sym:reexport :epsilon.crypto.totp
  '(generate-totp-secret compute-totp verify-totp totp-provisioning-uri))

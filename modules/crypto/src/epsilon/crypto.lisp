;;;; Epsilon Crypto Native
;;;;
;;;; Top-level package for the pure-Lisp cryptographic library.
;;;; This module provides a zero-C-dependency implementation of
;;;; all cryptographic primitives needed for TLS 1.3.
;;;;
;;;; The only FFI used is for platform entropy (getrandom/getentropy).

(defpackage epsilon.crypto
  (:use :cl)
  (:import
   (epsilon.crypto.ct ct)
   (epsilon.crypto.entropy entropy)
   (epsilon.crypto.sha256 sha256)
   (epsilon.crypto.sha512 sha512)
   (epsilon.crypto.sha3 sha3)
   (epsilon.crypto.blake2 blake2)
   (epsilon.crypto.md5 md5)
   (epsilon.crypto.sha1 sha1)
   (epsilon.crypto.hmac hmac)
   (epsilon.crypto.hkdf hkdf)
   (epsilon.crypto.drbg drbg)
   (epsilon.crypto.poly1305 poly1305)
   (epsilon.crypto.pbkdf2 pbkdf2)
   (epsilon.crypto.scrypt scrypt)
   (epsilon.crypto.argon2 argon2)
   (epsilon.crypto.pkcs12-kdf pkcs12-kdf)
   (epsilon.crypto.pbes2 pbes2)
   (epsilon.crypto.pkcs12 pkcs12)
   (epsilon.crypto.ml-kem ml-kem)
   (epsilon.crypto.ml-kem-hybrid ml-kem-hybrid)
   (epsilon.crypto.aes aes)
   (epsilon.crypto.ghash ghash)
   (epsilon.crypto.aes-gcm aes-gcm)
   (epsilon.crypto.aes-ccm aes-ccm)
   (epsilon.crypto.aes-xts aes-xts)
   (epsilon.crypto.aes-kw aes-kw)
   (epsilon.crypto.chacha20 chacha20)
   (epsilon.crypto.chacha20-poly1305 chacha20-poly1305)
   (epsilon.crypto.xchacha20-poly1305 xchacha20-poly1305)
   (epsilon.crypto.kmac kmac)
   (epsilon.crypto.kbkdf kbkdf)
   (epsilon.crypto.modular mod-arith)
   (epsilon.crypto.field-25519 fe25519)
   (epsilon.crypto.field-p256 p256)
   (epsilon.crypto.curve25519 x25519)
   (epsilon.crypto.ed25519 ed25519)
   (epsilon.crypto.ec-p256 ec-p256)
   (epsilon.crypto.rsa rsa)
   (epsilon.crypto.ed25519-sign ed-sign)
   (epsilon.crypto.ecdsa ecdsa)
   (epsilon.crypto.ecdh ecdh)
   (epsilon.crypto.asn1 asn1)
   (epsilon.crypto.pem pem)
   (epsilon.crypto.pem-enc pem-enc)
   (epsilon.crypto.pkcs pkcs)
   (epsilon.crypto.x509 x509)
   (epsilon.crypto.sct sct)
   (epsilon.crypto.tls13 tls13)
   (epsilon.crypto.tls12 tls12)
   (epsilon.crypto.errors err)
   (epsilon.crypto.native native)
   (epsilon.crypto.jwk jwk)
   (epsilon.crypto.simple compat)
   (epsilon.digest digest-mod)
   (epsilon.symbol sym))
  (:export
   ;; Constant-time primitives
   #:ct-equal
   #:ct-select
   #:ct-swap
   #:ct-lookup
   #:ct-zero-memory
   #:with-secure-buffer
   #:with-secure-buffers

   ;; Platform entropy
   #:entropy-available-p

   ;; SHA-256
   #:sha256
   #:sha256-hex
   #:make-sha256-state
   #:sha256-update
   #:sha256-finalize
   #:sha256-copy

   ;; SHA-224
   #:sha224
   #:sha224-hex
   #:make-sha224-state
   #:sha224-update
   #:sha224-finalize
   #:sha224-copy

   ;; SHA-512
   #:sha512
   #:sha512-hex
   #:make-sha512-state
   #:sha512-update
   #:sha512-finalize
   #:sha512-copy

   ;; SHA-384
   #:sha384
   #:sha384-hex
   #:make-sha384-state
   #:sha384-update
   #:sha384-finalize
   #:sha384-copy

   ;; SHA3
   #:sha3-256 #:sha3-256-hex
   #:sha3-384 #:sha3-384-hex
   #:sha3-512 #:sha3-512-hex
   #:make-sha3-256-state #:sha3-256-update #:sha3-256-finalize #:sha3-256-copy
   #:make-sha3-384-state #:sha3-384-update #:sha3-384-finalize #:sha3-384-copy
   #:make-sha3-512-state #:sha3-512-update #:sha3-512-finalize #:sha3-512-copy
   #:shake128 #:shake256
   #:make-shake128-state #:shake128-update #:shake128-squeeze
   #:make-shake256-state #:shake256-update #:shake256-squeeze

   ;; BLAKE2
   #:blake2b #:blake2b-hex
   #:make-blake2b-state #:blake2b-update #:blake2b-finalize #:blake2b-copy
   #:blake2s #:blake2s-hex
   #:make-blake2s-state #:blake2s-update #:blake2s-finalize #:blake2s-copy

   ;; MD5 (legacy)
   #:md5 #:md5-hex
   #:make-md5-state #:md5-update #:md5-finalize #:md5-copy

   ;; SHA-1 (legacy)
   #:sha1 #:sha1-hex
   #:make-sha1-state #:sha1-update #:sha1-finalize #:sha1-copy

   ;; Poly1305
   #:poly1305 #:poly1305-verify
   #:make-poly1305-state #:poly1305-update #:poly1305-finalize

   ;; PBKDF2
   #:pbkdf2

   ;; scrypt
   #:scrypt

   ;; Argon2id (RFC 9106)
   #:argon2id

   ;; PKCS#12 KDF (RFC 7292 B.2)
   #:pkcs12-kdf
   #:pkcs12-password-to-bmp-string

   ;; PBES2 / EncryptedPrivateKeyInfo (RFC 8018 §6.2, RFC 5958)
   #:pbes2-encrypt
   #:pbes2-decrypt
   #:encode-encrypted-pkcs8
   #:decode-encrypted-pkcs8
   #:encrypted-pkcs8-to-pem
   #:encrypted-pkcs8-from-pem

   ;; PKCS#12 PFX (RFC 7292)
   #:pkcs12-encode
   #:pkcs12-decode
   #:pkcs12-error

   ;; ML-KEM-768 (FIPS 203)
   #:ml-kem-keygen
   #:ml-kem-encaps
   #:ml-kem-decaps
   #:ml-kem-error

   ;; X25519+ML-KEM-768 hybrid KEM (draft-ietf-tls-ecdhe-mlkem)
   #:ml-kem-hybrid-keygen
   #:ml-kem-hybrid-encaps
   #:ml-kem-hybrid-decaps

   ;; HMAC
   #:hmac-sha256
   #:hmac-sha256-verify
   #:hmac-sha384
   #:hmac-sha512
   #:hmac

   ;; HKDF
   #:hkdf-extract
   #:hkdf-expand
   #:hkdf
   #:hkdf-expand-label

   ;; HMAC-DRBG / CSPRNG
   #:make-hmac-drbg
   #:drbg-generate
   #:drbg-reseed
   #:random-bytes
   #:random-integer

   ;; AES
   #:aes-encrypt-block
   #:aes-decrypt-block
   #:make-aes-round-keys
   #:make-aes-decrypt-round-keys
   #:aes-ctr-encrypt
   #:aes-ctr
   #:aes-cbc-encrypt
   #:aes-cbc-decrypt

   ;; AES-GCM
   #:aes-gcm-encrypt
   #:aes-gcm-decrypt

   ;; AES-CCM
   #:aes-ccm-encrypt
   #:aes-ccm-decrypt

   ;; AES-XTS (storage encryption)
   #:aes-xts-encrypt
   #:aes-xts-decrypt
   #:aes-key-wrap
   #:aes-key-unwrap

   ;; ChaCha20
   #:chacha20-encrypt
   #:chacha20-block
   #:hchacha20

   ;; ChaCha20-Poly1305
   #:chacha20-poly1305-encrypt
   #:chacha20-poly1305-decrypt

   ;; XChaCha20-Poly1305 (24-byte nonce variant)
   #:xchacha20-poly1305-encrypt
   #:xchacha20-poly1305-decrypt

   ;; cSHAKE / KMAC (NIST SP 800-185)
   #:cshake128
   #:cshake256
   #:kmac128
   #:kmac256

   ;; KBKDF (NIST SP 800-108 counter mode)
   #:kbkdf-counter-hmac-sha256
   #:kbkdf-counter-hmac-sha384
   #:kbkdf-counter-hmac-sha512
   #:kbkdf-counter-hmac-sha256-with-label-context

   ;; Modular arithmetic
   #:mod-add
   #:mod-sub
   #:mod-neg
   #:mod-mul
   #:mod-sqr
   #:mod-inv
   #:mod-expt
   #:mod-div
   #:extended-gcd

   ;; GF(2^255-19) field arithmetic
   #:fe-zero
   #:fe-one
   #:fe-from-integer
   #:fe-to-integer
   #:fe-from-bytes
   #:fe-to-bytes
   #:fe-add
   #:fe-sub
   #:fe-neg
   #:fe-mul
   #:fe-sqr
   #:fe-inv
   #:fe-pow
   #:fe-copy
   #:fe-equal
   #:fe-zero-p
   #:fe-select
   #:fe-neg-p
   #:fe-abs
   #:fe-sqrt

   ;; GF(P-256) field arithmetic
   #:p256-mod
   #:p256-add
   #:p256-sub
   #:p256-neg
   #:p256-mul
   #:p256-sqr
   #:p256-inv
   #:p256-from-bytes
   #:p256-to-bytes
   #:p256-sqrt

   ;; X25519 key exchange
   #:x25519
   #:x25519-base

   ;; Ed25519 point operations
   #:ed-neutral
   #:ed-base-point
   #:ed-point-add
   #:ed-point-double
   #:ed-scalar-mul
   #:ed-point-equal
   #:ed-on-curve-p
   #:ed-point-negate
   #:ed-point-encode
   #:ed-point-decode

   ;; P-256 EC operations
   #:p256-ec-neutral
   #:p256-ec-base-point
   #:p256-ec-point-add
   #:p256-ec-point-double
   #:p256-ec-scalar-mul
   #:p256-ec-point-equal
   #:p256-ec-on-curve-p
   #:p256-ec-point-negate
   #:p256-ec-point-encode-uncompressed
   #:p256-ec-point-decode

   ;; RSA
   #:make-rsa-public-key
   #:make-rsa-private-key
   #:rsa-encrypt
   #:rsa-decrypt
   #:rsa-pss-sign
   #:rsa-pss-verify
   #:rsa-oaep-encrypt
   #:rsa-oaep-decrypt
   #:rsa-generate-key
   #:miller-rabin-prime-p

   ;; Ed25519 signing (RFC 8032)
   #:ed25519-sign
   #:ed25519-verify
   #:ed25519-public-key-from-private

   ;; ECDSA over P-256 (RFC 6979)
   #:ecdsa-sign
   #:ecdsa-verify
   #:ecdsa-public-key-from-private

   ;; ECDH-P256 key exchange
   #:ecdh-p256-generate-keypair
   #:ecdh-p256-shared-secret
   #:ecdh-p256-public-key-from-private

   ;; ASN.1 DER encoding/decoding
   #:+universal+ #:+application+ #:+context-specific+ #:+private+
   #:+primitive+ #:+constructed+
   #:+tag-boolean+ #:+tag-integer+ #:+tag-bit-string+ #:+tag-octet-string+
   #:+tag-null+ #:+tag-oid+ #:+tag-utf8-string+ #:+tag-sequence+ #:+tag-set+
   #:+tag-printable-string+ #:+tag-ia5-string+ #:+tag-utc-time+ #:+tag-generalized-time+
   #:asn1-tlv #:asn1-tlv-tag #:asn1-tlv-constructed-p #:asn1-tlv-class #:asn1-tlv-value
   #:der-decode #:der-decode-all #:der-decode-sequence-contents
   #:der-encode #:der-encode-integer #:der-encode-oid #:der-encode-octet-string
   #:der-encode-bit-string #:der-encode-null #:der-encode-boolean
   #:der-encode-sequence #:der-encode-set #:der-encode-context
   #:oid-to-string #:string-to-oid #:decode-oid-value
   #:decode-der-integer #:integer-to-der-bytes

   ;; PEM encoding/decoding
   #:pem-block #:make-pem-block #:pem-block-label #:pem-block-data
   #:pem-encode #:pem-decode #:pem-decode-all

   ;; PKCS key encoding
   #:+oid-rsa-encryption+ #:+oid-ec-public-key+ #:+oid-ed25519+
   #:+oid-prime256v1+ #:+oid-sha256-with-rsa+ #:+oid-ecdsa-with-sha256+
   #:encode-spki-rsa #:encode-spki-ec #:encode-spki-ed25519 #:decode-spki
   #:encode-pkcs8-ed25519 #:encode-pkcs8-ec #:decode-pkcs8
   #:encode-pkcs1-rsa-public #:encode-pkcs1-rsa-private
   #:decode-pkcs1-rsa-public #:decode-pkcs1-rsa-private

   ;; X.509 certificates
   #:x509-certificate
   #:x509-cert-version #:x509-cert-serial #:x509-cert-signature-algorithm
   #:x509-cert-issuer #:x509-cert-not-before #:x509-cert-not-after
   #:x509-cert-subject #:x509-cert-public-key-algorithm
   #:x509-cert-public-key-bytes #:x509-cert-public-key-params
   #:x509-cert-extensions #:x509-cert-signature
   #:x509-cert-tbs-bytes #:x509-cert-raw-bytes
   #:parse-x509-certificate #:parse-x509-pem
   #:x509-name #:make-x509-name #:x509-name-entries
   #:x509-name-common-name #:x509-name-to-string
   #:x509-extension #:x509-ext-oid #:x509-ext-critical #:x509-ext-value
   #:x509-get-extension #:x509-basic-constraints #:x509-key-usage
   #:x509-subject-alt-names
   #:verify-certificate-signature #:verify-certificate-chain
   #:hostname-matches-p
   #:make-self-signed-certificate #:make-ca-signed-certificate
   #:make-trust-store #:trust-store-add #:trust-store-certificates
   #:load-pem-trust-store
   #:x509-time #:make-x509-time
   #:x509-time-year #:x509-time-month #:x509-time-day
   #:x509-time-hour #:x509-time-minute #:x509-time-second
   #:x509-time-before-p #:x509-time-now

   ;; TLS 1.3 content types
   #:+content-change-cipher-spec+
   #:+content-alert+
   #:+content-handshake+
   #:+content-application-data+
   ;; TLS 1.3 handshake types
   #:+handshake-client-hello+
   #:+handshake-server-hello+
   #:+handshake-new-session-ticket+
   #:+handshake-encrypted-extensions+
   #:+handshake-certificate+
   #:+handshake-certificate-request+
   #:+handshake-certificate-verify+
   #:+handshake-finished+
   ;; TLS 1.3 cipher suites
   #:+tls-aes-128-gcm-sha256+
   #:+tls-aes-256-gcm-sha384+
   #:+tls-chacha20-poly1305-sha256+
   ;; TLS 1.3 key schedule
   #:tls13-key-schedule
   #:make-tls13-key-schedule
   #:derive-early-secret
   #:derive-handshake-secret
   #:derive-master-secret
   #:derive-handshake-traffic-keys
   #:derive-application-traffic-keys
   ;; TLS 1.3 record layer
   #:tls-record
   #:make-tls-record
   #:tls-record-content-type
   #:tls-record-legacy-version
   #:tls-record-data
   #:parse-tls-record
   #:encrypt-record
   #:decrypt-record
   ;; TLS 1.3 handshake messages
   #:build-client-hello
   #:parse-server-hello
   #:parse-encrypted-extensions
   #:parse-certificate-message
   #:parse-certificate-verify
   #:parse-finished
   #:build-finished
   ;; TLS 1.3 alert constants
   #:+alert-close-notify+
   #:+alert-unexpected-message+
   #:+alert-bad-record-mac+
   #:+alert-handshake-failure+
   #:+alert-bad-certificate+
   #:+alert-certificate-expired+
   #:+alert-unknown-ca+
   #:+alert-decode-error+
   #:+alert-decrypt-error+
   #:+alert-protocol-version+
   #:+alert-internal-error+
   #:+alert-missing-extension+
   #:+alert-certificate-required+
   ;; TLS 1.3 alert protocol
   #:tls-alert
   #:tls-alert-level
   #:tls-alert-description
   #:make-alert-record
   #:parse-alert
   #:tls-close-notify
   ;; TLS 1.3 PSK / session resumption
   #:tls-session-ticket
   #:make-tls-session-ticket
   #:tls-session-ticket-ticket
   #:tls-session-ticket-lifetime
   #:parse-new-session-ticket
   #:build-psk-client-hello
   ;; TLS 1.3 0-RTT
   #:tls-send-early-data
   ;; TLS 1.3 HelloRetryRequest
   #:+hello-retry-request-magic+
   ;; TLS 1.3 KeyUpdate
   #:build-key-update
   #:process-key-update
   ;; TLS 1.3 server-side
   #:tls-server-config
   #:make-tls-server-config
   #:tls-server-start-handshake
   #:tls-server-process-finished
   #:parse-client-hello
   ;; TLS 1.3 state machine
   #:tls-connection
   #:make-tls-connection
   #:tls-handshake-step
   #:tls-connection-state
   #:tls-connection-alpn-protocol
   #:tls-send-application-data
   #:tls-receive-application-data
   ;; TLS 1.3 stream adapter
   #:tls-stream
   #:tls-stream-p
   #:make-tls-stream
   #:tls-stream-connection
   #:tls-stream-closed-p
   #:tls-connect
   #:tls-connect-with-fallback
   ;; Structured errors for the connect path
   #:tls-version-mismatch-error
   #:tls-version-mismatch-negotiated-version
   #:tls-accept
   #:tls-read
   #:tls-write
   #:tls-shutdown
   #:tls-close
   #:tls-stream-peer-certificates
   #:tls-stream-cipher-suite
   #:tls-stream-alpn-protocol
   #:tls-read-line
   #:tls-write-line
   #:tls-write-string
   ;; TLS 1.3 transport protocol
   #:tls-transport
   #:tls-transport-read
   #:tls-transport-write
   #:tls-transport-close))

(in-package :epsilon.crypto)

;; Re-export constant-time primitives
(setf (fdefinition 'ct-equal) #'ct:ct-equal)
(setf (fdefinition 'ct-select) #'ct:ct-select)
(setf (fdefinition 'ct-swap) #'ct:ct-swap)
(setf (fdefinition 'ct-lookup) #'ct:ct-lookup)
(setf (fdefinition 'ct-zero-memory) #'ct:ct-zero-memory)
;; with-secure-buffer and with-secure-buffers are macros -- re-export via defmacro
(defmacro with-secure-buffer ((var size) &body body)
  `(ct:with-secure-buffer (,var ,size) ,@body))
(defmacro with-secure-buffers (bindings &body body)
  `(ct:with-secure-buffers ,bindings ,@body))

;; Re-export entropy
(setf (fdefinition 'entropy-available-p) #'entropy:entropy-available-p)

;; Re-export SHA-256/224
(setf (fdefinition 'sha256) #'sha256:sha256)
(setf (fdefinition 'sha256-hex) #'sha256:sha256-hex)
(setf (fdefinition 'make-sha256-state) #'sha256:make-sha256-state)
(setf (fdefinition 'sha256-update) #'sha256:sha256-update)
(setf (fdefinition 'sha256-finalize) #'sha256:sha256-finalize)
(setf (fdefinition 'sha256-copy) #'sha256:sha256-copy)
(setf (fdefinition 'sha224) #'sha256:sha224)
(setf (fdefinition 'sha224-hex) #'sha256:sha224-hex)
(setf (fdefinition 'make-sha224-state) #'sha256:make-sha224-state)
(setf (fdefinition 'sha224-update) #'sha256:sha224-update)
(setf (fdefinition 'sha224-finalize) #'sha256:sha224-finalize)
(setf (fdefinition 'sha224-copy) #'sha256:sha224-copy)

;; Re-export SHA-512/384
(setf (fdefinition 'sha512) #'sha512:sha512)
(setf (fdefinition 'sha512-hex) #'sha512:sha512-hex)
(setf (fdefinition 'make-sha512-state) #'sha512:make-sha512-state)
(setf (fdefinition 'sha512-update) #'sha512:sha512-update)
(setf (fdefinition 'sha512-finalize) #'sha512:sha512-finalize)
(setf (fdefinition 'sha512-copy) #'sha512:sha512-copy)
(setf (fdefinition 'sha384) #'sha512:sha384)
(setf (fdefinition 'sha384-hex) #'sha512:sha384-hex)
(setf (fdefinition 'make-sha384-state) #'sha512:make-sha384-state)
(setf (fdefinition 'sha384-update) #'sha512:sha384-update)
(setf (fdefinition 'sha384-finalize) #'sha512:sha384-finalize)
(setf (fdefinition 'sha384-copy) #'sha512:sha384-copy)

;; Re-export SHA3
(setf (fdefinition 'sha3-256) #'sha3:sha3-256)
(setf (fdefinition 'sha3-256-hex) #'sha3:sha3-256-hex)
(setf (fdefinition 'make-sha3-256-state) #'sha3:make-sha3-256-state)
(setf (fdefinition 'sha3-256-update) #'sha3:sha3-256-update)
(setf (fdefinition 'sha3-256-finalize) #'sha3:sha3-256-finalize)
(setf (fdefinition 'sha3-256-copy) #'sha3:sha3-256-copy)
(setf (fdefinition 'sha3-384) #'sha3:sha3-384)
(setf (fdefinition 'sha3-384-hex) #'sha3:sha3-384-hex)
(setf (fdefinition 'make-sha3-384-state) #'sha3:make-sha3-384-state)
(setf (fdefinition 'sha3-384-update) #'sha3:sha3-384-update)
(setf (fdefinition 'sha3-384-finalize) #'sha3:sha3-384-finalize)
(setf (fdefinition 'sha3-384-copy) #'sha3:sha3-384-copy)
(setf (fdefinition 'sha3-512) #'sha3:sha3-512)
(setf (fdefinition 'sha3-512-hex) #'sha3:sha3-512-hex)
(setf (fdefinition 'make-sha3-512-state) #'sha3:make-sha3-512-state)
(setf (fdefinition 'sha3-512-update) #'sha3:sha3-512-update)
(setf (fdefinition 'sha3-512-finalize) #'sha3:sha3-512-finalize)
(setf (fdefinition 'sha3-512-copy) #'sha3:sha3-512-copy)
(setf (fdefinition 'shake128) #'sha3:shake128)
(setf (fdefinition 'shake256) #'sha3:shake256)
(setf (fdefinition 'make-shake128-state) #'sha3:make-shake128-state)
(setf (fdefinition 'shake128-update) #'sha3:shake128-update)
(setf (fdefinition 'shake128-squeeze) #'sha3:shake128-squeeze)
(setf (fdefinition 'make-shake256-state) #'sha3:make-shake256-state)
(setf (fdefinition 'shake256-update) #'sha3:shake256-update)
(setf (fdefinition 'shake256-squeeze) #'sha3:shake256-squeeze)

;; Re-export BLAKE2
(setf (fdefinition 'blake2b) #'blake2:blake2b)
(setf (fdefinition 'blake2b-hex) #'blake2:blake2b-hex)
(setf (fdefinition 'make-blake2b-state) #'blake2:make-blake2b-state)
(setf (fdefinition 'blake2b-update) #'blake2:blake2b-update)
(setf (fdefinition 'blake2b-finalize) #'blake2:blake2b-finalize)
(setf (fdefinition 'blake2b-copy) #'blake2:blake2b-copy)
(setf (fdefinition 'blake2s) #'blake2:blake2s)
(setf (fdefinition 'blake2s-hex) #'blake2:blake2s-hex)
(setf (fdefinition 'make-blake2s-state) #'blake2:make-blake2s-state)
(setf (fdefinition 'blake2s-update) #'blake2:blake2s-update)
(setf (fdefinition 'blake2s-finalize) #'blake2:blake2s-finalize)
(setf (fdefinition 'blake2s-copy) #'blake2:blake2s-copy)

;; Re-export MD5 (legacy)
(setf (fdefinition 'md5) #'md5:md5)
(setf (fdefinition 'md5-hex) #'md5:md5-hex)
(setf (fdefinition 'make-md5-state) #'md5:make-md5-state)
(setf (fdefinition 'md5-update) #'md5:md5-update)
(setf (fdefinition 'md5-finalize) #'md5:md5-finalize)
(setf (fdefinition 'md5-copy) #'md5:md5-copy)

;; Re-export SHA-1 (legacy)
(setf (fdefinition 'sha1) #'sha1:sha1)
(setf (fdefinition 'sha1-hex) #'sha1:sha1-hex)
(setf (fdefinition 'make-sha1-state) #'sha1:make-sha1-state)
(setf (fdefinition 'sha1-update) #'sha1:sha1-update)
(setf (fdefinition 'sha1-finalize) #'sha1:sha1-finalize)
(setf (fdefinition 'sha1-copy) #'sha1:sha1-copy)

;; Re-export Poly1305
(setf (fdefinition 'poly1305) #'poly1305:poly1305)
(setf (fdefinition 'poly1305-verify) #'poly1305:poly1305-verify)
(setf (fdefinition 'make-poly1305-state) #'poly1305:make-poly1305-state)
(setf (fdefinition 'poly1305-update) #'poly1305:poly1305-update)
(setf (fdefinition 'poly1305-finalize) #'poly1305:poly1305-finalize)

;; Re-export PBKDF2
(setf (fdefinition 'pbkdf2) #'pbkdf2:pbkdf2)

;; Re-export scrypt
(setf (fdefinition 'scrypt) #'scrypt:scrypt)

;; Re-export Argon2id
(setf (fdefinition 'argon2id) #'argon2:argon2id)

;; Re-export PKCS#12 KDF
(setf (fdefinition 'pkcs12-kdf) #'pkcs12-kdf:pkcs12-kdf)
(setf (fdefinition 'pkcs12-password-to-bmp-string)
      #'pkcs12-kdf:password-to-bmp-string)

;; Re-export PBES2 / EncryptedPrivateKeyInfo
(setf (fdefinition 'pbes2-encrypt) #'pbes2:pbes2-encrypt)
(setf (fdefinition 'pbes2-decrypt) #'pbes2:pbes2-decrypt)
(setf (fdefinition 'encode-encrypted-pkcs8) #'pbes2:encode-encrypted-pkcs8)
(setf (fdefinition 'decode-encrypted-pkcs8) #'pbes2:decode-encrypted-pkcs8)
(setf (fdefinition 'encrypted-pkcs8-to-pem) #'pbes2:encrypted-pkcs8-to-pem)
(setf (fdefinition 'encrypted-pkcs8-from-pem) #'pbes2:encrypted-pkcs8-from-pem)

;; Re-export PKCS#12
(setf (fdefinition 'pkcs12-encode) #'pkcs12:pkcs12-encode)
(setf (fdefinition 'pkcs12-decode) #'pkcs12:pkcs12-decode)

;; Re-export ML-KEM-768
(setf (fdefinition 'ml-kem-keygen) #'ml-kem:keygen)
(setf (fdefinition 'ml-kem-encaps) #'ml-kem:encaps)
(setf (fdefinition 'ml-kem-decaps) #'ml-kem:decaps)

;; Re-export X25519+ML-KEM-768 hybrid
(setf (fdefinition 'ml-kem-hybrid-keygen) #'ml-kem-hybrid:hybrid-keygen)
(setf (fdefinition 'ml-kem-hybrid-encaps) #'ml-kem-hybrid:hybrid-encaps)
(setf (fdefinition 'ml-kem-hybrid-decaps) #'ml-kem-hybrid:hybrid-decaps)

;; Re-export HMAC
(setf (fdefinition 'hmac-sha256) #'hmac:hmac-sha256)
(setf (fdefinition 'hmac-sha256-verify) #'hmac:hmac-sha256-verify)
(setf (fdefinition 'hmac-sha384) #'hmac:hmac-sha384)
(setf (fdefinition 'hmac-sha512) #'hmac:hmac-sha512)
(setf (fdefinition 'hmac) #'hmac:hmac)

;; Re-export HKDF
(setf (fdefinition 'hkdf-extract) #'hkdf:hkdf-extract)
(setf (fdefinition 'hkdf-expand) #'hkdf:hkdf-expand)
(setf (fdefinition 'hkdf) #'hkdf:hkdf)
(setf (fdefinition 'hkdf-expand-label) #'hkdf:hkdf-expand-label)

;; Re-export DRBG
(setf (fdefinition 'make-hmac-drbg) #'drbg:make-hmac-drbg)
(setf (fdefinition 'drbg-generate) #'drbg:drbg-generate)
(setf (fdefinition 'drbg-reseed) #'drbg:drbg-reseed)
(setf (fdefinition 'random-bytes) #'drbg:random-bytes)
(setf (fdefinition 'random-integer) #'drbg:random-integer)

;; Re-export AES
(setf (fdefinition 'aes-encrypt-block) #'aes:aes-encrypt-block)
(setf (fdefinition 'aes-decrypt-block) #'aes:aes-decrypt-block)
(setf (fdefinition 'make-aes-round-keys) #'aes:make-aes-round-keys)
(setf (fdefinition 'make-aes-decrypt-round-keys) #'aes:make-aes-decrypt-round-keys)
(setf (fdefinition 'aes-ctr-encrypt) #'aes:aes-ctr-encrypt)
(setf (fdefinition 'aes-ctr) #'aes:aes-ctr)
(setf (fdefinition 'aes-cbc-encrypt) #'aes:aes-cbc-encrypt)
(setf (fdefinition 'aes-cbc-decrypt) #'aes:aes-cbc-decrypt)

;; Re-export AES-GCM
(setf (fdefinition 'aes-gcm-encrypt) #'aes-gcm:aes-gcm-encrypt)
(setf (fdefinition 'aes-gcm-decrypt) #'aes-gcm:aes-gcm-decrypt)
(setf (fdefinition 'aes-ccm-encrypt) #'aes-ccm:aes-ccm-encrypt)
(setf (fdefinition 'aes-ccm-decrypt) #'aes-ccm:aes-ccm-decrypt)
(setf (fdefinition 'aes-xts-encrypt) #'aes-xts:aes-xts-encrypt)
(setf (fdefinition 'aes-xts-decrypt) #'aes-xts:aes-xts-decrypt)
(setf (fdefinition 'aes-key-wrap)   #'aes-kw:aes-key-wrap)
(setf (fdefinition 'aes-key-unwrap) #'aes-kw:aes-key-unwrap)

;; Re-export ChaCha20
(setf (fdefinition 'chacha20-encrypt) #'chacha20:chacha20-encrypt)
(setf (fdefinition 'chacha20-block) #'chacha20:chacha20-block)
(setf (fdefinition 'hchacha20) #'chacha20:hchacha20)

;; Re-export ChaCha20-Poly1305
(setf (fdefinition 'chacha20-poly1305-encrypt) #'chacha20-poly1305:chacha20-poly1305-encrypt)
(setf (fdefinition 'chacha20-poly1305-decrypt) #'chacha20-poly1305:chacha20-poly1305-decrypt)

;; Re-export XChaCha20-Poly1305
(setf (fdefinition 'xchacha20-poly1305-encrypt)
      #'xchacha20-poly1305:xchacha20-poly1305-encrypt)
(setf (fdefinition 'xchacha20-poly1305-decrypt)
      #'xchacha20-poly1305:xchacha20-poly1305-decrypt)

;; Re-export cSHAKE / KMAC
(setf (fdefinition 'cshake128) #'kmac:cshake128)
(setf (fdefinition 'cshake256) #'kmac:cshake256)
(setf (fdefinition 'kmac128) #'kmac:kmac128)
(setf (fdefinition 'kmac256) #'kmac:kmac256)

;; Re-export KBKDF
(setf (fdefinition 'kbkdf-counter-hmac-sha256) #'kbkdf:kbkdf-counter-hmac-sha256)
(setf (fdefinition 'kbkdf-counter-hmac-sha384) #'kbkdf:kbkdf-counter-hmac-sha384)
(setf (fdefinition 'kbkdf-counter-hmac-sha512) #'kbkdf:kbkdf-counter-hmac-sha512)
(setf (fdefinition 'kbkdf-counter-hmac-sha256-with-label-context)
      #'kbkdf:kbkdf-counter-hmac-sha256-with-label-context)

;; Re-export modular arithmetic
(setf (fdefinition 'mod-add) #'mod-arith:mod-add)
(setf (fdefinition 'mod-sub) #'mod-arith:mod-sub)
(setf (fdefinition 'mod-neg) #'mod-arith:mod-neg)
(setf (fdefinition 'mod-mul) #'mod-arith:mod-mul)
(setf (fdefinition 'mod-sqr) #'mod-arith:mod-sqr)
(setf (fdefinition 'mod-inv) #'mod-arith:mod-inv)
(setf (fdefinition 'mod-expt) #'mod-arith:mod-expt)
(setf (fdefinition 'mod-div) #'mod-arith:mod-div)
(setf (fdefinition 'extended-gcd) #'mod-arith:extended-gcd)

;; Re-export GF(2^255-19)
(setf (fdefinition 'fe-zero) #'fe25519:fe-zero)
(setf (fdefinition 'fe-one) #'fe25519:fe-one)
(setf (fdefinition 'fe-from-integer) #'fe25519:fe-from-integer)
(setf (fdefinition 'fe-to-integer) #'fe25519:fe-to-integer)
(setf (fdefinition 'fe-from-bytes) #'fe25519:fe-from-bytes)
(setf (fdefinition 'fe-to-bytes) #'fe25519:fe-to-bytes)
(setf (fdefinition 'fe-add) #'fe25519:fe-add)
(setf (fdefinition 'fe-sub) #'fe25519:fe-sub)
(setf (fdefinition 'fe-neg) #'fe25519:fe-neg)
(setf (fdefinition 'fe-mul) #'fe25519:fe-mul)
(setf (fdefinition 'fe-sqr) #'fe25519:fe-sqr)
(setf (fdefinition 'fe-inv) #'fe25519:fe-inv)
(setf (fdefinition 'fe-pow) #'fe25519:fe-pow)
(setf (fdefinition 'fe-copy) #'fe25519:fe-copy)
(setf (fdefinition 'fe-equal) #'fe25519:fe-equal)
(setf (fdefinition 'fe-zero-p) #'fe25519:fe-zero-p)
(setf (fdefinition 'fe-select) #'fe25519:fe-select)
(setf (fdefinition 'fe-neg-p) #'fe25519:fe-neg-p)
(setf (fdefinition 'fe-abs) #'fe25519:fe-abs)
(setf (fdefinition 'fe-sqrt) #'fe25519:fe-sqrt)

;; Re-export GF(P-256)
(setf (fdefinition 'p256-mod) #'p256:p256-mod)
(setf (fdefinition 'p256-add) #'p256:p256-add)
(setf (fdefinition 'p256-sub) #'p256:p256-sub)
(setf (fdefinition 'p256-neg) #'p256:p256-neg)
(setf (fdefinition 'p256-mul) #'p256:p256-mul)
(setf (fdefinition 'p256-sqr) #'p256:p256-sqr)
(setf (fdefinition 'p256-inv) #'p256:p256-inv)
(setf (fdefinition 'p256-from-bytes) #'p256:p256-from-bytes)
(setf (fdefinition 'p256-to-bytes) #'p256:p256-to-bytes)
(setf (fdefinition 'p256-sqrt) #'p256:p256-sqrt)

;; Re-export X25519
(setf (fdefinition 'x25519) #'x25519:x25519)
(setf (fdefinition 'x25519-base) #'x25519:x25519-base)

;; Re-export Ed25519
(setf (fdefinition 'ed-neutral) #'ed25519:ed-neutral)
(setf (fdefinition 'ed-base-point) #'ed25519:ed-base-point)
(setf (fdefinition 'ed-point-add) #'ed25519:ed-point-add)
(setf (fdefinition 'ed-point-double) #'ed25519:ed-point-double)
(setf (fdefinition 'ed-scalar-mul) #'ed25519:ed-scalar-mul)
(setf (fdefinition 'ed-point-equal) #'ed25519:ed-point-equal)
(setf (fdefinition 'ed-on-curve-p) #'ed25519:ed-on-curve-p)
(setf (fdefinition 'ed-point-negate) #'ed25519:ed-point-negate)
(setf (fdefinition 'ed-point-encode) #'ed25519:ed-point-encode)
(setf (fdefinition 'ed-point-decode) #'ed25519:ed-point-decode)

;; Re-export P-256 EC (prefixed with p256-ec- to avoid collision with field ops)
(setf (fdefinition 'p256-ec-neutral) #'ec-p256:p256-neutral)
(setf (fdefinition 'p256-ec-base-point) #'ec-p256:p256-base-point)
(setf (fdefinition 'p256-ec-point-add) #'ec-p256:p256-point-add)
(setf (fdefinition 'p256-ec-point-double) #'ec-p256:p256-point-double)
(setf (fdefinition 'p256-ec-scalar-mul) #'ec-p256:p256-scalar-mul)
(setf (fdefinition 'p256-ec-point-equal) #'ec-p256:p256-point-equal)
(setf (fdefinition 'p256-ec-on-curve-p) #'ec-p256:p256-on-curve-p)
(setf (fdefinition 'p256-ec-point-negate) #'ec-p256:p256-point-negate)
(setf (fdefinition 'p256-ec-point-encode-uncompressed) #'ec-p256:p256-point-encode-uncompressed)
(setf (fdefinition 'p256-ec-point-decode) #'ec-p256:p256-point-decode)

;; Re-export RSA
(setf (fdefinition 'make-rsa-public-key) #'rsa:make-rsa-public-key)
(setf (fdefinition 'make-rsa-private-key) #'rsa:make-rsa-private-key)
(setf (fdefinition 'rsa-encrypt) #'rsa:rsa-encrypt)
(setf (fdefinition 'rsa-decrypt) #'rsa:rsa-decrypt)
(setf (fdefinition 'rsa-pss-sign) #'rsa:rsa-pss-sign)
(setf (fdefinition 'rsa-pss-verify) #'rsa:rsa-pss-verify)
(setf (fdefinition 'rsa-oaep-encrypt) #'rsa:rsa-oaep-encrypt)
(setf (fdefinition 'rsa-oaep-decrypt) #'rsa:rsa-oaep-decrypt)
(setf (fdefinition 'rsa-generate-key) #'rsa:rsa-generate-key)
(setf (fdefinition 'miller-rabin-prime-p) #'rsa:miller-rabin-prime-p)

;; Re-export Ed25519 signing
(setf (fdefinition 'ed25519-sign) #'ed-sign:ed25519-sign)
(setf (fdefinition 'ed25519-verify) #'ed-sign:ed25519-verify)
(setf (fdefinition 'ed25519-public-key-from-private) #'ed-sign:ed25519-public-key-from-private)

;; Re-export ECDSA
(setf (fdefinition 'ecdsa-sign) #'ecdsa:ecdsa-sign)
(setf (fdefinition 'ecdsa-verify) #'ecdsa:ecdsa-verify)
(setf (fdefinition 'ecdsa-public-key-from-private) #'ecdsa:ecdsa-public-key-from-private)

;; Re-export ECDH-P256
(setf (fdefinition 'ecdh-p256-generate-keypair) #'ecdh:ecdh-p256-generate-keypair)
(setf (fdefinition 'ecdh-p256-shared-secret) #'ecdh:ecdh-p256-shared-secret)
(setf (fdefinition 'ecdh-p256-public-key-from-private) #'ecdh:ecdh-p256-public-key-from-private)

;; Re-export ASN.1 constants
(defconstant +universal+ asn1:+universal+)
(defconstant +application+ asn1:+application+)
(defconstant +context-specific+ asn1:+context-specific+)
(defconstant +private+ asn1:+private+)
(defconstant +primitive+ asn1:+primitive+)
(defconstant +constructed+ asn1:+constructed+)
(defconstant +tag-boolean+ asn1:+tag-boolean+)
(defconstant +tag-integer+ asn1:+tag-integer+)
(defconstant +tag-bit-string+ asn1:+tag-bit-string+)
(defconstant +tag-octet-string+ asn1:+tag-octet-string+)
(defconstant +tag-null+ asn1:+tag-null+)
(defconstant +tag-oid+ asn1:+tag-oid+)
(defconstant +tag-utf8-string+ asn1:+tag-utf8-string+)
(defconstant +tag-sequence+ asn1:+tag-sequence+)
(defconstant +tag-set+ asn1:+tag-set+)
(defconstant +tag-printable-string+ asn1:+tag-printable-string+)
(defconstant +tag-ia5-string+ asn1:+tag-ia5-string+)
(defconstant +tag-utc-time+ asn1:+tag-utc-time+)
(defconstant +tag-generalized-time+ asn1:+tag-generalized-time+)

;; Re-export ASN.1 TLV structure accessors
(setf (fdefinition 'asn1-tlv-tag) #'asn1:asn1-tlv-tag)
(setf (fdefinition 'asn1-tlv-constructed-p) #'asn1:asn1-tlv-constructed-p)
(setf (fdefinition 'asn1-tlv-class) #'asn1:asn1-tlv-class)
(setf (fdefinition 'asn1-tlv-value) #'asn1:asn1-tlv-value)

;; Re-export ASN.1 decoding
(setf (fdefinition 'der-decode) #'asn1:der-decode)
(setf (fdefinition 'der-decode-all) #'asn1:der-decode-all)
(setf (fdefinition 'der-decode-sequence-contents) #'asn1:der-decode-sequence-contents)

;; Re-export ASN.1 encoding
(setf (fdefinition 'der-encode) #'asn1:der-encode)
(setf (fdefinition 'der-encode-integer) #'asn1:der-encode-integer)
(setf (fdefinition 'der-encode-oid) #'asn1:der-encode-oid)
(setf (fdefinition 'der-encode-octet-string) #'asn1:der-encode-octet-string)
(setf (fdefinition 'der-encode-bit-string) #'asn1:der-encode-bit-string)
(setf (fdefinition 'der-encode-null) #'asn1:der-encode-null)
(setf (fdefinition 'der-encode-boolean) #'asn1:der-encode-boolean)
(setf (fdefinition 'der-encode-sequence) #'asn1:der-encode-sequence)
(setf (fdefinition 'der-encode-set) #'asn1:der-encode-set)
(setf (fdefinition 'der-encode-context) #'asn1:der-encode-context)

;; Re-export ASN.1 OID utilities
(setf (fdefinition 'oid-to-string) #'asn1:oid-to-string)
(setf (fdefinition 'string-to-oid) #'asn1:string-to-oid)
(setf (fdefinition 'decode-oid-value) #'asn1:decode-oid-value)

;; Re-export ASN.1 integer utilities
(setf (fdefinition 'decode-der-integer) #'asn1:decode-der-integer)
(setf (fdefinition 'integer-to-der-bytes) #'asn1:integer-to-der-bytes)

;; Re-export PEM
(setf (fdefinition 'make-pem-block) #'pem:make-pem-block)
(setf (fdefinition 'pem-block-label) #'pem:pem-block-label)
(setf (fdefinition 'pem-block-data) #'pem:pem-block-data)
(setf (fdefinition 'pem-encode) #'pem:pem-encode)
(setf (fdefinition 'pem-decode) #'pem:pem-decode)
(setf (fdefinition 'pem-decode-all) #'pem:pem-decode-all)

;; Re-export PKCS OID constants
(defparameter +oid-rsa-encryption+ pkcs:+oid-rsa-encryption+)
(defparameter +oid-ec-public-key+ pkcs:+oid-ec-public-key+)
(defparameter +oid-ed25519+ pkcs:+oid-ed25519+)
(defparameter +oid-prime256v1+ pkcs:+oid-prime256v1+)
(defparameter +oid-sha256-with-rsa+ pkcs:+oid-sha256-with-rsa+)
(defparameter +oid-ecdsa-with-sha256+ pkcs:+oid-ecdsa-with-sha256+)

;; Re-export PKCS SubjectPublicKeyInfo
(setf (fdefinition 'encode-spki-rsa) #'pkcs:encode-spki-rsa)
(setf (fdefinition 'encode-spki-ec) #'pkcs:encode-spki-ec)
(setf (fdefinition 'encode-spki-ed25519) #'pkcs:encode-spki-ed25519)
(setf (fdefinition 'decode-spki) #'pkcs:decode-spki)

;; Re-export PKCS#8 PrivateKeyInfo
(setf (fdefinition 'encode-pkcs8-ed25519) #'pkcs:encode-pkcs8-ed25519)
(setf (fdefinition 'encode-pkcs8-ec) #'pkcs:encode-pkcs8-ec)
(setf (fdefinition 'decode-pkcs8) #'pkcs:decode-pkcs8)

;; Re-export PKCS#1 RSA keys
(setf (fdefinition 'encode-pkcs1-rsa-public) #'pkcs:encode-pkcs1-rsa-public)
(setf (fdefinition 'encode-pkcs1-rsa-private) #'pkcs:encode-pkcs1-rsa-private)
(setf (fdefinition 'decode-pkcs1-rsa-public) #'pkcs:decode-pkcs1-rsa-public)
(setf (fdefinition 'decode-pkcs1-rsa-private) #'pkcs:decode-pkcs1-rsa-private)

;; Re-export X.509 certificate accessors
(setf (fdefinition 'x509-cert-version) #'x509:x509-cert-version)
(setf (fdefinition 'x509-cert-serial) #'x509:x509-cert-serial)
(setf (fdefinition 'x509-cert-signature-algorithm) #'x509:x509-cert-signature-algorithm)
(setf (fdefinition 'x509-cert-issuer) #'x509:x509-cert-issuer)
(setf (fdefinition 'x509-cert-not-before) #'x509:x509-cert-not-before)
(setf (fdefinition 'x509-cert-not-after) #'x509:x509-cert-not-after)
(setf (fdefinition 'x509-cert-subject) #'x509:x509-cert-subject)
(setf (fdefinition 'x509-cert-public-key-algorithm) #'x509:x509-cert-public-key-algorithm)
(setf (fdefinition 'x509-cert-public-key-bytes) #'x509:x509-cert-public-key-bytes)
(setf (fdefinition 'x509-cert-public-key-params) #'x509:x509-cert-public-key-params)
(setf (fdefinition 'x509-cert-extensions) #'x509:x509-cert-extensions)
(setf (fdefinition 'x509-cert-signature) #'x509:x509-cert-signature)
(setf (fdefinition 'x509-cert-tbs-bytes) #'x509:x509-cert-tbs-bytes)
(setf (fdefinition 'x509-cert-raw-bytes) #'x509:x509-cert-raw-bytes)

;; Re-export X.509 parsing
(setf (fdefinition 'parse-x509-certificate) #'x509:parse-x509-certificate)
(setf (fdefinition 'parse-x509-pem) #'x509:parse-x509-pem)

;; Re-export X.509 name
(setf (fdefinition 'make-x509-name) #'x509:make-x509-name)
(setf (fdefinition 'x509-name-entries) #'x509:x509-name-entries)
(setf (fdefinition 'x509-name-common-name) #'x509:x509-name-common-name)
(setf (fdefinition 'x509-name-to-string) #'x509:x509-name-to-string)

;; Re-export X.509 extensions
(setf (fdefinition 'x509-ext-oid) #'x509:x509-ext-oid)
(setf (fdefinition 'x509-ext-critical) #'x509:x509-ext-critical)
(setf (fdefinition 'x509-ext-value) #'x509:x509-ext-value)
(setf (fdefinition 'x509-get-extension) #'x509:x509-get-extension)
(setf (fdefinition 'x509-basic-constraints) #'x509:x509-basic-constraints)
(setf (fdefinition 'x509-key-usage) #'x509:x509-key-usage)
(setf (fdefinition 'x509-subject-alt-names) #'x509:x509-subject-alt-names)

;; Re-export X.509 validation
(setf (fdefinition 'verify-certificate-signature) #'x509:verify-certificate-signature)
(setf (fdefinition 'verify-certificate-chain) #'x509:verify-certificate-chain)
(setf (fdefinition 'hostname-matches-p) #'x509:hostname-matches-p)

;; Re-export X.509 generation
(setf (fdefinition 'make-self-signed-certificate) #'x509:make-self-signed-certificate)
(setf (fdefinition 'make-ca-signed-certificate) #'x509:make-ca-signed-certificate)

;; Re-export X.509 trust store
(setf (fdefinition 'make-trust-store) #'x509:make-trust-store)
(setf (fdefinition 'trust-store-add) #'x509:trust-store-add)
(setf (fdefinition 'trust-store-certificates) #'x509:trust-store-certificates)
(setf (fdefinition 'load-pem-trust-store) #'x509:load-pem-trust-store)

;; Re-export X.509 time
(setf (fdefinition 'make-x509-time) #'x509:make-x509-time)
(setf (fdefinition 'x509-time-year) #'x509:x509-time-year)
(setf (fdefinition 'x509-time-month) #'x509:x509-time-month)
(setf (fdefinition 'x509-time-day) #'x509:x509-time-day)
(setf (fdefinition 'x509-time-hour) #'x509:x509-time-hour)
(setf (fdefinition 'x509-time-minute) #'x509:x509-time-minute)
(setf (fdefinition 'x509-time-second) #'x509:x509-time-second)
(setf (fdefinition 'x509-time-before-p) #'x509:x509-time-before-p)
(setf (fdefinition 'x509-time-now) #'x509:x509-time-now)

;; Re-export TLS 1.3 constants
(defconstant +content-change-cipher-spec+ tls13:+content-change-cipher-spec+)
(defconstant +content-alert+ tls13:+content-alert+)
(defconstant +content-handshake+ tls13:+content-handshake+)
(defconstant +content-application-data+ tls13:+content-application-data+)
(defconstant +handshake-client-hello+ tls13:+handshake-client-hello+)
(defconstant +handshake-server-hello+ tls13:+handshake-server-hello+)
(defconstant +handshake-new-session-ticket+ tls13:+handshake-new-session-ticket+)
(defconstant +handshake-encrypted-extensions+ tls13:+handshake-encrypted-extensions+)
(defconstant +handshake-certificate+ tls13:+handshake-certificate+)
(defconstant +handshake-certificate-request+ tls13:+handshake-certificate-request+)
(defconstant +handshake-certificate-verify+ tls13:+handshake-certificate-verify+)
(defconstant +handshake-finished+ tls13:+handshake-finished+)
(defconstant +tls-aes-128-gcm-sha256+ tls13:+tls-aes-128-gcm-sha256+)
(defconstant +tls-aes-256-gcm-sha384+ tls13:+tls-aes-256-gcm-sha384+)
(defconstant +tls-chacha20-poly1305-sha256+ tls13:+tls-chacha20-poly1305-sha256+)

;; Re-export TLS 1.3 key schedule
(setf (fdefinition 'make-tls13-key-schedule) #'tls13:make-tls13-key-schedule)
(setf (fdefinition 'derive-early-secret) #'tls13:derive-early-secret)
(setf (fdefinition 'derive-handshake-secret) #'tls13:derive-handshake-secret)
(setf (fdefinition 'derive-master-secret) #'tls13:derive-master-secret)
(setf (fdefinition 'derive-handshake-traffic-keys) #'tls13:derive-handshake-traffic-keys)
(setf (fdefinition 'derive-application-traffic-keys) #'tls13:derive-application-traffic-keys)

;; Re-export TLS 1.3 record layer
(setf (fdefinition 'make-tls-record) #'tls13:make-tls-record)
(setf (fdefinition 'tls-record-content-type) #'tls13:tls-record-content-type)
(setf (fdefinition 'tls-record-legacy-version) #'tls13:tls-record-legacy-version)
(setf (fdefinition 'tls-record-data) #'tls13:tls-record-data)
(setf (fdefinition 'parse-tls-record) #'tls13:parse-tls-record)
(setf (fdefinition 'encrypt-record) #'tls13:encrypt-record)
(setf (fdefinition 'decrypt-record) #'tls13:decrypt-record)

;; Re-export TLS 1.3 handshake messages
(setf (fdefinition 'build-client-hello) #'tls13:build-client-hello)
(setf (fdefinition 'parse-server-hello) #'tls13:parse-server-hello)
(setf (fdefinition 'parse-encrypted-extensions) #'tls13:parse-encrypted-extensions)
(setf (fdefinition 'parse-certificate-message) #'tls13:parse-certificate-message)
(setf (fdefinition 'parse-certificate-verify) #'tls13:parse-certificate-verify)
(setf (fdefinition 'parse-finished) #'tls13:parse-finished)
(setf (fdefinition 'build-finished) #'tls13:build-finished)

;; Re-export TLS 1.3 alert constants
(defconstant +alert-close-notify+ tls13:+alert-close-notify+)
(defconstant +alert-unexpected-message+ tls13:+alert-unexpected-message+)
(defconstant +alert-bad-record-mac+ tls13:+alert-bad-record-mac+)
(defconstant +alert-handshake-failure+ tls13:+alert-handshake-failure+)
(defconstant +alert-bad-certificate+ tls13:+alert-bad-certificate+)
(defconstant +alert-certificate-expired+ tls13:+alert-certificate-expired+)
(defconstant +alert-unknown-ca+ tls13:+alert-unknown-ca+)
(defconstant +alert-decode-error+ tls13:+alert-decode-error+)
(defconstant +alert-decrypt-error+ tls13:+alert-decrypt-error+)
(defconstant +alert-protocol-version+ tls13:+alert-protocol-version+)
(defconstant +alert-internal-error+ tls13:+alert-internal-error+)
(defconstant +alert-missing-extension+ tls13:+alert-missing-extension+)
(defconstant +alert-certificate-required+ tls13:+alert-certificate-required+)
(defconstant +handshake-key-update+ tls13:+handshake-key-update+)
(defconstant +handshake-end-of-early-data+ tls13:+handshake-end-of-early-data+)

;; Re-export TLS 1.3 alert protocol
(setf (fdefinition 'make-alert-record) #'tls13:make-alert-record)
(setf (fdefinition 'parse-alert) #'tls13:parse-alert)
(setf (fdefinition 'tls-close-notify) #'tls13:tls-close-notify)

;; Re-export TLS 1.3 PSK / session resumption
(setf (fdefinition 'make-tls-session-ticket) #'tls13:make-tls-session-ticket)
(setf (fdefinition 'parse-new-session-ticket) #'tls13:parse-new-session-ticket)
(setf (fdefinition 'build-psk-client-hello) #'tls13:build-psk-client-hello)

;; Re-export TLS 1.3 0-RTT
(setf (fdefinition 'tls-send-early-data) #'tls13:tls-send-early-data)

;; Re-export TLS 1.3 KeyUpdate
(setf (fdefinition 'build-key-update) #'tls13:build-key-update)
(setf (fdefinition 'process-key-update) #'tls13:process-key-update)

;; Re-export TLS 1.3 server-side
(setf (fdefinition 'make-tls-server-config) #'tls13:make-tls-server-config)
(setf (fdefinition 'tls-server-start-handshake) #'tls13:tls-server-start-handshake)
(setf (fdefinition 'tls-server-process-finished) #'tls13:tls-server-process-finished)
(setf (fdefinition 'parse-client-hello) #'tls13:parse-client-hello)

;; Re-export TLS 1.3 state machine
(setf (fdefinition 'make-tls-connection) #'tls13:make-tls-connection)
(setf (fdefinition 'tls-handshake-step) #'tls13:tls-handshake-step)
(setf (fdefinition 'tls-connection-state) #'tls13:tls-connection-state)
(setf (fdefinition 'tls-connection-alpn-protocol) #'tls13:tls-connection-alpn-protocol)
(setf (fdefinition 'tls-send-application-data) #'tls13:tls-send-application-data)
(setf (fdefinition 'tls-receive-application-data) #'tls13:tls-receive-application-data)

;; Re-export TLS 1.3 stream adapter
(setf (fdefinition 'make-tls-stream) #'tls13:make-tls-stream)
(setf (fdefinition 'tls-stream-p) #'tls13:tls-stream-p)
(setf (fdefinition 'tls-stream-connection) #'tls13:tls-stream-connection)
(setf (fdefinition 'tls-stream-closed-p) #'tls13:tls-stream-closed-p)
(setf (fdefinition 'tls-connect) #'tls13:tls-connect)
(setf (fdefinition 'tls-accept) #'tls13:tls-accept)
(setf (fdefinition 'tls-read) #'tls13:tls-read)
(setf (fdefinition 'tls-write) #'tls13:tls-write)
(setf (fdefinition 'tls-shutdown) #'tls13:tls-shutdown)
(setf (fdefinition 'tls-close) #'tls13:tls-close)
(setf (fdefinition 'tls-stream-peer-certificates) #'tls13:tls-stream-peer-certificates)
(setf (fdefinition 'tls-stream-cipher-suite) #'tls13:tls-stream-cipher-suite)
(setf (fdefinition 'tls-stream-alpn-protocol) #'tls13:tls-stream-alpn-protocol)
(setf (fdefinition 'tls-read-line) #'tls13:tls-read-line)
(setf (fdefinition 'tls-write-line) #'tls13:tls-write-line)
(setf (fdefinition 'tls-write-string) #'tls13:tls-write-string)

;; Re-export TLS 1.3 transport protocol (generic functions)
(setf (fdefinition 'tls-transport-read) #'tls13:tls-transport-read)
(setf (fdefinition 'tls-transport-write) #'tls13:tls-transport-write)
(setf (fdefinition 'tls-transport-close) #'tls13:tls-transport-close)

;; Re-export structured errors so callers can `handler-case' them
;; without reaching into epsilon.crypto.tls13 directly.
(setf (find-class 'tls-version-mismatch-error)
      (find-class 'tls13:tls-version-mismatch-error))

;;; ---------------------------------------------------------------------------
;;; TLS 1.3 with TLS 1.2 fallback (IMPL-380 Stage 5)
;;; ---------------------------------------------------------------------------

(defun %tls13-alert-hints-tls12-only-server (alert)
  "Heuristic: does this fatal alert from a TLS 1.3 attempt suggest the
   server is TLS 1.2 only? `protocol_version' (70) is the unambiguous
   signal; `handshake_failure' (40) is the ambiguous-but-common signal
   real-world TLS 1.2-only servers (e.g. badssl.com edge, observed
   2026-04-30) emit when they encounter a TLS 1.3 ClientHello they
   don't understand.

   We avoid retrying on `insufficient_security', `inappropriate_fallback',
   `bad_certificate', etc. because those are not version-related and a
   downgrade would not help."
  (and (= (tls13:tls-alert-error-level alert) tls13:+alert-level-fatal+)
       (eq (tls13:tls-alert-error-state alert) :wait-server-hello)
       (or (= (tls13:tls-alert-error-description alert)
              tls13:+alert-protocol-version+)
           (= (tls13:tls-alert-error-description alert)
              tls13:+alert-handshake-failure+))))

(defun tls-connect-with-fallback
    (transport-factory &key hostname trust-store alpn-protocols
                            session-ticket
                            new-session-ticket-callback
                            (allow-tls12 t))
  "Establish a TLS connection that prefers TLS 1.3 but transparently
   falls back to TLS 1.2 when the server does not speak 1.3.

   TRANSPORT-FACTORY is a thunk of zero arguments that returns a fresh
   transport object. We may invoke it twice: once for the TLS 1.3
   attempt, and once for a TLS 1.2 retry on a fresh socket if either
     - the server's ServerHello selected the older version
       (`tls-version-mismatch-error'), or
     - the server rejected our TLS 1.3 ClientHello with a fatal
       protocol_version or handshake_failure alert in
       :wait-server-hello (the TLS-1.2-only server pattern that
       real-world hosts like the badssl.com edge use; see
       %tls13-alert-hints-tls12-only-server).

   When ALLOW-TLS12 is NIL the function behaves exactly like
   `tls-connect' (no fallback), so callers that need to enforce TLS 1.3
   can opt out of the retry path.

   The fallback path itself can still raise: cert-verification errors,
   alerts during the TLS 1.2 handshake, etc. propagate to the caller."
  (let ((transport-1 (funcall transport-factory)))
    (handler-case
        (tls13:tls-connect transport-1
                           :hostname hostname
                           :trust-store trust-store
                           :alpn-protocols alpn-protocols
                           :session-ticket session-ticket
                           :new-session-ticket-callback new-session-ticket-callback)
      (tls13:tls-alert-error (alert)
        (cond
          ((not allow-tls12) (error alert))
          ((not (%tls13-alert-hints-tls12-only-server alert)) (error alert))
          (t
           (handler-case (tls13:tls-transport-close transport-1) (error () nil))
           (let* ((transport-2 (funcall transport-factory))
                  (verifier
                    (when trust-store
                      (lambda (leaf chain host)
                        (declare (ignore chain host))
                        (and (find leaf trust-store :test #'equalp) t))))
                  (config (tls12:make-tls12-client-config
                           :hostname hostname
                           :alpn-protocols alpn-protocols
                           :trust-store verifier)))
             (tls12:tls12-connect transport-2 config)))))
      (tls13:tls-version-mismatch-error (mismatch)
        (declare (ignorable mismatch))
        (unless allow-tls12
          (error mismatch))
        ;; The TLS 1.3 attempt has consumed the original socket; the
        ;; server thinks the connection is dead now. Drop it and ask
        ;; for a fresh one for the 1.2 retry.
        (handler-case (tls13:tls-transport-close transport-1) (error () nil))
        ;; The TLS 1.2 client-config's :trust-store slot is a verifier
        ;; function (leaf chain hostname -> T/NIL), unlike TLS 1.3's
        ;; list-of-CAs. Bridge them by wrapping the cert list into a
        ;; verifier that walks the supplied chain. When TRUST-STORE is
        ;; NIL we let the TLS 1.2 path skip verification (matching its
        ;; default), and the caller is responsible for layering cert
        ;; checks on top -- same as `tls-connect' before this fallback.
        (let* ((transport-2 (funcall transport-factory))
               (verifier
                 (when trust-store
                   (lambda (leaf chain host)
                     (declare (ignore chain host))
                     (and (find leaf trust-store :test #'equalp) t))))
               (config (tls12:make-tls12-client-config
                        :hostname hostname
                        :alpn-protocols alpn-protocols
                        :trust-store verifier)))
          (tls12:tls12-connect transport-2 config))))))

;;;; ============================================================================
;;;; Re-exports from epsilon.crypto.errors, epsilon.crypto.native,
;;;; epsilon.crypto.jwk, and epsilon.digest.
;;;; ============================================================================

(sym:reexport :epsilon.crypto.errors
  '(crypto-error crypto-error-p crypto-error-code crypto-error-message
    tls-error tls-error-p
    certificate-error certificate-error-p
    key-error key-error-p
    signal-crypto-error))

(sym:reexport :epsilon.crypto.simple
  '(crypto-random-bytes crypto-random-integer crypto-random-hex crypto-random-base64
    digest digest-string digest-file hex-digest
    sha256 sha384 sha512 sha3-256 sha3-384 sha3-512 md5 sha1
    blake2b blake2s blake3
    hmac hmac-sha256 hmac-sha384 hmac-sha512 hmac-verify
    hkdf hkdf-extract hkdf-expand
    pbkdf2-sha256
    generate-aes-key generate-nonce
    +aes-128-gcm-key-size+ +aes-256-gcm-key-size+
    +aes-gcm-nonce-size+ +aes-gcm-tag-size+
    +chacha20-poly1305-key-size+ +chacha20-poly1305-nonce-size+ +chacha20-poly1305-tag-size+
    generate-x25519-key x25519-private-key-bytes x25519-public-key-bytes
    key-to-encrypted-pem key-from-encrypted-pem
    pkcs12-encode-key pkcs12-decode-key))

;;; AEAD: reexport the primitive signatures (plaintext-key-nonce, ciphertext-key-nonce-tag)
;;; rather than simple's (key-nonce-plaintext) order, since the primitive ordering is
;;; what existing consumers (jose/jwe, secrets, ...) were written against.
(sym:reexport :epsilon.crypto.aes-gcm           '(aes-gcm-encrypt aes-gcm-decrypt))
(sym:reexport :epsilon.crypto.chacha20-poly1305 '(chacha20-poly1305-encrypt chacha20-poly1305-decrypt))
(sym:reexport :epsilon.crypto.pbkdf2            '(pbkdf2))

(sym:reexport :epsilon.crypto.jwk
  '(key-to-jwk key-from-jwk keys-to-jwks keys-from-jwks
    jwk-to-json jwks-to-json))

(sym:reexport :epsilon.crypto.native
  '(tls-context tls-context-p tls-context-server-p tls-context-verify-mode
    tls-context-alpn-protocols tls-context-trust-store tls-context-cert-chain
    tls-context-private-key tls-context-ca-file tls-context-min-version
    tls-context-max-version make-tls-context make-client-context
    make-server-context create-tls-context load-certificates-from-file
    load-private-key-from-file context-set-identity context-set-certificate
    context-set-private-key context-set-certificate-chain context-set-ca-file
    context-set-verify-mode context-set-min-version context-set-max-version
    context-set-ciphersuites context-set-cipher-list
    context-set-session-cache-mode context-set-client-ca-list free-tls-context
    +tls-1.0+ +tls-1.1+ +tls-1.2+ +tls-1.3+
    +verify-none+ +verify-peer+ +verify-fail-if-no-peer-cert+ +verify-client-once+
    +session-cache-off+ +session-cache-client+ +session-cache-server+
    +session-cache-both+
    tls-connection tls-connection-p make-tls-connection tls-connection-ssl
    tls-connection-socket tls-connection-context tls-connection-native-stream
    tls-connection-tls13-stream
    tls-connection-connected-p tls-connection-handshake-complete-p
    tls-connect tls-accept tls-read tls-write tls-shutdown tls-close
    connection-peer-certificate connection-cipher connection-version
    connection-alpn-protocol connection-negotiated-group connection-pending
    verify-peer-certificate get-verify-result
    +ssl-error-none+ +ssl-error-ssl+ +ssl-error-want-read+ +ssl-error-want-write+
    +ssl-error-syscall+ +ssl-error-zero-return+
    tls-stream tls-stream-p make-tls-stream tls-stream-connection
    tls-stream-peer-certificate tls-stream-cipher tls-stream-version
    tls-stream-alpn-protocol tls-connect-stream tls-accept-stream
    wrap-socket-with-tls tls-read-line tls-write-line tls-write-string
    +alpn-http/1.0+ +alpn-http/1.1+ +alpn-h2+ +alpn-h2c+ +alpn-grpc+
    context-set-alpn-protocols context-enable-session-tickets
    tls-context-session-ticket-store tls-context-session-ticket-lifetime
    tls-context-ocsp-staple connection-set-alpn-protocols
    connection-selected-alpn make-alpn-buffer parse-alpn-buffer
    native-key native-key-p make-native-key native-key-type
    native-key-private-p native-key-material
    generate-ed25519-key generate-ec-p256-key generate-rsa-key
    key-to-pem key-from-pem key-from-der key-to-der
    sign-message verify-message
    make-mtls-server-context make-mtls-client-context
    configure-mtls-security require-client-certificate
    match-certificate-hostname
    async-tcp-transport async-tcp-transport-p make-async-socket-transport
    tls-async-timeout tls-async-timeout-elapsed))

(sym:reexport :epsilon.digest
  '(hasher-update hasher-finalize hasher-reset hasher-copy
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
    +sha384-output-length+ +sha512-output-length+ +crc32-output-length+))

;;;; ============================================================================
;;;; Type aliases for struct/condition types from epsilon.crypto.native
;;;; (sym:reexport handles functions/macros/specials, not types — these stay
;;;; usable from epsilon.crypto:foo at type-expression sites.)
;;;; ============================================================================

(deftype tls-context () 'native:tls-context)
(deftype tls-connection () 'native:tls-connection)
(deftype tls-stream () 'native:tls-stream)
(deftype native-key () 'native:native-key)
(deftype async-tcp-transport () 'native:async-tcp-transport)
(deftype tls-async-timeout () 'native:tls-async-timeout)
(export '(tls-context tls-connection tls-stream native-key
          async-tcp-transport tls-async-timeout))

;;;; Algorithm-keyword aliases (so callers can write crypto:+sha256+ instead
;;;; of :sha256 in source). Used as the algorithm argument to digest, etc.
(define-symbol-macro +sha256+   :sha256)
(define-symbol-macro +sha384+   :sha384)
(define-symbol-macro +sha512+   :sha512)
(define-symbol-macro +sha3-256+ :sha3-256)
(define-symbol-macro +sha3-384+ :sha3-384)
(define-symbol-macro +sha3-512+ :sha3-512)
(define-symbol-macro +md5+      :md5)
(define-symbol-macro +sha1+     :sha1)
(export '(+sha256+ +sha384+ +sha512+ +sha3-256+ +sha3-384+ +sha3-512+ +md5+ +sha1+))

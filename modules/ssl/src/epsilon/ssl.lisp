;;;; Epsilon Crypto Native
;;;;
;;;; Top-level package for the pure-Lisp cryptographic library.
;;;; This module provides a zero-C-dependency implementation of
;;;; all cryptographic primitives needed for TLS 1.3.
;;;;
;;;; The only FFI used is for platform entropy (getrandom/getentropy).

(defpackage epsilon.ssl
  (:use :cl)
  (:local-nicknames
   (#:ct #:epsilon.ssl.ct)
   (#:entropy #:epsilon.ssl.entropy)
   (#:sha256 #:epsilon.ssl.sha256)
   (#:sha512 #:epsilon.ssl.sha512)
   (#:sha3 #:epsilon.ssl.sha3)
   (#:blake2 #:epsilon.ssl.blake2)
   (#:md5 #:epsilon.ssl.md5)
   (#:sha1 #:epsilon.ssl.sha1)
   (#:hmac #:epsilon.ssl.hmac)
   (#:hkdf #:epsilon.ssl.hkdf)
   (#:drbg #:epsilon.ssl.drbg)
   (#:poly1305 #:epsilon.ssl.poly1305)
   (#:pbkdf2 #:epsilon.ssl.pbkdf2)
   (#:scrypt #:epsilon.ssl.scrypt)
   (#:aes #:epsilon.ssl.aes)
   (#:ghash #:epsilon.ssl.ghash)
   (#:aes-gcm #:epsilon.ssl.aes-gcm)
   (#:chacha20 #:epsilon.ssl.chacha20)
   (#:chacha20-poly1305 #:epsilon.ssl.chacha20-poly1305)
   (#:mod-arith #:epsilon.ssl.modular)
   (#:fe25519 #:epsilon.ssl.field-25519)
   (#:p256 #:epsilon.ssl.field-p256)
   (#:x25519 #:epsilon.ssl.curve25519)
   (#:ed25519 #:epsilon.ssl.ed25519)
   (#:ec-p256 #:epsilon.ssl.ec-p256)
   (#:rsa #:epsilon.ssl.rsa)
   (#:ed-sign #:epsilon.ssl.ed25519-sign)
   (#:ecdsa #:epsilon.ssl.ecdsa)
   (#:ecdh #:epsilon.ssl.ecdh)
   (#:asn1 #:epsilon.ssl.asn1)
   (#:pem #:epsilon.ssl.pem)
   (#:pkcs #:epsilon.ssl.pkcs)
   (#:x509 #:epsilon.ssl.x509)
   (#:tls13 #:epsilon.ssl.tls13)
   )
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

   ;; AES-GCM
   #:aes-gcm-encrypt
   #:aes-gcm-decrypt

   ;; ChaCha20
   #:chacha20-encrypt
   #:chacha20-block

   ;; ChaCha20-Poly1305
   #:chacha20-poly1305-encrypt
   #:chacha20-poly1305-decrypt

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

(in-package :epsilon.ssl)

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

;; Re-export AES-GCM
(setf (fdefinition 'aes-gcm-encrypt) #'aes-gcm:aes-gcm-encrypt)
(setf (fdefinition 'aes-gcm-decrypt) #'aes-gcm:aes-gcm-decrypt)

;; Re-export ChaCha20
(setf (fdefinition 'chacha20-encrypt) #'chacha20:chacha20-encrypt)
(setf (fdefinition 'chacha20-block) #'chacha20:chacha20-block)

;; Re-export ChaCha20-Poly1305
(setf (fdefinition 'chacha20-poly1305-encrypt) #'chacha20-poly1305:chacha20-poly1305-encrypt)
(setf (fdefinition 'chacha20-poly1305-decrypt) #'chacha20-poly1305:chacha20-poly1305-decrypt)

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

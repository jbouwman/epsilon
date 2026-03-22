;;;; epsilon.crypto compatibility API
;;;;
;;;; Provides functions matching the epsilon.crypto public interface,
;;;; backed by pure-Lisp implementations from epsilon.ssl.
;;;; This enables drop-in replacement of the OpenSSL-based backend.

(defpackage epsilon.ssl.compat
  (:use :cl)
  (:local-nicknames
   (#:sha256 #:epsilon.ssl.sha256)
   (#:sha512 #:epsilon.ssl.sha512)
   (#:sha3 #:epsilon.ssl.sha3)
   (#:sha1 #:epsilon.ssl.sha1)
   (#:md5 #:epsilon.ssl.md5)
   (#:blake2 #:epsilon.ssl.blake2)
   (#:hmac #:epsilon.ssl.hmac)
   (#:hkdf #:epsilon.ssl.hkdf)
   (#:pbkdf2 #:epsilon.ssl.pbkdf2)
   (#:scrypt-mod #:epsilon.ssl.scrypt)
   (#:aes-gcm #:epsilon.ssl.aes-gcm)
   (#:chacha #:epsilon.ssl.chacha20-poly1305)
   (#:drbg #:epsilon.ssl.drbg)
   (#:x25519 #:epsilon.ssl.curve25519)
   (#:ed-sign #:epsilon.ssl.ed25519-sign)
   (#:ecdsa #:epsilon.ssl.ecdsa)
   (#:ecdh #:epsilon.ssl.ecdh)
   (#:rsa #:epsilon.ssl.rsa)
   (#:pem #:epsilon.ssl.pem)
   (#:x509 #:epsilon.ssl.x509)
   (#:tls #:epsilon.ssl.tls13))
  (:export
   ;; Random
   #:crypto-random-bytes
   #:crypto-random-integer
   #:crypto-random-hex
   ;; Hashing
   #:digest
   #:digest-string
   #:hex-digest
   #:sha256
   #:sha384
   #:sha512
   #:sha3-256
   #:sha3-384
   #:sha3-512
   #:md5
   #:sha1
   #:blake2b
   #:blake2s
   #:blake3
   ;; HMAC
   #:hmac
   #:hmac-sha256
   #:hmac-sha384
   #:hmac-sha512
   #:hmac-verify
   ;; KDF
   #:hkdf
   #:hkdf-extract
   #:hkdf-expand
   #:pbkdf2
   #:pbkdf2-sha256
   #:scrypt
   ;; AEAD
   #:aes-gcm-encrypt
   #:aes-gcm-decrypt
   #:chacha20-poly1305-encrypt
   #:chacha20-poly1305-decrypt
   #:generate-aes-key
   #:generate-nonce
   ;; Constants
   #:+aes-128-gcm-key-size+
   #:+aes-256-gcm-key-size+
   #:+aes-gcm-nonce-size+
   #:+aes-gcm-tag-size+
   #:+chacha20-poly1305-key-size+
   #:+chacha20-poly1305-nonce-size+
   #:+chacha20-poly1305-tag-size+
   ;; TLS
   #:tls-connect-stream
   #:tls-accept-stream
   #:tls-read
   #:tls-write
   #:tls-read-line
   #:tls-write-line
   #:tls-write-string
   #:tls-shutdown
   #:tls-close
   #:tls-stream-p
   #:tls-stream-peer-certificate
   #:tls-stream-cipher
   #:tls-stream-alpn-protocol
   ;; X.509
   #:load-certificate
   #:certificate-subject
   #:certificate-issuer
   #:certificate-not-before
   #:certificate-not-after
   #:verify-certificate-chain))

(in-package :epsilon.ssl.compat)

;;; ---------------------------------------------------------------------------
;;; Internal helpers
;;; ---------------------------------------------------------------------------

(defun ensure-bytes (data)
  "Coerce DATA to a byte vector. Accepts byte vectors and strings."
  (etypecase data
    ((simple-array (unsigned-byte 8) (*)) data)
    ((array (unsigned-byte 8) (*))
     (let ((v (make-array (length data) :element-type '(unsigned-byte 8))))
       (replace v data)
       v))
    (string (map '(vector (unsigned-byte 8)) #'char-code data))))

(defun bytes-to-hex (bytes)
  "Convert a byte vector to a lowercase hex string."
  (let ((hex (make-string (* 2 (length bytes)))))
    (loop for b across bytes
          for i from 0 by 2
          do (let ((hi (ash b -4))
                   (lo (logand b #xf)))
               (setf (char hex i) (char "0123456789abcdef" hi))
               (setf (char hex (1+ i)) (char "0123456789abcdef" lo))))
    hex))

;;; ---------------------------------------------------------------------------
;;; Random number generation
;;; ---------------------------------------------------------------------------

(defun crypto-random-bytes (size)
  "Generate SIZE cryptographically secure random bytes."
  (drbg:random-bytes size))

(defun crypto-random-integer (max)
  "Generate a random integer in [0, MAX)."
  (drbg:random-integer max))

(defun crypto-random-hex (size)
  "Generate SIZE random bytes as a hex string."
  (bytes-to-hex (drbg:random-bytes size)))

;;; ---------------------------------------------------------------------------
;;; Hash functions
;;; ---------------------------------------------------------------------------

(defun digest (algorithm data)
  "Compute a message digest. ALGORITHM is a keyword (:sha256, :sha384, etc.)."
  (let ((bytes (etypecase data
                 ((simple-array (unsigned-byte 8) (*)) data)
                 (string (map '(vector (unsigned-byte 8)) #'char-code data)))))
    (ecase algorithm
      (:sha256 (sha256:sha256 bytes))
      (:sha384 (sha512:sha384 bytes))
      (:sha512 (sha512:sha512 bytes))
      (:sha3-256 (sha3:sha3-256 bytes))
      (:sha3-384 (sha3:sha3-384 bytes))
      (:sha3-512 (sha3:sha3-512 bytes))
      (:sha1 (sha1:sha1 bytes))
      (:md5 (md5:md5 bytes)))))

(defun digest-string (algorithm string)
  "Compute a digest of a string."
  (digest algorithm string))

(defun hex-digest (algorithm data)
  "Compute a digest and return as hex string."
  (bytes-to-hex (digest algorithm data)))

(defun sha256 (data)
  "Compute SHA-256 hash."
  (digest :sha256 data))

(defun sha384 (data)
  "Compute SHA-384 hash."
  (digest :sha384 data))

(defun sha512 (data)
  "Compute SHA-512 hash."
  (digest :sha512 data))

(defun sha3-256 (data)
  "Compute SHA3-256 hash."
  (digest :sha3-256 data))

(defun sha3-384 (data)
  "Compute SHA3-384 hash."
  (digest :sha3-384 data))

(defun sha3-512 (data)
  "Compute SHA3-512 hash."
  (digest :sha3-512 data))

(defun md5 (data)
  "Compute MD5 hash."
  (digest :md5 data))

(defun sha1 (data)
  "Compute SHA-1 hash."
  (digest :sha1 data))

(defun blake2b (data &key (digest-length 64) key)
  "Compute BLAKE2b hash."
  (let ((bytes (ensure-bytes data)))
    (if key
        (blake2:blake2b bytes :key key :digest-length digest-length)
        (blake2:blake2b bytes :digest-length digest-length))))

(defun blake2s (data &key (digest-length 32) key)
  "Compute BLAKE2s hash."
  (let ((bytes (ensure-bytes data)))
    (if key
        (blake2:blake2s bytes :key key :digest-length digest-length)
        (blake2:blake2s bytes :digest-length digest-length))))

(defun blake3 (data)
  "Compute BLAKE3 hash."
  (let* ((pkg (find-package "EPSILON.DIGEST.BLAKE3"))
         (fn (symbol-function (intern "BLAKE3" pkg))))
    (funcall fn (ensure-bytes data))))

;;; ---------------------------------------------------------------------------
;;; HMAC
;;; ---------------------------------------------------------------------------

(defun hmac (algorithm key message)
  "Compute HMAC with the given hash algorithm."
  (hmac:hmac algorithm (ensure-bytes key) (ensure-bytes message)))

(defun hmac-sha256 (key message)
  "Compute HMAC-SHA256."
  (hmac:hmac :sha256 (ensure-bytes key) (ensure-bytes message)))

(defun hmac-sha384 (key message)
  "Compute HMAC-SHA384."
  (hmac:hmac :sha384 (ensure-bytes key) (ensure-bytes message)))

(defun hmac-sha512 (key message)
  "Compute HMAC-SHA512."
  (hmac:hmac :sha512 (ensure-bytes key) (ensure-bytes message)))

(defun hmac-verify (algorithm key message expected-mac)
  "Verify an HMAC. Returns T if the MAC matches."
  (let ((computed (hmac algorithm key message)))
    (and (= (length computed) (length expected-mac))
         (loop for a across computed
               for b across expected-mac
               always (= a b)))))

;;; ---------------------------------------------------------------------------
;;; Key derivation
;;; ---------------------------------------------------------------------------

(defun hkdf (algorithm ikm salt info length)
  "HKDF key derivation (RFC 5869)."
  (let ((prk (hkdf:hkdf-extract algorithm salt ikm)))
    (hkdf:hkdf-expand algorithm prk info length)))

(defun hkdf-extract (algorithm salt ikm)
  "HKDF-Extract step."
  (hkdf:hkdf-extract algorithm salt ikm))

(defun hkdf-expand (algorithm prk info length)
  "HKDF-Expand step."
  (hkdf:hkdf-expand algorithm prk info length))

(defun pbkdf2 (algorithm password salt &key (iterations 200000) (key-length 32))
  "PBKDF2 key derivation."
  (let ((pw-bytes (etypecase password
                    ((simple-array (unsigned-byte 8) (*)) password)
                    (string (map '(vector (unsigned-byte 8)) #'char-code password))))
        (salt-bytes (etypecase salt
                      ((simple-array (unsigned-byte 8) (*)) salt)
                      (string (map '(vector (unsigned-byte 8)) #'char-code salt)))))
    (pbkdf2:pbkdf2 algorithm pw-bytes salt-bytes iterations key-length)))

(defun pbkdf2-sha256 (password salt &key (iterations 200000) (key-length 32))
  "PBKDF2 with SHA-256."
  (pbkdf2 :sha256 password salt :iterations iterations :key-length key-length))

(defun scrypt (password salt &key (n 32768) (r 8) (p 1) (key-length 32))
  "Scrypt key derivation."
  (let ((pw-bytes (etypecase password
                    ((simple-array (unsigned-byte 8) (*)) password)
                    (string (map '(vector (unsigned-byte 8)) #'char-code password))))
        (salt-bytes (etypecase salt
                      ((simple-array (unsigned-byte 8) (*)) salt)
                      (string (map '(vector (unsigned-byte 8)) #'char-code salt)))))
    (scrypt-mod:scrypt pw-bytes salt-bytes n r p key-length)))

;;; ---------------------------------------------------------------------------
;;; AEAD encryption
;;; ---------------------------------------------------------------------------

(defconstant +aes-128-gcm-key-size+ 16)
(defconstant +aes-256-gcm-key-size+ 32)
(defconstant +aes-gcm-nonce-size+ 12)
(defconstant +aes-gcm-tag-size+ 16)
(defconstant +chacha20-poly1305-key-size+ 32)
(defconstant +chacha20-poly1305-nonce-size+ 12)
(defconstant +chacha20-poly1305-tag-size+ 16)

(defun aes-gcm-encrypt (key nonce plaintext &key (aad (make-array 0 :element-type '(unsigned-byte 8))))
  "AES-GCM encrypt. Returns (values ciphertext tag)."
  (aes-gcm:aes-gcm-encrypt plaintext key nonce :aad aad))

(defun aes-gcm-decrypt (key nonce ciphertext tag &key (aad (make-array 0 :element-type '(unsigned-byte 8))))
  "AES-GCM decrypt. Returns plaintext or signals error on auth failure."
  (aes-gcm:aes-gcm-decrypt ciphertext key nonce tag :aad aad))

(defun chacha20-poly1305-encrypt (key nonce plaintext &key (aad (make-array 0 :element-type '(unsigned-byte 8))))
  "ChaCha20-Poly1305 encrypt. Returns (values ciphertext tag)."
  (chacha:chacha20-poly1305-encrypt plaintext key nonce :aad aad))

(defun chacha20-poly1305-decrypt (key nonce ciphertext tag &key (aad (make-array 0 :element-type '(unsigned-byte 8))))
  "ChaCha20-Poly1305 decrypt. Returns plaintext or signals error."
  (chacha:chacha20-poly1305-decrypt ciphertext key nonce tag :aad aad))

(defun generate-aes-key (&key (bits 256))
  "Generate a random AES key."
  (drbg:random-bytes (/ bits 8)))

(defun generate-nonce (&key (size 12))
  "Generate a random nonce."
  (drbg:random-bytes size))

;;; ---------------------------------------------------------------------------
;;; TLS (compatibility wrappers)
;;; ---------------------------------------------------------------------------

(defun tls-connect-stream (transport &key hostname alpn-protocols trust-store)
  "Establish a TLS 1.3 connection. Returns a TLS stream.
   TRANSPORT is an object implementing tls-transport-read/write/close."
  (tls:tls-connect transport
                   :hostname hostname
                   :alpn-protocols alpn-protocols
                   :trust-store trust-store))

(defun tls-accept-stream (transport config)
  "Accept a TLS 1.3 connection. Returns a TLS stream."
  (tls:tls-accept transport config))

(defun tls-read (stream buffer &key (start 0) (end (length buffer)))
  "Read from a TLS stream."
  (tls:tls-read stream buffer :start start :end end))

(defun tls-write (stream buffer &key (start 0) (end (length buffer)))
  "Write to a TLS stream."
  (tls:tls-write stream buffer :start start :end end))

(defun tls-read-line (stream &key (max-length 8192))
  "Read a line from a TLS stream."
  (tls:tls-read-line stream :max-length max-length))

(defun tls-write-line (stream string)
  "Write a line to a TLS stream."
  (tls:tls-write-line stream string))

(defun tls-write-string (stream string)
  "Write a string to a TLS stream."
  (tls:tls-write-string stream string))

(defun tls-shutdown (stream)
  "Send close_notify alert."
  (tls:tls-shutdown stream))

(defun tls-close (stream)
  "Close the TLS stream and transport."
  (tls:tls-close stream))

(defun tls-stream-p (obj)
  "Test if OBJ is a TLS stream."
  (tls:tls-stream-p obj))

(defun tls-stream-peer-certificate (stream)
  "Return the peer's certificate chain."
  (tls:tls-stream-peer-certificates stream))

(defun tls-stream-cipher (stream)
  "Return the negotiated cipher suite name as a string."
  (let ((suite (tls:tls-stream-cipher-suite stream)))
    (case suite
      (#.tls:+tls-aes-128-gcm-sha256+ "TLS_AES_128_GCM_SHA256")
      (#.tls:+tls-aes-256-gcm-sha384+ "TLS_AES_256_GCM_SHA384")
      (#.tls:+tls-chacha20-poly1305-sha256+ "TLS_CHACHA20_POLY1305_SHA256")
      (t (format nil "UNKNOWN(#x~4,'0X)" suite)))))

(defun tls-stream-alpn-protocol (stream)
  "Return the negotiated ALPN protocol."
  (tls:tls-stream-alpn-protocol stream))

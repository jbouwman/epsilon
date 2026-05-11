;;;; epsilon.crypto.simple - One-shot convenience wrappers over the primitive submodules.
;;;;
;;;; Mirrors the epsilon.http.simple shape: low-friction one-call APIs (digest, hmac,
;;;; aead, pbkdf2, ...) on top of the streaming/state-machine submodules
;;;; (epsilon.crypto.sha256, epsilon.crypto.aes-gcm, ...). Re-exported by
;;;; epsilon.crypto so callers don't need to know which submodule a primitive lives in.

(defpackage epsilon.crypto.simple
  (:use :cl)
  (:import
   (epsilon.crypto.sha256 sha256)
   (epsilon.crypto.sha512 sha512)
   (epsilon.crypto.sha3 sha3)
   (epsilon.crypto.sha1 sha1)
   (epsilon.crypto.md5 md5)
   (epsilon.crypto.blake2 blake2)
   (epsilon.crypto.hmac hmac)
   (epsilon.crypto.hkdf hkdf)
   (epsilon.crypto.pbkdf2 pbkdf2)
   (epsilon.crypto.aes-gcm aes-gcm)
   (epsilon.crypto.chacha20-poly1305 chacha)
   (epsilon.crypto.drbg drbg)
   (epsilon.crypto.curve25519 x25519)
   (epsilon.crypto.ed25519-sign ed-sign)
   (epsilon.crypto.ecdsa ecdsa)
   (epsilon.crypto.ecdh ecdh)
   (epsilon.crypto.rsa rsa)
   (epsilon.crypto.pem pem)
   (epsilon.crypto.x509 x509)
   (epsilon.crypto.tls13 tls)
   (epsilon.crypto.pbes2 pbes2)
   (epsilon.crypto.pkcs12 pkcs12)
   (epsilon.crypto.native native)
   (epsilon.encode b64))
  (:export
   ;; Random
   #:crypto-random-bytes
   #:crypto-random-integer
   #:crypto-random-hex
   #:crypto-random-base64
   ;; Hashing
   #:digest
   #:digest-string
   #:digest-file
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
   #:verify-certificate-chain
   ;; X25519 keypair convenience
   #:x25519-keypair
   #:x25519-keypair-p
   #:generate-x25519-key
   #:x25519-private-key-bytes
   #:x25519-public-key-bytes
   ;; Encrypted PEM (PBES2 + PKCS#8)
   #:key-to-encrypted-pem
   #:key-from-encrypted-pem
   ;; PKCS#12 PFX
   #:pkcs12-encode-key
   #:pkcs12-decode-key))

(in-package :epsilon.crypto.simple)

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

(defun crypto-random-base64 (size)
  "Generate SIZE random bytes encoded as a base64 string."
  (b64:base64-encode (drbg:random-bytes size)))

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

;;; ---------------------------------------------------------------------------
;;; File hashing
;;; ---------------------------------------------------------------------------

(defun digest-file (pathname &optional (algorithm :sha256))
  "Compute a message digest of the file at PATHNAME using ALGORITHM
   (a keyword like :sha256). Reads the file as binary and applies DIGEST.
   For very large files, prefer hash-reader from epsilon.digest with a
   buffered reader."
  (let ((bytes (with-open-file (stream pathname :element-type '(unsigned-byte 8))
                 (let ((buf (make-array (file-length stream)
                                        :element-type '(unsigned-byte 8))))
                   (read-sequence buf stream)
                   buf))))
    (digest algorithm bytes)))

;;; ---------------------------------------------------------------------------
;;; X25519 keypair convenience (for age, ECDH, ...)
;;; ---------------------------------------------------------------------------

(defstruct (x25519-keypair (:constructor %make-x25519-keypair (private public)))
  (private nil :type (simple-array (unsigned-byte 8) (32)))
  (public  nil :type (simple-array (unsigned-byte 8) (32))))

(defun generate-x25519-key ()
  "Generate a fresh X25519 keypair using the DRBG. Returns an x25519-keypair."
  (let* ((priv (drbg:random-bytes 32))
         (pub  (x25519:x25519-base priv)))
    (%make-x25519-keypair priv pub)))

(defun x25519-private-key-bytes (keypair)
  "Get the 32-byte private key from a keypair."
  (x25519-keypair-private keypair))

(defun x25519-public-key-bytes (keypair)
  "Get the 32-byte public key from a keypair."
  (x25519-keypair-public keypair))

;;; ---------------------------------------------------------------------------
;;; Encrypted PEM (PBES2 over a PKCS#8 PrivateKeyInfo)
;;; ---------------------------------------------------------------------------

(defun key-to-encrypted-pem (key password &rest pbes2-args
                                          &key &allow-other-keys)
  "Export a private native-key as a PEM-armoured ENCRYPTED PRIVATE KEY
   (PBES2 over a PKCS#8 PrivateKeyInfo). PBES2-ARGS are forwarded to
   pbes2:encrypted-pkcs8-to-pem and let callers override defaults
   such as cipher, PRF, and iteration count."
  (unless (native:native-key-private-p key)
    (error "key-to-encrypted-pem: cannot encrypt a public key"))
  (let ((pkcs8-der (native:key-to-der key :private-p t :format :pkcs8)))
    (apply #'pbes2:encrypted-pkcs8-to-pem pkcs8-der password pbes2-args)))

(defun key-from-encrypted-pem (pem-string password)
  "Parse a PEM-armoured ENCRYPTED PRIVATE KEY block and return the
   decrypted native-key. Signals an error on wrong password."
  (let ((pkcs8-der (pbes2:encrypted-pkcs8-from-pem pem-string password)))
    (native:key-from-der pkcs8-der :private-p t)))

;;; ---------------------------------------------------------------------------
;;; PKCS#12 PFX (a private key + optional certificates, password-protected)
;;; ---------------------------------------------------------------------------

(defun pkcs12-encode-key (key password &key certificates
                                            (iterations 210000)
                                            (mac-iterations 2048)
                                            (cipher :aes-256-cbc)
                                            (prf :hmac-sha256))
  "Build a PKCS#12 PFX byte vector containing the private KEY (a native-key)
   and optionally CERTIFICATES (a list of X.509 certificate DER byte vectors),
   all protected by PASSWORD."
  (unless (native:native-key-private-p key)
    (error "pkcs12-encode-key: cannot export a public key"))
  (let ((pkcs8-der (native:key-to-der key :private-p t :format :pkcs8)))
    (pkcs12:pkcs12-encode
     :certificates certificates
     :private-key pkcs8-der
     :password password
     :iterations iterations
     :mac-iterations mac-iterations
     :cipher cipher
     :prf prf)))

(defun pkcs12-decode-key (pfx-bytes password)
  "Parse a PKCS#12 PFX byte vector and return (values CERTIFICATE-DER-LIST
   NATIVE-KEY). Signals an error on a bad password, a tampered PFX, or any
   unsupported algorithm."
  (multiple-value-bind (certs pkcs8-der)
      (pkcs12:pkcs12-decode pfx-bytes password)
    (values certs
            (and pkcs8-der (native:key-from-der pkcs8-der :private-p t)))))

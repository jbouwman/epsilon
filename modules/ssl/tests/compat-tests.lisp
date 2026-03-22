;;;; Tests for epsilon.crypto compatibility API

(defpackage epsilon.ssl.compat-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:compat #:epsilon.ssl.compat))
  (:enter t))

(in-package :epsilon.ssl.compat-tests)

;;; ---------------------------------------------------------------------------
;;; Random generation
;;; ---------------------------------------------------------------------------

(deftest test-crypto-random-bytes
  "Generate random bytes"
  (let ((bytes (compat:crypto-random-bytes 32)))
    (assert-= (length bytes) 32)
    ;; Should not be all zeros
    (assert-true (some #'plusp bytes))))

(deftest test-crypto-random-integer
  "Generate random integer in range"
  (let ((n (compat:crypto-random-integer 1000)))
    (assert-true (>= n 0))
    (assert-true (< n 1000))))

(deftest test-crypto-random-hex
  "Generate random hex string"
  (let ((hex (compat:crypto-random-hex 16)))
    (assert-= (length hex) 32)  ; 16 bytes = 32 hex chars
    (assert-true (every (lambda (c) (or (digit-char-p c) (find c "abcdef"))) hex))))

;;; ---------------------------------------------------------------------------
;;; Hash functions
;;; ---------------------------------------------------------------------------

(deftest test-digest-sha256
  "SHA-256 via digest interface"
  (let ((hash (compat:digest :sha256 "")))
    (assert-= (length hash) 32))
  ;; String input
  (let ((hash (compat:digest-string :sha256 "hello")))
    (assert-= (length hash) 32)))

(deftest test-digest-sha384
  "SHA-384 via digest interface"
  (let ((hash (compat:digest :sha384 "")))
    (assert-= (length hash) 48)))

(deftest test-digest-sha512
  "SHA-512 via digest interface"
  (let ((hash (compat:digest :sha512 "")))
    (assert-= (length hash) 64)))

(deftest test-digest-sha3
  "SHA-3 variants via digest interface"
  (assert-= (length (compat:sha3-256 "test")) 32)
  (assert-= (length (compat:sha3-384 "test")) 48)
  (assert-= (length (compat:sha3-512 "test")) 64))

(deftest test-digest-md5-sha1
  "MD5 and SHA-1 via digest interface"
  (assert-= (length (compat:md5 "test")) 16)
  (assert-= (length (compat:sha1 "test")) 20))

(deftest test-hex-digest
  "Hex digest output"
  (let ((hex (compat:hex-digest :sha256 "")))
    (assert-= (length hex) 64)  ; 32 bytes = 64 hex chars
    (assert-equal hex "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")))

(deftest test-blake2b
  "BLAKE2b via compatibility API"
  (assert-= (length (compat:blake2b "test")) 64)
  (assert-= (length (compat:blake2b "test" :digest-length 32)) 32))

(deftest test-blake2s
  "BLAKE2s via compatibility API"
  (assert-= (length (compat:blake2s "test")) 32)
  (assert-= (length (compat:blake2s "test" :digest-length 16)) 16))

(deftest test-blake3
  "BLAKE3 via compatibility API (requires epsilon.crypto loaded)"
  (when (find-package "EPSILON.DIGEST.BLAKE3")
    (assert-= (length (compat:blake3 "test")) 32)))

;;; ---------------------------------------------------------------------------
;;; HMAC
;;; ---------------------------------------------------------------------------

(deftest test-hmac-sha256
  "HMAC-SHA256"
  (let* ((key (compat:crypto-random-bytes 32))
         (mac (compat:hmac-sha256 key "test message")))
    (assert-= (length mac) 32)
    ;; Should be consistent
    (assert-equalp mac (compat:hmac :sha256 key "test message"))))

(deftest test-hmac-sha384
  "HMAC-SHA384"
  (let* ((key (compat:crypto-random-bytes 32))
         (mac (compat:hmac-sha384 key "test message")))
    (assert-= (length mac) 48)))

(deftest test-hmac-sha512
  "HMAC-SHA512"
  (let* ((key (compat:crypto-random-bytes 32))
         (mac (compat:hmac-sha512 key "test message")))
    (assert-= (length mac) 64)))

(deftest test-hmac-verify
  "HMAC verification"
  (let* ((key (compat:crypto-random-bytes 32))
         (mac (compat:hmac-sha256 key "hello")))
    (assert-true (compat:hmac-verify :sha256 key "hello" mac))
    (assert-not (compat:hmac-verify :sha256 key "world" mac))))

;;; ---------------------------------------------------------------------------
;;; Key derivation
;;; ---------------------------------------------------------------------------

(deftest test-hkdf
  "HKDF key derivation"
  (let* ((ikm (compat:crypto-random-bytes 32))
         (salt (compat:crypto-random-bytes 32))
         (info (map '(vector (unsigned-byte 8)) #'char-code "test"))
         (key (compat:hkdf :sha256 ikm salt info 32)))
    (assert-= (length key) 32)
    ;; Deterministic
    (assert-equalp key (compat:hkdf :sha256 ikm salt info 32))))

(deftest test-pbkdf2-sha256
  "PBKDF2-SHA256 key derivation"
  (let ((key (compat:pbkdf2-sha256 "password" "salt" :iterations 1000 :key-length 32)))
    (assert-= (length key) 32)
    ;; Deterministic
    (assert-equalp key (compat:pbkdf2-sha256 "password" "salt" :iterations 1000 :key-length 32))))

;;; ---------------------------------------------------------------------------
;;; AEAD
;;; ---------------------------------------------------------------------------

(deftest test-aes-gcm-roundtrip
  "AES-GCM encrypt/decrypt round trip"
  (let* ((key (compat:generate-aes-key :bits 256))
         (nonce (compat:generate-nonce))
         (plaintext (map '(vector (unsigned-byte 8)) #'char-code "Hello, World!")))
    (multiple-value-bind (ciphertext tag)
        (compat:aes-gcm-encrypt key nonce plaintext)
      (assert-= (length ciphertext) (length plaintext))
      (assert-= (length tag) 16)
      (let ((decrypted (compat:aes-gcm-decrypt key nonce ciphertext tag)))
        (assert-equalp decrypted plaintext)))))

(deftest test-aes-gcm-with-aad
  "AES-GCM with additional authenticated data"
  (let* ((key (compat:generate-aes-key :bits 128))
         (nonce (compat:generate-nonce))
         (plaintext (map '(vector (unsigned-byte 8)) #'char-code "secret"))
         (aad (map '(vector (unsigned-byte 8)) #'char-code "public header")))
    (multiple-value-bind (ct tag)
        (compat:aes-gcm-encrypt key nonce plaintext :aad aad)
      (let ((pt (compat:aes-gcm-decrypt key nonce ct tag :aad aad)))
        (assert-equalp pt plaintext)))))

(deftest test-chacha20-poly1305-roundtrip
  "ChaCha20-Poly1305 encrypt/decrypt round trip"
  (let* ((key (compat:crypto-random-bytes 32))
         (nonce (compat:generate-nonce))
         (plaintext (map '(vector (unsigned-byte 8)) #'char-code "Hello, World!")))
    (multiple-value-bind (ciphertext tag)
        (compat:chacha20-poly1305-encrypt key nonce plaintext)
      (let ((decrypted (compat:chacha20-poly1305-decrypt key nonce ciphertext tag)))
        (assert-equalp decrypted plaintext)))))

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

(deftest test-constants
  "AEAD constants match expected values"
  (assert-= compat:+aes-128-gcm-key-size+ 16)
  (assert-= compat:+aes-256-gcm-key-size+ 32)
  (assert-= compat:+aes-gcm-nonce-size+ 12)
  (assert-= compat:+aes-gcm-tag-size+ 16)
  (assert-= compat:+chacha20-poly1305-key-size+ 32)
  (assert-= compat:+chacha20-poly1305-nonce-size+ 12)
  (assert-= compat:+chacha20-poly1305-tag-size+ 16))

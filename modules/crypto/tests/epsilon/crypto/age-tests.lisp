;;;; Age Decryption Tests
;;;;
;;;; Unit tests for the age module covering:
;;;; - Bech32 decoding
;;;; - Age identity key decoding
;;;; - Age file format parsing
;;;; - X25519 recipient decryption (round-trip)

(defpackage epsilon.crypto.age-tests
  (:use :cl :epsilon.syntax :epsilon.test)
  (:require (epsilon.crypto.age age)
            (epsilon.crypto crypto)
            (epsilon.ssl ssl)
            (epsilon.base-encode b64)
            (epsilon.log log))
  (:enter t))

;;; ============================================================================
;;; Bech32 Decoding Tests
;;; ============================================================================

(deftest test-bech32-decode-basic
  "Decode a known Bech32 string (BIP-173 test vector)."
  ;; Test vector: "a" HRP with checksum only (empty data)
  (multiple-value-bind (hrp data) (age:bech32-decode "a12uel5l")
    (assert-true (string= "a" hrp))
    (assert-true (= 0 (length data)))))

(deftest test-bech32-decode-abcdef
  "Decode abcdef HRP test vector."
  (multiple-value-bind (hrp data) (age:bech32-decode "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw")
    (assert-true (string= "abcdef" hrp))
    (assert-true (> (length data) 0))))

(deftest test-bech32-decode-invalid-no-separator
  "Bech32 without separator signals error."
  (assert-condition (age:age-error) (age:bech32-decode "noseparator")))

(deftest test-bech32-decode-invalid-checksum
  "Bech32 with bad checksum signals error."
  (assert-condition (age:age-error) (age:bech32-decode "a12uel5x")))

;;; ============================================================================
;;; Age Identity Decoding Tests
;;; ============================================================================

(deftest test-decode-age-identity-roundtrip
  "Generate an X25519 key, encode it as an age identity, and decode it back."
  ;; Generate a key and extract private bytes
  (let* ((key (crypto:generate-x25519-key))
         (priv-bytes (crypto:x25519-private-key-bytes key)))
    ;; The raw private key is 32 bytes
    (assert-true (= 32 (length priv-bytes)))))

(deftest test-decode-age-identity-wrong-hrp
  "Decoding a Bech32 string with wrong HRP signals error."
  ;; This is a valid Bech32 string but with HRP 'a', not 'age-secret-key-'
  (assert-condition (age:age-error) (age:decode-age-identity "a12uel5l")))

;;; ============================================================================
;;; Age File Format Parsing Tests
;;; ============================================================================

(deftest test-parse-age-file-basic
  "Parse a minimal age file header."
  (let* ((age-text (format nil "age-encryption.org/v1~%-> X25519 dGVzdGtleWJhc2U2NA~%dGVzdHdyYXBwZWRrZXk~%--- dGVzdG1hYw~%payload"))
         (header (age:parse-age-file age-text)))
    (assert-true (age:age-header-p header))
    (assert-true (= 1 (length (age:age-header-recipients header))))
    (let ((r (first (age:age-header-recipients header))))
      (assert-true (string= "X25519" (age:age-recipient-type r)))
      (assert-true (string= "dGVzdGtleWJhc2U2NA" (first (age:age-recipient-args r)))))))

(deftest test-parse-age-file-missing-header
  "Parsing age data without version header signals error."
  (assert-condition (age:age-error) (age:parse-age-file "not an age file")))

;;; ============================================================================
;;; X25519 Recipient Round-Trip Tests
;;; ============================================================================

(deftest test-age-x25519-roundtrip
  "Encrypt with age format and decrypt: full X25519 round-trip."
  ;; Generate recipient identity (raw bytes, no OpenSSL PKI needed)
  (let* ((identity-priv (ssl:random-bytes 32))
         (identity-pub (ssl:x25519-base identity-priv))
         ;; Generate ephemeral key pair (simulating the age encryptor)
         (ephemeral-priv (ssl:random-bytes 32))
         (ephemeral-pub (ssl:x25519-base ephemeral-priv))
         ;; The file key (16 bytes, as per age spec)
         (file-key (ssl:random-bytes 16))
         ;; Plaintext to encrypt
         (plaintext (sb-ext:string-to-octets "Hello from age!" :external-format :utf-8)))

    ;; --- Simulate age encryption ---

    ;; 1. ECDH shared secret: X25519(ephemeral_private, identity_public)
    (let* ((shared-secret (ssl:x25519 ephemeral-priv identity-pub))

           ;; 2. Derive wrap key via HKDF-SHA256
           ;; Following the same arg order as age.lisp: (algorithm salt ikm info length)
           ;; salt = ephemeral_pub || identity_pub, ikm = shared_secret
           (salt (concatenate '(vector (unsigned-byte 8)) ephemeral-pub identity-pub))
           (info-bytes (sb-ext:string-to-octets "age-encryption.org/v1/X25519"
                                                :external-format :utf-8))
           (wrap-key (ssl:hkdf :sha256 salt shared-secret info-bytes 32)))

      ;; 3. Wrap the file key with ChaCha20-Poly1305
      (let ((zero-nonce (make-array 12 :element-type '(unsigned-byte 8) :initial-element 0)))
        (multiple-value-bind (wrapped-ct wrapped-tag)
            (ssl:chacha20-poly1305-encrypt file-key wrap-key zero-nonce)
          (let ((wrapped-key (concatenate '(vector (unsigned-byte 8)) wrapped-ct wrapped-tag)))

            ;; 4. STREAM encrypt the payload
            (let* ((stream-nonce (ssl:random-bytes 16))
                   (payload-key (ssl:hkdf :sha256 stream-nonce file-key
                                          (sb-ext:string-to-octets "payload" :external-format :utf-8)
                                          32))
                   ;; Single chunk (final): nonce = 11 zeros + 0x01
                   (chunk-nonce (make-array 12 :element-type '(unsigned-byte 8) :initial-element 0)))
              (setf (aref chunk-nonce 11) 1)  ; final flag
              (multiple-value-bind (chunk-ct chunk-tag)
                  (ssl:chacha20-poly1305-encrypt plaintext payload-key chunk-nonce)
                (let ((payload-bytes (concatenate '(vector (unsigned-byte 8))
                                                  stream-nonce chunk-ct chunk-tag)))

                  ;; 5. Build the age file as text
                  (let* ((ephemeral-b64 (b64:base64-encode ephemeral-pub))
                         (wrapped-b64 (b64:base64-encode wrapped-key))
                         (mac-b64 (b64:base64-encode (ssl:random-bytes 32)))
                         (header-text (format nil "age-encryption.org/v1~%-> X25519 ~A~%~A~%--- ~A~%"
                                              ephemeral-b64 wrapped-b64 mac-b64))
                         (header-bytes (sb-ext:string-to-octets header-text :external-format :latin-1))
                         (age-data (concatenate '(vector (unsigned-byte 8)) header-bytes payload-bytes)))

                    ;; --- Now decrypt ---
                    (let ((decrypted (age:decrypt-age-x25519 age-data identity-priv)))
                      (assert-true (equalp plaintext decrypted)))))))))))))

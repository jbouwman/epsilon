;;;; XChaCha20-Poly1305 AEAD (draft-irtf-cfrg-xchacha-03)
;;;;
;;;; Extended-nonce variant of ChaCha20-Poly1305. Accepts a 24-byte
;;;; nonce, eliminating the practical risk of nonce reuse from random
;;;; selection (compared to RFC 8439's 12-byte nonce). Used by libsodium,
;;;; age, WireGuard variants, and several other ecosystems.
;;;;
;;;; Construction (per draft Section 2.3):
;;;;   subkey = HChaCha20(key, nonce[0..15])
;;;;   chacha-nonce = 00 00 00 00 || nonce[16..23]
;;;;   ciphertext, tag = ChaCha20-Poly1305(subkey, chacha-nonce, aad, plaintext)

(defpackage epsilon.crypto.xchacha20-poly1305
  (:use :cl)
  (:import
   (epsilon.crypto.chacha20 chacha)
   (epsilon.crypto.chacha20-poly1305 cp))
  (:export
   #:xchacha20-poly1305-encrypt
   #:xchacha20-poly1305-decrypt))

(in-package :epsilon.crypto.xchacha20-poly1305)

(defun %derive-subkey-and-nonce (key nonce24)
  "Compute the (subkey, 12-byte chacha nonce) pair for a 24-byte XChaCha
   nonce. Subkey = HChaCha20(key, nonce[0..15]); chacha-nonce = four
   zero bytes followed by nonce[16..23]."
  (declare (type (simple-array (unsigned-byte 8) (*)) key nonce24))
  (assert (= (length key) 32) (key) "XChaCha20-Poly1305 key must be 32 bytes")
  (assert (= (length nonce24) 24) (nonce24)
          "XChaCha20-Poly1305 nonce must be 24 bytes")
  (let ((subkey (chacha:hchacha20 key (subseq nonce24 0 16)))
        (chacha-nonce (make-array 12 :element-type '(unsigned-byte 8)
                                  :initial-element 0)))
    (replace chacha-nonce nonce24 :start1 4 :start2 16 :end2 24)
    (values subkey chacha-nonce)))

(defun xchacha20-poly1305-encrypt (plaintext key nonce
                                   &key (aad (make-array 0 :element-type
                                                            '(unsigned-byte 8))))
  "Encrypt PLAINTEXT under XChaCha20-Poly1305.
   KEY is 32 bytes; NONCE is 24 bytes; AAD is optional associated data.
   Returns (values ciphertext tag) where TAG is 16 bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) plaintext key nonce aad))
  (multiple-value-bind (subkey chacha-nonce)
      (%derive-subkey-and-nonce key nonce)
    (cp:chacha20-poly1305-encrypt plaintext subkey chacha-nonce :aad aad)))

(defun xchacha20-poly1305-decrypt (ciphertext key nonce tag
                                   &key (aad (make-array 0 :element-type
                                                            '(unsigned-byte 8))))
  "Decrypt CIPHERTEXT produced by XChaCha20-Poly1305.
   KEY is 32 bytes; NONCE is 24 bytes; TAG is 16 bytes; AAD must match
   the value supplied at encryption time. Signals an error if the tag
   does not authenticate, otherwise returns the plaintext."
  (declare (type (simple-array (unsigned-byte 8) (*))
                 ciphertext key nonce tag aad))
  (multiple-value-bind (subkey chacha-nonce)
      (%derive-subkey-and-nonce key nonce)
    (cp:chacha20-poly1305-decrypt ciphertext subkey chacha-nonce tag :aad aad)))

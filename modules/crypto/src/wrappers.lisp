;;;; Wrapper Functions for Specialized Modules
;;;;
;;;; This file provides wrapper functions that call specialized packages

(in-package :epsilon.crypto)

;;;; KDF wrapper functions
(defun pbkdf2 (password salt &key 
                       (iterations 100000)
                       (key-length 32)
                       (digest +digest-sha256+))
  "Derive a key from a password using PBKDF2."
  (epsilon.crypto.kdf:pbkdf2 password salt 
                             :iterations iterations
                             :key-length key-length
                             :digest digest))

(defun hkdf (input-key-material &key salt info (length 32) (digest +digest-sha256+))
  "Derive keys using HKDF."
  (epsilon.crypto.kdf:hkdf input-key-material
                          :salt salt
                          :info info
                          :length length
                          :digest digest))

(defun scrypt (password salt &key (n 16384) (r 8) (p 1) (key-length 32))
  "Derive a key using scrypt."
  (epsilon.crypto.kdf:scrypt password salt
                            :n n :r r :p p
                            :key-length key-length))

;;;; BLAKE2 wrapper functions
(defun blake2b (data &key (output-length 64) key salt personalization)
  "Compute BLAKE2b hash."
  (epsilon.crypto.blake2:blake2b data 
                                 :output-length output-length
                                 :key key
                                 :salt salt
                                 :personalization personalization))

(defun blake2s (data &key (output-length 32) key)
  "Compute BLAKE2s hash."
  (epsilon.crypto.blake2:blake2s data
                                 :output-length output-length
                                 :key key))

;;;; AEAD wrapper functions
(defun aes-gcm-encrypt (plaintext key &key iv aad)
  "Encrypt data using AES-GCM."
  (epsilon.crypto.aead:aes-gcm-encrypt plaintext key :iv iv :aad aad))

(defun aes-gcm-decrypt (ciphertext key tag iv &key aad)
  "Decrypt AES-GCM encrypted data."
  (epsilon.crypto.aead:aes-gcm-decrypt ciphertext key tag iv :aad aad))

(defun chacha20-poly1305-encrypt (plaintext key &key nonce aad)
  "Encrypt data using ChaCha20-Poly1305."
  (epsilon.crypto.aead:chacha20-poly1305-encrypt plaintext key :nonce nonce :aad aad))

(defun chacha20-poly1305-decrypt (ciphertext key tag nonce &key aad)
  "Decrypt ChaCha20-Poly1305 encrypted data."
  (epsilon.crypto.aead:chacha20-poly1305-decrypt ciphertext key tag nonce :aad aad))
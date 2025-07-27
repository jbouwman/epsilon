;;;; Main entry point for epsilon.crypto module
;;;; Re-exports all additional cryptographic and encoding functionality

(in-package #:epsilon.crypto)

;; Re-export checksum functions
(import '(epsilon.checksum:crc32
          epsilon.checksum:crc32-sequence))

(export '(crc32
          crc32-sequence))

;; Re-export base64 functions
(import '(epsilon.base64:base64-encode
          epsilon.base64:base64-decode
          epsilon.base64:base64-encode-string
          epsilon.base64:base64-decode-string))

(export '(base64-encode
          base64-decode
          base64-encode-string
          base64-decode-string))

;; Re-export UUID functions
(import '(epsilon.uuid:uuid
          epsilon.uuid:uuid-string
          epsilon.uuid:make-uuid
          epsilon.uuid:make-uuid-string))

(export '(uuid
          uuid-string
          make-uuid
          make-uuid-string))

;; Re-export keyring functions
(import '(epsilon.crypto.keyring:*keyring*
          epsilon.crypto.keyring:define-keyring
          epsilon.crypto.keyring:get-key
          epsilon.crypto.keyring:set-key
          epsilon.crypto.keyring:delete-key
          epsilon.crypto.keyring:list-keys))

(export '(*keyring*
          define-keyring
          get-key
          set-key
          delete-key
          list-keys))

;; Re-export signature functions
(import '(epsilon.crypto.signatures:sign
          epsilon.crypto.signatures:verify
          epsilon.crypto.signatures:make-signature))

(export '(sign
          verify
          make-signature))
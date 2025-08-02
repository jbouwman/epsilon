;;;; Package definitions for epsilon.crypto module
;;;; This module provides additional cryptographic functions, checksums, and encoding utilities

(defpackage #:epsilon.crypto
  (:use #:cl)
  (:documentation "Main crypto module package that provides additional crypto functionality")
  (:export
   ;; Checksum functions
   #:crc32
   #:crc32-sequence
   
   ;; Base64 encoding
   #:base64-encode
   #:base64-decode
   #:base64-encode-string
   #:base64-decode-string
   
   ;; UUID generation
   #:uuid
   #:uuid-string
   #:make-uuid
   #:make-uuid-string
   
   ;; Key management
   #:*keyring*
   #:define-keyring
   #:get-key
   #:set-key
   #:delete-key
   #:list-keys
   
   ;; Signatures
   #:sign
   #:verify
   #:make-signature))

(defpackage #:epsilon.checksum
  (:use #:cl)
  (:export
   #:crc32
   #:crc32-sequence))

(defpackage #:epsilon.base64
  (:use #:cl #:epsilon.type)
  (:export
   #:base64-stream-to-integer
   #:base64-stream-to-string
   #:base64-stream-to-stream
   #:base64-stream-to-usb8-array
   #:base64-string-to-integer
   #:base64-string-to-string
   #:base64-string-to-stream
   #:base64-string-to-usb8-array
   #:string-to-base64-string
   #:string-to-base64-stream
   #:usb8-array-to-base64-string
   #:usb8-array-to-base64-stream
   #:stream-to-base64-string
   #:stream-to-base64-stream
   #:integer-to-base64-string
   #:integer-to-base64-stream

   ;; Conditions.
   #:base64-error
   #:bad-base64-character
   #:incomplete-base64-data

   ;; For creating custom encode/decode tables.
   #:make-decode-table
   #:+decode-table+
   #:+uri-decode-table+))

(defpackage #:epsilon.uuid
  (:use #:cl)
  (:export
   #:uuid
   #:uuid-string
   #:make-uuid
   #:make-uuid-string))

(defpackage #:epsilon.crypto.keyring
  (:use #:cl)
  (:export
   #:*keyring*
   #:define-keyring
   #:get-key
   #:set-key
   #:delete-key
   #:list-keys))

(defpackage #:epsilon.crypto.signatures
  (:use #:cl)
  (:export
   #:sign
   #:verify
   #:make-signature))
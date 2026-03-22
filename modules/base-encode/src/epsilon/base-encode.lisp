;;;; Epsilon Base Encode Module - Main Entry Point
;;;;
;;;; This package re-exports the public API from all base encoding
;;;; sub-packages, providing a single import point for users.
;;;;
;;;; Example:
;;;;   (import (epsilon.base-encode enc))
;;;;   (enc:base64-encode data)
;;;;   (enc:base16-decode "deadbeef")
;;;;   (enc:encode :base64 data)

(defpackage epsilon.base-encode
  (:use :cl)
  (:require (epsilon.base-encode.conditions cond)
            (epsilon.base-encode.tables tbl)
            (epsilon.base-encode.base16 b16)
            (epsilon.base-encode.base32 b32)
            (epsilon.base-encode.base64 b64)
            (epsilon.base-encode.bech32 bech)
            (epsilon.base-encode.streams str))
  (:export
   ;; High-level dispatch
   encode
   decode

   ;; Condition types
   encoding-error
   encoding-error-input
   encoding-error-position
   encoding-error-encoding-name
   bad-character
   bad-character-code
   incomplete-input
   invalid-length
   invalid-length-expected
   checksum-error
   checksum-error-expected
   checksum-error-actual

   ;; Table types and constructors
   encode-table
   decode-table
   make-encode-table
   make-decode-table
   add-aliases

   ;; Table constants
   +base16-encode+
   +base16-decode+
   +base32-encode+
   +base32-decode+
   +base32hex-encode+
   +base32hex-decode+
   +base32-crockford-encode+
   +base32-crockford-decode+
   +base64-encode+
   +base64-decode+
   +base64url-encode+
   +base64url-decode+
   +bech32-encode+
   +bech32-decode+

   ;; Base16
   base16-encode
   base16-decode
   base16-encode-byte
   base16-decode-byte

   ;; Base32
   base32-encode
   base32-decode
   base32-encode-crockford
   base32-decode-crockford
   base32-encode-crockford-128
   base32-decode-crockford-128

   ;; Base64
   base64-encode
   base64-decode
   base64-encode-url
   base64-decode-url
   base64-encode-string
   base64-decode-string

   ;; Bech32
   bech32-encode
   bech32-decode
   bech32-polymod
   bech32-hrp-expand

   ;; Stream constructors
   encoding-writer
   decoding-reader
   make-base64-encoding-writer
   make-base64-decoding-reader
   make-base32-encoding-writer
   make-base32-decoding-reader
   make-base16-encoding-writer
   make-base16-decoding-reader)
  (:enter t))

;;; ---------------------------------------------------------------------------
;;; Error Conditions
;;; ---------------------------------------------------------------------------

(setf (symbol-function 'encoding-error-input) #'cond:encoding-error-input)
(setf (symbol-function 'encoding-error-position) #'cond:encoding-error-position)
(setf (symbol-function 'encoding-error-encoding-name) #'cond:encoding-error-encoding-name)
(setf (symbol-function 'bad-character-code) #'cond:bad-character-code)
(setf (symbol-function 'invalid-length-expected) #'cond:invalid-length-expected)
(setf (symbol-function 'checksum-error-expected) #'cond:checksum-error-expected)
(setf (symbol-function 'checksum-error-actual) #'cond:checksum-error-actual)

(deftype encoding-error () 'cond:encoding-error)
(deftype bad-character () 'cond:bad-character)
(deftype incomplete-input () 'cond:incomplete-input)
(deftype invalid-length () 'cond:invalid-length)
(deftype checksum-error () 'cond:checksum-error)

;;; ---------------------------------------------------------------------------
;;; Table Types and Constructors
;;; ---------------------------------------------------------------------------

(deftype encode-table () 'tbl:encode-table)
(deftype decode-table () 'tbl:decode-table)

(setf (symbol-function 'make-encode-table) #'tbl:make-encode-table)
(setf (symbol-function 'make-decode-table) #'tbl:make-decode-table)
(setf (symbol-function 'add-aliases) #'tbl:add-aliases)

;;; ---------------------------------------------------------------------------
;;; Table Constants
;;; ---------------------------------------------------------------------------

(defconstant +base16-encode+ tbl:+base16-encode+)
(defconstant +base16-decode+ tbl:+base16-decode+)
(defconstant +base32-encode+ tbl:+base32-encode+)
(defconstant +base32-decode+ tbl:+base32-decode+)
(defconstant +base32hex-encode+ tbl:+base32hex-encode+)
(defconstant +base32hex-decode+ tbl:+base32hex-decode+)
(defconstant +base32-crockford-encode+ tbl:+base32-crockford-encode+)
(defconstant +base32-crockford-decode+ tbl:+base32-crockford-decode+)
(defconstant +base64-encode+ tbl:+base64-encode+)
(defconstant +base64-decode+ tbl:+base64-decode+)
(defconstant +base64url-encode+ tbl:+base64url-encode+)
(defconstant +base64url-decode+ tbl:+base64url-decode+)
(defconstant +bech32-encode+ tbl:+bech32-encode+)
(defconstant +bech32-decode+ tbl:+bech32-decode+)

;;; ---------------------------------------------------------------------------
;;; Base16 (Hex)
;;; ---------------------------------------------------------------------------

(setf (symbol-function 'base16-encode) #'b16:encode)
(setf (symbol-function 'base16-decode) #'b16:decode)
(setf (symbol-function 'base16-encode-byte) #'b16:encode-byte)
(setf (symbol-function 'base16-decode-byte) #'b16:decode-byte)

;;; ---------------------------------------------------------------------------
;;; Base32
;;; ---------------------------------------------------------------------------

(setf (symbol-function 'base32-encode) #'b32:encode)
(setf (symbol-function 'base32-decode) #'b32:decode)
(setf (symbol-function 'base32-encode-crockford) #'b32:encode-crockford)
(setf (symbol-function 'base32-decode-crockford) #'b32:decode-crockford)
(setf (symbol-function 'base32-encode-crockford-128) #'b32:encode-crockford-128)
(setf (symbol-function 'base32-decode-crockford-128) #'b32:decode-crockford-128)

;;; ---------------------------------------------------------------------------
;;; Base64
;;; ---------------------------------------------------------------------------

(setf (symbol-function 'base64-encode) #'b64:encode)
(setf (symbol-function 'base64-decode) #'b64:decode)
(setf (symbol-function 'base64-encode-url) #'b64:encode-url)
(setf (symbol-function 'base64-decode-url) #'b64:decode-url)
(setf (symbol-function 'base64-encode-string) #'b64:encode-string)
(setf (symbol-function 'base64-decode-string) #'b64:decode-string)

;;; ---------------------------------------------------------------------------
;;; Bech32
;;; ---------------------------------------------------------------------------

(setf (symbol-function 'bech32-encode) #'bech:encode)
(setf (symbol-function 'bech32-decode) #'bech:decode)
(setf (symbol-function 'bech32-polymod) #'bech:polymod)
(setf (symbol-function 'bech32-hrp-expand) #'bech:hrp-expand)

;;; ---------------------------------------------------------------------------
;;; Stream Constructors
;;; ---------------------------------------------------------------------------

(deftype encoding-writer () 'str:encoding-writer)
(deftype decoding-reader () 'str:decoding-reader)

(setf (symbol-function 'make-base64-encoding-writer) #'str:make-base64-encoding-writer)
(setf (symbol-function 'make-base64-decoding-reader) #'str:make-base64-decoding-reader)
(setf (symbol-function 'make-base32-encoding-writer) #'str:make-base32-encoding-writer)
(setf (symbol-function 'make-base32-decoding-reader) #'str:make-base32-decoding-reader)
(setf (symbol-function 'make-base16-encoding-writer) #'str:make-base16-encoding-writer)
(setf (symbol-function 'make-base16-decoding-reader) #'str:make-base16-decoding-reader)

;;; ---------------------------------------------------------------------------
;;; High-Level Dispatch
;;; ---------------------------------------------------------------------------

(defun encode (encoding data &rest args)
  "Encode DATA using ENCODING.
   ENCODING: :base16 :hex :base32 :base32-crockford :base32-crockford-128
             :base64 :base64url :bech32
   DATA: byte vector (or HRP + bytes for bech32).
   Returns a string."
  (ecase encoding
    ((:base16 :hex)
     (apply #'b16:encode data args))
    (:base32
     (apply #'b32:encode data args))
    (:base32-crockford
     (b32:encode-crockford data))
    (:base32-crockford-128
     (b32:encode-crockford-128 data))
    (:base64
     (apply #'b64:encode data args))
    (:base64url
     (apply #'b64:encode-url data args))
    (:bech32
     ;; For bech32, DATA is the HRP string, first arg is the byte vector
     (apply #'bech:encode data args))))

(defun decode (encoding string &rest args)
  "Decode STRING using ENCODING.
   ENCODING: :base16 :hex :base32 :base32-crockford :base32-crockford-128
             :base64 :base64url :bech32
   Returns a byte vector (or values HRP + bytes for bech32)."
  (ecase encoding
    ((:base16 :hex)
     (apply #'b16:decode string args))
    (:base32
     (apply #'b32:decode string args))
    (:base32-crockford
     (b32:decode-crockford string))
    (:base32-crockford-128
     (b32:decode-crockford-128 string))
    (:base64
     (apply #'b64:decode string args))
    (:base64url
     (b64:decode-url string))
    (:bech32
     (bech:decode string))))

;;;; epsilon.base-encode.tables - Encode/decode table infrastructure
;;;;
;;;; Provides types, constructors, and pre-computed constant tables for
;;;; all supported base encodings. Decode tables use O(1) array lookup
;;;; indexed by character code.
;;;;
;;;; Sentinel values in decode tables:
;;;;   -1 = invalid character
;;;;   -2 = padding character
;;;;   -3 = whitespace (ignorable)
;;;;   0..N = decoded value

(defpackage epsilon.base-encode.tables
  (:use :cl :epsilon.syntax)
  (:export
   ;; Types
   #:encode-table
   #:decode-table

   ;; Constructors
   #:make-encode-table
   #:make-decode-table
   #:add-aliases

   ;; Base16 tables
   #:+base16-encode+
   #:+base16-decode+

   ;; Base32 RFC 4648 tables
   #:+base32-encode+
   #:+base32-decode+
   #:+base32hex-encode+
   #:+base32hex-decode+

   ;; Base32 Crockford tables
   #:+base32-crockford-encode+
   #:+base32-crockford-decode+

   ;; Base64 tables
   #:+base64-encode+
   #:+base64-decode+
   #:+base64url-encode+
   #:+base64url-decode+

   ;; Bech32 tables
   #:+bech32-encode+
   #:+bech32-decode+)
  (:enter t))

;;; ============================================================================
;;; Types and Constructors
;;; ============================================================================
;;; Wrapped in eval-when so types and constructors are available at
;;; compile time for the define-constant forms below.

(eval-when (:compile-toplevel :load-toplevel :execute)

  (deftype encode-table () '(simple-string))

  (deftype decode-table () '(simple-array (signed-byte 8) (128)))

  (defun make-encode-table (alphabet)
    "Create an encode table from an alphabet string.
     Validates that all characters are unique and within ASCII range."
    (declare (type string alphabet))
    (assert (< (length alphabet) 128)
            (alphabet)
            "Alphabet too large: ~D characters (max 127)" (length alphabet))
    ;; Check uniqueness
    (let ((seen (make-array 128 :element-type 'bit :initial-element 0)))
      (loop for ch across alphabet
            for code = (char-code ch)
            do (assert (< code 128) (ch) "Non-ASCII character in alphabet: ~S" ch)
               (assert (zerop (aref seen code)) (ch) "Duplicate character in alphabet: ~S" ch)
               (setf (aref seen code) 1)))
    (coerce alphabet 'simple-string))

  (defun make-decode-table (encode-table
                            &key (pad-char nil)
                                 (whitespace-chars '(#\Linefeed #\Return #\Space #\Tab))
                                 (case-fold nil))
    "Build a decode table from an encode table.
     PAD-CHAR if provided is mapped to -2.
     WHITESPACE-CHARS are mapped to -3.
     CASE-FOLD if true maps both upper and lowercase to the same value."
    (let ((dt (make-array 128 :element-type '(signed-byte 8) :initial-element -1)))
      (declare (type decode-table dt))
      (loop for ch across encode-table
            for index from 0
            for code = (char-code ch)
            do (setf (aref dt code) index)
               (when case-fold
                 (let ((upper (char-code (char-upcase ch)))
                       (lower (char-code (char-downcase ch))))
                   (setf (aref dt upper) index)
                   (setf (aref dt lower) index))))
      (when pad-char
        (setf (aref dt (char-code pad-char)) -2))
      (loop for ch in whitespace-chars
            for code = (char-code ch)
            when (= -1 (aref dt code))
              do (setf (aref dt code) -3))
      dt))

  (defun add-aliases (decode-table alist)
    "Add character aliases to a decode table.
     ALIST is a list of (char . value) pairs."
    (declare (type decode-table decode-table))
    (loop for (ch . value) in alist
          for code = (char-code ch)
          do (setf (aref decode-table code) value))
    decode-table)

) ; end eval-when

;;; ============================================================================
;;; Pre-computed Constants
;;; ============================================================================

;;; --- Base16 (Hex) ---

(define-constant +base16-encode+
  (make-encode-table "0123456789abcdef")
  "Base16 (hex) encode table, lowercase.")

(define-constant +base16-decode+
  (make-decode-table +base16-encode+ :case-fold t)
  "Base16 decode table, case-insensitive.")

;;; --- Base32 RFC 4648 ---

(define-constant +base32-encode+
  (make-encode-table "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")
  "RFC 4648 Section 6 base32 encode table.")

(define-constant +base32-decode+
  (make-decode-table +base32-encode+ :pad-char #\= :case-fold t)
  "RFC 4648 base32 decode table, case-insensitive.")

(define-constant +base32hex-encode+
  (make-encode-table "0123456789ABCDEFGHIJKLMNOPQRSTUV")
  "RFC 4648 Section 7 base32hex encode table.")

(define-constant +base32hex-decode+
  (make-decode-table +base32hex-encode+ :pad-char #\= :case-fold t)
  "RFC 4648 base32hex decode table, case-insensitive.")

;;; --- Base32 Crockford ---

(define-constant +base32-crockford-encode+
  (make-encode-table "0123456789abcdefghjkmnpqrstvwxyz")
  "Crockford's base32 encode table (lowercase).")

(define-constant +base32-crockford-decode+
  (let ((dt (make-decode-table +base32-crockford-encode+ :case-fold t)))
    ;; Crockford confusion tolerance: i/I/l/L -> 1, o/O -> 0
    (add-aliases dt (list (cons #\i 1) (cons #\I 1)
                          (cons #\l 1) (cons #\L 1)
                          (cons #\o 0) (cons #\O 0))))
  "Crockford's base32 decode table with confusion tolerance.")

;;; --- Base64 RFC 4648 ---

(define-constant +base64-encode+
  (make-encode-table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
  "RFC 4648 Section 4 base64 encode table.")

(define-constant +base64-decode+
  (make-decode-table +base64-encode+ :pad-char #\=)
  "RFC 4648 base64 decode table with whitespace ignore.")

;;; --- Base64url RFC 4648 ---

(define-constant +base64url-encode+
  (make-encode-table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")
  "RFC 4648 Section 5 base64url encode table.")

(define-constant +base64url-decode+
  (make-decode-table +base64url-encode+ :pad-char #\=)
  "RFC 4648 base64url decode table.")

;;; --- Bech32 BIP-173 ---

(define-constant +bech32-encode+
  (make-encode-table "qpzry9x8gf2tvdw0s3jn54khce6mua7l")
  "BIP-173 bech32 encode table.")

(define-constant +bech32-decode+
  (make-decode-table +bech32-encode+ :case-fold t)
  "BIP-173 bech32 decode table, case-insensitive.")

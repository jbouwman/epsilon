;;;; Character Encoding Tests
;;;;
;;;; Tests for the simplified character encoding interface that uses
;;;; SBCL's built-in encoding support. Focuses on the core functionality
;;;; that's actually used by other modules.

(defpackage :epsilon.lib.char-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (:char :epsilon.lib.char)))

(in-package :epsilon.lib.char-tests)

(defparameter *ascii*
  "Why optimize?"
  "ASCII")

(defparameter *latin1*
  "Gödel"
  "Latin-1 accented characters")

(defparameter *utf8*
  "∀x∈ℕ, x² ≥ 0 ∧ π ≈ 3.14159…"
  "UTF-8 mathematical symbols")

(defparameter *utf8-emoji*
  "🤓📚💻⚡"
  "UTF-8 emoji")

;;;; Utilities

(defun round-trip-test (encoding test-string description)
  "Test that string->octets->string is identity"
  (let* ((octets (char:string-to-bytes test-string :encoding encoding))
         (result (char:bytes-to-string octets :encoding encoding)))
    (is-equal result test-string description)))

(defun encoding-error-test (encoding bad-string description)
  "Test that encoding properly signals errors"
  (is-thrown (char:character-encoding-error)
    (char:string-to-bytes bad-string :encoding encoding :errorp t)
    description))

(defun decoding-error-test (encoding bad-octets description)
  "Test that decoding properly signals errors"
  (is-thrown (char:character-decoding-error)
    (char:bytes-to-string bad-octets :encoding encoding :errorp t)
    description))

;;;; ASCII Tests

(deftest ascii
  
  (round-trip-test :ascii *ascii*
                   "ASCII round-trip")
  
  (is-equalp 
      (char:string-to-bytes "ABC" :encoding :ascii)
      #(65 66 67)
      "ASCII encoding of simple string")
  
  (is-equalp
      (char:bytes-to-string #(72 101 108 108 111) :encoding :ascii)
      "Hello"
      "ASCII decoding of simple octets")
  
  (encoding-error-test :ascii "Café" 
                       "ASCII encoding error on non-ASCII character")
  
  (decoding-error-test :ascii #(65 200 67)
                       "ASCII decoding error on high bit set"))

;;;; ISO-8859-1 Tests

(deftest iso-8859
  
  (round-trip-test :iso-8859-1 *latin1*
                   "ISO-8859-1 round-trip")
  
  (is-equalp
      (char:string-to-bytes "Café" :encoding :iso-8859-1)
      #(67 97 102 233)
      "ISO-8859-1 encoding with accented character")
  
  (is-equalp
      (char:bytes-to-string #(67 97 102 233) :encoding :iso-8859-1)
      "Café"
      "ISO-8859-1 decoding with accented character")
  
  ;; Test all 256 possible values (ISO-8859-1 is a complete 1:1 mapping)
  (let ((all-bytes (make-array 256 :element-type '(unsigned-byte 8))))
    (dotimes (i 256)
      (setf (aref all-bytes i) i))
    (round-trip-test :iso-8859-1 
                     (char:bytes-to-string all-bytes :encoding :iso-8859-1)
                     "ISO-8859-1 full byte range round-trip")))

;;;; UTF-8 Tests

(deftest utf-8
  
  (round-trip-test :utf-8 *ascii*
                   "UTF-8 round-trip: ASCII subset")
  
  (round-trip-test :utf-8 *utf8*
                   "UTF-8 round-trip: Mathematical symbols")
  
  (round-trip-test :utf-8 *utf8-emoji*
                   "UTF-8 round-trip: Modern emoji")
  
  ;; Test specific UTF-8 byte sequences
  (is-equalp
      (char:string-to-bytes "λ" :encoding :utf-8)
      #(206 187)
      "UTF-8 encoding of lambda (2-byte sequence)")
  
  (is-equalp
      (char:string-to-bytes "∀" :encoding :utf-8)
      #(226 136 128)
      "UTF-8 encoding of for-all (3-byte sequence)")
  
  (is-equalp
      (char:string-to-bytes "🤓" :encoding :utf-8)
      #(240 159 164 147)
      "UTF-8 encoding of nerd emoji (4-byte sequence)")
  
  ;; Test UTF-8 decoding
  (is-equalp
      (char:bytes-to-string #(206 187) :encoding :utf-8)
      "λ"
      "UTF-8 decoding of lambda")
  
  (is-equalp
      (char:bytes-to-string #(226 136 128) :encoding :utf-8)
      "∀"
      "UTF-8 decoding of for-all")
  
  ;; Test error conditions
  (decoding-error-test :utf-8 #(255 254)
                       "UTF-8 decoding error on invalid starter bytes")
  
  (decoding-error-test :utf-8 #(194 32)
                       "UTF-8 decoding error on invalid continuation byte")
  
  (decoding-error-test :utf-8 #(224 159)
                       "UTF-8 decoding error on incomplete sequence")
  
  ;; Test overlong sequences (should be rejected)
  (decoding-error-test :utf-8 #(192 128)
                       "UTF-8 decoding error on overlong 2-byte sequence")
  
  (decoding-error-test :utf-8 #(224 128 128)
                       "UTF-8 decoding error on overlong 3-byte sequence"))

;;;; Error Suppression Tests

(deftest error-suppression
  
  ;; Test that error suppression works
  (let ((epsilon.lib.char:*suppress-character-coding-errors* t))
    (is
        (char:string-to-bytes "Café" :encoding :ascii)
        "ASCII encoding with error suppression enabled")
    
    (is
        (char:bytes-to-string #(255 254) :encoding :utf-8)
        "UTF-8 decoding with error suppression enabled")))

;;;; Performance Tests (basic)

(deftest performance
  
  ;; Test with larger strings
  (round-trip-test :ascii (make-string 1000 :initial-element #\A)
                   "ASCII performance test: 1000 character string")
  
  (round-trip-test :utf-8 (make-string 1000 :initial-element #\λ)
                   "UTF-8 performance test: 1000 lambda characters"))

;;;; Integration Tests

(deftest integration
  
  ;; Test mixed content
  (let ((mixed-string "ASCII λ 🤓"))
    (round-trip-test :utf-8 mixed-string
                     "Mixed ASCII/Unicode content")
    
    ;; Test that we can convert between encodings for compatible subsets
    (let* ((ascii-part "What, Me Optimize?")
           (utf8-octets (char:string-to-bytes ascii-part :encoding :utf-8))
           (ascii-octets (char:string-to-bytes ascii-part :encoding :ascii)))
      (is-equalp utf8-octets ascii-octets
          "ASCII subset identical in UTF-8 and ASCII encodings"))))
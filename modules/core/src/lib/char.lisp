;;;; This module provides string/byte conversion using SBCL's built-in encoding support.

(defpackage :epsilon.char
  (:use :cl)
  (:export
   ;; Core types
   :unicode-char
   :code-point
   
   ;; Main string/byte conversion functions
   :string-to-bytes
   :bytes-to-string
   
   ;; Error handling
   :*suppress-character-coding-errors*
   :character-coding-error
   :character-encoding-error
   :character-decoding-error
   ))

(in-package :epsilon.char)

;;;; Core Types

(deftype u8 () '(unsigned-byte 8))
(deftype ->u8 (&optional (size '*)) `(simple-array (unsigned-byte 8) (,size)))

(deftype code-point () '(mod #x110000))

(deftype unicode-char ()
  "Character type for Unicode characters."
  'character)

;;;; Error Handling

(defparameter *suppress-character-coding-errors* nil
  "If non-NIL, encoding/decoding errors are suppressed.")

(define-condition character-coding-error (error)
  ((encoding :initarg :encoding :reader character-coding-error-encoding)
   (position :initarg :position :reader character-coding-error-position)))

(define-condition character-encoding-error (character-coding-error)
  ((code :initarg :code :reader character-encoding-error-code))
  (:report (lambda (c s)
             (format s "Unable to encode character code point ~A as ~S."
                     (character-encoding-error-code c)
                     (character-coding-error-encoding c)))))

(define-condition character-decoding-error (character-coding-error)
  ((octets :initarg :octets :reader character-decoding-error-octets))
  (:report (lambda (c s)
             (format s "Illegal ~S character starting at position ~D."
                     (character-coding-error-encoding c)
                     (character-coding-error-position c)))))

;;;; String/Byte Conversion using SBCL's built-in support

(defun string-to-bytes (string &key (encoding :utf-8) (start 0) end 
                              (errorp (not *suppress-character-coding-errors*)))
  "Convert STRING to U8 vector using ENCODING (defaults to UTF-8).
Uses SBCL's built-in SB-EXT:STRING-TO-OCTETS for efficiency."
  (handler-case
      (let ((octets (sb-ext:string-to-octets string 
                                             :external-format encoding
                                             :start start 
                                             :end end)))
        (coerce octets '(simple-array (unsigned-byte 8) (*))))
    (error (e)
      (declare (ignore e))
      (if errorp
          (error 'character-encoding-error 
                 :encoding encoding 
                 :position (or start 0))
          ;; Return replacement bytes when error suppression is enabled
          (coerce #(63) '(simple-array (unsigned-byte 8) (*)))))))

(defun bytes-to-string (octets &key (encoding :utf-8) (start 0) end
                               (errorp (not *suppress-character-coding-errors*)))
  "Convert U8 vector OCTETS to string using ENCODING (defaults to UTF-8).
Uses SBCL's built-in SB-EXT:OCTETS-TO-STRING for efficiency."
  (handler-case
      (sb-ext:octets-to-string (coerce octets '(vector (unsigned-byte 8)))
                               :external-format encoding
                               :start start 
                               :end end)
    (error (e)
      (declare (ignore e))
      (if errorp
          (error 'character-decoding-error 
                 :encoding encoding 
                 :octets octets
                 :position (or start 0))
          ;; Return a replacement character string when error suppression is enabled
          (string #\?)))))


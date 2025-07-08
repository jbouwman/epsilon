;;;; Simplified Character Encoding Interface
;;;;
;;;; This module provides a simplified interface for character encoding operations,
;;;; primarily using SBCL's built-in encoding support for common encodings like UTF-8.
;;;; Only includes functionality that's actually used by other modules.
;;;;
;;;; Key Features:
;;;; - UTF-8 string/byte conversion using SBCL's built-in support
;;;; - Simple error handling for encoding/decoding failures
;;;; - Backwards compatibility with existing epsilon.lib.char API
;;;;
;;;; Dependencies: None (simplified to remove epsilon.lib.type dependency)
;;;; Platform: SBCL (uses SB-EXT string conversion functions)

(defpackage :epsilon.lib.char
  (:use :cl)
  (:export
   ;; Core types
   :unicode-char
   :code-point
   
   ;; Main string/byte conversion functions
   :string-to-bytes
   :bytes-to-string
   
   ;; Character encoding interface (minimal)
   :get-character-encoding
   :enc-max-units-per-char
   
   ;; Error handling
   :*suppress-character-coding-errors*
   :character-coding-error
   :character-encoding-error
   :character-decoding-error
   ))

(in-package :epsilon.lib.char)

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

;;;; Minimal Character Encoding Interface
;;;; 
;;;; This provides just enough interface to maintain compatibility with
;;;; existing code that expects the old epsilon.lib.char API.

(defstruct character-encoding
  name
  max-units-per-char)

(defparameter *character-encodings* 
  (list (make-character-encoding :name :utf-8 :max-units-per-char 4)
        (make-character-encoding :name :ascii :max-units-per-char 1)
        (make-character-encoding :name :latin-1 :max-units-per-char 1)
        (make-character-encoding :name :iso-8859-1 :max-units-per-char 1)))

(defun get-character-encoding (name)
  "Get character encoding descriptor by name."
  (when (eq name :default)
    (setf name :utf-8))
  (or (find name *character-encodings* :key #'character-encoding-name)
      (error "Unknown character encoding: ~S" name)))

(defun enc-max-units-per-char (encoding)
  "Get maximum units per character for encoding."
  (etypecase encoding
    (character-encoding (character-encoding-max-units-per-char encoding))
    (symbol (character-encoding-max-units-per-char (get-character-encoding encoding)))))
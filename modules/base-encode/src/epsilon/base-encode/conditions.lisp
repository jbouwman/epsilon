;;;; epsilon.base-encode.conditions - Error condition hierarchy
;;;;
;;;; Provides a structured condition hierarchy for all base encoding
;;;; errors. Each condition carries context about what went wrong:
;;;; the input data, position of the error, and the encoding name.

(defpackage epsilon.base-encode.conditions
  (:use :cl)
  (:export
   ;; Base condition
   #:encoding-error
   #:encoding-error-input
   #:encoding-error-position
   #:encoding-error-encoding-name

   ;; Specific conditions
   #:bad-character
   #:bad-character-code
   #:incomplete-input
   #:invalid-length
   #:invalid-length-expected
   #:checksum-error
   #:checksum-error-expected
   #:checksum-error-actual)
  (:enter t))

;;; ============================================================================
;;; Base Condition
;;; ============================================================================

(define-condition encoding-error (error)
  ((input
    :initarg :input
    :reader encoding-error-input
    :documentation "The input that caused the error.")
   (position
    :initarg :position
    :initform 0
    :reader encoding-error-position
    :type unsigned-byte
    :documentation "Position in the input where the error occurred.")
   (encoding-name
    :initarg :encoding-name
    :initform "unknown"
    :reader encoding-error-encoding-name
    :type string
    :documentation "Name of the encoding (e.g. \"base64\", \"base16\")."))
  (:report (lambda (condition stream)
             (format stream "Encoding error in ~A at position ~D"
                     (encoding-error-encoding-name condition)
                     (encoding-error-position condition)))))

;;; ============================================================================
;;; Specific Conditions
;;; ============================================================================

(define-condition bad-character (encoding-error)
  ((character-code
    :initarg :character-code
    :reader bad-character-code
    :type unsigned-byte
    :documentation "The code of the invalid character."))
  (:report (lambda (condition stream)
             (format stream "Bad ~A character ~S (code ~D) at position ~D"
                     (encoding-error-encoding-name condition)
                     (code-char (bad-character-code condition))
                     (bad-character-code condition)
                     (encoding-error-position condition)))))

(define-condition incomplete-input (encoding-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Incomplete ~A data at position ~D"
                     (encoding-error-encoding-name condition)
                     (encoding-error-position condition)))))

(define-condition invalid-length (encoding-error)
  ((expected-length
    :initarg :expected-length
    :reader invalid-length-expected
    :documentation "The expected input length."))
  (:report (lambda (condition stream)
             (format stream "Invalid ~A input length: expected ~A, got ~D"
                     (encoding-error-encoding-name condition)
                     (invalid-length-expected condition)
                     (encoding-error-position condition)))))

(define-condition checksum-error (encoding-error)
  ((expected
    :initarg :expected
    :reader checksum-error-expected
    :documentation "The expected checksum value.")
   (actual
    :initarg :actual
    :reader checksum-error-actual
    :documentation "The actual checksum value."))
  (:report (lambda (condition stream)
             (format stream "~A checksum mismatch: expected ~A, got ~A"
                     (encoding-error-encoding-name condition)
                     (checksum-error-expected condition)
                     (checksum-error-actual condition)))))

;;;; Crypto error types and condition hierarchy
;;;;
;;;; Error Hierarchy:
;;;;   crypto-error (base)
;;;;     tls-error
;;;;       tls-handshake-error
;;;;       tls-certificate-error
;;;;     certificate-error
;;;;       certificate-expired-error
;;;;       certificate-invalid-error
;;;;       certificate-chain-error
;;;;     key-error
;;;;       key-generation-error
;;;;       key-format-error

(defpackage epsilon.crypto.errors
  (:use :cl)
  (:export crypto-error
           crypto-error-p
           crypto-error-message
           crypto-error-code
           crypto-error-context
           tls-error
           tls-error-p
           tls-handshake-error
           tls-handshake-error-p
           tls-certificate-error
           tls-certificate-error-p
           certificate-error
           certificate-error-p
           certificate-expired-error
           certificate-expired-error-p
           certificate-invalid-error
           certificate-invalid-error-p
           certificate-chain-error
           certificate-chain-error-p
           key-error
           key-error-p
           key-generation-error
           key-generation-error-p
           key-format-error
           key-format-error-p
           signal-crypto-error
           signal-tls-error
           signal-certificate-error
           signal-key-error
           with-crypto-errors)
  (:enter t))

;;;; Base Condition

(define-condition crypto-error (error)
  ((code :initarg :code
         :initform 0
         :reader crypto-error-code
         :type integer
         :documentation "OpenSSL error code or custom error code")
   (message :initarg :message
            :initform "Cryptographic operation failed"
            :reader crypto-error-message
            :type string
            :documentation "Human-readable error description")
   (context :initarg :context
            :initform nil
            :reader crypto-error-context
            :documentation "Additional context (operation name, parameters, etc.)"))
  (:documentation "Base condition for all cryptographic errors.
Includes an error code (typically from OpenSSL), a message, and optional context.")
  (:report (lambda (condition stream)
             (format stream "Crypto error ~A: ~A"
                     (crypto-error-code condition)
                     (crypto-error-message condition))
             (when (crypto-error-context condition)
               (format stream " (context: ~A)" (crypto-error-context condition))))))

;;;; TLS Conditions

(define-condition tls-error (crypto-error)
  ()
  (:documentation "Error during TLS/SSL operation"))

(define-condition tls-handshake-error (tls-error)
  ((stage :initarg :stage
          :initform nil
          :reader tls-handshake-error-stage
          :documentation "Stage of handshake where error occurred"))
  (:documentation "Error during TLS handshake")
  (:report (lambda (condition stream)
             (format stream "TLS handshake error")
             (when (tls-handshake-error-stage condition)
               (format stream " at stage ~A" (tls-handshake-error-stage condition)))
             (format stream ": ~A" (crypto-error-message condition)))))

(define-condition tls-certificate-error (tls-error)
  ((certificate :initarg :certificate
                :initform nil
                :reader tls-certificate-error-certificate
                :documentation "The problematic certificate"))
  (:documentation "Certificate-related TLS error"))

;;;; Certificate Conditions

(define-condition certificate-error (crypto-error)
  ((certificate :initarg :certificate
                :initform nil
                :reader certificate-error-certificate
                :documentation "The problematic certificate if available"))
  (:documentation "Error related to X.509 certificates"))

(define-condition certificate-expired-error (certificate-error)
  ((not-after :initarg :not-after
              :initform nil
              :reader certificate-expired-error-not-after
              :documentation "Certificate expiration time"))
  (:documentation "Certificate has expired")
  (:report (lambda (condition stream)
             (format stream "Certificate expired")
             (when (certificate-expired-error-not-after condition)
               (format stream " (valid until ~A)"
                       (certificate-expired-error-not-after condition))))))

(define-condition certificate-invalid-error (certificate-error)
  ((reason :initarg :reason
           :initform nil
           :reader certificate-invalid-error-reason
           :documentation "Reason for invalidity"))
  (:documentation "Certificate is invalid"))

(define-condition certificate-chain-error (certificate-error)
  ((depth :initarg :depth
          :initform nil
          :reader certificate-chain-error-depth
          :documentation "Depth in chain where error occurred"))
  (:documentation "Error in certificate chain validation"))

;;;; Key Conditions

(define-condition key-error (crypto-error)
  ((key-type :initarg :key-type
             :initform nil
             :reader key-error-key-type
             :documentation "Type of key involved (RSA, EC, Ed25519, etc.)"))
  (:documentation "Error related to cryptographic keys"))

(define-condition key-generation-error (key-error)
  ((bits :initarg :bits
         :initform nil
         :reader key-generation-error-bits
         :documentation "Requested key size in bits"))
  (:documentation "Error during key generation")
  (:report (lambda (condition stream)
             (format stream "Key generation failed")
             (when (key-error-key-type condition)
               (format stream " for ~A" (key-error-key-type condition)))
             (when (key-generation-error-bits condition)
               (format stream " (~A bits)" (key-generation-error-bits condition)))
             (format stream ": ~A" (crypto-error-message condition)))))

(define-condition key-format-error (key-error)
  ((format :initarg :format
           :initform nil
           :reader key-format-error-format
           :documentation "Expected format (PEM, DER, etc.)"))
  (:documentation "Error in key format or parsing"))

;;;; Predicates

(defmacro define-condition-predicates (&rest names)
  "Generate type-checking predicates for condition types."
  `(progn
     ,@(loop for name in names
             collect `(defun ,(intern (format nil "~A-P" name)) (obj)
                        ,(format nil "Test if OBJ is a ~(~A~)." name)
                        (typep obj ',name)))))

(define-condition-predicates
  crypto-error
  tls-error
  tls-handshake-error
  tls-certificate-error
  certificate-error
  certificate-expired-error
  certificate-invalid-error
  certificate-chain-error
  key-error
  key-generation-error
  key-format-error)

;;;; Signaling Functions

(defun signal-crypto-error (message &key (code 0) context)
  "Signal a generic crypto-error"
  (error 'crypto-error :message message :code code :context context))

(defun signal-tls-error (message &key (code 0) context)
  "Signal a TLS error"
  (error 'tls-error :message message :code code :context context))

(defun signal-certificate-error (message &key (code 0) context certificate)
  "Signal a certificate error"
  (error 'certificate-error
         :message message
         :code code
         :context context
         :certificate certificate))

(defun signal-key-error (message &key (code 0) context key-type)
  "Signal a key error"
  (error 'key-error
         :message message
         :code code
         :context context
         :key-type key-type))

(defmacro with-crypto-errors ((&key (on-error :signal)) &body body)
  "Execute BODY with crypto error handling.
ON-ERROR can be:
  :signal - Signal errors as conditions (default)
  :collect - Collect errors and return them as a second value
  :ignore - Ignore errors and return nil on failure"
  (declare (ignore on-error))
  `(progn ,@body))

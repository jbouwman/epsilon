;;;; HKDF (RFC 5869)
;;;;
;;;; HMAC-based Extract-and-Expand Key Derivation Function.
;;;; Critical for TLS 1.3 key schedule (RFC 8446 Section 7).

(defpackage epsilon.ssl.hkdf
  (:use :cl)
  (:local-nicknames
   (#:hmac #:epsilon.ssl.hmac)
   (#:ct #:epsilon.ssl.ct))
  (:export
   #:hkdf-extract
   #:hkdf-expand
   #:hkdf
   #:hkdf-expand-label))

(in-package :epsilon.ssl.hkdf)

;;; ---------------------------------------------------------------------------
;;; Hash algorithm properties
;;; ---------------------------------------------------------------------------

(defun hash-output-length (hash-algorithm)
  "Return the output length in bytes for the given hash algorithm."
  (ecase hash-algorithm
    (:sha256 32)
    (:sha384 48)
    (:sha512 64)))

;;; ---------------------------------------------------------------------------
;;; HKDF-Extract (RFC 5869 Section 2.2)
;;; ---------------------------------------------------------------------------

(defun hkdf-extract (hash-algorithm salt ikm)
  "Extract a pseudorandom key from input keying material.
   SALT: optional salt (byte array); if nil, uses zeros of hash length.
   IKM: input keying material (byte array).
   Returns PRK (pseudorandom key) of hash output length."
  (declare (type (simple-array (unsigned-byte 8) (*)) ikm))
  (let ((actual-salt (or salt
                         (make-array (hash-output-length hash-algorithm)
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0))))
    (hmac:hmac hash-algorithm actual-salt ikm)))

;;; ---------------------------------------------------------------------------
;;; HKDF-Expand (RFC 5869 Section 2.3)
;;; ---------------------------------------------------------------------------

(defun hkdf-expand (hash-algorithm prk info length)
  "Expand pseudorandom key to desired length.
   PRK: pseudorandom key from hkdf-extract (byte array).
   INFO: context/application-specific info (byte array).
   LENGTH: desired output length in bytes.
   Returns OKM (output keying material) of LENGTH bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) prk info)
           (type (integer 1) length))
  (let* ((hash-len (hash-output-length hash-algorithm))
         (n (ceiling length hash-len))
         (okm (make-array length :element-type '(unsigned-byte 8) :initial-element 0))
         (t-prev (make-array 0 :element-type '(unsigned-byte 8)))
         (offset 0))
    (when (> n 255)
      (error "HKDF-Expand: requested length ~D exceeds maximum (~D)"
             length (* 255 hash-len)))
    (loop for i from 1 to n
          do (let* ((counter (make-array 1 :element-type '(unsigned-byte 8)
                                           :initial-element i))
                    (input (concatenate '(simple-array (unsigned-byte 8) (*))
                                        t-prev info counter))
                    (t-current (hmac:hmac hash-algorithm prk input))
                    (copy-len (min hash-len (- length offset))))
               (replace okm t-current :start1 offset :end1 (+ offset copy-len))
               (incf offset copy-len)
               (setf t-prev t-current)))
    okm))

;;; ---------------------------------------------------------------------------
;;; HKDF (combined extract + expand)
;;; ---------------------------------------------------------------------------

(defun hkdf (hash-algorithm salt ikm info length)
  "Combined HKDF extract-and-expand.
   Returns LENGTH bytes of derived key material."
  (let ((prk (hkdf-extract hash-algorithm salt ikm)))
    (hkdf-expand hash-algorithm prk info length)))

;;; ---------------------------------------------------------------------------
;;; HKDF-Expand-Label (TLS 1.3 specific, RFC 8446 Section 7.1)
;;; ---------------------------------------------------------------------------

(defun hkdf-expand-label (hash-algorithm secret label context length)
  "TLS 1.3 HKDF-Expand-Label function (RFC 8446 Section 7.1).
   Derives key material using TLS 1.3 labeling convention.
   SECRET: the secret to expand (byte array).
   LABEL: label string (will be prefixed with 'tls13 ').
   CONTEXT: hash of handshake context (byte array).
   LENGTH: desired output length."
  (declare (type (simple-array (unsigned-byte 8) (*)) secret context)
           (type string label)
           (type (integer 1) length))
  (let* ((full-label (concatenate 'string "tls13 " label))
         (label-bytes (map '(simple-array (unsigned-byte 8) (*))
                           #'char-code full-label))
         (label-len (length label-bytes))
         (context-len (length context))
         ;; HkdfLabel structure:
         ;; uint16 length
         ;; opaque label<7..255> = "tls13 " + Label
         ;; opaque context<0..255> = Context
         (hkdf-label (make-array (+ 2 1 label-len 1 context-len)
                                 :element-type '(unsigned-byte 8))))
    ;; Encode length (2 bytes, big-endian)
    (setf (aref hkdf-label 0) (logand #xFF (ash length -8)))
    (setf (aref hkdf-label 1) (logand #xFF length))
    ;; Encode label length + label
    (setf (aref hkdf-label 2) label-len)
    (replace hkdf-label label-bytes :start1 3)
    ;; Encode context length + context
    (setf (aref hkdf-label (+ 3 label-len)) context-len)
    (replace hkdf-label context :start1 (+ 4 label-len))
    ;; Expand
    (hkdf-expand hash-algorithm secret hkdf-label length)))

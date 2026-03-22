;;;; PKCS Key Encoding/Decoding
;;;;
;;;; Implements SubjectPublicKeyInfo (RFC 5280), PKCS#8 PrivateKeyInfo,
;;;; PKCS#1 RSA key encoding, and SEC 1 EC key encoding.

(defpackage epsilon.ssl.pkcs
  (:use :cl)
  (:local-nicknames
   (#:asn1 #:epsilon.ssl.asn1))
  (:export
   ;; Well-known OIDs
   #:+oid-rsa-encryption+
   #:+oid-ec-public-key+
   #:+oid-ed25519+
   #:+oid-prime256v1+
   #:+oid-sha256-with-rsa+
   #:+oid-ecdsa-with-sha256+
   ;; SubjectPublicKeyInfo
   #:encode-spki-rsa
   #:encode-spki-ec
   #:encode-spki-ed25519
   #:decode-spki
   ;; PKCS#8 PrivateKeyInfo
   #:encode-pkcs8-ed25519
   #:encode-pkcs8-ec
   #:decode-pkcs8
   ;; PKCS#1 RSA key encoding
   #:encode-pkcs1-rsa-public
   #:encode-pkcs1-rsa-private
   #:decode-pkcs1-rsa-public
   #:decode-pkcs1-rsa-private))

(in-package :epsilon.ssl.pkcs)

;;; ---------------------------------------------------------------------------
;;; Well-known OIDs
;;; ---------------------------------------------------------------------------

(defparameter +oid-rsa-encryption+ '(1 2 840 113549 1 1 1))
(defparameter +oid-ec-public-key+ '(1 2 840 10045 2 1))
(defparameter +oid-ed25519+ '(1 3 101 112))
(defparameter +oid-prime256v1+ '(1 2 840 10045 3 1 7))
(defparameter +oid-sha256-with-rsa+ '(1 2 840 113549 1 1 11))
(defparameter +oid-ecdsa-with-sha256+ '(1 2 840 10045 4 3 2))

;;; ---------------------------------------------------------------------------
;;; PKCS#1 RSA key encoding
;;; ---------------------------------------------------------------------------

(defun encode-pkcs1-rsa-public (n e)
  "Encode RSA public key as PKCS#1 RSAPublicKey DER.
   RSAPublicKey ::= SEQUENCE { modulus INTEGER, publicExponent INTEGER }"
  (asn1:der-encode-sequence
   (asn1:der-encode-integer n)
   (asn1:der-encode-integer e)))

(defun decode-pkcs1-rsa-public (der)
  "Decode PKCS#1 RSAPublicKey DER. Returns (values n e)."
  (let* ((tlv (asn1:der-decode der))
         (children (asn1:der-decode-sequence-contents tlv)))
    (values (asn1:decode-der-integer (asn1:asn1-tlv-value (first children)))
            (asn1:decode-der-integer (asn1:asn1-tlv-value (second children))))))

(defun encode-pkcs1-rsa-private (n e d &key p q dp dq qinv)
  "Encode RSA private key as PKCS#1 RSAPrivateKey DER.
   RSAPrivateKey ::= SEQUENCE { version, n, e, d, p, q, dp, dq, qinv }"
  (apply #'asn1:der-encode-sequence
         (asn1:der-encode-integer 0)  ; version
         (asn1:der-encode-integer n)
         (asn1:der-encode-integer e)
         (asn1:der-encode-integer d)
         (append
          (when p
            (list (asn1:der-encode-integer p)
                  (asn1:der-encode-integer q)
                  (asn1:der-encode-integer dp)
                  (asn1:der-encode-integer dq)
                  (asn1:der-encode-integer qinv))))))

(defun decode-pkcs1-rsa-private (der)
  "Decode PKCS#1 RSAPrivateKey DER.
   Returns (values n e d p q dp dq qinv)."
  (let* ((tlv (asn1:der-decode der))
         (children (asn1:der-decode-sequence-contents tlv)))
    ;; children: version, n, e, d, [p, q, dp, dq, qinv]
    (let ((n (asn1:decode-der-integer (asn1:asn1-tlv-value (second children))))
          (e (asn1:decode-der-integer (asn1:asn1-tlv-value (third children))))
          (d (asn1:decode-der-integer (asn1:asn1-tlv-value (fourth children)))))
      (if (> (length children) 4)
          (values n e d
                  (asn1:decode-der-integer (asn1:asn1-tlv-value (fifth children)))
                  (asn1:decode-der-integer (asn1:asn1-tlv-value (sixth children)))
                  (asn1:decode-der-integer (asn1:asn1-tlv-value (seventh children)))
                  (asn1:decode-der-integer (asn1:asn1-tlv-value (eighth children)))
                  (asn1:decode-der-integer (asn1:asn1-tlv-value (ninth children))))
          (values n e d)))))

;;; ---------------------------------------------------------------------------
;;; SubjectPublicKeyInfo (RFC 5280)
;;; ---------------------------------------------------------------------------

(defun encode-spki-rsa (n e)
  "Encode RSA public key as SubjectPublicKeyInfo DER."
  (let ((pkcs1 (encode-pkcs1-rsa-public n e)))
    (asn1:der-encode-sequence
     ;; AlgorithmIdentifier { OID rsaEncryption, NULL }
     (asn1:der-encode-sequence
      (asn1:der-encode-oid +oid-rsa-encryption+)
      (asn1:der-encode-null))
     ;; BIT STRING containing PKCS#1 RSAPublicKey
     (asn1:der-encode-bit-string pkcs1))))

(defun encode-spki-ec (public-key-bytes &optional (curve-oid +oid-prime256v1+))
  "Encode EC public key as SubjectPublicKeyInfo DER.
   PUBLIC-KEY-BYTES is the uncompressed SEC1 encoding (04 || x || y)."
  (asn1:der-encode-sequence
   ;; AlgorithmIdentifier { OID ecPublicKey, OID curve }
   (asn1:der-encode-sequence
    (asn1:der-encode-oid +oid-ec-public-key+)
    (asn1:der-encode-oid curve-oid))
   ;; BIT STRING containing the public key point
   (asn1:der-encode-bit-string public-key-bytes)))

(defun encode-spki-ed25519 (public-key-bytes)
  "Encode Ed25519 public key as SubjectPublicKeyInfo DER.
   PUBLIC-KEY-BYTES is the 32-byte encoded public key."
  (asn1:der-encode-sequence
   ;; AlgorithmIdentifier { OID Ed25519 } (no parameters)
   (asn1:der-encode-sequence
    (asn1:der-encode-oid +oid-ed25519+))
   ;; BIT STRING containing the 32-byte public key
   (asn1:der-encode-bit-string public-key-bytes)))

(defun decode-spki (der)
  "Decode SubjectPublicKeyInfo DER.
   Returns (values algorithm-oid parameters-tlv public-key-bytes)."
  (let* ((tlv (asn1:der-decode der))
         (children (asn1:der-decode-sequence-contents tlv))
         ;; AlgorithmIdentifier
         (alg-id (first children))
         (alg-children (asn1:der-decode-sequence-contents alg-id))
         (oid-bytes (asn1:asn1-tlv-value (first alg-children)))
         (oid (asn1:decode-oid-value oid-bytes))
         (params (when (> (length alg-children) 1) (second alg-children)))
         ;; BIT STRING
         (bit-string-value (asn1:asn1-tlv-value (second children)))
         ;; Remove the unused-bits byte
         (key-bytes (subseq bit-string-value 1)))
    (values oid params key-bytes)))

;;; ---------------------------------------------------------------------------
;;; PKCS#8 PrivateKeyInfo (RFC 5958)
;;; ---------------------------------------------------------------------------

(defun encode-pkcs8-ed25519 (private-key-bytes)
  "Encode Ed25519 private key as PKCS#8 PrivateKeyInfo DER.
   PRIVATE-KEY-BYTES is the 32-byte private key seed."
  ;; The private key is wrapped in an OCTET STRING inside the PKCS#8 structure
  (let ((inner-key (asn1:der-encode-octet-string private-key-bytes)))
    (asn1:der-encode-sequence
     ;; version
     (asn1:der-encode-integer 0)
     ;; AlgorithmIdentifier { OID Ed25519 }
     (asn1:der-encode-sequence
      (asn1:der-encode-oid +oid-ed25519+))
     ;; OCTET STRING containing the CurvePrivateKey (another OCTET STRING)
     (asn1:der-encode-octet-string inner-key))))

(defun encode-pkcs8-ec (private-key-integer public-key-bytes
                         &optional (curve-oid +oid-prime256v1+))
  "Encode EC private key as PKCS#8 PrivateKeyInfo DER."
  (let* ((key-len (/ (1- (length public-key-bytes)) 2))  ; 32 for P-256
         (d-bytes (asn1:integer-to-der-bytes private-key-integer))
         ;; Pad to key-len if needed
         (padded (if (< (length d-bytes) key-len)
                     (let ((result (make-array key-len :element-type '(unsigned-byte 8) :initial-element 0)))
                       (replace result d-bytes :start1 (- key-len (length d-bytes)))
                       result)
                     d-bytes))
         ;; ECPrivateKey (SEC 1)
         (ec-priv (asn1:der-encode-sequence
                   (asn1:der-encode-integer 1)  ; version
                   (asn1:der-encode-octet-string padded)
                   ;; [1] public key (optional)
                   (asn1:der-encode-context 1 (asn1:der-encode-bit-string public-key-bytes)
                                            :constructed t))))
    (asn1:der-encode-sequence
     ;; version
     (asn1:der-encode-integer 0)
     ;; AlgorithmIdentifier
     (asn1:der-encode-sequence
      (asn1:der-encode-oid +oid-ec-public-key+)
      (asn1:der-encode-oid curve-oid))
     ;; OCTET STRING containing ECPrivateKey
     (asn1:der-encode-octet-string ec-priv))))

(defun decode-pkcs8 (der)
  "Decode PKCS#8 PrivateKeyInfo DER.
   Returns (values algorithm-oid parameters-tlv private-key-bytes)."
  (let* ((tlv (asn1:der-decode der))
         (children (asn1:der-decode-sequence-contents tlv))
         ;; Skip version (first child)
         ;; AlgorithmIdentifier
         (alg-id (second children))
         (alg-children (asn1:der-decode-sequence-contents alg-id))
         (oid-bytes (asn1:asn1-tlv-value (first alg-children)))
         (oid (asn1:decode-oid-value oid-bytes))
         (params (when (> (length alg-children) 1) (second alg-children)))
         ;; OCTET STRING containing the key
         (key-octet-string (asn1:asn1-tlv-value (third children))))
    (values oid params key-octet-string)))

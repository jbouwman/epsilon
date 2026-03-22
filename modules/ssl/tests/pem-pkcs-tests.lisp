;;;; Tests for PEM and PKCS encoding/decoding

(defpackage epsilon.ssl.pem-pkcs-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:pem #:epsilon.ssl.pem)
   (#:pkcs #:epsilon.ssl.pkcs)
   (#:asn1 #:epsilon.ssl.asn1))
  (:enter t))

(in-package :epsilon.ssl.pem-pkcs-tests)

;;; ---------------------------------------------------------------------------
;;; PEM encoding/decoding
;;; ---------------------------------------------------------------------------

(deftest test-pem-encode-decode-roundtrip
  "PEM encode/decode round-trip"
  (let* ((data (make-array 16 :element-type '(unsigned-byte 8)
                              :initial-contents '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
         (block (pem:make-pem-block "TEST" data))
         (encoded (pem:pem-encode block))
         (decoded (pem:pem-decode encoded)))
    (assert-true decoded)
    (assert-equal (pem:pem-block-label decoded) "TEST")
    (assert-equalp (pem:pem-block-data decoded) data)))

(deftest test-pem-encode-certificate-label
  "PEM encoding uses correct labels"
  (let* ((data (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4)))
         (block (pem:make-pem-block "CERTIFICATE" data))
         (encoded (pem:pem-encode block)))
    (assert-true (search "-----BEGIN CERTIFICATE-----" encoded))
    (assert-true (search "-----END CERTIFICATE-----" encoded))))

(deftest test-pem-decode-real-format
  "PEM decode of real-format PEM text"
  (let* ((pem-text (format nil "-----BEGIN PUBLIC KEY-----~%dGVzdA==~%-----END PUBLIC KEY-----~%"))
         (block (pem:pem-decode pem-text)))
    (assert-true block)
    (assert-equal (pem:pem-block-label block) "PUBLIC KEY")
    ;; "test" in base64 = dGVzdA==
    (assert-equalp (pem:pem-block-data block)
                   (make-array 4 :element-type '(unsigned-byte 8)
                                 :initial-contents '(116 101 115 116)))))

(deftest test-pem-decode-multiple-blocks
  "PEM decode of multiple blocks"
  (let* ((pem-text (format nil "-----BEGIN A-----~%AQID~%-----END A-----~%-----BEGIN B-----~%BAUF~%-----END B-----~%"))
         (blocks (pem:pem-decode-all pem-text)))
    (assert-= (length blocks) 2)
    (assert-equal (pem:pem-block-label (first blocks)) "A")
    (assert-equal (pem:pem-block-label (second blocks)) "B")))

(deftest test-pem-large-data
  "PEM with data that spans multiple 64-char lines"
  (let* ((data (make-array 100 :element-type '(unsigned-byte 8) :initial-element #xAA))
         (block (pem:make-pem-block "TEST" data))
         (encoded (pem:pem-encode block))
         (decoded (pem:pem-decode encoded)))
    (assert-equalp (pem:pem-block-data decoded) data)))

;;; ---------------------------------------------------------------------------
;;; PKCS#1 RSA key encoding
;;; ---------------------------------------------------------------------------

(deftest test-pkcs1-rsa-public-roundtrip
  "PKCS#1 RSAPublicKey encode/decode round-trip"
  (let* ((n (* 61 53))  ; 3233
         (e 17)
         (der (pkcs:encode-pkcs1-rsa-public n e)))
    (multiple-value-bind (n2 e2) (pkcs:decode-pkcs1-rsa-public der)
      (assert-= n2 n)
      (assert-= e2 e))))

(deftest test-pkcs1-rsa-public-large-key
  "PKCS#1 RSAPublicKey with larger key"
  (let* ((n #xd7b4f0c8c9e67a58b4e3b0c0a7e5f9d32c6a1f8e5d4c3b2a1908070605040301)
         (e 65537)
         (der (pkcs:encode-pkcs1-rsa-public n e)))
    (multiple-value-bind (n2 e2) (pkcs:decode-pkcs1-rsa-public der)
      (assert-= n2 n)
      (assert-= e2 e))))

(deftest test-pkcs1-rsa-private-roundtrip
  "PKCS#1 RSAPrivateKey encode/decode round-trip"
  (let* ((p 61) (q 53)
         (n (* p q))
         (e 17) (d 2753))
    (let ((der (pkcs:encode-pkcs1-rsa-private n e d :p p :q q
                                                :dp (mod d (1- p))
                                                :dq (mod d (1- q))
                                                :qinv 38)))
      (multiple-value-bind (n2 e2 d2 p2 q2) (pkcs:decode-pkcs1-rsa-private der)
        (assert-= n2 n)
        (assert-= e2 e)
        (assert-= d2 d)
        (assert-= p2 p)
        (assert-= q2 q)))))

;;; ---------------------------------------------------------------------------
;;; SubjectPublicKeyInfo
;;; ---------------------------------------------------------------------------

(deftest test-spki-rsa-roundtrip
  "SubjectPublicKeyInfo RSA encode/decode round-trip"
  (let* ((n (* 61 53))
         (e 17)
         (der (pkcs:encode-spki-rsa n e)))
    (multiple-value-bind (oid _params key-bytes) (pkcs:decode-spki der)
      (declare (ignore _params))
      (assert-equal oid pkcs:+oid-rsa-encryption+)
      ;; key-bytes should be a PKCS#1 RSAPublicKey
      (multiple-value-bind (n2 e2) (pkcs:decode-pkcs1-rsa-public key-bytes)
        (assert-= n2 n)
        (assert-= e2 e)))))

(deftest test-spki-ed25519-roundtrip
  "SubjectPublicKeyInfo Ed25519 encode/decode round-trip"
  (let* ((pk (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x42))
         (der (pkcs:encode-spki-ed25519 pk)))
    (multiple-value-bind (oid _params key-bytes) (pkcs:decode-spki der)
      (declare (ignore _params))
      (assert-equal oid pkcs:+oid-ed25519+)
      (assert-equalp key-bytes pk))))

(deftest test-spki-ec-roundtrip
  "SubjectPublicKeyInfo EC (P-256) encode/decode round-trip"
  ;; Fake 65-byte uncompressed point (04 || x || y)
  (let* ((pk (make-array 65 :element-type '(unsigned-byte 8) :initial-element #x01))
         (der (pkcs:encode-spki-ec pk)))
    (setf (aref pk 0) #x04)
    (setf der (pkcs:encode-spki-ec pk))
    (multiple-value-bind (oid _params key-bytes) (pkcs:decode-spki der)
      (declare (ignore _params))
      (assert-equal oid pkcs:+oid-ec-public-key+)
      (assert-equalp key-bytes pk))))

;;; ---------------------------------------------------------------------------
;;; PKCS#8 PrivateKeyInfo
;;; ---------------------------------------------------------------------------

(deftest test-pkcs8-ed25519-roundtrip
  "PKCS#8 Ed25519 private key encode/decode round-trip"
  (let* ((sk (make-array 32 :element-type '(unsigned-byte 8) :initial-element #xAB))
         (der (pkcs:encode-pkcs8-ed25519 sk)))
    (multiple-value-bind (oid _params key-bytes) (pkcs:decode-pkcs8 der)
      (declare (ignore _params))
      (assert-equal oid pkcs:+oid-ed25519+)
      ;; key-bytes is an OCTET STRING containing another OCTET STRING (the seed)
      (let* ((inner-tlv (asn1:der-decode key-bytes))
             (seed (asn1:asn1-tlv-value inner-tlv)))
        (assert-equalp seed sk)))))

;;; ---------------------------------------------------------------------------
;;; Combined PEM + PKCS
;;; ---------------------------------------------------------------------------

(deftest test-pem-pkcs-rsa-public
  "PEM-encoded RSA public key round-trip"
  (let* ((n 3233) (e 17)
         (der (pkcs:encode-spki-rsa n e))
         (block (pem:make-pem-block "PUBLIC KEY" der))
         (pem-text (pem:pem-encode block))
         (decoded-block (pem:pem-decode pem-text)))
    (assert-equal (pem:pem-block-label decoded-block) "PUBLIC KEY")
    (multiple-value-bind (_oid _params key-bytes) (pkcs:decode-spki (pem:pem-block-data decoded-block))
      (declare (ignore _oid _params))
      (multiple-value-bind (n2 e2) (pkcs:decode-pkcs1-rsa-public key-bytes)
        (assert-= n2 n)
        (assert-= e2 e)))))

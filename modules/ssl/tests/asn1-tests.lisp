;;;; Tests for ASN.1 DER encoding/decoding

(defpackage epsilon.ssl.asn1-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:asn1 #:epsilon.ssl.asn1))
  (:enter t))

(in-package :epsilon.ssl.asn1-tests)

;;; ---------------------------------------------------------------------------
;;; Integer encoding/decoding
;;; ---------------------------------------------------------------------------

(deftest test-der-integer-zero
  "DER INTEGER encoding of zero"
  (let ((encoded (asn1:der-encode-integer 0)))
    (assert-equalp encoded (hex-to-bytes "020100"))))

(deftest test-der-integer-positive-small
  "DER INTEGER encoding of small positive"
  (let ((encoded (asn1:der-encode-integer 42)))
    (assert-equalp encoded (hex-to-bytes "02012a"))))

(deftest test-der-integer-127
  "DER INTEGER encoding of 127"
  (let ((encoded (asn1:der-encode-integer 127)))
    (assert-equalp encoded (hex-to-bytes "02017f"))))

(deftest test-der-integer-128
  "DER INTEGER encoding of 128 (needs padding byte)"
  (let ((encoded (asn1:der-encode-integer 128)))
    (assert-equalp encoded (hex-to-bytes "02020080"))))

(deftest test-der-integer-256
  "DER INTEGER encoding of 256"
  (let ((encoded (asn1:der-encode-integer 256)))
    (assert-equalp encoded (hex-to-bytes "02020100"))))

(deftest test-der-integer-negative
  "DER INTEGER encoding of -1"
  (let ((encoded (asn1:der-encode-integer -1)))
    (assert-equalp encoded (hex-to-bytes "0201ff"))))

(deftest test-der-integer-negative-128
  "DER INTEGER encoding of -128"
  (let ((encoded (asn1:der-encode-integer -128)))
    (assert-equalp encoded (hex-to-bytes "020180"))))

(deftest test-der-integer-negative-129
  "DER INTEGER encoding of -129"
  (let ((encoded (asn1:der-encode-integer -129)))
    (assert-equalp encoded (hex-to-bytes "0202ff7f"))))

(deftest test-decode-integer-zero
  "Decode DER INTEGER zero"
  (assert-= (asn1:decode-der-integer (hex-to-bytes "00")) 0))

(deftest test-decode-integer-positive
  "Decode DER INTEGER positive"
  (assert-= (asn1:decode-der-integer (hex-to-bytes "2a")) 42)
  (assert-= (asn1:decode-der-integer (hex-to-bytes "0080")) 128))

(deftest test-decode-integer-negative
  "Decode DER INTEGER negative"
  (assert-= (asn1:decode-der-integer (hex-to-bytes "ff")) -1)
  (assert-= (asn1:decode-der-integer (hex-to-bytes "80")) -128))

(deftest test-integer-roundtrip
  "Integer encode/decode round-trip"
  (loop for n in '(0 1 42 127 128 255 256 65535 -1 -128 -129 -32768 123456789)
        do (let* ((encoded (asn1:integer-to-der-bytes n))
                  (decoded (asn1:decode-der-integer encoded)))
             (assert-= decoded n))))

;;; ---------------------------------------------------------------------------
;;; OID encoding/decoding
;;; ---------------------------------------------------------------------------

(deftest test-oid-encode-rsa
  "Encode RSA OID (1.2.840.113549.1.1.1)"
  (let ((encoded (asn1:der-encode-oid '(1 2 840 113549 1 1 1))))
    ;; 06 09 2a 86 48 86 f7 0d 01 01 01
    (assert-equalp encoded (hex-to-bytes "06092a864886f70d010101"))))

(deftest test-oid-decode-rsa
  "Decode RSA OID"
  (let* ((components (asn1:decode-oid-value (hex-to-bytes "2a864886f70d010101"))))
    (assert-equal components '(1 2 840 113549 1 1 1))))

(deftest test-oid-encode-sha256
  "Encode SHA-256 OID (2.16.840.1.101.3.4.2.1)"
  (let ((encoded (asn1:der-encode-oid '(2 16 840 1 101 3 4 2 1))))
    (assert-equalp encoded (hex-to-bytes "0609608648016503040201"))))

(deftest test-oid-string-roundtrip
  "OID string conversion round-trip"
  (let* ((oid '(1 2 840 113549 1 1 11))
         (str (asn1:oid-to-string oid))
         (parsed (asn1:string-to-oid str)))
    (assert-equal str "1.2.840.113549.1.1.11")
    (assert-equal parsed oid)))

;;; ---------------------------------------------------------------------------
;;; TLV decode/encode round-trips
;;; ---------------------------------------------------------------------------

(deftest test-decode-null
  "Decode DER NULL"
  (multiple-value-bind (tlv pos) (asn1:der-decode (hex-to-bytes "0500"))
    (assert-= (asn1:asn1-tlv-tag tlv) asn1:+tag-null+)
    (assert-= (length (asn1:asn1-tlv-value tlv)) 0)
    (assert-= pos 2)))

(deftest test-decode-boolean
  "Decode DER BOOLEAN"
  (multiple-value-bind (tlv pos) (asn1:der-decode (hex-to-bytes "0101ff"))
    (assert-= (asn1:asn1-tlv-tag tlv) asn1:+tag-boolean+)
    (assert-= (aref (asn1:asn1-tlv-value tlv) 0) #xFF)
    (assert-= pos 3)))

(deftest test-decode-sequence
  "Decode DER SEQUENCE containing INTEGER and NULL"
  ;; Build SEQUENCE { INTEGER 42, NULL } programmatically
  (let ((data (asn1:der-encode-sequence
               (asn1:der-encode-integer 42)
               (asn1:der-encode-null))))
    (multiple-value-bind (tlv pos) (asn1:der-decode data)
      (assert-= (asn1:asn1-tlv-tag tlv) asn1:+tag-sequence+)
      (assert-true (asn1:asn1-tlv-constructed-p tlv))
      (let ((children (asn1:asn1-tlv-value tlv)))
        (assert-= (length children) 2)
        (assert-= (asn1:asn1-tlv-tag (first children)) asn1:+tag-integer+)
        (assert-= (asn1:asn1-tlv-tag (second children)) asn1:+tag-null+))
      (assert-= pos (length data)))))

(deftest test-encode-decode-roundtrip-integer
  "DER INTEGER encode/decode round-trip via TLV"
  (let* ((encoded (asn1:der-encode-integer 65537))
         (tlv (asn1:der-decode encoded))
         (re-encoded (asn1:der-encode tlv)))
    (assert-equalp encoded re-encoded)))

(deftest test-encode-decode-roundtrip-sequence
  "DER SEQUENCE encode/decode round-trip"
  (let* ((encoded (asn1:der-encode-sequence
                   (asn1:der-encode-integer 1)
                   (asn1:der-encode-octet-string
                    (make-array 4 :element-type '(unsigned-byte 8)
                                  :initial-contents '(1 2 3 4)))
                   (asn1:der-encode-null)))
         (tlv (asn1:der-decode encoded))
         (re-encoded (asn1:der-encode tlv)))
    (assert-equalp encoded re-encoded)))

(deftest test-encode-decode-roundtrip-oid
  "DER OID encode/decode round-trip via TLV"
  (let* ((encoded (asn1:der-encode-oid '(1 2 840 113549 1 1 1)))
         (tlv (asn1:der-decode encoded))
         (re-encoded (asn1:der-encode tlv)))
    (assert-equalp encoded re-encoded)))

;;; ---------------------------------------------------------------------------
;;; Bit string
;;; ---------------------------------------------------------------------------

(deftest test-encode-bit-string
  "DER BIT STRING encoding"
  (let* ((content (make-array 3 :element-type '(unsigned-byte 8)
                                :initial-contents '(#x01 #x02 #x03)))
         (encoded (asn1:der-encode-bit-string content)))
    ;; Tag 03, Length 04, unused-bits 00, data 01 02 03
    (assert-equalp encoded (hex-to-bytes "030400010203"))))

;;; ---------------------------------------------------------------------------
;;; Context-specific tags
;;; ---------------------------------------------------------------------------

(deftest test-context-specific-encode
  "Context-specific tag encoding"
  (let* ((inner (asn1:der-encode-integer 42))
         (encoded (asn1:der-encode-context 0 inner)))
    (multiple-value-bind (tlv pos) (asn1:der-decode encoded)
      (assert-= (asn1:asn1-tlv-class tlv) asn1:+context-specific+)
      (assert-= (asn1:asn1-tlv-tag tlv) 0)
      (assert-= pos (length encoded)))))

;;; ---------------------------------------------------------------------------
;;; Long length encoding
;;; ---------------------------------------------------------------------------

(deftest test-long-length
  "DER long length encoding (> 127 bytes)"
  (let* ((big-data (make-array 200 :element-type '(unsigned-byte 8) :initial-element #xAA))
         (encoded (asn1:der-encode-octet-string big-data)))
    (multiple-value-bind (tlv pos) (asn1:der-decode encoded)
      (assert-= (length (asn1:asn1-tlv-value tlv)) 200)
      (assert-= pos (length encoded)))))

;;; ---------------------------------------------------------------------------
;;; Nested sequences
;;; ---------------------------------------------------------------------------

(deftest test-nested-sequence
  "Nested SEQUENCE structures"
  (let* ((inner (asn1:der-encode-sequence
                 (asn1:der-encode-integer 1)
                 (asn1:der-encode-integer 2)))
         (outer (asn1:der-encode-sequence
                 inner
                 (asn1:der-encode-integer 3))))
    (multiple-value-bind (tlv pos) (asn1:der-decode outer)
      (let ((children (asn1:asn1-tlv-value tlv)))
        (assert-= (length children) 2)
        ;; First child is a SEQUENCE
        (assert-true (asn1:asn1-tlv-constructed-p (first children)))
        (assert-= (length (asn1:asn1-tlv-value (first children))) 2)
        ;; Second child is INTEGER 3
        (assert-= (asn1:asn1-tlv-tag (second children)) asn1:+tag-integer+))
      (assert-= pos (length outer)))))

;;;; Signed Certificate Timestamp parsing (RFC 6962, Certificate Transparency)
;;;;
;;;; A `SignedCertificateTimestamp' is the receipt a CT log issues when
;;;; it accepts a (pre)certificate, proving the cert is publicly
;;;; logged. Browsers and TLS-monitoring tooling consult these to
;;;; detect mis-issuance.
;;;;
;;;; Wire format (RFC 6962 §3.2, TLS-style binary, all big-endian):
;;;;
;;;;   struct {
;;;;     Version    sct_version;       // 1 byte; v1 = 0
;;;;     LogID      id;                 // 32 bytes (SHA-256 of log pubkey)
;;;;     uint64     timestamp;          // 8 bytes; ms since Unix epoch
;;;;     CtExtensions extensions;       // u16-length-prefixed
;;;;     digitally-signed struct {      // u8 hash_alg
;;;;       ...                          // u8 sig_alg
;;;;       ...                          // u16 sig_length
;;;;       opaque signature[]           // <sig_length> bytes
;;;;     };
;;;;   } SignedCertificateTimestamp;
;;;;
;;;; A `SignedCertificateTimestampList' (RFC 6962 §3.3) carries a list
;;;; of SCTs inside a single OCTET STRING:
;;;;
;;;;   uint16 list_length;
;;;;   repeated:
;;;;     uint16 sct_length;
;;;;     SCT bytes
;;;;
;;;; Two delivery channels carry the list:
;;;;   - X.509 extension OID 1.3.6.1.4.1.11129.2.4.2 (embedded in cert).
;;;;     The extension's extnValue OCTET STRING wraps a DER OCTET STRING
;;;;     whose contents are the SCTList.
;;;;   - TLS extension `signed_certificate_timestamp' (#18). The
;;;;     extension_data IS the SCTList bytes (no DER wrapping).

(defpackage epsilon.crypto.sct
  (:use :cl)
  (:import (epsilon.crypto.asn1 asn1)
           (epsilon.crypto.x509 x509))
  (:export
   ;; SCT structure
   #:sct
   #:sct-p
   #:sct-version
   #:sct-log-id
   #:sct-timestamp
   #:sct-extensions
   #:sct-hash-algorithm
   #:sct-signature-algorithm
   #:sct-signature
   #:sct-raw-bytes
   ;; Algorithm keyword helpers
   #:sct-hash-keyword
   #:sct-sig-keyword
   ;; OIDs and helpers
   #:+oid-ct-precertificate-scts+
   #:parse-sct
   #:parse-sct-list
   #:cert-embedded-scts))

(in-package :epsilon.crypto.sct)

(defparameter +oid-ct-precertificate-scts+
  '(1 3 6 1 4 1 11129 2 4 2)
  "X.509 extension OID for SignedCertificateTimestampList embedded in a
   leaf certificate (RFC 6962 §3.3).")

(defstruct sct
  "One SignedCertificateTimestamp. Fields are normalised away from the
   wire format: TIMESTAMP is the integer milliseconds since the Unix
   epoch, EXTENSIONS / LOG-ID / SIGNATURE are byte vectors, and the
   algorithm fields are TLS-style (HASH-ALGORITHM / SIGNATURE-ALGORITHM
   are single-byte codepoints; convenience accessors below map them to
   keywords)."
  (version 0 :type fixnum)
  (log-id #() :type (simple-array (unsigned-byte 8) (*)))
  (timestamp 0 :type integer)
  (extensions #() :type (simple-array (unsigned-byte 8) (*)))
  (hash-algorithm 0 :type fixnum)
  (signature-algorithm 0 :type fixnum)
  (signature #() :type (simple-array (unsigned-byte 8) (*)))
  (raw-bytes #() :type (simple-array (unsigned-byte 8) (*))))

;;; ---------------------------------------------------------------------------
;;; TLS-style integer / byte-string readers
;;; ---------------------------------------------------------------------------

(defun %read-u8 (bytes pos)
  (values (aref bytes pos) (1+ pos)))

(defun %read-u16 (bytes pos)
  (values (logior (ash (aref bytes pos) 8) (aref bytes (1+ pos)))
          (+ pos 2)))

(defun %read-u64 (bytes pos)
  (let ((v 0))
    (loop for i from 0 below 8
          do (setf v (logior (ash v 8) (aref bytes (+ pos i)))))
    (values v (+ pos 8))))

(defun %read-bytes (bytes pos len)
  (values (subseq bytes pos (+ pos len)) (+ pos len)))

;;; ---------------------------------------------------------------------------
;;; TLS algorithm code -> keyword (RFC 5246 / RFC 8446)
;;; ---------------------------------------------------------------------------

(defun sct-hash-keyword (sct)
  "Map the TLS HashAlgorithm byte to a keyword (or :unknown)."
  (case (sct-hash-algorithm sct)
    (0 :none)
    (1 :md5)
    (2 :sha1)
    (3 :sha224)
    (4 :sha256)
    (5 :sha384)
    (6 :sha512)
    (otherwise :unknown)))

(defun sct-sig-keyword (sct)
  "Map the TLS SignatureAlgorithm byte to a keyword (or :unknown)."
  (case (sct-signature-algorithm sct)
    (0 :anonymous)
    (1 :rsa)
    (2 :dsa)
    (3 :ecdsa)
    (otherwise :unknown)))

;;; ---------------------------------------------------------------------------
;;; Parsing
;;; ---------------------------------------------------------------------------

(defun parse-sct (bytes &key (start 0) (end nil))
  "Parse one SCT body (no length prefix) from BYTES[START..END].
   Returns an `sct' struct."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (let* ((end (or end (length bytes)))
         (pos start)
         (raw (subseq bytes start end)))
    (multiple-value-bind (version p) (%read-u8 bytes pos)
      (setf pos p)
      (multiple-value-bind (log-id p) (%read-bytes bytes pos 32)
        (setf pos p)
        (multiple-value-bind (timestamp p) (%read-u64 bytes pos)
          (setf pos p)
          (multiple-value-bind (ext-len p) (%read-u16 bytes pos)
            (setf pos p)
            (multiple-value-bind (extensions p) (%read-bytes bytes pos ext-len)
              (setf pos p)
              (multiple-value-bind (hash-alg p) (%read-u8 bytes pos)
                (setf pos p)
                (multiple-value-bind (sig-alg p) (%read-u8 bytes pos)
                  (setf pos p)
                  (multiple-value-bind (sig-len p) (%read-u16 bytes pos)
                    (setf pos p)
                    (multiple-value-bind (signature p)
                        (%read-bytes bytes pos sig-len)
                      (declare (ignore p))
                      (make-sct :version version
                                :log-id log-id
                                :timestamp timestamp
                                :extensions extensions
                                :hash-algorithm hash-alg
                                :signature-algorithm sig-alg
                                :signature signature
                                :raw-bytes raw))))))))))))

(defun parse-sct-list (bytes &key (start 0) (end nil))
  "Parse a SignedCertificateTimestampList body (RFC 6962 §3.3):

     uint16 list_length;
     repeated: { uint16 sct_length; <sct bytes> }

   BYTES[START..END] should be the unwrapped list bytes -- if you have
   the X.509 extension's extnValue, see `cert-embedded-scts' which
   handles the extra DER OCTET STRING layer."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (let* ((end (or end (length bytes)))
         (pos start))
    (multiple-value-bind (list-len p) (%read-u16 bytes pos)
      (setf pos p)
      (let ((list-end (min end (+ pos list-len)))
            (out nil))
        (loop while (< pos list-end)
              do (multiple-value-bind (sct-len p) (%read-u16 bytes pos)
                   (setf pos p)
                   (push (parse-sct bytes :start pos :end (+ pos sct-len))
                         out)
                   (incf pos sct-len)))
        (nreverse out)))))

(defun cert-embedded-scts (cert)
  "Extract the list of `sct' structs embedded in the given X.509
   certificate via the `1.3.6.1.4.1.11129.2.4.2' extension. Returns
   NIL when the extension is absent.

   The extension's extnValue is itself a DER OCTET STRING wrapping the
   SCTList; we unwrap that layer here so callers don't have to."
  (let ((ext (x509:x509-get-extension cert +oid-ct-precertificate-scts+)))
    (when ext
      (let* ((outer-bytes (x509:x509-ext-value ext))
             ;; outer-bytes is itself a DER OCTET STRING -- decode it
             ;; to get to the raw SCTList wire form.
             (inner-tlv (asn1:der-decode outer-bytes))
             (list-bytes (asn1:asn1-tlv-value inner-tlv)))
        (parse-sct-list list-bytes)))))

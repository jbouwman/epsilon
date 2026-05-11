;;;; Tests for the OCSP response parser.

(defpackage epsilon.crypto.ocsp-tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.crypto.ocsp ocsp)
   (epsilon.crypto.asn1 asn1)
   (epsilon.crypto.sha1 sha1)
   (epsilon.crypto.x509 x509)
   (epsilon.crypto.drbg drbg)
   (epsilon.crypto.ecdh ecdh)
   (epsilon.crypto.ecdsa ecdsa)
   (epsilon.crypto.ec-p256 ec-p256)
   (epsilon.crypto.ed25519-sign ed-sign)))

(in-package :epsilon.crypto.ocsp-tests)

;;; ---------------------------------------------------------------------------
;;; Fixture builders
;;;
;;; Rather than carry a binary blob in the repo we synthesize a minimal
;;; BasicOCSPResponse + OCSPResponse via the asn1 encoder. This exercises
;;; every code path in the parser without depending on a captured Let's
;;; Encrypt response (those will land in the fetcher's tests).
;;; ---------------------------------------------------------------------------

(defun %bytes (s)
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code s))

(defun %generalized-time (s)
  (asn1:der-encode-tagged 24 (%bytes s)))

(defun %sha1-algid ()
  ;; SHA-1 OID 1.3.14.3.2.26 with NULL parameters.
  (asn1:der-encode-sequence
   (asn1:der-encode-oid '(1 3 14 3 2 26))
   (asn1:der-encode-null)))

(defun %ecdsa-sha256-algid ()
  (asn1:der-encode-sequence
   (asn1:der-encode-oid '(1 2 840 10045 4 3 2))))

(defun %make-cert-id (&optional (serial 42))
  ;; CertID ::= SEQUENCE { hashAlgorithm, issuerNameHash, issuerKeyHash, serial }
  ;; The exact hash values do not matter for the parser test -- it
  ;; skips the certID.
  (asn1:der-encode-sequence
   (%sha1-algid)
   (asn1:der-encode-octet-string (make-array 20 :element-type '(unsigned-byte 8)
                                                :initial-element 0))
   (asn1:der-encode-octet-string (make-array 20 :element-type '(unsigned-byte 8)
                                                :initial-element 1))
   (asn1:der-encode-integer serial)))

(defun %good-status ()
  ;; CertStatus ::= CHOICE { good [0] IMPLICIT NULL, ... }
  ;; IMPLICIT NULL with context tag 0: empty primitive context-0 TLV.
  (asn1:der-encode-context 0 (make-array 0 :element-type '(unsigned-byte 8))))

(defun %revoked-status (revocation-time)
  ;; revoked [1] IMPLICIT RevokedInfo ::= SEQUENCE { revocationTime, ... }
  ;; With IMPLICIT, the SEQUENCE tag is replaced by context [1] constructed.
  (asn1:der-encode-context 1 (%generalized-time revocation-time)
                              :constructed t))

(defun %unknown-status ()
  ;; unknown [2] IMPLICIT UnknownInfo ::= NULL
  (asn1:der-encode-context 2 (make-array 0 :element-type '(unsigned-byte 8))))

(defun %make-single-response (cert-status &key (this-update "20260408000000Z")
                                                next-update)
  (let ((children
          (list (%make-cert-id)
                cert-status
                (%generalized-time this-update))))
    (when next-update
      (setf children (append children
                             (list (asn1:der-encode-context
                                    0 (%generalized-time next-update)
                                    :constructed t)))))
    (apply #'asn1:der-encode-sequence children)))

(defun %make-response-data (single-responses)
  ;; ResponseData ::= SEQUENCE {
  ;;   version [0] EXPLICIT Version DEFAULT v1,  -- omitted (default)
  ;;   responderID            ResponderID,
  ;;   producedAt             GeneralizedTime,
  ;;   responses              SEQUENCE OF SingleResponse,
  ;;   responseExtensions [1] EXPLICIT Extensions OPTIONAL }  -- omitted
  (asn1:der-encode-sequence
   ;; responderID -- byKey [2] KeyHash. With IMPLICIT it's a primitive
   ;; context-2 OCTET STRING. The parser ignores responderID contents.
   (asn1:der-encode-context 2 (make-array 20 :element-type '(unsigned-byte 8)
                                              :initial-element 0))
   (%generalized-time "20260408000000Z")
   (apply #'asn1:der-encode-sequence single-responses)))

(defun %make-basic-ocsp-response (single-responses)
  (asn1:der-encode-sequence
   (%make-response-data single-responses)
   (%ecdsa-sha256-algid)
   (asn1:der-encode-bit-string (make-array 64 :element-type '(unsigned-byte 8)
                                              :initial-element 0))))

(defun %make-ocsp-response (status &key basic-bytes)
  ;; OCSPResponse ::= SEQUENCE {
  ;;   responseStatus  ENUMERATED,
  ;;   responseBytes [0] EXPLICIT ResponseBytes OPTIONAL }
  (let* ((status-bytes (asn1:der-encode-tagged
                        10 (asn1::integer-to-der-bytes status))))
    (cond
      ((null basic-bytes)
       (asn1:der-encode-sequence status-bytes))
      (t
       (let ((response-bytes
               (asn1:der-encode-sequence
                (asn1:der-encode-oid '(1 3 6 1 5 5 7 48 1 1))
                (asn1:der-encode-octet-string basic-bytes))))
         (asn1:der-encode-sequence
          status-bytes
          (asn1:der-encode-context 0 response-bytes :constructed t)))))))

;;; ---------------------------------------------------------------------------
;;; Tests
;;; ---------------------------------------------------------------------------

(deftest test-parse-good-single-response-with-next-update
  "Successful response with one good cert and a nextUpdate field."
  (let* ((sr (%make-single-response (%good-status)
                                    :this-update "20260408000000Z"
                                    :next-update "20260415000000Z"))
         (basic (%make-basic-ocsp-response (list sr)))
         (top   (%make-ocsp-response 0 :basic-bytes basic))
         (parsed (ocsp:parse-ocsp-response top)))
    (assert-eq :successful (ocsp:ocsp-response-status parsed))
    (assert-true (ocsp:ocsp-response-basic-bytes parsed))
    (let ((srs (ocsp:ocsp-response-single-responses parsed)))
      (assert-= 1 (length srs))
      (let ((s (first srs)))
        (assert-eq :good (ocsp:ocsp-single-response-cert-status s))
        (assert-equal "20260408000000Z"
                      (ocsp:ocsp-single-response-this-update s))
        (assert-equal "20260415000000Z"
                      (ocsp:ocsp-single-response-next-update s))))))

(deftest test-parse-good-single-response-without-next-update
  "nextUpdate is OPTIONAL; the parser must accept its absence."
  (let* ((sr (%make-single-response (%good-status)))
         (basic (%make-basic-ocsp-response (list sr)))
         (top (%make-ocsp-response 0 :basic-bytes basic))
         (parsed (ocsp:parse-ocsp-response top)))
    (assert-eq :successful (ocsp:ocsp-response-status parsed))
    (let ((s (first (ocsp:ocsp-response-single-responses parsed))))
      (assert-eq :good (ocsp:ocsp-single-response-cert-status s))
      (assert-true (ocsp:ocsp-single-response-this-update s))
      (assert-false (ocsp:ocsp-single-response-next-update s)))))

(deftest test-parse-revoked-status
  "A revoked SingleResponse exposes the revocation time."
  (let* ((sr (%make-single-response (%revoked-status "20260101000000Z")))
         (basic (%make-basic-ocsp-response (list sr)))
         (top (%make-ocsp-response 0 :basic-bytes basic))
         (parsed (ocsp:parse-ocsp-response top))
         (s (first (ocsp:ocsp-response-single-responses parsed))))
    (assert-eq :revoked (ocsp:ocsp-single-response-cert-status s))
    (assert-equal "20260101000000Z"
                  (ocsp:ocsp-single-response-revocation-time s))))

(deftest test-parse-unknown-status
  (let* ((sr (%make-single-response (%unknown-status)))
         (basic (%make-basic-ocsp-response (list sr)))
         (top (%make-ocsp-response 0 :basic-bytes basic))
         (parsed (ocsp:parse-ocsp-response top))
         (s (first (ocsp:ocsp-response-single-responses parsed))))
    (assert-eq :unknown (ocsp:ocsp-single-response-cert-status s))))

(deftest test-parse-non-successful-status-has-no-payload
  "A tryLater response is structurally valid but carries no
   responseBytes; the parser surfaces the status keyword and leaves
   single-responses empty."
  (let* ((bytes (%make-ocsp-response 3))
         (parsed (ocsp:parse-ocsp-response bytes)))
    (assert-eq :try-later (ocsp:ocsp-response-status parsed))
    (assert-false (ocsp:ocsp-response-single-responses parsed))))

;;; ---------------------------------------------------------------------------
;;; OCSP request builder
;;; ---------------------------------------------------------------------------

(defun %fresh-self-signed ()
  (let* ((sk (drbg:random-bytes 32))
         (pk (ed-sign:ed25519-public-key-from-private sk))
         (der (x509:make-self-signed-certificate
               :key-type :ed25519 :private-key sk
               :public-key-bytes pk :subject "localhost"
               :dns-names '("localhost"))))
    (x509:parse-x509-certificate der)))

(deftest test-make-ocsp-request-structure
  "OCSPRequest carries a single CertID with SHA-1 algid, SHA-1 hashes
   of the issuer Name DER and the issuer SPK bits, and the target
   cert's serial. For a self-signed cert, issuer=subject, so
   issuerNameHash = SHA-1(subject Name DER) and
   issuerKeyHash = SHA-1(pubkey bytes)."
  (let* ((cert (%fresh-self-signed))
         (req-bytes (ocsp:make-ocsp-request cert cert))
         ;; OCSPRequest ::= SEQUENCE { tbsRequest SEQUENCE { requestList SEQUENCE { Request SEQUENCE { CertID SEQUENCE } } } }
         (ocsp-req (asn1:der-decode req-bytes))
         (tbs (first (asn1:der-decode-sequence-contents ocsp-req)))
         (req-list (first (asn1:der-decode-sequence-contents tbs)))
         (request (first (asn1:der-decode-sequence-contents req-list)))
         (cert-id (first (asn1:der-decode-sequence-contents request)))
         (cid-children (asn1:der-decode-sequence-contents cert-id)))
    ;; hashAlgorithm
    (let* ((alg-children (asn1:der-decode-sequence-contents (first cid-children)))
           (alg-oid (asn1:decode-oid-value
                     (asn1:asn1-tlv-value (first alg-children)))))
      (assert-equal '(1 3 14 3 2 26) alg-oid))
    ;; issuerNameHash
    (let ((name-hash (asn1:asn1-tlv-value (second cid-children))))
      (assert-equalp (sha1:sha1 (x509:x509-cert-raw-subject-bytes cert))
                     name-hash))
    ;; issuerKeyHash
    (let ((key-hash (asn1:asn1-tlv-value (third cid-children))))
      (assert-equalp (sha1:sha1 (x509:x509-cert-public-key-bytes cert))
                     key-hash))
    ;; serialNumber
    (let ((serial (asn1:decode-der-integer
                   (asn1:asn1-tlv-value (fourth cid-children)))))
      (assert-= serial (x509:x509-cert-serial cert)))))

(deftest test-make-ocsp-request-deterministic
  "Building the request twice against the same cert pair yields
   identical bytes. OCSPRequest has no nonce by default, so a stable
   CertID fingerprint is expected."
  (let* ((cert (%fresh-self-signed))
         (a (ocsp:make-ocsp-request cert cert))
         (b (ocsp:make-ocsp-request cert cert)))
    (assert-equalp a b)))

;;; ---------------------------------------------------------------------------
;;; Signature verification
;;;
;;; Generates a P-256 keypair, builds a self-signed responder cert
;;; that carries it, synthesizes a BasicOCSPResponse signed with that
;;; key, and verifies the round trip. A corrupted signature fails
;;; cleanly.
;;; ---------------------------------------------------------------------------

(defun %ecdsa-p256-responder ()
  "Returns (values private-int responder-cert)."
  (multiple-value-bind (priv pub-point) (ecdh:ecdh-p256-generate-keypair)
    (let* ((pub-bytes (ec-p256:p256-point-encode-uncompressed pub-point))
           (der (x509:make-self-signed-certificate
                 :key-type :ecdsa-p256
                 :private-key priv
                 :public-key-bytes pub-bytes
                 :subject "ocsp-responder"
                 :dns-names '("ocsp.example.test"))))
      (values priv (x509:parse-x509-certificate der)))))

(defun %encode-ecdsa-signature (r s)
  (asn1:der-encode-sequence
   (asn1:der-encode-integer r)
   (asn1:der-encode-integer s)))

(defun %build-signed-basic-ocsp-response (private-key)
  "Produce (values raw-ocsp-response-bytes responder-tbs-bytes).
   The returned bytes are a full OCSPResponse suitable for
   parse-ocsp-response + verify-basic-ocsp-response."
  (let* ((sr (%make-single-response (%good-status)
                                    :this-update "20260408000000Z"
                                    :next-update "20260415000000Z"))
         (response-data (%make-response-data (list sr)))
         (sig-alg (asn1:der-encode-sequence
                   (asn1:der-encode-oid '(1 2 840 10045 4 3 2))))
         (sig-bytes (multiple-value-bind (r s)
                        (ecdsa:ecdsa-sign private-key response-data)
                      (%encode-ecdsa-signature r s)))
         (signature-bs (asn1:der-encode-bit-string sig-bytes))
         (basic (asn1:der-encode-sequence
                 response-data
                 sig-alg
                 signature-bs)))
    (values (%make-ocsp-response 0 :basic-bytes basic)
            response-data)))

(deftest test-verify-basic-ocsp-response-ecdsa-p256
  "A BasicOCSPResponse signed with ECDSA-P256-SHA256 verifies against
   the responder cert that carries the matching public key, and a
   corrupted signature fails cleanly."
  (multiple-value-bind (priv responder) (%ecdsa-p256-responder)
    (multiple-value-bind (raw tbs) (%build-signed-basic-ocsp-response priv)
      (declare (ignore tbs))
      (let ((parsed (ocsp:parse-ocsp-response raw)))
        (assert-eq :successful (ocsp:ocsp-response-status parsed))
        (assert-true (ocsp:verify-basic-ocsp-response parsed responder))
        ;; Corrupt the signature bytes: flip one bit.
        (let* ((sig (ocsp:ocsp-response-signature-bytes parsed))
               (corrupt (copy-seq sig)))
          (setf (aref corrupt (1- (length corrupt)))
                (logxor (aref corrupt (1- (length corrupt))) 1))
          (setf (ocsp::ocsp-response-signature-bytes parsed) corrupt)
          (assert-false (ocsp:verify-basic-ocsp-response parsed responder)))))))

(deftest test-verify-basic-ocsp-response-wrong-cert
  "A valid signature does not verify against an unrelated responder
   cert (different keypair)."
  (multiple-value-bind (priv responder) (%ecdsa-p256-responder)
    (declare (ignore responder))
    (multiple-value-bind (raw tbs) (%build-signed-basic-ocsp-response priv)
      (declare (ignore tbs))
      (let ((parsed (ocsp:parse-ocsp-response raw)))
        (multiple-value-bind (ignored other-responder) (%ecdsa-p256-responder)
          (declare (ignore ignored))
          (assert-false (ocsp:verify-basic-ocsp-response parsed other-responder)))))))

(deftest test-parse-multiple-single-responses
  (let* ((sr1 (%make-single-response (%good-status)
                                     :this-update "20260408000000Z"))
         (sr2 (%make-single-response (%revoked-status "20260101000000Z")
                                     :this-update "20260408000000Z"))
         (basic (%make-basic-ocsp-response (list sr1 sr2)))
         (top (%make-ocsp-response 0 :basic-bytes basic))
         (parsed (ocsp:parse-ocsp-response top))
         (srs (ocsp:ocsp-response-single-responses parsed)))
    (assert-= 2 (length srs))
    (assert-eq :good    (ocsp:ocsp-single-response-cert-status (first srs)))
    (assert-eq :revoked (ocsp:ocsp-single-response-cert-status (second srs)))))

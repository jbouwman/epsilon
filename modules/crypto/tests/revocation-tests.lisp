;;;; Tests for revocation-aware certificate-chain validation
;;;;
;;;; Covers the IMPL-329 D1/D2 integration: `verify-certificate-chain'
;;;; consults a caller-supplied list of CRLs, plus an optional
;;;; revocation callback (the OCSP entry point), and rejects the chain
;;;; on a :revoked answer from either source.

(defpackage epsilon.crypto.revocation-tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.crypto.x509 x509)
   (epsilon.crypto.ocsp ocsp)
   (epsilon.crypto.test-pki tpki)
   (epsilon.crypto.asn1 asn1)))

(in-package :epsilon.crypto.revocation-tests)

(defparameter *fixed-now*
  (x509:make-x509-time :year 2026 :month 6 :day 1)
  "A fixed validation time inside the default 1-year cert validity
   window used by `tpki:make-test-chain'.")

(defun %parsed-chain (chain)
  "Return (leaf-parsed root-parsed) from a 2-cert test chain."
  (values (x509:parse-x509-certificate (tpki:test-chain-leaf-cert chain))
          (x509:parse-x509-certificate (tpki:test-chain-root-cert chain))))

;;; ---------------------------------------------------------------------------
;;; CRL integration
;;; ---------------------------------------------------------------------------

(deftest test-chain-passes-with-empty-crl
  "A CRL with no entries does not block a chain."
  (let* ((chain (tpki:make-test-chain)))
    (multiple-value-bind (leaf root) (%parsed-chain chain)
      (let* ((crl-der (x509:make-crl
                       :issuer-cert root
                       :ca-key-type (tpki:test-key-type
                                     (tpki:test-chain-root-key chain))
                       :ca-private-key (tpki:test-key-private-key
                                        (tpki:test-chain-root-key chain))
                       :this-update *fixed-now*))
             (crl (x509:parse-x509-crl crl-der :issuer-cert root)))
        (multiple-value-bind (valid reason)
            (x509:verify-certificate-chain
             (list leaf root)
             :time *fixed-now*
             :crls (list crl))
          (declare (ignore reason))
          (assert-true valid))))))

(deftest test-chain-rejected-when-leaf-on-crl
  "A CRL listing the leaf's serial rejects the chain."
  (let* ((chain (tpki:make-test-chain)))
    (multiple-value-bind (leaf root) (%parsed-chain chain)
      (let* ((entry (x509:make-x509-crl-entry
                     :serial (x509:x509-cert-serial leaf)
                     :revocation-date *fixed-now*))
             (crl-der (x509:make-crl
                       :issuer-cert root
                       :ca-key-type (tpki:test-key-type
                                     (tpki:test-chain-root-key chain))
                       :ca-private-key (tpki:test-key-private-key
                                        (tpki:test-chain-root-key chain))
                       :this-update *fixed-now*
                       :revoked-entries (list entry)))
             (crl (x509:parse-x509-crl crl-der :issuer-cert root)))
        (multiple-value-bind (valid reason)
            (x509:verify-certificate-chain
             (list leaf root)
             :time *fixed-now*
             :crls (list crl))
          (assert-false valid)
          (assert-true (search "revoked" reason :test #'char-equal)))))))

(deftest test-chain-ignores-crl-from-foreign-issuer
  "A CRL signed by an unrelated CA must not influence the chain even
   if it happens to list the leaf's serial number."
  (let* ((real-chain (tpki:make-test-chain))
         (foreign-chain (tpki:make-test-chain
                         :root-subject "Unrelated Root")))
    (multiple-value-bind (leaf root) (%parsed-chain real-chain)
      (multiple-value-bind (_ foreign-root) (%parsed-chain foreign-chain)
        (declare (ignore _))
        (let* ((entry (x509:make-x509-crl-entry
                       :serial (x509:x509-cert-serial leaf)
                       :revocation-date *fixed-now*))
               (crl-der (x509:make-crl
                         :issuer-cert foreign-root
                         :ca-key-type (tpki:test-key-type
                                       (tpki:test-chain-root-key
                                        foreign-chain))
                         :ca-private-key (tpki:test-key-private-key
                                          (tpki:test-chain-root-key
                                           foreign-chain))
                         :this-update *fixed-now*
                         :revoked-entries (list entry)))
               (foreign-crl (x509:parse-x509-crl crl-der
                                                 :issuer-cert foreign-root)))
          (multiple-value-bind (valid reason)
              (x509:verify-certificate-chain
               (list leaf root)
               :time *fixed-now*
               :crls (list foreign-crl))
            (declare (ignore reason))
            (assert-true valid)))))))

(deftest test-chain-rejected-when-intermediate-revoked
  "A CRL revoking the intermediate (signed by the root) rejects a
   3-cert chain."
  (let* ((chain (tpki:make-test-chain
                 :intermediate-key-type :ed25519)))
    (let* ((leaf (x509:parse-x509-certificate
                  (tpki:test-chain-leaf-cert chain)))
           (intermediate (x509:parse-x509-certificate
                          (tpki:test-chain-intermediate-cert chain)))
           (root (x509:parse-x509-certificate
                  (tpki:test-chain-root-cert chain)))
           (entry (x509:make-x509-crl-entry
                   :serial (x509:x509-cert-serial intermediate)
                   :revocation-date *fixed-now*))
           (crl-der (x509:make-crl
                     :issuer-cert root
                     :ca-key-type (tpki:test-key-type
                                   (tpki:test-chain-root-key chain))
                     :ca-private-key (tpki:test-key-private-key
                                      (tpki:test-chain-root-key chain))
                     :this-update *fixed-now*
                     :revoked-entries (list entry)))
           (crl (x509:parse-x509-crl crl-der :issuer-cert root)))
      (multiple-value-bind (valid reason)
          (x509:verify-certificate-chain
           (list leaf intermediate root)
           :time *fixed-now*
           :crls (list crl))
        (assert-false valid)
        (assert-true (search "depth 1" reason :test #'char-equal))))))

;;; ---------------------------------------------------------------------------
;;; OCSP integration via the revocation callback
;;;
;;; The OCSP test fixtures here cannot carry a real responder
;;; signature (the test PKI never exposes the issuer's private key as
;;; a generic SignerInterface), so callers either pre-validate or
;;; pass :validate nil to `make-revocation-callback'. The shape of
;;; the callback contract is what these tests exercise.
;;; ---------------------------------------------------------------------------

(defun %bytes (s)
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code s))

(defun %generalized-time (s)
  (asn1:der-encode-tagged 24 (%bytes s)))

(defun %ecdsa-sha256-algid ()
  (asn1:der-encode-sequence
   (asn1:der-encode-oid '(1 2 840 10045 4 3 2))))

(defun %good-status ()
  (asn1:der-encode-context 0 (make-array 0 :element-type '(unsigned-byte 8))))

(defun %revoked-status (revocation-time)
  (asn1:der-encode-context 1 (%generalized-time revocation-time)
                              :constructed t))

(defun %make-single-response (cert-id-bytes status-bytes
                              &key (this-update "20260601000000Z"))
  (asn1:der-encode-sequence
   cert-id-bytes
   status-bytes
   (%generalized-time this-update)))

(defun %make-basic-ocsp-response (single-responses)
  (asn1:der-encode-sequence
   (asn1:der-encode-sequence
    (asn1:der-encode-context 2 (make-array 20 :element-type '(unsigned-byte 8)
                                              :initial-element 0))
    (%generalized-time "20260601000000Z")
    (apply #'asn1:der-encode-sequence single-responses))
   (%ecdsa-sha256-algid)
   (asn1:der-encode-bit-string (make-array 64 :element-type '(unsigned-byte 8)
                                              :initial-element 0))))

(defun %make-ocsp-response-bytes (basic-bytes)
  (asn1:der-encode-sequence
   (asn1:der-encode-tagged 10 (asn1::integer-to-der-bytes 0))
   (asn1:der-encode-context
    0 (asn1:der-encode-sequence
       (asn1:der-encode-oid '(1 3 6 1 5 5 7 48 1 1))
       (asn1:der-encode-octet-string basic-bytes))
    :constructed t)))

(defun %synthetic-response-for (target-cert issuer-cert status-bytes)
  "Build a parsed OCSPResponse whose single-response references
   TARGET-CERT/ISSUER-CERT and whose certStatus is STATUS-BYTES."
  (let* ((cert-id (ocsp:make-cert-id target-cert issuer-cert))
         (sr (%make-single-response cert-id status-bytes))
         (basic (%make-basic-ocsp-response (list sr)))
         (top (%make-ocsp-response-bytes basic)))
    (ocsp:parse-ocsp-response top)))

(deftest test-ocsp-status-for-matches-real-cert-id
  "ocsp-status-for returns the SingleResponse status when the certID
   matches the (target, issuer) pair, and NIL otherwise."
  (let* ((chain (tpki:make-test-chain))
         (other (tpki:make-test-chain
                 :root-subject "Other Root"
                 :leaf-subject "other.test.example")))
    (multiple-value-bind (leaf root) (%parsed-chain chain)
      (multiple-value-bind (other-leaf other-root) (%parsed-chain other)
        (let ((response (%synthetic-response-for leaf root (%good-status))))
          (assert-eq :good (ocsp:ocsp-status-for response leaf root))
          ;; Wrong target / issuer pair -> NIL (no SingleResponse for them).
          (assert-false (ocsp:ocsp-status-for response other-leaf other-root)))))))

(deftest test-callback-rejects-chain-on-revoked
  "verify-certificate-chain rejects when the callback says :revoked."
  (let* ((chain (tpki:make-test-chain)))
    (multiple-value-bind (leaf root) (%parsed-chain chain)
      (let* ((response (%synthetic-response-for
                        leaf root (%revoked-status "20260101000000Z")))
             (callback (ocsp:make-revocation-callback
                        (list response) :validate nil)))
        (multiple-value-bind (valid reason)
            (x509:verify-certificate-chain
             (list leaf root)
             :time *fixed-now*
             :revocation-callback callback)
          (assert-false valid)
          (assert-true (search "revoked" reason :test #'char-equal)))))))

(deftest test-callback-accepts-chain-on-good
  "A :good answer from the callback does not block validation."
  (let* ((chain (tpki:make-test-chain)))
    (multiple-value-bind (leaf root) (%parsed-chain chain)
      (let* ((response (%synthetic-response-for leaf root (%good-status)))
             (callback (ocsp:make-revocation-callback
                        (list response) :validate nil)))
        (multiple-value-bind (valid reason)
            (x509:verify-certificate-chain
             (list leaf root)
             :time *fixed-now*
             :revocation-callback callback)
          (declare (ignore reason))
          (assert-true valid))))))

(deftest test-callback-no-info-leaves-chain-valid
  "When the callback returns NIL (no information for this cert), the
   chain validates because revocation enforcement is opt-in by the
   caller."
  (let* ((chain (tpki:make-test-chain))
         (other (tpki:make-test-chain
                 :leaf-subject "other.test.example")))
    (multiple-value-bind (leaf root) (%parsed-chain chain)
      (multiple-value-bind (other-leaf other-root) (%parsed-chain other)
        (let* ((response (%synthetic-response-for
                          other-leaf other-root (%revoked-status
                                                 "20260101000000Z")))
               (callback (ocsp:make-revocation-callback
                          (list response) :validate nil)))
          (multiple-value-bind (valid reason)
              (x509:verify-certificate-chain
               (list leaf root)
               :time *fixed-now*
               :revocation-callback callback)
            (declare (ignore reason))
            (assert-true valid)))))))

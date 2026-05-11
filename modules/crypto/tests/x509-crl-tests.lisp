;;;; Tests for X.509 CRL parsing and signing (RFC 5280 §5).
;;;;
;;;; A CRL builder is included in `epsilon.crypto.x509' (`make-crl'), so
;;;; we can round-trip parse(build(...)) without depending on external
;;;; vendor CRLs that may rotate.

(defpackage epsilon.crypto.x509-crl-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.crypto.x509 x509)
           (epsilon.crypto.test-pki tpki)))

(in-package :epsilon.crypto.x509-crl-tests)

(defun %ca-pair (key-type)
  "Build a self-signed CA cert + parsed cert + key, suitable for
   issuing CRLs against."
  (let* ((key (tpki:generate-test-key key-type))
         (cert-der (x509:make-self-signed-certificate
                    :subject "Test CRL CA"
                    :serial 1
                    :not-before (x509:make-x509-time :year 2025 :month 1 :day 1)
                    :not-after  (x509:make-x509-time :year 2030 :month 1 :day 1)
                    :key-type (tpki:test-key-type key)
                    :private-key (tpki:test-key-private-key key)
                    :public-key-bytes (tpki:test-key-public-key-bytes key)
                    :is-ca t)))
    (values key cert-der (x509:parse-x509-certificate cert-der))))

;;; ---------------------------------------------------------------------------
;;; Empty CRL: no revoked entries
;;; ---------------------------------------------------------------------------

(deftest test-crl-empty-roundtrip-ed25519
  "Empty CRL signed by an Ed25519 CA round-trips through parse"
  (multiple-value-bind (ca-key ca-der ca-cert) (%ca-pair :ed25519)
    (declare (ignore ca-der))
    (let* ((crl-der (x509:make-crl
                     :issuer-cert ca-cert
                     :ca-key-type (tpki:test-key-type ca-key)
                     :ca-private-key (tpki:test-key-private-key ca-key)
                     :this-update (x509:make-x509-time :year 2026 :month 1 :day 1)
                     :next-update (x509:make-x509-time :year 2026 :month 7 :day 1)))
           (crl (x509:parse-x509-crl crl-der :verify nil)))
      (assert-= 2 (x509:x509-crl-version crl))
      (assert-true (x509:x509-crl-this-update crl))
      (assert-true (x509:x509-crl-next-update crl))
      (assert-true (null (x509:x509-crl-revoked-entries crl)))
      (assert-true (x509:verify-crl-signature crl ca-cert)))))

;;; ---------------------------------------------------------------------------
;;; Populated CRL: a few revoked serials
;;; ---------------------------------------------------------------------------

(deftest test-crl-populated-roundtrip-ec-p256
  "CRL with revoked entries round-trips and lookups work"
  (multiple-value-bind (ca-key ca-der ca-cert) (%ca-pair :ecdsa-p256)
    (declare (ignore ca-der))
    (let* ((entries (list (x509:make-x509-crl-entry
                           :serial 1001
                           :revocation-date
                           (x509:make-x509-time :year 2026 :month 1 :day 5))
                          (x509:make-x509-crl-entry
                           :serial 1002
                           :revocation-date
                           (x509:make-x509-time :year 2026 :month 1 :day 6))
                          (x509:make-x509-crl-entry
                           :serial 9999
                           :revocation-date
                           (x509:make-x509-time :year 2026 :month 1 :day 7))))
           (crl-der (x509:make-crl
                     :issuer-cert ca-cert
                     :ca-key-type (tpki:test-key-type ca-key)
                     :ca-private-key (tpki:test-key-private-key ca-key)
                     :this-update (x509:make-x509-time :year 2026 :month 1 :day 8)
                     :next-update (x509:make-x509-time :year 2026 :month 7 :day 8)
                     :revoked-entries entries))
           (crl (x509:parse-x509-crl crl-der)))
      (assert-= 3 (length (x509:x509-crl-revoked-entries crl)))
      ;; Each parsed serial appears
      (let ((serials (mapcar #'x509:x509-crl-entry-serial
                             (x509:x509-crl-revoked-entries crl))))
        (assert-true (member 1001 serials))
        (assert-true (member 1002 serials))
        (assert-true (member 9999 serials))
        ;; A non-revoked serial does not.
        (assert-not (member 5555 serials)))
      ;; crl-revokes-p as the lookup helper
      (assert-true (x509:crl-revokes-p crl 1001))
      (assert-true (x509:crl-revokes-p crl 9999))
      (assert-not (x509:crl-revokes-p crl 5555))
      ;; Signature still verifies
      (assert-true (x509:verify-crl-signature crl ca-cert)))))

;;; ---------------------------------------------------------------------------
;;; CRLNumber extension is preserved
;;; ---------------------------------------------------------------------------

(deftest test-crl-with-crl-number-extension
  "The CRLNumber extension survives parse/serialize"
  (multiple-value-bind (ca-key ca-der ca-cert) (%ca-pair :ed25519)
    (declare (ignore ca-der))
    (let* ((crl-der (x509:make-crl
                     :issuer-cert ca-cert
                     :ca-key-type (tpki:test-key-type ca-key)
                     :ca-private-key (tpki:test-key-private-key ca-key)
                     :this-update (x509:make-x509-time :year 2026 :month 1 :day 1)
                     :crl-number 42))
           (crl (x509:parse-x509-crl crl-der)))
      (let ((ext (find '(2 5 29 20) (x509:x509-crl-extensions crl)
                       :key #'x509:x509-ext-oid :test #'equal)))
        (assert-true ext)
        ;; The extension value is a DER OCTET STRING wrapping an INTEGER 42.
        ;; Verify the trailing byte is 42 (after DER INTEGER tag/length).
        (let* ((octet-bytes (x509:x509-ext-value ext))
               ;; The extension `value' is the OCTET STRING contents (already
               ;; unwrapped by the parser): a DER INTEGER `02 01 2a'.
               (last-byte (aref octet-bytes (1- (length octet-bytes)))))
          (assert-= 42 last-byte))))))

;;; ---------------------------------------------------------------------------
;;; A CRL signed by one CA must NOT verify against a different CA's
;;; certificate. This catches mis-signing or wrong-issuer mistakes.
;;; ---------------------------------------------------------------------------

(deftest test-crl-rejects-wrong-issuer
  "Verification fails when the issuer cert is not the actual signer"
  (multiple-value-bind (signer-key signer-der signer-cert) (%ca-pair :ed25519)
    (declare (ignore signer-der))
    (multiple-value-bind (other-key other-der other-cert) (%ca-pair :ed25519)
      (declare (ignore other-key other-der))
      (let* ((crl-der (x509:make-crl
                       :issuer-cert signer-cert
                       :ca-key-type (tpki:test-key-type signer-key)
                       :ca-private-key (tpki:test-key-private-key signer-key)
                       :this-update (x509:make-x509-time
                                     :year 2026 :month 1 :day 1)))
             (crl (x509:parse-x509-crl crl-der :verify nil)))
        (assert-true (x509:verify-crl-signature crl signer-cert))
        (assert-not (x509:verify-crl-signature crl other-cert))))))

;;; ---------------------------------------------------------------------------
;;; PEM round-trip
;;; ---------------------------------------------------------------------------

(deftest test-crl-pem-roundtrip
  "Encode CRL DER as PEM and parse back via parse-x509-crl-pem"
  (multiple-value-bind (ca-key ca-der ca-cert) (%ca-pair :ed25519)
    (declare (ignore ca-der))
    (let* ((crl-der (x509:make-crl
                     :issuer-cert ca-cert
                     :ca-key-type (tpki:test-key-type ca-key)
                     :ca-private-key (tpki:test-key-private-key ca-key)
                     :this-update (x509:make-x509-time :year 2026 :month 1 :day 1)
                     :revoked-entries
                     (list (x509:make-x509-crl-entry
                            :serial 7
                            :revocation-date
                            (x509:make-x509-time :year 2026 :month 1 :day 1)))))
           ;; Wrap into PEM
           (pem-text (epsilon.crypto.pem:pem-encode
                      (epsilon.crypto.pem:make-pem-block "X509 CRL" crl-der)))
           (crl (x509:parse-x509-crl-pem pem-text :verify nil)))
      (assert-true crl)
      (assert-= 1 (length (x509:x509-crl-revoked-entries crl)))
      (assert-= 7 (x509:x509-crl-entry-serial
                   (first (x509:x509-crl-revoked-entries crl))))
      (assert-true (x509:verify-crl-signature crl ca-cert)))))
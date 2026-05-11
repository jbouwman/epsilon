;;;; OCSP response parser (RFC 6960)
;;;;
;;;; This module decodes a DER-encoded OCSPResponse far enough to drive
;;;; the stapling cache that lives in the proxy / TLS server config:
;;;;
;;;;   * top-level responseStatus  (must be `successful' to be useful)
;;;;   * BasicOCSPResponse extraction
;;;;   * SingleResponse certStatus, thisUpdate, nextUpdate
;;;;   * the raw BasicOCSPResponse bytes (this is what gets stapled --
;;;;     the wire format for the CertificateEntry status_request
;;;;     extension is the inner OCTET STRING contents, exactly the
;;;;     bytes a TLS client expects per RFC 6066 section 8 / RFC 8446
;;;;     section 4.4.2.1).
;;;;
;;;; Signature verification against the responder cert is intentionally
;;;; not in this module yet -- the fetcher will validate the responder
;;;; chain (or accept a delegated responder cert embedded in `certs`)
;;;; before placing the bytes in the staple cache. The TLS layer never
;;;; trusts a staple it has not previously verified.

(defpackage epsilon.crypto.ocsp
  (:use :cl)
  (:import
   (epsilon.crypto.asn1 asn1)
   (epsilon.crypto.sha1 sha1)
   (epsilon.crypto.x509 x509)
   (epsilon.crypto.pkcs pkcs)
   (epsilon.crypto.rsa rsa)
   (epsilon.crypto.ecdsa ecdsa)
   (epsilon.crypto.ec-p256 ec-p256))
  (:export
   ;; structs
   #:ocsp-response
   #:ocsp-response-status
   #:ocsp-response-produced-at
   #:ocsp-response-single-responses
   #:ocsp-response-basic-bytes
   #:ocsp-response-tbs-bytes
   #:ocsp-response-signature-algorithm
   #:ocsp-response-signature-bytes
   #:ocsp-response-embedded-certs
   #:ocsp-single-response
   #:ocsp-single-response-cert-status
   #:ocsp-single-response-this-update
   #:ocsp-single-response-next-update
   #:ocsp-single-response-revocation-time
   #:ocsp-single-response-cert-id-bytes
   ;; entry points
   #:parse-ocsp-response
   #:parse-basic-ocsp-response
   #:make-ocsp-request
   #:make-cert-id
   #:verify-basic-ocsp-response
   #:validate-ocsp-response
   #:ocsp-status-for
   #:make-revocation-callback))

(in-package :epsilon.crypto.ocsp)

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

;; id-pkix-ocsp-basic OBJECT IDENTIFIER ::= 1.3.6.1.5.5.7.48.1.1
(defparameter +id-pkix-ocsp-basic+ '(1 3 6 1 5 5 7 48 1 1))

(defconstant +tag-enumerated+ 10)
(defconstant +tag-generalized-time+ 24)

(defstruct ocsp-response
  "Parsed OCSP response. RAW-BYTES is the original input; BASIC-BYTES
   is the inner BasicOCSPResponse DER (the bytes a TLS server staples
   when responseStatus = successful); TBS-BYTES is the DER of
   tbsResponseData, which is the byte sequence the responder signed."
  (status :unknown)              ; one of the keyword statuses below
  (raw-bytes nil)
  (basic-bytes nil)
  (tbs-bytes nil)
  (signature-algorithm nil)      ; OID component list
  (signature-bytes nil)          ; raw signature bits (no unused-bits byte)
  (embedded-certs nil :type list) ; DER bytes of any certs [0] EXPLICIT entries
  (produced-at nil)              ; raw GeneralizedTime string, or NIL
  (single-responses nil :type list))

(defstruct ocsp-single-response
  (cert-status :unknown)         ; :good :revoked :unknown
  (this-update nil)              ; raw GeneralizedTime string
  (next-update nil)              ; raw GeneralizedTime string, or NIL
  (revocation-time nil)          ; only set when cert-status = :revoked
  (cert-id-bytes nil))           ; raw DER of the certID, for matching

;;; ---------------------------------------------------------------------------
;;; TLV helpers (built on epsilon.crypto.asn1)
;;; ---------------------------------------------------------------------------

(defun %tag (tlv) (asn1:asn1-tlv-tag tlv))
(defun %class (tlv) (asn1:asn1-tlv-class tlv))
(defun %constructed-p (tlv) (asn1:asn1-tlv-constructed-p tlv))
(defun %children (tlv) (asn1:asn1-tlv-value tlv))

(defun %is-context (tlv tag-number)
  (and (= (%class tlv) asn1:+context-specific+)
       (= (%tag tlv) tag-number)))

(defun %is-universal (tlv tag-number)
  (and (= (%class tlv) asn1:+universal+)
       (= (%tag tlv) tag-number)))

(defun %expect-sequence (tlv where)
  (unless (and (%is-universal tlv asn1:+tag-sequence+)
               (%constructed-p tlv))
    (error "OCSP: expected SEQUENCE in ~A" where))
  (%children tlv))

(defun %decode-enumerated (tlv)
  (unless (%is-universal tlv +tag-enumerated+)
    (error "OCSP: expected ENUMERATED, got tag ~D class ~D"
           (%tag tlv) (%class tlv)))
  (asn1:decode-der-integer (%children tlv)))

(defun %decode-octet-string (tlv)
  (unless (%is-universal tlv asn1:+tag-octet-string+)
    (error "OCSP: expected OCTET STRING"))
  (%children tlv))

(defun %decode-oid (tlv)
  (unless (%is-universal tlv asn1:+tag-oid+)
    (error "OCSP: expected OID"))
  (asn1:decode-oid-value (%children tlv)))

(defun %decode-generalized-time (tlv)
  (unless (%is-universal tlv +tag-generalized-time+)
    (error "OCSP: expected GeneralizedTime, got tag ~D" (%tag tlv)))
  (map 'string #'code-char (%children tlv)))

(defun %status-code-to-keyword (n)
  (case n
    (0 :successful)
    (1 :malformed-request)
    (2 :internal-error)
    (3 :try-later)
    (5 :sig-required)
    (6 :unauthorized)
    (t :unknown)))

;;; ---------------------------------------------------------------------------
;;; Top-level entry point
;;; ---------------------------------------------------------------------------

(defun parse-ocsp-response (bytes)
  "Decode an OCSPResponse (RFC 6960). On the success path returns an
   ocsp-response with parsed single-responses; on a non-successful
   responseStatus returns an ocsp-response with the status keyword set
   and no further data populated."
  (let* ((tlv (asn1:der-decode bytes))
         (children (%expect-sequence tlv "OCSPResponse"))
         (status-tlv (first children))
         (status (%status-code-to-keyword (%decode-enumerated status-tlv)))
         (out (make-ocsp-response :status status :raw-bytes bytes)))
    (when (and (eq status :successful) (rest children))
      (let* ((response-bytes-tlv (second children)))
        (unless (%is-context response-bytes-tlv 0)
          (error "OCSP: missing responseBytes [0] EXPLICIT wrapper"))
        (let* ((rb-children (%children response-bytes-tlv))
               (rb-seq (first rb-children))
               (rb-fields (%expect-sequence rb-seq "ResponseBytes"))
               (oid (%decode-oid (first rb-fields)))
               (octet-tlv (second rb-fields))
               (basic-bytes (%decode-octet-string octet-tlv)))
          (unless (equal oid +id-pkix-ocsp-basic+)
            (error "OCSP: unsupported response type OID ~A" oid))
          (setf (ocsp-response-basic-bytes out) basic-bytes)
          (parse-basic-ocsp-response basic-bytes :into out))))
    out))

(defun parse-basic-ocsp-response (bytes &key into)
  "Decode a BasicOCSPResponse (the inner OCTET STRING of an
   OCSPResponse). Returns an ocsp-response. When INTO is supplied the
   parsed fields are stored there and INTO is returned."
  (let* ((out (or into (make-ocsp-response :status :successful
                                            :basic-bytes bytes)))
         (tlv (asn1:der-decode bytes))
         (basic-fields (%expect-sequence tlv "BasicOCSPResponse"))
         (tbs-tlv (first basic-fields))
         (sig-alg-tlv (second basic-fields))
         (sig-bs-tlv (third basic-fields)))
    ;; Capture the DER bytes of tbsResponseData for later signature
    ;; verification, plus the sig alg and signature bits themselves.
    (setf (ocsp-response-tbs-bytes out) (asn1:der-encode tbs-tlv))
    (let* ((alg-children (asn1:der-decode-sequence-contents sig-alg-tlv)))
      (setf (ocsp-response-signature-algorithm out)
            (asn1:decode-oid-value (asn1:asn1-tlv-value (first alg-children)))))
    (let ((bs (asn1:asn1-tlv-value sig-bs-tlv)))
      ;; BIT STRING leading byte is the unused-bits count, always 0
      ;; for a full-byte signature.
      (setf (ocsp-response-signature-bytes out) (subseq bs 1)))
    ;; Optional certs [0] EXPLICIT SEQUENCE OF Certificate
    (let ((maybe-certs (fourth basic-fields)))
      (when (and maybe-certs (%is-context maybe-certs 0))
        (let* ((cert-seq (first (%children maybe-certs)))
               (cert-tlvs (%expect-sequence cert-seq "certs")))
          (setf (ocsp-response-embedded-certs out)
                (mapcar #'asn1:der-encode cert-tlvs)))))
    (parse-response-data tbs-tlv out)
    out))

(defun parse-response-data (tlv out)
  (let* ((fields (%expect-sequence tlv "ResponseData"))
         (idx 0))
    ;; version [0] EXPLICIT Version DEFAULT v1 -- skip if present
    (when (and (nth idx fields) (%is-context (nth idx fields) 0))
      (incf idx))
    ;; responderID -- ignored (we don't care which responder signed it
    ;; at the parse layer)
    (incf idx)
    ;; producedAt
    (let ((pa (nth idx fields)))
      (incf idx)
      (setf (ocsp-response-produced-at out)
            (%decode-generalized-time pa)))
    ;; responses SEQUENCE OF SingleResponse
    (let* ((responses-tlv (nth idx fields))
           (single-tlvs (%expect-sequence responses-tlv "responses"))
           (parsed nil))
      (dolist (sr-tlv single-tlvs)
        (push (parse-single-response sr-tlv) parsed))
      (setf (ocsp-response-single-responses out) (nreverse parsed)))
    out))

;;; ---------------------------------------------------------------------------
;;; OCSPRequest builder (RFC 6960 Section 4.1)
;;;
;;;   OCSPRequest ::= SEQUENCE {
;;;     tbsRequest                  TBSRequest,
;;;     optionalSignature  [0] EXPLICIT Signature OPTIONAL }
;;;
;;;   TBSRequest ::= SEQUENCE {
;;;     version             [0] EXPLICIT Version DEFAULT v1,
;;;     requestorName       [1] EXPLICIT GeneralName OPTIONAL,
;;;     requestList             SEQUENCE OF Request,
;;;     requestExtensions   [2] EXPLICIT Extensions OPTIONAL }
;;;
;;;   Request ::= SEQUENCE {
;;;     reqCert                    CertID,
;;;     singleRequestExtensions [0] EXPLICIT Extensions OPTIONAL }
;;;
;;;   CertID ::= SEQUENCE {
;;;     hashAlgorithm  AlgorithmIdentifier,   -- SHA-1
;;;     issuerNameHash OCTET STRING,           -- SHA-1 of issuer Name DER
;;;     issuerKeyHash  OCTET STRING,           -- SHA-1 of issuer SPK bits
;;;     serialNumber   CertificateSerialNumber }
;;;
;;; We emit a minimal, unsigned OCSPRequest with a single CertID. That
;;; is the shape every public OCSP responder (Let's Encrypt, DigiCert,
;;; etc.) accepts for a single-cert query, and the one the fetcher
;;; sends per certificate in its staple cache.
;;; ---------------------------------------------------------------------------

;; SHA-1 AlgorithmIdentifier = SEQUENCE { OID 1.3.14.3.2.26, NULL }
(defparameter +oid-sha1+ '(1 3 14 3 2 26))

(defun %sha1-algid ()
  (asn1:der-encode-sequence
   (asn1:der-encode-oid +oid-sha1+)
   (asn1:der-encode-null)))

(defun make-cert-id (target-cert issuer-cert)
  "Build a DER-encoded CertID for TARGET-CERT under ISSUER-CERT.
   Computes SHA-1 over the issuer's Name DER and over the issuer's
   subjectPublicKey BIT STRING value (RFC 6960 4.1.1)."
  (let* ((issuer-name-hash (sha1:sha1 (x509:x509-cert-raw-subject-bytes
                                       issuer-cert)))
         (issuer-key-hash  (sha1:sha1 (x509:x509-cert-public-key-bytes
                                       issuer-cert)))
         (serial (x509:x509-cert-serial target-cert)))
    (asn1:der-encode-sequence
     (%sha1-algid)
     (asn1:der-encode-octet-string issuer-name-hash)
     (asn1:der-encode-octet-string issuer-key-hash)
     (asn1:der-encode-integer serial))))

(defun make-ocsp-request (target-cert issuer-cert)
  "Build a DER-encoded OCSPRequest for a single cert. Unsigned, no
   requestor name, no extensions. Suitable for HTTP POST (or the GET
   base64url'd form) to any RFC 6960-compliant responder."
  (let* ((cert-id (make-cert-id target-cert issuer-cert))
         (request (asn1:der-encode-sequence cert-id))
         (request-list (asn1:der-encode-sequence request))
         (tbs-request (asn1:der-encode-sequence request-list)))
    (asn1:der-encode-sequence tbs-request)))

;;; ---------------------------------------------------------------------------
;;; Signature verification (RFC 6960 4.2.2.2)
;;;
;;; A BasicOCSPResponse is signed either by the issuing CA directly or
;;; by a delegated responder -- a cert issued by that CA carrying the
;;; id-kp-OCSPSigning EKU. This function accepts a caller-supplied
;;; responder cert and does not walk EKUs; the fetcher is responsible
;;; for picking the right one (issuer cert, or embedded `certs`
;;; responder cert whose issuer matches the query chain and whose EKU
;;; includes OCSPSigning).
;;; ---------------------------------------------------------------------------

(defun verify-basic-ocsp-response (response responder-cert)
  "Verify RESPONSE's signature over its tbsResponseData using the
   responder certificate's public key. Returns T on success, NIL on
   any failure (unsupported algorithm, key type mismatch, bad sig)."
  (let ((sig-alg (ocsp-response-signature-algorithm response))
        (tbs (ocsp-response-tbs-bytes response))
        (sig (ocsp-response-signature-bytes response))
        (pk-alg (x509:x509-cert-public-key-algorithm responder-cert))
        (pk-bytes (x509:x509-cert-public-key-bytes responder-cert)))
    (unless (and sig-alg tbs sig pk-alg pk-bytes)
      (return-from verify-basic-ocsp-response nil))
    (handler-case
        (cond
          ;; RSA PKCS#1 v1.5 with SHA-256.
          ((equal sig-alg pkcs:+oid-sha256-with-rsa+)
           (when (equal pk-alg pkcs:+oid-rsa-encryption+)
             (multiple-value-bind (n e) (pkcs:decode-pkcs1-rsa-public pk-bytes)
               (rsa:pkcs1-v15-verify (rsa:make-rsa-public-key n e)
                                     tbs sig :hash :sha256))))
          ;; ECDSA P-256 with SHA-256.
          ((equal sig-alg pkcs:+oid-ecdsa-with-sha256+)
           (when (equal pk-alg pkcs:+oid-ec-public-key+)
             (let ((point (ec-p256:p256-point-decode pk-bytes)))
               (when point
                 (let* ((sig-tlv (asn1:der-decode sig))
                        (sig-children (asn1:der-decode-sequence-contents sig-tlv))
                        (r (asn1:decode-der-integer
                            (asn1:asn1-tlv-value (first sig-children))))
                        (s (asn1:decode-der-integer
                            (asn1:asn1-tlv-value (second sig-children)))))
                   (ecdsa:ecdsa-verify point tbs r s))))))
          (t nil))
      (error () nil))))

(defun parse-single-response (tlv)
  (let* ((fields (%expect-sequence tlv "SingleResponse"))
         (out (make-ocsp-single-response))
         (idx 0))
    ;; certID -- retain raw DER so callers can match a target (cert,
    ;; issuer) pair against this single response (see `ocsp-status-for').
    (setf (ocsp-single-response-cert-id-bytes out)
          (asn1:der-encode (nth idx fields)))
    (incf idx)
    ;; certStatus CHOICE: tag class context-specific
    (let ((cs (nth idx fields)))
      (incf idx)
      (cond
        ((%is-context cs 0) (setf (ocsp-single-response-cert-status out) :good))
        ((%is-context cs 1)
         (setf (ocsp-single-response-cert-status out) :revoked)
         ;; revoked is IMPLICIT RevokedInfo SEQUENCE { revocationTime
         ;; GeneralizedTime, ... }. With IMPLICIT tagging the SEQUENCE
         ;; tag is overwritten by the context tag, so the children of
         ;; the context-tagged TLV are the SEQUENCE contents directly.
         (let ((rev-children (%children cs)))
           (when rev-children
             (setf (ocsp-single-response-revocation-time out)
                   (handler-case (%decode-generalized-time (first rev-children))
                     (error () nil))))))
        ((%is-context cs 2) (setf (ocsp-single-response-cert-status out) :unknown))
        (t (error "OCSP: unrecognized certStatus tag ~D" (%tag cs)))))
    ;; thisUpdate
    (let ((tu (nth idx fields)))
      (incf idx)
      (setf (ocsp-single-response-this-update out)
            (%decode-generalized-time tu)))
    ;; nextUpdate [0] EXPLICIT GeneralizedTime OPTIONAL
    (let ((nu (nth idx fields)))
      (when (and nu (%is-context nu 0))
        (incf idx)
        (let ((inner (first (%children nu))))
          (setf (ocsp-single-response-next-update out)
                (%decode-generalized-time inner)))))
    out))

;;; ---------------------------------------------------------------------------
;;; OCSP response validation (RFC 6960 Section 3.2)
;;; ---------------------------------------------------------------------------

(defun %generalized-time-now ()
  "Current UTC time as a GeneralizedTime string (YYYYMMDDHHMMSSZ)."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0DZ"
            year month day hour min sec)))

(defun validate-ocsp-response (response &key responder-cert issuer-cert
                                             (now nil))
  "Validate an OCSP response for freshness, signature, and responder
   EKU. NOW is a GeneralizedTime string (defaults to current time).
   Returns (values valid-p reason)."
  (unless (eq (ocsp-response-status response) :successful)
    (return-from validate-ocsp-response
      (values nil (format nil "non-successful status: ~A"
                          (ocsp-response-status response)))))
  ;; Check freshness of each SingleResponse
  (let ((now-str (or now (%generalized-time-now))))
    (dolist (sr (ocsp-response-single-responses response))
      (let ((this-update (ocsp-single-response-this-update sr))
            (next-update (ocsp-single-response-next-update sr)))
        (when (and this-update (string> this-update now-str))
          (return-from validate-ocsp-response
            (values nil "thisUpdate is in the future")))
        (when (and next-update (string< next-update now-str))
          (return-from validate-ocsp-response
            (values nil "nextUpdate has passed"))))))
  ;; Verify signature and responder EKU
  (when responder-cert
    ;; Delegated responder (not the issuer itself) must have OCSPSigning
    (when (and issuer-cert
               (not (equalp (x509:x509-cert-raw-subject-bytes
                             responder-cert)
                            (x509:x509-cert-raw-subject-bytes
                             issuer-cert))))
      (let ((eku-ext (x509:x509-get-extension
                      responder-cert
                      x509:+oid-extended-key-usage+)))
        (unless eku-ext
          (return-from validate-ocsp-response
            (values nil "delegated responder missing EKU")))
        (let ((ekus (x509:parse-extended-key-usage eku-ext)))
          (unless (member x509:+oid-ocsp-signing+ ekus :test #'equal)
            (return-from validate-ocsp-response
              (values nil "responder missing OCSPSigning EKU"))))))
    (unless (verify-basic-ocsp-response response responder-cert)
      (return-from validate-ocsp-response
        (values nil "signature verification failed"))))
  (values t nil))

;;; ---------------------------------------------------------------------------
;;; Revocation lookup helpers
;;; ---------------------------------------------------------------------------

(defun ocsp-status-for (response target-cert issuer-cert)
  "Return the OCSP status (:good, :revoked, or :unknown) for
   TARGET-CERT under ISSUER-CERT in RESPONSE, or NIL when RESPONSE
   carries no SingleResponse for that pair. Matching is by exact
   certID bytes -- caller must use the same hashAlgorithm (SHA-1) the
   parser exposes."
  (when (eq (ocsp-response-status response) :successful)
    (let ((target-id (make-cert-id target-cert issuer-cert)))
      (loop for sr in (ocsp-response-single-responses response)
            when (let ((id (ocsp-single-response-cert-id-bytes sr)))
                   (and id (equalp target-id id)))
              return (ocsp-single-response-cert-status sr)))))

(defun make-revocation-callback (responses &key (now nil) (validate t))
  "Build a callback suitable for `x509:verify-certificate-chain'
   :revocation-callback. RESPONSES is a list of parsed `ocsp-response'.
   The returned function takes a (cert issuer) pair and returns one of
   :good, :revoked, :unknown, or NIL (no information).

   When VALIDATE is true (the default), each response is checked for
   freshness against NOW (a GeneralizedTime string, defaulting to the
   current time) and -- when an issuer is available -- for signature
   validity against the issuer cert. A response that fails validation
   is silently skipped.

   Signature verification accepts either the chain issuer itself or a
   delegated responder cert embedded in the response (RFC 6960 4.2.2.2
   section). Delegated responder certs are required to carry the
   id-kp-OCSPSigning EKU and to be issued by the chain issuer; chain
   verification of the embedded cert against the chain issuer is left
   to the caller, who already has the chain in hand."
  (lambda (cert issuer)
    (block search
      (dolist (response responses nil)
        (let ((status (ocsp-status-for response cert issuer)))
          (when status
            (let ((accept t))
              (when (and validate issuer)
                (multiple-value-bind (ok reason)
                    (validate-ocsp-response response
                                            :responder-cert issuer
                                            :issuer-cert issuer
                                            :now now)
                  (declare (ignore reason))
                  (setf accept ok)))
              (when accept
                (return-from search status)))))))))

;;;; PKCS#12 PFX encode and decode (RFC 7292)
;;;;
;;;; Implements the common modern PKCS#12 profile produced by OpenSSL 3.x:
;;;;
;;;;   PFX
;;;;     version = v3(3)
;;;;     authSafe = ContentInfo { id-data, OCTET STRING { AuthenticatedSafe } }
;;;;     macData
;;;;       mac = DigestInfo (HMAC-SHA256, computed over AuthenticatedSafe bytes)
;;;;       macSalt (random)
;;;;       iterations
;;;;
;;;;   AuthenticatedSafe = SEQUENCE {
;;;;     ContentInfo { id-encryptedData, PBES2(AES-256-CBC)(SafeContents {certBag}) },
;;;;     ContentInfo { id-data, OCTET STRING { SafeContents {pkcs8ShroudedKeyBag} } }
;;;;   }
;;;;
;;;; The cert bag is PBES2-encrypted at the ContentInfo level; the key
;;;; bag is already shrouded (it is an EncryptedPrivateKeyInfo inside a
;;;; plain ContentInfo), so there is no need to encrypt it twice.
;;;;
;;;; Decode will also accept a plain-data cert bag (no encryption) and
;;;; either an encrypted or plain key bag, to tolerate variant formats.
;;;; Legacy PBE-SHA1-3DES and RC2 schemes are not supported.

(defpackage epsilon.crypto.pkcs12
  (:use :cl)
  (:import (epsilon.crypto.asn1 asn1)
            (epsilon.crypto.pem pem)
            (epsilon.crypto.pbes2 pbes2)
            (epsilon.crypto.pkcs12-kdf p12kdf)
            (epsilon.crypto.hmac hmac)
            (epsilon.crypto.sha256 sha256)
            (epsilon.crypto.drbg drbg))
  (:import-from :epsilon.crypto.primitives #:ensure-byte-vector)
  (:export
   #:pkcs12-encode
   #:pkcs12-decode
   #:pkcs12-to-pem
   #:pkcs12-from-pem
   #:pkcs12-error))

(in-package :epsilon.crypto.pkcs12)

(define-condition pkcs12-error (error)
  ((message :initarg :message :reader pkcs12-error-message))
  (:report (lambda (c s) (format s "PKCS#12: ~A" (pkcs12-error-message c)))))

(defun p12-error (fmt &rest args)
  (error 'pkcs12-error :message (apply #'format nil fmt args)))

;;; ---------------------------------------------------------------------------
;;; OIDs
;;; ---------------------------------------------------------------------------

(defparameter +oid-data+                 '(1 2 840 113549 1 7 1))
(defparameter +oid-encrypted-data+       '(1 2 840 113549 1 7 6))
(defparameter +oid-key-bag+              '(1 2 840 113549 1 12 10 1 1))
(defparameter +oid-pkcs8-shrouded-key-bag+ '(1 2 840 113549 1 12 10 1 2))
(defparameter +oid-cert-bag+             '(1 2 840 113549 1 12 10 1 3))
(defparameter +oid-x509-certificate+     '(1 2 840 113549 1 9 22 1))
(defparameter +oid-sha256+               '(2 16 840 1 101 3 4 2 1))
(defparameter +oid-sha1+                 '(1 3 14 3 2 26))

;;; ---------------------------------------------------------------------------
;;; ASN.1 navigation helpers
;;; ---------------------------------------------------------------------------

(defun expect-universal (tlv tag description)
  (unless (and (= (asn1:asn1-tlv-tag tlv) tag)
               (= (asn1:asn1-tlv-class tlv) asn1:+universal+))
    (p12-error "expected ~A (tag ~D), got tag ~D class ~D"
               description tag
               (asn1:asn1-tlv-tag tlv) (asn1:asn1-tlv-class tlv))))

(defun seq-children (tlv)
  (expect-universal tlv asn1:+tag-sequence+ "SEQUENCE")
  (asn1:der-decode-sequence-contents tlv))

(defun oid-of (tlv)
  (expect-universal tlv asn1:+tag-oid+ "OID")
  (asn1:decode-oid-value (asn1:asn1-tlv-value tlv)))

(defun integer-of (tlv)
  (expect-universal tlv asn1:+tag-integer+ "INTEGER")
  (asn1:decode-der-integer (asn1:asn1-tlv-value tlv)))

(defun octet-string-of (tlv)
  (expect-universal tlv asn1:+tag-octet-string+ "OCTET STRING")
  (asn1:asn1-tlv-value tlv))

(defun context-0-contents (tlv)
  "Unwrap a [0] EXPLICIT context-tagged TLV and return the single inner
   TLV. Does not handle the [0] IMPLICIT case."
  (unless (and (= (asn1:asn1-tlv-class tlv) asn1:+context-specific+)
               (= (asn1:asn1-tlv-tag tlv) 0))
    (p12-error "expected [0] EXPLICIT wrapper, got tag ~D class ~D"
               (asn1:asn1-tlv-tag tlv) (asn1:asn1-tlv-class tlv)))
  (let ((children (asn1:asn1-tlv-value tlv)))
    (unless (and (listp children) (= (length children) 1))
      (p12-error "expected exactly one child under [0] EXPLICIT wrapper"))
    (first children)))


;;; ---------------------------------------------------------------------------
;;; MAC helpers (RFC 7292 §4 + Appendix B)
;;; ---------------------------------------------------------------------------

(defstruct hash-descriptor
  name         ; keyword
  oid          ; OID list
  output-size  ; bytes
  pkcs12-kdf-kw ; keyword for pkcs12-kdf
  hmac-kw)      ; keyword for hmac

(defparameter +mac-hashes+
  (list
   (make-hash-descriptor :name :sha256 :oid +oid-sha256+ :output-size 32
                         :pkcs12-kdf-kw :sha256 :hmac-kw :sha256)))
;; SHA-1 intentionally omitted: modern OpenSSL defaults to SHA-256, and
;; HMAC-SHA1 is not wired up by the current `epsilon.crypto.hmac` module.

(defun hash-by-name (name)
  (or (find name +mac-hashes+ :key #'hash-descriptor-name)
      (p12-error "unsupported MAC hash ~A" name)))

(defun hash-by-oid (oid)
  (or (find oid +mac-hashes+ :key #'hash-descriptor-oid :test #'equal)
      (p12-error "unsupported MAC algorithm OID ~A" oid)))

(defun compute-mac (password salt iterations auth-safe-bytes hash-desc)
  "Compute the PKCS#12 MAC over AUTH-SAFE-BYTES using PASSWORD and SALT.
   HASH-DESC is a `hash-descriptor`; MAC key is derived via the PKCS#12
   KDF with id=3 (MAC key), then HMAC-hash is run over the input."
  (let* ((pw-bmp (p12kdf:password-to-bmp-string
                  (etypecase password
                    (string password)
                    ((simple-array (unsigned-byte 8) (*))
                     (sb-ext:octets-to-string
                      password :external-format :utf-8)))))
         (mac-key (p12kdf:pkcs12-kdf pw-bmp salt p12kdf:+pkcs12-id-mac+
                                     (hash-descriptor-output-size hash-desc)
                                     iterations
                                     :hash (hash-descriptor-pkcs12-kdf-kw hash-desc))))
    (hmac:hmac (hash-descriptor-hmac-kw hash-desc) mac-key auth-safe-bytes)))

(defun encode-mac-data (mac salt iterations hash-desc)
  (asn1:der-encode-sequence
   ;; DigestInfo
   (asn1:der-encode-sequence
    (asn1:der-encode-sequence
     (asn1:der-encode-oid (hash-descriptor-oid hash-desc))
     (asn1:der-encode-null))
    (asn1:der-encode-octet-string mac))
   ;; macSalt
   (asn1:der-encode-octet-string salt)
   ;; iterations
   (asn1:der-encode-integer iterations)))

(defun parse-mac-data (tlv)
  "Decode a MacData TLV and return (values mac-bytes salt iterations hash-desc)."
  (let* ((children (seq-children tlv)))
    (unless (>= (length children) 2)
      (p12-error "MacData must contain at least (DigestInfo, macSalt)"))
    (let* ((digest-info-tlv (first children))
           (salt-tlv (second children))
           (iter-tlv (third children))
           (di-children (seq-children digest-info-tlv))
           (alg-tlv (first di-children))
           (digest-tlv (second di-children))
           (alg-children (seq-children alg-tlv))
           (hash-oid (oid-of (first alg-children)))
           (hash-desc (hash-by-oid hash-oid))
           (mac-value (octet-string-of digest-tlv))
           (salt (octet-string-of salt-tlv))
           (iterations (if iter-tlv (integer-of iter-tlv) 1)))
      (values mac-value salt iterations hash-desc))))

;;; ---------------------------------------------------------------------------
;;; SafeBag construction and parsing
;;; ---------------------------------------------------------------------------

(defun encode-safe-bag (bag-oid bag-value-der)
  "SafeBag = SEQUENCE { bagId OID, bagValue [0] EXPLICIT ANY }. Omits
   bagAttributes (optional SET OF PKCS12Attribute)."
  (asn1:der-encode-sequence
   (asn1:der-encode-oid bag-oid)
   (asn1:der-encode-context 0 bag-value-der :constructed t)))

(defun encode-cert-bag (cert-der)
  "certBag value = SEQUENCE { certId OID, certValue [0] EXPLICIT
   OCTET STRING }. certId is x509Certificate (1.2.840.113549.1.9.22.1)."
  (asn1:der-encode-sequence
   (asn1:der-encode-oid +oid-x509-certificate+)
   (asn1:der-encode-context 0 (asn1:der-encode-octet-string cert-der)
                            :constructed t)))

(defun encode-cert-safe-bag (cert-der)
  (encode-safe-bag +oid-cert-bag+ (encode-cert-bag cert-der)))

(defun encode-key-safe-bag-shrouded (pkcs8-der password &rest pbes2-args
                                      &key &allow-other-keys)
  (let ((epki (apply #'pbes2:encode-encrypted-pkcs8 pkcs8-der password pbes2-args)))
    (encode-safe-bag +oid-pkcs8-shrouded-key-bag+ epki)))

(defun encode-safe-contents (&rest bag-bytes)
  (apply #'asn1:der-encode-sequence bag-bytes))

(defun parse-safe-bag (tlv password)
  "Parse a single SafeBag TLV. Returns a tagged value:
     (:cert cert-der)   if it is a certBag with an x509Certificate
     (:key pkcs8-der)   if it is a keyBag or pkcs8ShroudedKeyBag
     (:other oid)       otherwise"
  (let* ((children (seq-children tlv))
         (bag-oid (oid-of (first children)))
         ;; bagValue is [0] EXPLICIT wrapping the actual value TLV.
         (bag-value (context-0-contents (second children))))
    (cond
      ((equal bag-oid +oid-cert-bag+)
       (let* ((cert-bag-children (seq-children bag-value))
              (cert-oid (oid-of (first cert-bag-children)))
              (wrapped (context-0-contents (second cert-bag-children))))
         (unless (equal cert-oid +oid-x509-certificate+)
           (p12-error "certBag has unknown certId ~A" cert-oid))
         (list :cert (octet-string-of wrapped))))
      ((equal bag-oid +oid-pkcs8-shrouded-key-bag+)
       ;; bagValue here is the raw DER of an EncryptedPrivateKeyInfo.
       ;; der-decode has already decoded it into a TLV structure; we
       ;; need to re-encode and hand it to the PBES2 layer.
       (let ((epki-der (asn1:der-encode bag-value)))
         (list :key (pbes2:decode-encrypted-pkcs8 epki-der password))))
      ((equal bag-oid +oid-key-bag+)
       ;; Plaintext PKCS#8 PrivateKeyInfo (unencrypted key bag).
       (list :key (asn1:der-encode bag-value)))
      (t
       (list :other bag-oid)))))

(defun parse-safe-contents (safe-contents-bytes password)
  "Parse SafeContents bytes into a list of parse-safe-bag results."
  (let* ((tlv (asn1:der-decode safe-contents-bytes))
         (children (seq-children tlv)))
    (loop for bag in children
          collect (parse-safe-bag bag password))))

;;; ---------------------------------------------------------------------------
;;; ContentInfo encoding and parsing
;;; ---------------------------------------------------------------------------

(defun encode-content-info-data (inner-bytes)
  "ContentInfo { id-data, [0] EXPLICIT OCTET STRING { inner-bytes } }."
  (asn1:der-encode-sequence
   (asn1:der-encode-oid +oid-data+)
   (asn1:der-encode-context 0 (asn1:der-encode-octet-string inner-bytes)
                            :constructed t)))

(defun encode-content-info-encrypted-data (pbes2-algid-der ciphertext)
  "ContentInfo { id-encryptedData, [0] EXPLICIT EncryptedData }.
   EncryptedData = SEQUENCE { version INTEGER(0),
                              EncryptedContentInfo }
   EncryptedContentInfo = SEQUENCE { contentType OID (id-data),
                                     contentEncryptionAlgorithm AlgorithmIdentifier,
                                     encryptedContent [0] IMPLICIT OCTET STRING }"
  (let* ((encrypted-content-info
           (asn1:der-encode-sequence
            (asn1:der-encode-oid +oid-data+)
            pbes2-algid-der
            ;; encryptedContent is [0] IMPLICIT OCTET STRING: primitive
            ;; context tag 0 whose contents are the raw ciphertext.
            (asn1:der-encode-context 0 ciphertext)))
         (encrypted-data
           (asn1:der-encode-sequence
            (asn1:der-encode-integer 0)  ; version
            encrypted-content-info)))
    (asn1:der-encode-sequence
     (asn1:der-encode-oid +oid-encrypted-data+)
     (asn1:der-encode-context 0 encrypted-data :constructed t))))

(defun parse-content-info (tlv password)
  "Parse a ContentInfo TLV and return the decrypted SafeContents bytes.
   Handles both id-data (plain) and id-encryptedData (PBES2-encrypted)."
  (let* ((children (seq-children tlv))
         (content-type (oid-of (first children)))
         (content-tlv (context-0-contents (second children))))
    (cond
      ((equal content-type +oid-data+)
       ;; content is an OCTET STRING whose bytes ARE the SafeContents.
       (octet-string-of content-tlv))
      ((equal content-type +oid-encrypted-data+)
       (let* ((ed-children (seq-children content-tlv))
              ;; EncryptedData.version = first child (ignored)
              (eci-children (seq-children (second ed-children)))
              (inner-content-type (oid-of (first eci-children))))
         (unless (equal inner-content-type +oid-data+)
           (p12-error "EncryptedContentInfo.contentType is ~A, expected id-data"
                      inner-content-type))
         (let* ((algid-tlv (second eci-children))
                (encrypted-tlv (third eci-children))
                (algid-der (asn1:der-encode algid-tlv))
                ;; [0] IMPLICIT OCTET STRING: the tlv's value bytes ARE
                ;; the ciphertext (no nested TLV to unwrap).
                (ciphertext (unless (null encrypted-tlv)
                              (asn1:asn1-tlv-value encrypted-tlv))))
           (unless (and (= (asn1:asn1-tlv-class encrypted-tlv) asn1:+context-specific+)
                        (= (asn1:asn1-tlv-tag encrypted-tlv) 0))
             (p12-error "EncryptedContentInfo.encryptedContent is not [0] IMPLICIT"))
           (pbes2:pbes2-decrypt password algid-der ciphertext))))
      (t
       (p12-error "unsupported ContentInfo type OID ~A" content-type)))))

;;; ---------------------------------------------------------------------------
;;; Top-level encode
;;; ---------------------------------------------------------------------------

(defun pkcs12-encode (&key certificates private-key password
                          (iterations 210000)
                          (mac-iterations 2048)
                          (cipher :aes-256-cbc)
                          (prf :hmac-sha256)
                          (mac-hash :sha256))
  "Build a PKCS#12 PFX (DER bytes) containing CERTIFICATES (a list of
   X.509 certificate DER byte vectors) and PRIVATE-KEY (a PKCS#8
   PrivateKeyInfo DER byte vector), both protected with PASSWORD.

   The produced PFX follows the modern OpenSSL 3.x layout: the cert
   bag is PBES2-encrypted at the ContentInfo layer, the key bag is a
   pkcs8ShroudedKeyBag inside a plain ContentInfo (so the key material
   is protected exactly once), and the PFX is MAC-sealed with HMAC-SHA256
   over the AuthenticatedSafe bytes. The MAC key is derived via the
   PKCS#12 KDF with iteration count MAC-ITERATIONS."
  (unless password
    (p12-error "password is required"))
  (unless private-key
    (p12-error "private-key (PKCS#8 DER) is required"))
  (let* ((certs (or certificates '()))
         (cert-bags (mapcar #'encode-cert-safe-bag certs))
         (cert-safe-contents (apply #'encode-safe-contents cert-bags))
         ;; Encrypt the cert SafeContents at the ContentInfo layer.
         (cert-content-info
           (if cert-bags
               (multiple-value-bind (algid ct)
                   (pbes2:pbes2-encrypt password cert-safe-contents
                                         :cipher cipher :prf prf
                                         :iterations iterations)
                 (encode-content-info-encrypted-data algid ct))
               ;; No certs: emit an empty plain data ContentInfo.
               (encode-content-info-data cert-safe-contents)))
         ;; Shroud the private key.
         (key-bag (encode-key-safe-bag-shrouded private-key password
                                                 :cipher cipher :prf prf
                                                 :iterations iterations))
         (key-safe-contents (encode-safe-contents key-bag))
         (key-content-info (encode-content-info-data key-safe-contents))
         ;; AuthenticatedSafe = SEQUENCE OF ContentInfo.
         (auth-safe-inner
           (asn1:der-encode-sequence cert-content-info key-content-info))
         ;; Wrap in the outer ContentInfo{data, OCTET STRING}.
         (auth-safe-content-info (encode-content-info-data auth-safe-inner))
         ;; Compute MAC over the AuthenticatedSafe bytes.
         (hash-desc (hash-by-name mac-hash))
         (mac-salt (drbg:random-bytes 8))
         (mac (compute-mac password mac-salt mac-iterations
                           auth-safe-inner hash-desc))
         (mac-data (encode-mac-data mac mac-salt mac-iterations hash-desc)))
    (asn1:der-encode-sequence
     (asn1:der-encode-integer 3)  ; version
     auth-safe-content-info
     mac-data)))

;;; ---------------------------------------------------------------------------
;;; Top-level decode
;;; ---------------------------------------------------------------------------

(defun pkcs12-decode (pfx-bytes password)
  "Parse a PKCS#12 PFX DER blob and return (values CERTS KEY) where
   CERTS is a list of X.509 certificate DER byte vectors and KEY is the
   inner PKCS#8 PrivateKeyInfo DER byte vector. Signals PKCS12-ERROR on
   malformed input, unsupported algorithms, or MAC verification failure.

   The MAC is verified before any content is returned, so a bad password
   or tampered blob is detected without leaking partial state."
  (let* ((pfx-tlv (asn1:der-decode (ensure-byte-vector pfx-bytes)))
         (pfx-children (seq-children pfx-tlv)))
    (unless (>= (length pfx-children) 2)
      (p12-error "PFX must contain version + authSafe"))
    (let ((version (integer-of (first pfx-children)))
          (auth-safe-ci (second pfx-children))
          (mac-data-tlv (third pfx-children)))
      (unless (= version 3)
        (p12-error "unsupported PFX version ~D (expected 3)" version))
      ;; authSafe is ContentInfo{id-data} containing an OCTET STRING
      ;; whose contents ARE the AuthenticatedSafe SEQUENCE.
      (let* ((auth-safe-children (seq-children auth-safe-ci))
             (outer-ct (oid-of (first auth-safe-children))))
        (unless (equal outer-ct +oid-data+)
          (p12-error "outer authSafe must be ContentInfo{id-data}, got ~A"
                     outer-ct))
        (let* ((auth-safe-wrapper
                 (context-0-contents (second auth-safe-children)))
               (auth-safe-inner (octet-string-of auth-safe-wrapper)))
          ;; Verify MAC before trusting the contents.
          (when mac-data-tlv
            (multiple-value-bind (mac-value mac-salt mac-iterations hash-desc)
                (parse-mac-data mac-data-tlv)
              (let ((expected (compute-mac password mac-salt mac-iterations
                                           auth-safe-inner hash-desc)))
                (unless (equalp expected mac-value)
                  (p12-error "MAC verification failed ~
                              (wrong password or tampered PFX)")))))
          ;; Walk the AuthenticatedSafe.
          (let* ((auth-safe-tlv (asn1:der-decode auth-safe-inner))
                 (content-infos (seq-children auth-safe-tlv))
                 (all-bags
                   (loop for ci in content-infos
                         for safe-contents-bytes = (parse-content-info ci password)
                         append (parse-safe-contents safe-contents-bytes password)))
                 (certs '())
                 (key nil))
            (dolist (bag all-bags)
              (ecase (first bag)
                (:cert (push (second bag) certs))
                (:key
                 (when key
                   (p12-error "PFX contains multiple private keys; only one ~
                               supported"))
                 (setf key (second bag)))
                (:other
                 ;; Ignore unknown bag types silently -- callers that
                 ;; need exact fidelity can parse manually.
                 nil)))
            (values (nreverse certs) key)))))))

;;; ---------------------------------------------------------------------------
;;; Convenience: PEM wrapping
;;; ---------------------------------------------------------------------------
;;;
;;; PKCS#12 is most commonly distributed as raw DER (.p12 / .pfx files).
;;; OpenSSL does not conventionally PEM-armour PFX, so these helpers are
;;; minor conveniences rather than required interop points.

(defun pkcs12-to-pem (pfx-bytes)
  (pem:pem-encode (pem:make-pem-block "PKCS12" pfx-bytes)))

(defun pkcs12-from-pem (pem-string)
  (let ((block (pem:pem-decode pem-string)))
    (unless block
      (p12-error "no PEM block found"))
    (pem:pem-block-data block)))

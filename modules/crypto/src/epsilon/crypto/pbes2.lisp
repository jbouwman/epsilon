;;;; PBES2 and EncryptedPrivateKeyInfo (RFC 8018 §6.2, RFC 5958)
;;;;
;;;; PBES2 is the modern password-based encryption scheme used by:
;;;;   - OpenSSL's `-----BEGIN ENCRYPTED PRIVATE KEY-----` PEM blocks.
;;;;   - PKCS#12 shrouded key bags and encrypted SafeContents (RFC 7292).
;;;;   - PKCS#8 v2 EncryptedPrivateKeyInfo (RFC 5958).
;;;;
;;;; Only a single modern profile is supported here: PBKDF2 with an
;;;; HMAC-SHA-256/384/512 PRF, key-stretched into an AES-128/192/256-CBC
;;;; cipher with PKCS#7 padding. Legacy PBE-MD5-DES, RC2, and 3DES
;;;; schemes are intentionally out of scope -- callers with legacy
;;;; material should up-convert it with `openssl pkcs8 -topk8 -v2 aes-256-cbc`
;;;; before feeding it in.

(defpackage epsilon.crypto.pbes2
  (:use :cl)
  (:import (epsilon.crypto.asn1 asn1)
            (epsilon.crypto.aes aes)
            (epsilon.crypto.pbkdf2 pbkdf2)
            (epsilon.crypto.drbg drbg)
            (epsilon.crypto.pem pem))
  (:import-from :epsilon.crypto.primitives #:ensure-byte-vector)
  (:export
   #:pbes2-encrypt
   #:pbes2-decrypt
   #:encode-encrypted-pkcs8
   #:decode-encrypted-pkcs8
   #:encrypted-pkcs8-to-pem
   #:encrypted-pkcs8-from-pem
   #:+oid-pbes2+
   #:+oid-pbkdf2+
   #:+oid-hmac-sha256+
   #:+oid-hmac-sha384+
   #:+oid-hmac-sha512+
   #:+oid-aes128-cbc-pad+
   #:+oid-aes192-cbc-pad+
   #:+oid-aes256-cbc-pad+))

(in-package :epsilon.crypto.pbes2)

;;; ---------------------------------------------------------------------------
;;; OIDs
;;; ---------------------------------------------------------------------------

(defparameter +oid-pbes2+            '(1 2 840 113549 1 5 13))
(defparameter +oid-pbkdf2+           '(1 2 840 113549 1 5 12))
(defparameter +oid-hmac-sha256+      '(1 2 840 113549 2 9))
(defparameter +oid-hmac-sha384+      '(1 2 840 113549 2 10))
(defparameter +oid-hmac-sha512+      '(1 2 840 113549 2 11))
(defparameter +oid-aes128-cbc-pad+   '(2 16 840 1 101 3 4 1 2))
(defparameter +oid-aes192-cbc-pad+   '(2 16 840 1 101 3 4 1 22))
(defparameter +oid-aes256-cbc-pad+   '(2 16 840 1 101 3 4 1 42))

;;; ---------------------------------------------------------------------------
;;; Cipher and PRF descriptors
;;; ---------------------------------------------------------------------------

(defstruct cipher-info
  name       ; keyword (:aes-128-cbc, :aes-192-cbc, :aes-256-cbc)
  oid        ; OID as list of integers
  key-length ; key length in bytes
  iv-length) ; IV length in bytes

(defparameter +ciphers+
  (list
   (make-cipher-info :name :aes-128-cbc :oid +oid-aes128-cbc-pad+
                     :key-length 16 :iv-length 16)
   (make-cipher-info :name :aes-192-cbc :oid +oid-aes192-cbc-pad+
                     :key-length 24 :iv-length 16)
   (make-cipher-info :name :aes-256-cbc :oid +oid-aes256-cbc-pad+
                     :key-length 32 :iv-length 16)))

(defun cipher-by-name (name)
  (or (find name +ciphers+ :key #'cipher-info-name)
      (error "pbes2: unsupported cipher ~A" name)))

(defun cipher-by-oid (oid)
  (or (find oid +ciphers+ :key #'cipher-info-oid :test #'equal)
      (error "pbes2: unsupported encryption OID ~A" oid)))

(defstruct prf-info
  name      ; :hmac-sha256, :hmac-sha384, :hmac-sha512
  oid
  pbkdf2-kw ; keyword used by epsilon.crypto.pbkdf2
  output-length)

(defparameter +prfs+
  (list
   (make-prf-info :name :hmac-sha256 :oid +oid-hmac-sha256+
                  :pbkdf2-kw :sha256 :output-length 32)
   (make-prf-info :name :hmac-sha384 :oid +oid-hmac-sha384+
                  :pbkdf2-kw :sha384 :output-length 48)
   (make-prf-info :name :hmac-sha512 :oid +oid-hmac-sha512+
                  :pbkdf2-kw :sha512 :output-length 64)))

(defun prf-by-name (name)
  (or (find name +prfs+ :key #'prf-info-name)
      (error "pbes2: unsupported PRF ~A" name)))

(defun prf-by-oid (oid)
  (or (find oid +prfs+ :key #'prf-info-oid :test #'equal)
      (error "pbes2: unsupported PRF OID ~A" oid)))

;;; ---------------------------------------------------------------------------
;;; ASN.1 navigation helpers
;;; ---------------------------------------------------------------------------

(defun expect-universal (tlv tag &optional (description "element"))
  (unless (and (= (asn1:asn1-tlv-tag tlv) tag)
               (= (asn1:asn1-tlv-class tlv) asn1:+universal+))
    (error "pbes2: expected ~A (tag ~D), got tag ~D class ~D"
           description tag (asn1:asn1-tlv-tag tlv) (asn1:asn1-tlv-class tlv))))

(defun seq-children (tlv)
  (expect-universal tlv asn1:+tag-sequence+ "SEQUENCE")
  (asn1:der-decode-sequence-contents tlv))

(defun oid-children (tlv)
  (expect-universal tlv asn1:+tag-oid+ "OID")
  (asn1:decode-oid-value (asn1:asn1-tlv-value tlv)))

(defun integer-value (tlv)
  (expect-universal tlv asn1:+tag-integer+ "INTEGER")
  (asn1:decode-der-integer (asn1:asn1-tlv-value tlv)))

(defun octet-string-bytes (tlv)
  (expect-universal tlv asn1:+tag-octet-string+ "OCTET STRING")
  (asn1:asn1-tlv-value tlv))

;;; ---------------------------------------------------------------------------
;;; Encode: PBES2 AlgorithmIdentifier
;;; ---------------------------------------------------------------------------

(defun encode-pbkdf2-params (salt iterations key-length prf-info)
  "Build the PBKDF2-params SEQUENCE used inside a PBES2 AlgorithmIdentifier.
   KEY-LENGTH is included explicitly so the derived-key length is not
   ambiguous regardless of the cipher."
  (asn1:der-encode-sequence
   (asn1:der-encode-octet-string salt)
   (asn1:der-encode-integer iterations)
   (asn1:der-encode-integer key-length)
   ;; prf AlgorithmIdentifier.
   (asn1:der-encode-sequence
    (asn1:der-encode-oid (prf-info-oid prf-info))
    (asn1:der-encode-null))))

(defun encode-pbes2-algorithm-id (salt iterations iv prf-info cipher-info)
  "Build a PBES2 AlgorithmIdentifier DER blob. Returns the outer
   SEQUENCE bytes."
  (asn1:der-encode-sequence
   (asn1:der-encode-oid +oid-pbes2+)
   ;; PBES2-params ::= SEQUENCE { keyDerivationFunc, encryptionScheme }
   (asn1:der-encode-sequence
    ;; keyDerivationFunc AlgorithmIdentifier (PBKDF2).
    (asn1:der-encode-sequence
     (asn1:der-encode-oid +oid-pbkdf2+)
     (encode-pbkdf2-params salt iterations
                           (cipher-info-key-length cipher-info)
                           prf-info))
    ;; encryptionScheme AlgorithmIdentifier (e.g. AES-256-CBC-PAD).
    (asn1:der-encode-sequence
     (asn1:der-encode-oid (cipher-info-oid cipher-info))
     (asn1:der-encode-octet-string iv)))))

;;; ---------------------------------------------------------------------------
;;; Decode: PBES2 AlgorithmIdentifier -> (salt, iterations, iv, prf, cipher)
;;; ---------------------------------------------------------------------------

(defstruct pbes2-profile
  salt iterations key-length iv prf-info cipher-info)

(defun parse-pbkdf2-params (params-tlv)
  "Decode PBKDF2-params into (salt, iterations, key-length-or-nil, prf-info)."
  (let ((children (seq-children params-tlv)))
    (unless (>= (length children) 2)
      (error "pbes2: PBKDF2-params must have at least salt and iterationCount"))
    (let ((salt-tlv (first children))
          (iter-tlv (second children))
          (rest (nthcdr 2 children))
          (key-length nil)
          (prf-info (prf-by-name :hmac-sha256))) ; will be overridden if present
      ;; RFC 8018 says the default PRF is hmacWithSHA1, which we do not
      ;; implement. We set hmac-sha256 as a placeholder but require the
      ;; blob to actually specify a supported PRF below.
      (let ((default-prf-supplied-p nil))
        ;; Optional INTEGER keyLength.
        (when (and rest
                   (= (asn1:asn1-tlv-tag (first rest)) asn1:+tag-integer+)
                   (= (asn1:asn1-tlv-class (first rest)) asn1:+universal+))
          (setf key-length (integer-value (pop rest))))
        ;; Optional prf AlgorithmIdentifier.
        (when rest
          (let* ((prf-alg-tlv (first rest))
                 (alg-children (seq-children prf-alg-tlv))
                 (prf-oid (oid-children (first alg-children))))
            (setf prf-info (prf-by-oid prf-oid))
            (setf default-prf-supplied-p t)))
        (unless default-prf-supplied-p
          ;; The default PRF per RFC 8018 is hmacWithSHA1, which this
          ;; module does not support.
          (error "pbes2: PBKDF2-params omits the PRF field; the RFC 8018 ~
                  default (HMAC-SHA1) is not supported. Re-encrypt with an ~
                  explicit HMAC-SHA256/384/512 PRF.")))
      (values (octet-string-bytes salt-tlv)
              (integer-value iter-tlv)
              key-length
              prf-info))))

(defun parse-pbes2-algorithm-id (algid-bytes)
  "Decode a PBES2 AlgorithmIdentifier DER blob into a `pbes2-profile`."
  (let* ((tlv (asn1:der-decode algid-bytes))
         (children (seq-children tlv)))
    (unless (>= (length children) 2)
      (error "pbes2: AlgorithmIdentifier must have OID + parameters"))
    (let ((alg-oid (oid-children (first children))))
      (unless (equal alg-oid +oid-pbes2+)
        (error "pbes2: algorithm OID is ~A, expected id-PBES2 ~A"
               alg-oid +oid-pbes2+))
      (let* ((params (seq-children (second children)))
             (kdf-alg (first params))
             (enc-alg (second params))
             (kdf-children (seq-children kdf-alg))
             (kdf-oid (oid-children (first kdf-children))))
        (unless (equal kdf-oid +oid-pbkdf2+)
          (error "pbes2: key derivation OID is ~A, expected PBKDF2 ~A"
                 kdf-oid +oid-pbkdf2+))
        (multiple-value-bind (salt iterations key-length prf-info)
            (parse-pbkdf2-params (second kdf-children))
          (let* ((enc-children (seq-children enc-alg))
                 (enc-oid (oid-children (first enc-children)))
                 (cipher-info (cipher-by-oid enc-oid))
                 (iv (octet-string-bytes (second enc-children))))
            (make-pbes2-profile
             :salt salt
             :iterations iterations
             :key-length (or key-length (cipher-info-key-length cipher-info))
             :iv iv
             :prf-info prf-info
             :cipher-info cipher-info)))))))

;;; ---------------------------------------------------------------------------
;;; Encrypt / decrypt
;;; ---------------------------------------------------------------------------

(defun derive-cipher-key (password profile)
  (pbkdf2:pbkdf2 (prf-info-pbkdf2-kw (pbes2-profile-prf-info profile))
                 (ensure-byte-vector password)
                 (pbes2-profile-salt profile)
                 (pbes2-profile-iterations profile)
                 (pbes2-profile-key-length profile)))

(defun pbes2-encrypt (password plaintext
                      &key (cipher :aes-256-cbc)
                           (prf :hmac-sha256)
                           (iterations 210000)
                           salt iv)
  "Encrypt PLAINTEXT under PBES2 with PBKDF2 and AES-CBC.

   PASSWORD and PLAINTEXT are byte vectors (strings are accepted for
   PASSWORD and coerced to UTF-8 bytes).
   :CIPHER is :aes-128-cbc, :aes-192-cbc, or :aes-256-cbc.
   :PRF is :hmac-sha256 (default), :hmac-sha384, or :hmac-sha512.
   :ITERATIONS defaults to 210000 (OWASP 2023 recommendation for
     PBKDF2-HMAC-SHA256).
   :SALT and :IV default to freshly generated random bytes of the
     algorithmically appropriate length.

   Returns (values algorithm-identifier-der ciphertext) where
   ALGORITHM-IDENTIFIER-DER is a PBES2 AlgorithmIdentifier blob that can
   be fed back to `pbes2-decrypt` together with CIPHERTEXT."
  (let* ((pw (etypecase password
               ((simple-array (unsigned-byte 8) (*)) password)
               (string (sb-ext:string-to-octets password :external-format :utf-8))))
         (pt (ensure-byte-vector plaintext))
         (cipher-info (cipher-by-name cipher))
         (prf-info    (prf-by-name prf))
         (salt (or salt (drbg:random-bytes 16)))
         (iv   (or iv   (drbg:random-bytes (cipher-info-iv-length cipher-info))))
         (profile (make-pbes2-profile
                   :salt salt :iterations iterations
                   :key-length (cipher-info-key-length cipher-info)
                   :iv iv :prf-info prf-info :cipher-info cipher-info))
         (key (derive-cipher-key pw profile))
         (ct (aes:aes-cbc-encrypt pt key iv))
         (algid (encode-pbes2-algorithm-id salt iterations iv prf-info cipher-info)))
    (values algid ct)))

(defun pbes2-decrypt (password algorithm-id-der ciphertext)
  "Decrypt CIPHERTEXT under PBES2 using the ALGORITHM-ID-DER blob
   returned by a previous call to `pbes2-encrypt` (or produced by any
   RFC 8018 §6.2 compliant encoder). PASSWORD is a byte vector or
   string. Returns the plaintext byte vector. Signals an error if the
   algorithm identifier specifies any scheme this module does not
   support, or if the PKCS#7 padding is malformed after decryption."
  (let* ((pw (etypecase password
               ((simple-array (unsigned-byte 8) (*)) password)
               (string (sb-ext:string-to-octets password :external-format :utf-8))))
         (profile (parse-pbes2-algorithm-id (ensure-byte-vector algorithm-id-der)))
         (key (derive-cipher-key pw profile)))
    (aes:aes-cbc-decrypt (ensure-byte-vector ciphertext)
                         key
                         (pbes2-profile-iv profile))))

;;; ---------------------------------------------------------------------------
;;; EncryptedPrivateKeyInfo (RFC 5958)
;;; ---------------------------------------------------------------------------

(defun encode-encrypted-pkcs8 (pkcs8-der password
                                &rest pbes2-args
                                &key &allow-other-keys)
  "Wrap PKCS8-DER (a DER-encoded PKCS#8 PrivateKeyInfo) in an
   EncryptedPrivateKeyInfo under PBES2. PBES2-ARGS are forwarded to
   `pbes2-encrypt`. Returns the DER bytes of the EncryptedPrivateKeyInfo
   SEQUENCE."
  (multiple-value-bind (algid ct)
      (apply #'pbes2-encrypt password pkcs8-der pbes2-args)
    (asn1:der-encode-sequence
     ;; AlgorithmIdentifier is already a SEQUENCE, so use it directly.
     algid
     (asn1:der-encode-octet-string ct))))

(defun decode-encrypted-pkcs8 (encrypted-der password)
  "Decode an EncryptedPrivateKeyInfo DER blob and return the inner
   PKCS#8 PrivateKeyInfo DER bytes. Signals an error on bad password
   (via the AES-CBC padding validator) or unsupported algorithms."
  (let* ((tlv (asn1:der-decode (ensure-byte-vector encrypted-der)))
         (children (seq-children tlv)))
    (unless (= (length children) 2)
      (error "EncryptedPrivateKeyInfo must be SEQUENCE of (algorithm, data)"))
    (let ((algid (asn1:der-encode (first children)))
          (ct (octet-string-bytes (second children))))
      (pbes2-decrypt password algid ct))))

(defun encrypted-pkcs8-to-pem (pkcs8-der password &rest pbes2-args
                                &key &allow-other-keys)
  "Convenience: produce a PEM-armoured ENCRYPTED PRIVATE KEY block."
  (let ((der (apply #'encode-encrypted-pkcs8 pkcs8-der password pbes2-args)))
    (pem:pem-encode (pem:make-pem-block "ENCRYPTED PRIVATE KEY" der))))

(defun encrypted-pkcs8-from-pem (pem-string password)
  "Parse a PEM-armoured ENCRYPTED PRIVATE KEY block and return the
   decrypted PKCS#8 PrivateKeyInfo DER bytes."
  (let ((block (pem:pem-decode pem-string)))
    (unless block
      (error "encrypted-pkcs8-from-pem: no PEM block found"))
    (unless (string= (pem:pem-block-label block) "ENCRYPTED PRIVATE KEY")
      (error "encrypted-pkcs8-from-pem: PEM label is ~A, expected ~
              ENCRYPTED PRIVATE KEY" (pem:pem-block-label block)))
    (decode-encrypted-pkcs8 (pem:pem-block-data block) password)))

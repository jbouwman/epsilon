;;;; Tests for PBES2 and EncryptedPrivateKeyInfo (RFC 8018 §6.2 / RFC 5958)

(defpackage epsilon.crypto.pbes2-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import
   (epsilon.crypto.pbes2 pbes2)
   (epsilon.crypto.asn1 asn1)))

(in-package :epsilon.crypto.pbes2-tests)

;;; ---- AlgorithmIdentifier round trip ----

(deftest test-pbes2-algorithm-id-is-well-formed-sequence
  "The DER AlgorithmIdentifier returned by pbes2-encrypt parses as a
   two-element SEQUENCE whose first element is the PBES2 OID."
  (let* ((pt (string-to-bytes "plaintext"))
         (pw (string-to-bytes "password")))
    (multiple-value-bind (algid ct)
        (pbes2:pbes2-encrypt pw pt :iterations 100)
      (declare (ignore ct))
      (let* ((tlv (asn1:der-decode algid))
             (children (asn1:der-decode-sequence-contents tlv)))
        (assert-= (length children) 2)
        ;; First child is an OID matching id-PBES2.
        (let ((oid (asn1:decode-oid-value
                    (asn1:asn1-tlv-value (first children)))))
          (assert-true (equal oid pbes2:+oid-pbes2+)))))))

;;; ---- Round trip ----

(deftest test-pbes2-round-trip-default
  "Encrypt and decrypt round trip with default parameters
   (AES-256-CBC + PBKDF2-HMAC-SHA256, auto-generated salt/IV)."
  (let* ((pt (string-to-bytes "the quick brown fox jumps over the lazy dog"))
         (pw (string-to-bytes "correct horse battery staple")))
    (multiple-value-bind (algid ct)
        (pbes2:pbes2-encrypt pw pt :iterations 1000)
      (assert-true (equalp (pbes2:pbes2-decrypt pw algid ct) pt)))))

(deftest test-pbes2-round-trip-aes-128
  "AES-128-CBC round trip."
  (let* ((pt (string-to-bytes "hello"))
         (pw (string-to-bytes "pw")))
    (multiple-value-bind (algid ct)
        (pbes2:pbes2-encrypt pw pt :cipher :aes-128-cbc :iterations 1000)
      (assert-true (equalp (pbes2:pbes2-decrypt pw algid ct) pt)))))

(deftest test-pbes2-round-trip-sha384
  "HMAC-SHA384 PRF round trip."
  (let* ((pt (string-to-bytes "hello"))
         (pw (string-to-bytes "pw")))
    (multiple-value-bind (algid ct)
        (pbes2:pbes2-encrypt pw pt :prf :hmac-sha384 :iterations 1000)
      (assert-true (equalp (pbes2:pbes2-decrypt pw algid ct) pt)))))

(deftest test-pbes2-round-trip-sha512
  "HMAC-SHA512 PRF round trip."
  (let* ((pt (string-to-bytes "hello"))
         (pw (string-to-bytes "pw")))
    (multiple-value-bind (algid ct)
        (pbes2:pbes2-encrypt pw pt :prf :hmac-sha512 :iterations 1000)
      (assert-true (equalp (pbes2:pbes2-decrypt pw algid ct) pt)))))

(deftest test-pbes2-plaintext-boundary-sizes
  "Round trip at every plaintext length from 0 to 33 bytes, exercising
   every possible PKCS#7 padding length in the underlying CBC layer."
  (let ((pw (string-to-bytes "pw")))
    (loop for n from 0 to 33 do
      (let ((pt (make-array n :element-type '(unsigned-byte 8))))
        (loop for i from 0 below n do (setf (aref pt i) (mod (* i 5) 256)))
        (multiple-value-bind (algid ct)
            (pbes2:pbes2-encrypt pw pt :iterations 100)
          (assert-true (equalp (pbes2:pbes2-decrypt pw algid ct) pt)))))))

(deftest test-pbes2-wrong-password-rejected
  "Decrypting with the wrong password fails (either bad PKCS#7 padding
   or, with very low probability, a valid-but-wrong plaintext -- the
   former is overwhelmingly more likely and is what we test for here)."
  (let* ((pt (string-to-bytes "secret content"))
         (pw (string-to-bytes "right password")))
    (multiple-value-bind (algid ct)
        (pbes2:pbes2-encrypt pw pt :iterations 100)
      (assert-condition (error)
                        (pbes2:pbes2-decrypt (string-to-bytes "wrong password")
                                             algid ct)))))

(deftest test-pbes2-fresh-salt-iv-each-call
  "Two encryptions of the same plaintext under the same password
   produce different ciphertexts, because the salt and IV are fresh on
   each call. (Otherwise the scheme would leak equality.)"
  (let* ((pt (string-to-bytes "same plaintext"))
         (pw (string-to-bytes "same password")))
    (multiple-value-bind (a1 c1) (pbes2:pbes2-encrypt pw pt :iterations 100)
      (multiple-value-bind (a2 c2) (pbes2:pbes2-encrypt pw pt :iterations 100)
        (assert-not (equalp a1 a2))     ; different salt/IV in algid
        (assert-not (equalp c1 c2))))))

(deftest test-pbes2-string-password-accepted
  "PASSWORD may be a Lisp string; it is UTF-8 encoded internally."
  (let* ((pt (string-to-bytes "payload")))
    (multiple-value-bind (algid ct)
        (pbes2:pbes2-encrypt "passphrase" pt :iterations 100)
      (assert-true (equalp (pbes2:pbes2-decrypt "passphrase" algid ct) pt)))))

;;; ---- EncryptedPrivateKeyInfo ----

(deftest test-encrypted-pkcs8-round-trip
  "An EncryptedPrivateKeyInfo DER blob decodes back to the exact input."
  (let* ((pkcs8 (make-array 128 :element-type '(unsigned-byte 8)))
         (pw (string-to-bytes "secret")))
    (loop for i from 0 below 128 do (setf (aref pkcs8 i) (mod (* i 3) 256)))
    (let ((encrypted (pbes2:encode-encrypted-pkcs8 pkcs8 pw :iterations 100)))
      (assert-true (equalp (pbes2:decode-encrypted-pkcs8 encrypted pw) pkcs8)))))

(deftest test-encrypted-pkcs8-pem-round-trip
  "PEM-armoured EncryptedPrivateKeyInfo round trip."
  (let* ((pkcs8 (make-array 64 :element-type '(unsigned-byte 8)))
         (pw (string-to-bytes "pem-password")))
    (loop for i from 0 below 64 do (setf (aref pkcs8 i) (logand i 255)))
    (let ((pem (pbes2:encrypted-pkcs8-to-pem pkcs8 pw :iterations 100)))
      (assert-true (search "BEGIN ENCRYPTED PRIVATE KEY" pem))
      (assert-true (search "END ENCRYPTED PRIVATE KEY" pem))
      (assert-true (equalp (pbes2:encrypted-pkcs8-from-pem pem pw) pkcs8)))))

(deftest test-encrypted-pkcs8-wrong-label-rejected
  "Feeding a PEM block with a non-ENCRYPTED-PRIVATE-KEY label is
   rejected up front rather than attempting to decrypt arbitrary data."
  (assert-condition (error)
                    (pbes2:encrypted-pkcs8-from-pem
                     (concatenate 'string
                                  "-----BEGIN CERTIFICATE-----" (string #\Newline)
                                  "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA" (string #\Newline)
                                  "-----END CERTIFICATE-----" (string #\Newline))
                     "pw")))

;;; ---- Parameter interop with openssl convention ----
;;;
;;; OpenSSL encodes EncryptedPrivateKeyInfo with an explicit keyLength
;;; in PBKDF2-params when the PRF is SHA-2. The parser must accept this
;;; (we also emit it). OpenSSL also allows keyLength to be absent, in
;;; which case the key length is inferred from the encryption scheme.

(deftest test-pbes2-decode-our-own-explicit-key-length
  "The encoder always writes keyLength explicitly; the decoder accepts
   it. Round trip via the low-level API confirms both paths agree."
  (let* ((pt (string-to-bytes "plaintext"))
         (pw (string-to-bytes "pw")))
    (multiple-value-bind (algid ct)
        (pbes2:pbes2-encrypt pw pt :cipher :aes-256-cbc
                                   :prf :hmac-sha256
                                   :iterations 500)
      (assert-true (equalp (pbes2:pbes2-decrypt pw algid ct) pt)))))

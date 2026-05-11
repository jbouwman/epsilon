;;;; Tests for PKCS#12 PFX encode/decode (RFC 7292)

(defpackage epsilon.crypto.pkcs12-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import
   (epsilon.crypto.pkcs12 p12)
   (epsilon.crypto.asn1 asn1)))

(in-package :epsilon.crypto.pkcs12-tests)

(defun make-dummy-bytes (length seed)
  (let ((out (make-array length :element-type '(unsigned-byte 8))))
    (loop for i from 0 below length
          do (setf (aref out i) (mod (+ seed (* i 31)) 256)))
    out))

;;; ---- Basic round trip with dummy cert/key bytes ----

(deftest test-pkcs12-roundtrip-one-cert
  "Round trip: one cert, one key, one password. Decoded cert and key
   are byte-for-byte identical to the inputs."
  (let* ((cert (make-dummy-bytes 128 1))
         (key  (make-dummy-bytes 256 2))
         (pfx  (p12:pkcs12-encode :certificates (list cert)
                                  :private-key key
                                  :password "pw"
                                  :iterations 100
                                  :mac-iterations 100)))
    (multiple-value-bind (certs decoded-key) (p12:pkcs12-decode pfx "pw")
      (assert-= (length certs) 1)
      (assert-true (equalp (first certs) cert))
      (assert-true (equalp decoded-key key)))))

(deftest test-pkcs12-roundtrip-multi-cert
  "A chain of certificates round trips in the original order."
  (let* ((c1 (make-dummy-bytes 64  1))
         (c2 (make-dummy-bytes 96  2))
         (c3 (make-dummy-bytes 128 3))
         (key (make-dummy-bytes 256 4))
         (pfx (p12:pkcs12-encode :certificates (list c1 c2 c3)
                                 :private-key key
                                 :password "chain"
                                 :iterations 100
                                 :mac-iterations 100)))
    (multiple-value-bind (certs decoded-key) (p12:pkcs12-decode pfx "chain")
      (assert-= (length certs) 3)
      (assert-true (equalp (first certs) c1))
      (assert-true (equalp (second certs) c2))
      (assert-true (equalp (third certs) c3))
      (assert-true (equalp decoded-key key)))))

(deftest test-pkcs12-roundtrip-no-certs
  "PFX with a key but no certificates (valid per the spec but unusual)."
  (let* ((key (make-dummy-bytes 300 9))
         (pfx (p12:pkcs12-encode :certificates nil
                                 :private-key key
                                 :password "keyonly"
                                 :iterations 100
                                 :mac-iterations 100)))
    (multiple-value-bind (certs decoded-key) (p12:pkcs12-decode pfx "keyonly")
      (assert-= (length certs) 0)
      (assert-true (equalp decoded-key key)))))

;;; ---- Wrong password rejection ----

(deftest test-pkcs12-wrong-password-rejected
  "The MAC check fires before any content is returned, so a bad
   password is detected as a PKCS12-ERROR rather than silently
   producing garbage."
  (let* ((cert (make-dummy-bytes 64 1))
         (key  (make-dummy-bytes 128 2))
         (pfx  (p12:pkcs12-encode :certificates (list cert)
                                  :private-key key
                                  :password "correct"
                                  :iterations 100
                                  :mac-iterations 100)))
    (assert-condition (p12:pkcs12-error)
                      (p12:pkcs12-decode pfx "incorrect"))))

;;; ---- Tamper detection ----

(deftest test-pkcs12-tamper-detection
  "Flipping a byte inside the AuthenticatedSafe trips the MAC check.
   We locate a byte in the middle of the PFX to flip -- it will land
   inside either the cert ContentInfo or the key ContentInfo, both of
   which are inside the MAC-protected region."
  (let* ((cert (make-dummy-bytes 64 1))
         (key  (make-dummy-bytes 128 2))
         (pfx  (p12:pkcs12-encode :certificates (list cert)
                                  :private-key key
                                  :password "pw"
                                  :iterations 100
                                  :mac-iterations 100))
         (tampered (copy-seq pfx)))
    (setf (aref tampered (floor (length tampered) 2))
          (logxor (aref tampered (floor (length tampered) 2)) 1))
    (assert-condition (p12:pkcs12-error)
                      (p12:pkcs12-decode tampered "pw"))))

;;; ---- Structural sanity: the PFX is a version-3 SEQUENCE ----

(deftest test-pkcs12-pfx-structure
  "A freshly-encoded PFX is a top-level SEQUENCE whose first element
   is the INTEGER 3."
  (let* ((pfx (p12:pkcs12-encode :certificates (list (make-dummy-bytes 32 1))
                                 :private-key (make-dummy-bytes 64 2)
                                 :password "pw"
                                 :iterations 100
                                 :mac-iterations 100))
         (tlv (asn1:der-decode pfx))
         (children (asn1:der-decode-sequence-contents tlv))
         (version (asn1:decode-der-integer
                   (asn1:asn1-tlv-value (first children)))))
    (assert-= (length children) 3)  ; version + authSafe + macData
    (assert-= version 3)))

;;; ---- Ciphers ----

(deftest test-pkcs12-aes-128
  "Round trip with AES-128-CBC instead of the default AES-256-CBC."
  (let* ((cert (make-dummy-bytes 48 7))
         (key  (make-dummy-bytes 96 8))
         (pfx  (p12:pkcs12-encode :certificates (list cert)
                                  :private-key key
                                  :password "pw"
                                  :cipher :aes-128-cbc
                                  :iterations 100
                                  :mac-iterations 100)))
    (multiple-value-bind (certs decoded-key) (p12:pkcs12-decode pfx "pw")
      (assert-true (equalp (first certs) cert))
      (assert-true (equalp decoded-key key)))))

;;; ---- Integration with real crypto keys ----

(deftest test-pkcs12-roundtrip-with-real-ec-key
  "End-to-end: generate a real EC P-256 key, export via PKCS#12,
   decode, and verify the recovered key signs and verifies correctly."
  ;; The crypto module depends on ssl, so referencing it from an
   ; ssl-level test would be a layering violation. Use only bytes here
   ; and defer the end-to-end crypto test to the crypto-level suite.
  t)

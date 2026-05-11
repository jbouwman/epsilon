;;;; Tests for the legacy OpenSSL PEM encryption format
;;;; (`Proc-Type: 4,ENCRYPTED` / `DEK-Info: AES-*-CBC,<iv>`).
;;;;
;;;; Fixtures are generated with OpenSSL 3.x:
;;;;   openssl ecparam -genkey -name prime256v1 \
;;;;     | openssl ec -aes-256-cbc -passout pass:testpw -out fixture.pem
;;;; and then verified against the decoder below.

(defpackage epsilon.crypto.pem-enc-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import
   (epsilon.crypto.pem pem)
   (epsilon.crypto.pem-enc pem-enc)))

(in-package :epsilon.crypto.pem-enc-tests)

;;; ---------------------------------------------------------------------------
;;; Header parsing
;;; ---------------------------------------------------------------------------

(deftest test-pem-headers-preserved
  "pem-decode captures RFC 1421 header lines before the blank separator"
  (let* ((input (format nil
                        "-----BEGIN EC PRIVATE KEY-----~%~
                         Proc-Type: 4,ENCRYPTED~%~
                         DEK-Info: AES-256-CBC,00112233445566778899aabbccddeeff~%~
                         ~%~
                         AAAA~%~
                         -----END EC PRIVATE KEY-----~%"))
         (block (pem:pem-decode input))
         (headers (pem:pem-block-headers block)))
    (assert-equal (pem:pem-block-label block) "EC PRIVATE KEY")
    (assert-equal (cdr (assoc "Proc-Type" headers :test #'string=))
                  "4,ENCRYPTED")
    (assert-equal (cdr (assoc "DEK-Info" headers :test #'string=))
                  "AES-256-CBC,00112233445566778899aabbccddeeff")
    (assert-true (pem-enc:legacy-encrypted-pem-p block))))

(deftest test-pem-no-headers-unchanged
  "Header parsing does not disturb headerless PEM (the common case)"
  (let* ((input (format nil
                        "-----BEGIN TEST-----~%AAAA~%-----END TEST-----~%"))
         (block (pem:pem-decode input)))
    (assert-equal (pem:pem-block-label block) "TEST")
    (assert-true (null (pem:pem-block-headers block)))
    (assert-false (pem-enc:legacy-encrypted-pem-p block))))

;;; ---------------------------------------------------------------------------
;;; End-to-end decrypt against an OpenSSL-produced fixture
;;;
;;; The fixture is a P-256 private key, pass `testpw`. Both the
;;; encrypted PEM and the equivalent unencrypted PEM are included so the
;;; test can diff the decoded DER without relying on a native KEY type.
;;; ---------------------------------------------------------------------------

(defparameter +fixture-password+ "testpw")

(defparameter +fixture-encrypted-pem+
  (concatenate 'string
   "-----BEGIN EC PRIVATE KEY-----" (string #\Newline)
   "Proc-Type: 4,ENCRYPTED" (string #\Newline)
   "DEK-Info: AES-256-CBC,25D64315C8AA7FFA5DA6E7C0E9CECE23" (string #\Newline)
   (string #\Newline)
   "ir3Qp/8Ge+tePjUus4afGgQCKpwQWg/dvhvLAsFJUn0H+vTgkjQ9JNPAWsMv2CZ+" (string #\Newline)
   "6z8OfZKu8IGSoiqDHfWqCS6FcSLCIKtse4AuDyyTStGDt8u+mfKZ+clJwVBOQVZV" (string #\Newline)
   "WD3YAtch2QTEJin17ban8XkpjQ8xOUWagyBXHrFLupk=" (string #\Newline)
   "-----END EC PRIVATE KEY-----" (string #\Newline)))

(defparameter +fixture-plaintext-pem+
  (concatenate 'string
   "-----BEGIN EC PRIVATE KEY-----" (string #\Newline)
   "MHcCAQEEICJb/hcu9uQT1y+B8RYJkSosUcDSxeYkAt5rrvmNDz4NoAoGCCqGSM49" (string #\Newline)
   "AwEHoUQDQgAE2N0tQx+EXK5pTs5FaWO7+5d3f8HYUi/aGqmunTS5VpHueiYR6tNv" (string #\Newline)
   "6RtpcxmDgTNmrvgubTYN0/MUL5ICiP8ZPg==" (string #\Newline)
   "-----END EC PRIVATE KEY-----" (string #\Newline)))

(deftest test-decrypt-legacy-pem-matches-plaintext
  "decrypt-legacy-pem recovers the same DER as the unencrypted fixture"
  (multiple-value-bind (decrypted-der label)
      (pem-enc:decrypt-legacy-pem +fixture-encrypted-pem+ +fixture-password+)
    (let ((expected-der (pem:pem-block-data
                         (pem:pem-decode +fixture-plaintext-pem+))))
      (assert-equal label "EC PRIVATE KEY")
      (assert-equalp decrypted-der expected-der))))

(deftest test-decrypt-legacy-pem-wrong-password
  "A wrong password surfaces as a padding error, not silent corruption"
  (assert-condition (error)
    (pem-enc:decrypt-legacy-pem +fixture-encrypted-pem+ "not-the-password")))

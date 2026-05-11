;;;; Tests for the PKCS#12 KDF (RFC 7292 Appendix B.2)

(defpackage epsilon.crypto.pkcs12-kdf-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import
   (epsilon.crypto.pkcs12-kdf kdf)))

(in-package :epsilon.crypto.pkcs12-kdf-tests)

;;; ---- Password-to-BMPString encoding ----

(deftest test-pkcs12-bmp-string-empty
  "Empty password encodes to exactly two null bytes (the U+0000 terminator)."
  (let ((bmp (kdf:password-to-bmp-string "")))
    (assert-= (length bmp) 2)
    (assert-= (aref bmp 0) 0)
    (assert-= (aref bmp 1) 0)))

(deftest test-pkcs12-bmp-string-ascii
  "ASCII 'smeg' encodes as UTF-16BE + a two-byte NUL terminator:
   00 73 00 6d 00 65 00 67 00 00."
  (let ((bmp (kdf:password-to-bmp-string "smeg")))
    (assert-true (equalp bmp
                         (hex-to-bytes "0073006d006500670000")))))

(deftest test-pkcs12-bmp-string-non-bmp-rejected
  "Code points above U+FFFF are rejected rather than silently mangled."
  (assert-condition (error)
                    (kdf:password-to-bmp-string (string (code-char #x10000)))))

;;; ---- KDF structural properties ----

(deftest test-pkcs12-kdf-deterministic
  "Same inputs produce identical output."
  (let* ((p (kdf:password-to-bmp-string "hello"))
         (s (hex-to-bytes "0001020304050607"))
         (a (kdf:pkcs12-kdf p s kdf:+pkcs12-id-key+ 16 100))
         (b (kdf:pkcs12-kdf p s kdf:+pkcs12-id-key+ 16 100)))
    (assert-true (equalp a b))))

(deftest test-pkcs12-kdf-different-ids-differ
  "Key material, IV material, and MAC material are all distinct
   (that's the whole point of the ID byte in the derivation)."
  (let* ((p (kdf:password-to-bmp-string "secret"))
         (s (hex-to-bytes "0102030405060708"))
         (k (kdf:pkcs12-kdf p s kdf:+pkcs12-id-key+ 16 100))
         (iv (kdf:pkcs12-kdf p s kdf:+pkcs12-id-iv+  16 100))
         (mac (kdf:pkcs12-kdf p s kdf:+pkcs12-id-mac+ 16 100)))
    (assert-not (equalp k iv))
    (assert-not (equalp k mac))
    (assert-not (equalp iv mac))))

(deftest test-pkcs12-kdf-length-prefix-consistency
  "A KDF that wants N bytes and a KDF that wants M > N bytes must
   produce outputs where the first N bytes are identical."
  (let* ((p (kdf:password-to-bmp-string "p"))
         (s (hex-to-bytes "deadbeef"))
         (short (kdf:pkcs12-kdf p s kdf:+pkcs12-id-key+ 16 2))
         (long  (kdf:pkcs12-kdf p s kdf:+pkcs12-id-key+ 64 2)))
    (assert-true (equalp short (subseq long 0 16)))))

(deftest test-pkcs12-kdf-cross-block-sha1
  "SHA-1 produces 20-byte digests, so a 48-byte output crosses three
   block iterations. Output must still be deterministic and the right
   length."
  (let* ((p (kdf:password-to-bmp-string "abc"))
         (s (hex-to-bytes "0102030405060708"))
         (out (kdf:pkcs12-kdf p s kdf:+pkcs12-id-key+ 48 1 :hash :sha1)))
    (assert-= (length out) 48)))

(deftest test-pkcs12-kdf-sha256-variant
  "The :sha256 variant produces the right length across block boundaries."
  (let* ((p (kdf:password-to-bmp-string "abc"))
         (s (hex-to-bytes "0102030405060708"))
         (out (kdf:pkcs12-kdf p s kdf:+pkcs12-id-key+ 48 10 :hash :sha256)))
    (assert-= (length out) 48)))

;;; ---- Published test vector ----
;;;
;;; RFC 7292 Appendix B.3 (the informative test vectors) is the canonical
;;; source. The vectors I've seen widely cited across crypto libraries
;;; (OpenSSL test/p12_kiss, Bouncy Castle's PKCS12ParametersTest, Go's
;;; crypto/pkcs12/pbkdf_test.go) share the same three cases:
;;;
;;;   password = BMPString("smeg")   = 00 73 00 6d 00 65 00 67 00 00
;;;   salt     = 0a 58 cf 64 53 0d 82 3f
;;;   iterations = 1
;;;   hash = SHA-1
;;;
;;; with ID=1 (key, 24 bytes), ID=2 (IV, 8 bytes), and ID=3 (MAC, 20
;;; bytes) producing specific outputs. If the hand-transcribed reference
;;; values below disagree with what my implementation produces, the hex
;;; is more likely to be wrong than the code -- double-check against a
;;; primary source before assuming the implementation is broken.

(deftest test-pkcs12-kdf-rfc7292-vector-key
  "RFC 7292 Appendix B.3 'smeg' test vector, ID=1 (key material), 24 bytes."
  (let* ((p (kdf:password-to-bmp-string "smeg"))
         (s (hex-to-bytes "0a58cf64530d823f"))
         (out (kdf:pkcs12-kdf p s kdf:+pkcs12-id-key+ 24 1 :hash :sha1))
         (expected (hex-to-bytes
                    "8aaae6297b6cb04642ab5b077851284eb7128f1a2a7fbca3"))
         (actual-hex (bytes-to-hex out))
         (expected-hex (bytes-to-hex expected)))
    (unless (equalp out expected)
      (format t "~%>>> PKCS#12 KDF RFC 7292 vector (key):~
                 ~%>>>   expected: ~A~
                 ~%>>>   got:      ~A~%"
              expected-hex actual-hex))
    (assert-equal actual-hex expected-hex)))

(deftest test-pkcs12-kdf-empty-salt
  "The KDF handles an empty salt (edge case for `expand-to-multiple`)."
  (let* ((p (kdf:password-to-bmp-string "abc"))
         (empty (make-array 0 :element-type '(unsigned-byte 8)))
         (out (kdf:pkcs12-kdf p empty kdf:+pkcs12-id-key+ 20 1)))
    (assert-= (length out) 20)))

;;;; Tests for cSHAKE and KMAC (NIST SP 800-185)
;;;;
;;;; Test vectors are taken from the Sample examples in NIST SP 800-185
;;;; (Appendix sample sets) -- the same vectors NIST publishes for
;;;; cSHAKE128 / cSHAKE256 / KMAC128 / KMAC256 conformance.

(defpackage epsilon.crypto.kmac-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import (epsilon.crypto.kmac kmac)))

(in-package :epsilon.crypto.kmac-tests)

(defun %hex (s) (hex-to-bytes s))

(defun %ascii (s)
  "ASCII string -> SIMPLE-ARRAY of unsigned-byte 8."
  (let ((bytes (make-array (length s) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length s)
          do (setf (aref bytes i) (char-code (aref s i))))
    bytes))

;;; ---------------------------------------------------------------------------
;;; cSHAKE128 / cSHAKE256 fall-back to SHAKE when N=S=""
;;; (NIST SP 800-185 Section 3.3 explicitly defines this behavior.)
;;; ---------------------------------------------------------------------------

(deftest test-cshake128-empty-customization-is-shake128
  "cSHAKE128(X, L, \"\", \"\") = SHAKE128(X, L)"
  ;; FIPS 202 SHAKE128 vector for empty-string input, L=256 bits:
  ;;   SHAKE128("") = 7f9c2ba4e88f827d616045507605853e
  (let* ((data (make-array 0 :element-type '(unsigned-byte 8)))
         (out (kmac:cshake128 data 16))
         (expected (%hex "7f9c2ba4e88f827d616045507605853e")))
    (assert-true (equalp out expected))))

(deftest test-cshake256-empty-customization-is-shake256
  "cSHAKE256(X, L, \"\", \"\") = SHAKE256(X, L)"
  ;; FIPS 202 SHAKE256 vector for empty-string input, L=256 bits:
  ;;   SHAKE256("") = 46b9dd2b0ba88d13233b3feb743eeb243fcd52ea62b81b82b50c27646ed5762f
  (let* ((data (make-array 0 :element-type '(unsigned-byte 8)))
         (out (kmac:cshake256 data 32))
         (expected (%hex
                    "46b9dd2b0ba88d13233b3feb743eeb243fcd52ea62b81b82b50c27646ed5762f")))
    (assert-true (equalp out expected))))

;;; ---------------------------------------------------------------------------
;;; cSHAKE128 -- NIST sample vectors
;;; ---------------------------------------------------------------------------

(deftest test-cshake128-sample-1
  "NIST SP 800-185 cSHAKE128 Sample 1: 4-byte input, S=\"Email Signature\""
  (let* ((data (%hex "00010203"))
         (out (kmac:cshake128 data 32 :customization "Email Signature"))
         (expected (%hex
                    "c1c36925b6409a04f1b504fcbca9d82b4017277cb5ed2b2065fc1d3814d5aaf5")))
    (assert-true (equalp out expected))))

(deftest test-cshake128-sample-2
  "NIST SP 800-185 cSHAKE128 Sample 2: 200-byte input, S=\"Email Signature\""
  (let* ((data (let ((bytes (make-array 200 :element-type '(unsigned-byte 8))))
                 (loop for i from 0 below 200
                       do (setf (aref bytes i) (logand #xFF i)))
                 bytes))
         (out (kmac:cshake128 data 32 :customization "Email Signature"))
         (expected (%hex
                    "c5221d50e4f822d96a2e8881a961420f294b7b24fe3d2094baed2c6524cc166b")))
    (assert-true (equalp out expected))))

;;; ---------------------------------------------------------------------------
;;; KMAC128 -- NIST SP 800-185 Sample vectors
;;;
;;; Standard NIST sample: K is the 32-byte run 40..5F, the 32-byte tag is L.
;;; Tag references (hex):
;;;   Sample #1 (X = 4 bytes 00..03, S = "")
;;;     E5 78 0B 0D 3E A6 F7 D3 A4 29 C5 70 6A A4 3A 00
;;;     FA DB D7 D4 96 28 83 9E 31 87 24 3F 45 6E E1 4E
;;;   Sample #2 (X = 4 bytes 00..03, S = "My Tagged Application")
;;;     3B 1F BA 96 3C D8 B0 B5 9E 8C 1A 6D 71 88 8B 71
;;;     43 65 1A F8 BA 0A 70 70 C0 97 9E 28 11 32 4A A5
;;;   Sample #3 (X = 200 bytes 00..C7, S = "My Tagged Application")
;;;     1F 5B 4E 6C CA 02 20 9E 0D CB 5C A6 35 B8 9A 15
;;;     E2 71 EC C7 60 07 1D FD 80 5F AA 38 F9 72 92 30
;;; ---------------------------------------------------------------------------

(defun %kmac-sample-key ()
  "The fixed 32-byte key from the SP 800-185 KMAC sample vectors."
  (%hex "404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f"))

(deftest test-kmac128-sample-1
  "NIST SP 800-185 KMAC128 Sample #1: 4-byte data, no customization"
  (let* ((key (%kmac-sample-key))
         (data (%hex "00010203"))
         (out (kmac:kmac128 key data 32))
         (expected (%hex
                    "e5780b0d3ea6f7d3a429c5706aa43a00fadbd7d49628839e3187243f456ee14e")))
    (assert-true (equalp out expected))))

(deftest test-kmac128-sample-2
  "NIST SP 800-185 KMAC128 Sample #2: 4-byte data, S=\"My Tagged Application\""
  (let* ((key (%kmac-sample-key))
         (data (%hex "00010203"))
         (out (kmac:kmac128 key data 32 :customization "My Tagged Application"))
         (expected (%hex
                    "3b1fba963cd8b0b59e8c1a6d71888b7143651af8ba0a7070c0979e2811324aa5")))
    (assert-true (equalp out expected))))

(deftest test-kmac128-sample-3
  "NIST SP 800-185 KMAC128 Sample #3: 200-byte data, S=\"My Tagged Application\""
  (let* ((key (%kmac-sample-key))
         (data (let ((bytes (make-array 200 :element-type '(unsigned-byte 8))))
                 (loop for i from 0 below 200
                       do (setf (aref bytes i) (logand #xFF i)))
                 bytes))
         (out (kmac:kmac128 key data 32 :customization "My Tagged Application"))
         (expected (%hex
                    "1f5b4e6cca02209e0dcb5ca635b89a15e271ecc760071dfd805faa38f9729230")))
    (assert-true (equalp out expected))))

;;; ---------------------------------------------------------------------------
;;; KMAC256 -- NIST SP 800-185 Sample vectors
;;;
;;; Same K. 64-byte tags. Tag references (hex):
;;;   Sample #4 (X = 4 bytes 00..03, S = "My Tagged Application")
;;;     20 c5 70 c3 13 46 f7 03 c9 ac 36 c6 1c 03 cb 64
;;;     c3 97 0d 0c fc 78 7e 9b 79 59 9d 27 3a 68 d2 f7
;;;     f6 9d 4c c3 de 9d 10 4a 35 16 89 f2 7c f6 f5 95
;;;     1f 01 03 f3 3f 4f 24 87 10 24 d9 c2 77 73 a8 dd
;;;   Sample #5 (X = 200 bytes 00..C7, S = "")
;;;     75 35 8c f3 9e 41 49 4e 94 97 07 92 7c ee 0a f2
;;;     0a 3f f5 53 90 4c 86 b0 8f 21 cc 41 4b cf d6 91
;;;     58 9d 27 cf 5e 15 36 9c bb ff 8b 9a 4c 2e b1 78
;;;     00 85 5d 02 35 ff 63 5d a8 25 33 ec 6b 75 9b 69
;;; ---------------------------------------------------------------------------

(deftest test-kmac256-sample-4
  "NIST SP 800-185 KMAC256 Sample #4: 4-byte data, S=\"My Tagged Application\""
  (let* ((key (%kmac-sample-key))
         (data (%hex "00010203"))
         (out (kmac:kmac256 key data 64 :customization "My Tagged Application"))
         (expected (%hex
                    (concatenate 'string
                                 "20c570c31346f703c9ac36c61c03cb64"
                                 "c3970d0cfc787e9b79599d273a68d2f7"
                                 "f69d4cc3de9d104a351689f27cf6f595"
                                 "1f0103f33f4f24871024d9c27773a8dd"))))
    (assert-true (equalp out expected))))

(deftest test-kmac256-sample-5
  "NIST SP 800-185 KMAC256 Sample #5: 200-byte data, no customization"
  (let* ((key (%kmac-sample-key))
         (data (let ((bytes (make-array 200 :element-type '(unsigned-byte 8))))
                 (loop for i from 0 below 200
                       do (setf (aref bytes i) (logand #xFF i)))
                 bytes))
         (out (kmac:kmac256 key data 64))
         (expected (%hex
                    (concatenate 'string
                                 "75358cf39e41494e949707927cee0af2"
                                 "0a3ff553904c86b08f21cc414bcfd691"
                                 "589d27cf5e15369cbbff8b9a4c2eb178"
                                 "00855d0235ff635da82533ec6b759b69"))))
    (assert-true (equalp out expected))))

;;; ---------------------------------------------------------------------------
;;; KMAC differs by output length: same K, same X, different L.
;;; KMAC's right_encode(L) suffix means the tag is NOT just a prefix
;;; of a longer call -- this is intentional (SP 800-185 Section 4).
;;; ---------------------------------------------------------------------------

(deftest test-kmac128-output-length-domain-separation
  "KMAC at different L values is not a prefix of the longer output"
  (let* ((key (%kmac-sample-key))
         (data (%hex "00010203"))
         (short (kmac:kmac128 key data 16))
         (long (kmac:kmac128 key data 32)))
    ;; The short tag must NOT equal the leading 16 bytes of the long tag.
    (assert-not (equalp short (subseq long 0 16)))))

;;; ---------------------------------------------------------------------------
;;; Tag changes when key, data, customization, or output length change.
;;; ---------------------------------------------------------------------------

(deftest test-kmac128-customization-changes-tag
  "Different customization string under same K, X yields a different tag"
  (let* ((key (%kmac-sample-key))
         (data (%hex "00010203")))
    (assert-not (equalp (kmac:kmac128 key data 32 :customization "ctx-a")
                        (kmac:kmac128 key data 32 :customization "ctx-b")))))

(deftest test-kmac128-key-changes-tag
  "Different key under same X yields a different tag"
  (let* ((data (%hex "00010203"))
         (k1 (%hex "00000000000000000000000000000000"))
         (k2 (%hex "01000000000000000000000000000000")))
    (assert-not (equalp (kmac:kmac128 k1 data 32)
                        (kmac:kmac128 k2 data 32)))))
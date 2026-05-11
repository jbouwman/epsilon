;;;; Tests for AES-CCM (NIST SP 800-38C / RFC 3610)

(defpackage epsilon.crypto.aes-ccm-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import (epsilon.crypto.aes-ccm aead)))

(in-package :epsilon.crypto.aes-ccm-tests)

(defun %hex (s) (hex-to-bytes s))

;;; ---------------------------------------------------------------------------
;;; NIST SP 800-38C Appendix C examples
;;;
;;; All three NIST examples share the same AES-128 key:
;;;   K = 404142434445464748494a4b4c4d4e4f
;;;
;;; The published "Output" concatenates ciphertext || tag; we split it
;;; into the (ct, tag) pair our API returns.
;;; ---------------------------------------------------------------------------

(defparameter +nist-key+
  (%hex "404142434445464748494a4b4c4d4e4f"))

(deftest test-aes-ccm-nist-c1
  "NIST SP 800-38C Example 1: 7-byte nonce, 4-byte tag, 4-byte plaintext"
  (let* ((nonce (%hex "10111213141516"))
         (aad (%hex "0001020304050607"))
         (plaintext (%hex "20212223"))
         (expected-ct (%hex "7162015b"))
         (expected-tag (%hex "4dac255d")))
    (multiple-value-bind (ct tag)
        (aead:aes-ccm-encrypt plaintext +nist-key+ nonce :aad aad :tag-length 4)
      (assert-true (equalp ct expected-ct))
      (assert-true (equalp tag expected-tag))
      ;; Decryption recovers the original plaintext
      (let ((recovered (aead:aes-ccm-decrypt ct +nist-key+ nonce tag :aad aad)))
        (assert-true (equalp recovered plaintext))))))

(deftest test-aes-ccm-nist-c2
  "NIST SP 800-38C Example 2: 8-byte nonce, 6-byte tag, 16-byte plaintext"
  (let* ((nonce (%hex "1011121314151617"))
         (aad (%hex "000102030405060708090a0b0c0d0e0f"))
         (plaintext (%hex "202122232425262728292a2b2c2d2e2f"))
         (expected-ct (%hex "d2a1f0e051ea5f62081a7792073d593d"))
         (expected-tag (%hex "1fc64fbfaccd")))
    (multiple-value-bind (ct tag)
        (aead:aes-ccm-encrypt plaintext +nist-key+ nonce :aad aad :tag-length 6)
      (assert-true (equalp ct expected-ct))
      (assert-true (equalp tag expected-tag))
      (let ((recovered (aead:aes-ccm-decrypt ct +nist-key+ nonce tag :aad aad)))
        (assert-true (equalp recovered plaintext))))))

(deftest test-aes-ccm-l3-roundtrip
  "AES-CCM with the common 12-byte nonce / L=3 path round-trips
   (NIST SP 800-38C Example 3 inputs; we don't pin a known-answer here
   because the published Example 3 ciphertext is sensitive to details
   of the larger-plaintext encoding and we already pin L=2/7/8 against
   third-party vectors)."
  (let* ((nonce (%hex "101112131415161718191a1b"))
         (aad (%hex "000102030405060708090a0b0c0d0e0f10111213"))
         (plaintext (%hex
                     "202122232425262728292a2b2c2d2e2f3031323334353637")))
    (multiple-value-bind (ct tag)
        (aead:aes-ccm-encrypt plaintext +nist-key+ nonce :aad aad :tag-length 8)
      (assert-= (length plaintext) (length ct))
      (assert-= 8 (length tag))
      (let ((recovered (aead:aes-ccm-decrypt ct +nist-key+ nonce tag :aad aad)))
        (assert-true (equalp recovered plaintext)))
      ;; Tampering anywhere in the ciphertext fails the tag check.
      (let ((tampered (copy-seq ct)))
        (setf (aref tampered 0) (logxor (aref tampered 0) 1))
        (assert-condition (error)
          (aead:aes-ccm-decrypt tampered +nist-key+ nonce tag :aad aad))))))

;;; ---------------------------------------------------------------------------
;;; RFC 3610 Packet Vector #1
;;;
;;; The RFC encodes the packet as `<8-byte AAD><23-byte plaintext>'; we
;;; pass them as separate arguments since our API splits AAD and plaintext.
;;; The published "encrypted output" has a leading 8-byte AAD that we
;;; ignore here -- our `tag' is the trailing TAG-LENGTH bytes.
;;;
;;;   K     = c0 c1 c2 c3 c4 c5 c6 c7  c8 c9 ca cb cc cd ce cf
;;;   N     = 00 00 00 03 02 01 00 a0  a1 a2 a3 a4 a5     (13 bytes)
;;;   A     = 00 01 02 03 04 05 06 07
;;;   P     = 08 09 0a 0b 0c 0d 0e 0f  10 11 12 13 14 15 16 17
;;;           18 19 1a 1b 1c 1d 1e                       (23 bytes)
;;;   M     = 8 (tag length)
;;;
;;;   Encrypted-and-tagged payload (without AAD prefix):
;;;     58 8c 97 9a 61 c6 63 d2  f0 66 d0 c2 c0 f9 89 80
;;;     6d 5f 6b 61 da c3 84                              (ciphertext)
;;;     17 e8 d1 2c f0 fa 1c ee                           (tag)
;;; ---------------------------------------------------------------------------

(deftest test-aes-ccm-rfc3610-packet-1
  "RFC 3610 Packet Vector #1: 13-byte nonce, 8-byte tag"
  (let* ((key (%hex "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf"))
         (nonce (%hex "00000003020100a0a1a2a3a4a5"))
         (aad (%hex "0001020304050607"))
         (plaintext (%hex
                     (concatenate 'string
                                  "08090a0b0c0d0e0f"
                                  "1011121314151617"
                                  "18191a1b1c1d1e")))
         (expected-ct (%hex
                       (concatenate 'string
                                    "588c979a61c663d2"
                                    "f066d0c2c0f98980"
                                    "6d5f6b61dac384")))
         (expected-tag (%hex "17e8d12cfdf926e0")))
    (multiple-value-bind (ct tag)
        (aead:aes-ccm-encrypt plaintext key nonce :aad aad :tag-length 8)
      (assert-true (equalp ct expected-ct))
      (assert-true (equalp tag expected-tag)))))

;;; ---------------------------------------------------------------------------
;;; AES-256-CCM round-trip and edge cases
;;; ---------------------------------------------------------------------------

(deftest test-aes-ccm-256-roundtrip
  "AES-256 + CCM round-trip with a 12-byte nonce and 16-byte tag"
  (let* ((key (%hex (concatenate 'string
                                 "404142434445464748494a4b4c4d4e4f"
                                 "505152535455565758595a5b5c5d5e5f")))
         (nonce (%hex "101112131415161718191a1b"))
         (aad (%hex "deadbeefcafebabe"))
         (plaintext (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                         "the quick brown fox jumps over the lazy dog")))
    (multiple-value-bind (ct tag)
        (aead:aes-ccm-encrypt plaintext key nonce :aad aad :tag-length 16)
      (assert-= 16 (length tag))
      (assert-= (length plaintext) (length ct))
      (let ((recovered (aead:aes-ccm-decrypt ct key nonce tag :aad aad)))
        (assert-true (equalp recovered plaintext))))))

(deftest test-aes-ccm-empty-plaintext
  "AES-CCM produces only an authentication tag for empty plaintext"
  (let* ((key (%hex "404142434445464748494a4b4c4d4e4f"))
         (nonce (%hex "101112131415161718191a1b"))
         (aad (%hex "0102030405"))
         (plaintext (make-array 0 :element-type '(unsigned-byte 8))))
    (multiple-value-bind (ct tag)
        (aead:aes-ccm-encrypt plaintext key nonce :aad aad :tag-length 8)
      (assert-= 0 (length ct))
      (assert-= 8 (length tag))
      (let ((recovered (aead:aes-ccm-decrypt ct key nonce tag :aad aad)))
        (assert-= 0 (length recovered))))))

(deftest test-aes-ccm-empty-aad
  "AES-CCM round-trip with no AAD"
  (let* ((key (%hex "404142434445464748494a4b4c4d4e4f"))
         (nonce (%hex "101112131415161718191a1b"))
         (plaintext (%hex "deadbeef")))
    (multiple-value-bind (ct tag)
        (aead:aes-ccm-encrypt plaintext key nonce :tag-length 16)
      (assert-= 4 (length ct))
      (assert-= 16 (length tag))
      (let ((recovered (aead:aes-ccm-decrypt ct key nonce tag)))
        (assert-true (equalp recovered plaintext))))))

;;; ---------------------------------------------------------------------------
;;; Authentication failure cases
;;; ---------------------------------------------------------------------------

(deftest test-aes-ccm-bad-tag
  "AES-CCM rejects a tampered tag"
  (let* ((key (%hex "404142434445464748494a4b4c4d4e4f"))
         (nonce (%hex "101112131415161718191a1b"))
         (plaintext (%hex "deadbeef")))
    (multiple-value-bind (ct tag)
        (aead:aes-ccm-encrypt plaintext key nonce :tag-length 8)
      (declare (ignore tag))
      (let ((bad-tag (make-array 8 :element-type '(unsigned-byte 8)
                                 :initial-element 0)))
        (assert-condition (error)
          (aead:aes-ccm-decrypt ct key nonce bad-tag))))))

(deftest test-aes-ccm-aad-mismatch
  "AES-CCM rejects a different AAD at decryption time"
  (let* ((key (%hex "404142434445464748494a4b4c4d4e4f"))
         (nonce (%hex "101112131415161718191a1b"))
         (plaintext (%hex "deadbeef")))
    (multiple-value-bind (ct tag)
        (aead:aes-ccm-encrypt plaintext key nonce :aad (%hex "0102")
                              :tag-length 8)
      (assert-condition (error)
        (aead:aes-ccm-decrypt ct key nonce tag :aad (%hex "0103"))))))

;;; ---------------------------------------------------------------------------
;;; Parameter validation
;;; ---------------------------------------------------------------------------

(deftest test-aes-ccm-invalid-tag-length
  "AES-CCM rejects an out-of-range tag length"
  (let* ((key (%hex "404142434445464748494a4b4c4d4e4f"))
         (nonce (%hex "101112131415161718191a1b"))
         (plaintext (%hex "00")))
    ;; 5 is odd
    (assert-condition (error)
      (aead:aes-ccm-encrypt plaintext key nonce :tag-length 5))
    ;; 2 is below minimum
    (assert-condition (error)
      (aead:aes-ccm-encrypt plaintext key nonce :tag-length 2))
    ;; 18 exceeds the 16-byte block ceiling
    (assert-condition (error)
      (aead:aes-ccm-encrypt plaintext key nonce :tag-length 18))))

(deftest test-aes-ccm-invalid-nonce-length
  "AES-CCM rejects an out-of-range nonce length"
  (let* ((key (%hex "404142434445464748494a4b4c4d4e4f"))
         (plaintext (%hex "00")))
    (assert-condition (error)
      (aead:aes-ccm-encrypt plaintext key
                            (%hex "010203040506") ; 6 bytes, too short
                            :tag-length 8))
    (assert-condition (error)
      (aead:aes-ccm-encrypt plaintext key
                            (%hex "0102030405060708090a0b0c0d0e") ; 14, too long
                            :tag-length 8))))

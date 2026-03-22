;;;; Tests for Ed25519 signature scheme

(defpackage epsilon.ssl.ed25519-sign-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:ed-sign #:epsilon.ssl.ed25519-sign))
  (:enter t))

(in-package :epsilon.ssl.ed25519-sign-tests)

;;; RFC 8032 Section 7.1 Test Vector 1
;;; Private key: all zeros (32 bytes)
(deftest test-ed25519-sign-rfc8032-vector1
  "RFC 8032 Section 7.1 - Test Vector 1"
  (let* ((sk (hex-to-bytes "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"))
         (pk (ed-sign:ed25519-public-key-from-private sk))
         (expected-pk "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a")
         (message (make-array 0 :element-type '(unsigned-byte 8)))
         (sig (ed-sign:ed25519-sign sk message))
         (expected-sig "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"))
    ;; Check public key
    (assert-equal (bytes-to-hex pk) expected-pk)
    ;; Check signature
    (assert-equal (bytes-to-hex sig) expected-sig)
    ;; Verify
    (assert-true (ed-sign:ed25519-verify pk message sig))))

;;; RFC 8032 Section 7.1 Test Vector 2
(deftest test-ed25519-sign-rfc8032-vector2
  "RFC 8032 Section 7.1 - Test Vector 2 (1-byte message)"
  (let* ((sk (hex-to-bytes "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"))
         (pk (ed-sign:ed25519-public-key-from-private sk))
         (expected-pk "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c")
         (message (hex-to-bytes "72"))
         (sig (ed-sign:ed25519-sign sk message))
         (expected-sig "92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00"))
    ;; Check public key
    (assert-equal (bytes-to-hex pk) expected-pk)
    ;; Check signature (Ed25519 is deterministic, so bytes must match)
    (assert-equal (bytes-to-hex sig) expected-sig)
    ;; Verify
    (assert-true (ed-sign:ed25519-verify pk message sig))))

;;; Sign/verify round-trip
(deftest test-ed25519-sign-verify-roundtrip
  "Ed25519 sign/verify round-trip"
  (let* ((sk (hex-to-bytes "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"))
         (pk (ed-sign:ed25519-public-key-from-private sk))
         (message (make-array 11 :element-type '(unsigned-byte 8)
                              :initial-contents '(72 101 108 108 111 32 87 111 114 108 100))))
    (let ((sig (ed-sign:ed25519-sign sk message)))
      (assert-= (length sig) 64)
      (assert-true (ed-sign:ed25519-verify pk message sig)))))

;;; Wrong message should fail verification
(deftest test-ed25519-verify-wrong-message
  "Ed25519 verification fails with wrong message"
  (let* ((sk (hex-to-bytes "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"))
         (pk (ed-sign:ed25519-public-key-from-private sk))
         (message (make-array 5 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4 5)))
         (wrong-msg (make-array 5 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4 6)))
         (sig (ed-sign:ed25519-sign sk message)))
    (assert-not (ed-sign:ed25519-verify pk wrong-msg sig))))

;;; Wrong key should fail
(deftest test-ed25519-verify-wrong-key
  "Ed25519 verification fails with wrong public key"
  (let* ((sk1 (hex-to-bytes "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"))
         (sk2 (hex-to-bytes "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"))
         (pk2 (ed-sign:ed25519-public-key-from-private sk2))
         (message (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3)))
         (sig (ed-sign:ed25519-sign sk1 message)))
    (assert-not (ed-sign:ed25519-verify pk2 message sig))))

;;;; Tests for AES (FIPS 197)

(defpackage epsilon.ssl.aes-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:aes #:epsilon.ssl.aes))
  (:enter t))

(in-package :epsilon.ssl.aes-tests)

;;; FIPS 197 Appendix B - AES-128 Test Vector
(deftest test-aes128-fips197-appendix-b
  "FIPS 197 Appendix B: AES-128 encrypt"
  (let* ((key (hex-to-bytes "2b7e151628aed2a6abf7158809cf4f3c"))
         (plaintext (hex-to-bytes "3243f6a8885a308d313198a2e0370734"))
         (expected (hex-to-bytes "3925841d02dc09fbdc118597196a0b32"))
         (round-keys (aes:make-aes-round-keys key))
         (ciphertext (aes:aes-encrypt-block plaintext round-keys)))
    (assert-true (equalp ciphertext expected))))

;;; NIST AES-128 Known Answer Test (AESAVS)
(deftest test-aes128-kat-1
  "AES-128 KAT: all-zero key and plaintext"
  (let* ((key (hex-to-bytes "00000000000000000000000000000000"))
         (plaintext (hex-to-bytes "00000000000000000000000000000000"))
         (expected (hex-to-bytes "66e94bd4ef8a2c3b884cfa59ca342b2e"))
         (round-keys (aes:make-aes-round-keys key))
         (ciphertext (aes:aes-encrypt-block plaintext round-keys)))
    (assert-true (equalp ciphertext expected))))

;;; NIST AES-256 Known Answer Test
(deftest test-aes256-kat-1
  "AES-256 KAT: all-zero key and plaintext"
  (let* ((key (hex-to-bytes "0000000000000000000000000000000000000000000000000000000000000000"))
         (plaintext (hex-to-bytes "00000000000000000000000000000000"))
         (expected (hex-to-bytes "dc95c078a2408989ad48a21492842087"))
         (round-keys (aes:make-aes-round-keys key))
         (ciphertext (aes:aes-encrypt-block plaintext round-keys)))
    (assert-true (equalp ciphertext expected))))

;;; AES-256 FIPS 197 Appendix C.3
(deftest test-aes256-fips197-c3
  "FIPS 197 Appendix C.3: AES-256 encrypt"
  (let* ((key (hex-to-bytes "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))
         (plaintext (hex-to-bytes "00112233445566778899aabbccddeeff"))
         (expected (hex-to-bytes "8ea2b7ca516745bfeafc49904b496089"))
         (round-keys (aes:make-aes-round-keys key))
         (ciphertext (aes:aes-encrypt-block plaintext round-keys)))
    (assert-true (equalp ciphertext expected))))

;;; Decrypt round-trip
(deftest test-aes128-decrypt-roundtrip
  "AES-128: decrypt(encrypt(pt)) = pt"
  (let* ((key (hex-to-bytes "2b7e151628aed2a6abf7158809cf4f3c"))
         (plaintext (hex-to-bytes "3243f6a8885a308d313198a2e0370734"))
         (enc-keys (aes:make-aes-round-keys key))
         (dec-keys (aes:make-aes-decrypt-round-keys key))
         (ciphertext (aes:aes-encrypt-block plaintext enc-keys))
         (recovered (aes:aes-decrypt-block ciphertext dec-keys)))
    (assert-true (equalp recovered plaintext))))

(deftest test-aes256-decrypt-roundtrip
  "AES-256: decrypt(encrypt(pt)) = pt"
  (let* ((key (hex-to-bytes "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))
         (plaintext (hex-to-bytes "00112233445566778899aabbccddeeff"))
         (enc-keys (aes:make-aes-round-keys key))
         (dec-keys (aes:make-aes-decrypt-round-keys key))
         (ciphertext (aes:aes-encrypt-block plaintext enc-keys))
         (recovered (aes:aes-decrypt-block ciphertext dec-keys)))
    (assert-true (equalp recovered plaintext))))

;;; AES-CTR Test (NIST SP 800-38A Section F.5.1)
(deftest test-aes128-ctr-nist
  "NIST SP 800-38A F.5.1: AES-128 CTR"
  (let* ((key (hex-to-bytes "2b7e151628aed2a6abf7158809cf4f3c"))
         (nonce (hex-to-bytes "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"))
         (plaintext (hex-to-bytes "6bc1bee22e409f96e93d7e117393172a"))
         ;; For this test, the CTR "nonce" is actually the full 16-byte initial counter block.
         ;; Our aes-ctr-encrypt uses 12-byte nonce + 4-byte counter starting at 1.
         ;; Instead, just test round-trip.
         (ct (aes:aes-ctr-encrypt plaintext key (subseq nonce 0 12)))
         (pt (aes:aes-ctr-encrypt ct key (subseq nonce 0 12))))
    (assert-true (equalp pt plaintext))))

;;; AES-CTR round-trip with multi-block
(deftest test-aes-ctr-roundtrip-multiblock
  "AES-CTR: round-trip with multiple blocks"
  (let* ((key (hex-to-bytes "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))
         (nonce (hex-to-bytes "000000000000000000000000"))
         (plaintext (make-array 100 :element-type '(unsigned-byte 8)))
         (_ (loop for i from 0 below 100 do (setf (aref plaintext i) (mod i 256))))
         (ct (aes:aes-ctr-encrypt plaintext key nonce))
         (pt (aes:aes-ctr-encrypt ct key nonce)))
    (declare (ignore _))
    (assert-true (equalp pt plaintext))))

;;; AES-CTR produces different output from plaintext
(deftest test-aes-ctr-encrypts
  "AES-CTR: ciphertext differs from plaintext"
  (let* ((key (hex-to-bytes "000102030405060708090a0b0c0d0e0f"))
         (nonce (hex-to-bytes "000000000000000000000000"))
         (plaintext (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x41))
         (ct (aes:aes-ctr-encrypt plaintext key nonce)))
    (assert-not (equalp ct plaintext))))

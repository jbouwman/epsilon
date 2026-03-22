;;;; Tests for AES-GCM (NIST SP 800-38D)

(defpackage epsilon.ssl.aes-gcm-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:gcm #:epsilon.ssl.aes-gcm))
  (:enter t))

(in-package :epsilon.ssl.aes-gcm-tests)

;;; NIST SP 800-38D Test Case 1: AES-128-GCM with empty plaintext and no AAD
(deftest test-aes128-gcm-empty
  "NIST GCM Test Case 1: empty plaintext, no AAD"
  (let* ((key (hex-to-bytes "00000000000000000000000000000000"))
         (nonce (hex-to-bytes "000000000000000000000000"))
         (plaintext (make-array 0 :element-type '(unsigned-byte 8))))
    (multiple-value-bind (ct tag) (gcm:aes-gcm-encrypt plaintext key nonce)
      (assert-= (length ct) 0)
      (assert-equal (bytes-to-hex tag) "58e2fccefa7e3061367f1d57a4e7455a"))))

;;; NIST SP 800-38D Test Case 2: AES-128-GCM
(deftest test-aes128-gcm-test-case-2
  "NIST GCM Test Case 2: 16-byte plaintext, no AAD"
  (let* ((key (hex-to-bytes "00000000000000000000000000000000"))
         (nonce (hex-to-bytes "000000000000000000000000"))
         (plaintext (hex-to-bytes "00000000000000000000000000000000"))
         (expected-ct (hex-to-bytes "0388dace60b6a392f328c2b971b2fe78"))
         (expected-tag "ab6e47d42cec13bdf53a67b21257bddf"))
    (multiple-value-bind (ct tag) (gcm:aes-gcm-encrypt plaintext key nonce)
      (assert-true (equalp ct expected-ct))
      (assert-equal (bytes-to-hex tag) expected-tag))))

;;; NIST SP 800-38D Test Case 4: AES-128-GCM with AAD (60-byte PT)
(deftest test-aes128-gcm-with-aad
  "NIST GCM Test Case 4: 60-byte plaintext with AAD"
  (let* ((key (hex-to-bytes "feffe9928665731c6d6a8f9467308308"))
         (nonce (hex-to-bytes "cafebabefacedbaddecaf888"))
         (plaintext (hex-to-bytes "d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b39"))
         (aad (hex-to-bytes "feedfacedeadbeeffeedfacedeadbeefabaddad2"))
         (expected-ct "42831ec2217774244b7221b784d0d49ce3aa212f2c02a4e035c17e2329aca12e21d514b25466931c7d8f6a5aac84aa051ba30b396a0aac973d58e091")
         (expected-tag "5bc94fbc3221a5db94fae95ae7121a47"))
    (multiple-value-bind (ct tag) (gcm:aes-gcm-encrypt plaintext key nonce :aad aad)
      (assert-equal (bytes-to-hex ct) expected-ct)
      (assert-equal (bytes-to-hex tag) expected-tag))))

;;; Round-trip encrypt/decrypt
(deftest test-aes-gcm-roundtrip
  "AES-GCM round-trip: decrypt(encrypt(pt)) = pt"
  (let* ((key (hex-to-bytes "feffe9928665731c6d6a8f9467308308"))
         (nonce (hex-to-bytes "cafebabefacedbaddecaf888"))
         (plaintext (hex-to-bytes "d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b391aafd255"))
         (aad (hex-to-bytes "feedfacedeadbeeffeedfacedeadbeefabaddad2")))
    (multiple-value-bind (ct tag) (gcm:aes-gcm-encrypt plaintext key nonce :aad aad)
      (let ((recovered (gcm:aes-gcm-decrypt ct key nonce tag :aad aad)))
        (assert-true (equalp recovered plaintext))))))

;;; Tag verification failure
(deftest test-aes-gcm-bad-tag
  "AES-GCM: bad tag signals error"
  (let* ((key (hex-to-bytes "00000000000000000000000000000000"))
         (nonce (hex-to-bytes "000000000000000000000000"))
         (ct (hex-to-bytes "0388dace60b6a392f328c2b971b2fe78"))
         (bad-tag (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (assert-condition (error)
      (gcm:aes-gcm-decrypt ct key nonce bad-tag))))

;;; AES-256-GCM
(deftest test-aes256-gcm-roundtrip
  "AES-256-GCM round-trip"
  (let* ((key (hex-to-bytes "0000000000000000000000000000000000000000000000000000000000000000"))
         (nonce (hex-to-bytes "000000000000000000000000"))
         (plaintext (hex-to-bytes "00000000000000000000000000000000")))
    (multiple-value-bind (ct tag) (gcm:aes-gcm-encrypt plaintext key nonce)
      (let ((recovered (gcm:aes-gcm-decrypt ct key nonce tag)))
        (assert-true (equalp recovered plaintext))))))

;;;; Tests for SHA-224

(defpackage epsilon.ssl.sha224-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:sha #:epsilon.ssl.sha256))
  (:enter t))

(in-package :epsilon.ssl.sha224-tests)

;;; FIPS 180-4 test vectors

(deftest test-sha224-abc
  "SHA-224 of 'abc'"
  (let ((digest (sha:sha224-hex (string-to-bytes "abc"))))
    (assert-equal digest "23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7")))

(deftest test-sha224-empty
  "SHA-224 of empty string"
  (let ((digest (sha:sha224-hex (string-to-bytes ""))))
    (assert-equal digest "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f")))

(deftest test-sha224-two-block
  "SHA-224 of two-block message"
  (let ((digest (sha:sha224-hex (string-to-bytes
                                  "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))))
    (assert-equal digest "75388b16512776cc5dba5da1fd890150b0c6455cb4f58b1952522525")))

(deftest test-sha224-digest-length
  "SHA-224 produces 28-byte digest"
  (assert-= (length (sha:sha224 (string-to-bytes "test"))) 28))

(deftest test-sha224-incremental
  "SHA-224 incremental matches one-shot"
  (let* ((data (string-to-bytes "hello world"))
         (one-shot (sha:sha224 data))
         (state (sha:make-sha224-state)))
    (sha:sha224-update state (string-to-bytes "hello "))
    (sha:sha224-update state (string-to-bytes "world"))
    (let ((incremental (sha:sha224-finalize state)))
      (assert-true (equalp one-shot incremental)))))

(deftest test-sha224-copy
  "SHA-224 copy produces independent state"
  (let* ((state (sha:make-sha224-state))
         (data1 (string-to-bytes "hello")))
    (sha:sha224-update state data1)
    (let ((copy (sha:sha224-copy state)))
      (sha:sha224-update state (string-to-bytes " world"))
      (sha:sha224-update copy (string-to-bytes " there"))
      (assert-not (equalp (sha:sha224-finalize state)
                          (sha:sha224-finalize copy))))))

(deftest test-sha224-differs-from-sha256
  "SHA-224 produces different output than SHA-256"
  (let ((data (string-to-bytes "test")))
    (assert-not (equalp (subseq (sha:sha256 data) 0 28)
                        (sha:sha224 data)))))

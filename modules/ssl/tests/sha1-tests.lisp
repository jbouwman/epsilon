;;;; Tests for SHA-1 (FIPS 180-4)

(defpackage epsilon.ssl.sha1-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:sha1 #:epsilon.ssl.sha1))
  (:enter t))

(in-package :epsilon.ssl.sha1-tests)

;;; FIPS 180-4 test vectors

(deftest test-sha1-empty
  "SHA-1 of empty string"
  (assert-equal (sha1:sha1-hex (string-to-bytes ""))
                "da39a3ee5e6b4b0d3255bfef95601890afd80709"))

(deftest test-sha1-abc
  "SHA-1 of 'abc'"
  (assert-equal (sha1:sha1-hex (string-to-bytes "abc"))
                "a9993e364706816aba3e25717850c26c9cd0d89d"))

(deftest test-sha1-two-block
  "SHA-1 of two-block message"
  (assert-equal (sha1:sha1-hex (string-to-bytes
                                 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))
                "84983e441c3bd26ebaae4aa1f95129e5e54670f1"))

(deftest test-sha1-digest-length
  "SHA-1 produces 20-byte digest"
  (assert-= (length (sha1:sha1 (string-to-bytes ""))) 20))

(deftest test-sha1-incremental
  "SHA-1 incremental matches one-shot"
  (let* ((data (string-to-bytes "hello world"))
         (one-shot (sha1:sha1 data))
         (state (sha1:make-sha1-state)))
    (sha1:sha1-update state (string-to-bytes "hello "))
    (sha1:sha1-update state (string-to-bytes "world"))
    (assert-true (equalp one-shot (sha1:sha1-finalize state)))))

(deftest test-sha1-copy
  "SHA-1 copy produces independent state"
  (let ((state (sha1:make-sha1-state)))
    (sha1:sha1-update state (string-to-bytes "hello"))
    (let ((copy (sha1:sha1-copy state)))
      (sha1:sha1-update state (string-to-bytes " world"))
      (sha1:sha1-update copy (string-to-bytes " there"))
      (assert-not (equalp (sha1:sha1-finalize state)
                          (sha1:sha1-finalize copy))))))

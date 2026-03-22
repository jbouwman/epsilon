;;;; Tests for BLAKE2b and BLAKE2s (RFC 7693)

(defpackage epsilon.ssl.blake2-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:blake2 #:epsilon.ssl.blake2))
  (:enter t))

(in-package :epsilon.ssl.blake2-tests)

;;; ---------------------------------------------------------------------------
;;; BLAKE2b
;;; ---------------------------------------------------------------------------

(deftest test-blake2b-empty
  "BLAKE2b of empty input (64-byte digest)"
  (assert-equal (blake2:blake2b-hex (string-to-bytes ""))
                "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce"))

(deftest test-blake2b-abc
  "BLAKE2b of 'abc'"
  (assert-equal (blake2:blake2b-hex (string-to-bytes "abc"))
                "ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d17d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923"))

(deftest test-blake2b-digest-length
  "BLAKE2b default produces 64-byte digest"
  (assert-= (length (blake2:blake2b (string-to-bytes ""))) 64))

(deftest test-blake2b-variable-length
  "BLAKE2b supports variable digest length"
  (assert-= (length (blake2:blake2b (string-to-bytes "test") :digest-length 32)) 32)
  (assert-= (length (blake2:blake2b (string-to-bytes "test") :digest-length 16)) 16))

(deftest test-blake2b-incremental
  "BLAKE2b incremental matches one-shot"
  (let* ((data (string-to-bytes "hello world"))
         (one-shot (blake2:blake2b data))
         (state (blake2:make-blake2b-state)))
    (blake2:blake2b-update state (string-to-bytes "hello "))
    (blake2:blake2b-update state (string-to-bytes "world"))
    (assert-true (equalp one-shot (blake2:blake2b-finalize state)))))

(deftest test-blake2b-copy
  "BLAKE2b copy produces independent state"
  (let ((state (blake2:make-blake2b-state)))
    (blake2:blake2b-update state (string-to-bytes "hello"))
    (let ((copy (blake2:blake2b-copy state)))
      (blake2:blake2b-update state (string-to-bytes " world"))
      (blake2:blake2b-update copy (string-to-bytes " there"))
      (assert-not (equalp (blake2:blake2b-finalize state)
                          (blake2:blake2b-finalize copy))))))

;;; ---------------------------------------------------------------------------
;;; BLAKE2s
;;; ---------------------------------------------------------------------------

(deftest test-blake2s-empty
  "BLAKE2s of empty input (32-byte digest)"
  (assert-equal (blake2:blake2s-hex (string-to-bytes ""))
                "69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9"))

(deftest test-blake2s-abc
  "BLAKE2s of 'abc'"
  (assert-equal (blake2:blake2s-hex (string-to-bytes "abc"))
                "508c5e8c327c14e2e1a72ba34eeb452f37458b209ed63a294d999b4c86675982"))

(deftest test-blake2s-digest-length
  "BLAKE2s default produces 32-byte digest"
  (assert-= (length (blake2:blake2s (string-to-bytes ""))) 32))

(deftest test-blake2s-incremental
  "BLAKE2s incremental matches one-shot"
  (let* ((data (string-to-bytes "hello world"))
         (one-shot (blake2:blake2s data))
         (state (blake2:make-blake2s-state)))
    (blake2:blake2s-update state (string-to-bytes "hello "))
    (blake2:blake2s-update state (string-to-bytes "world"))
    (assert-true (equalp one-shot (blake2:blake2s-finalize state)))))

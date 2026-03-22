;;;; Tests for MD5 (RFC 1321)

(defpackage epsilon.ssl.md5-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:md5 #:epsilon.ssl.md5))
  (:enter t))

(in-package :epsilon.ssl.md5-tests)

;;; RFC 1321 test vectors

(deftest test-md5-empty
  "MD5 of empty string"
  (assert-equal (md5:md5-hex (string-to-bytes ""))
                "d41d8cd98f00b204e9800998ecf8427e"))

(deftest test-md5-a
  "MD5 of 'a'"
  (assert-equal (md5:md5-hex (string-to-bytes "a"))
                "0cc175b9c0f1b6a831c399e269772661"))

(deftest test-md5-abc
  "MD5 of 'abc'"
  (assert-equal (md5:md5-hex (string-to-bytes "abc"))
                "900150983cd24fb0d6963f7d28e17f72"))

(deftest test-md5-message-digest
  "MD5 of 'message digest'"
  (assert-equal (md5:md5-hex (string-to-bytes "message digest"))
                "f96b697d7cb7938d525a2f31aaf161d0"))

(deftest test-md5-alphabet
  "MD5 of full alphabet"
  (assert-equal (md5:md5-hex (string-to-bytes "abcdefghijklmnopqrstuvwxyz"))
                "c3fcd3d76192e4007dfb496cca67e13b"))

(deftest test-md5-digest-length
  "MD5 produces 16-byte digest"
  (assert-= (length (md5:md5 (string-to-bytes ""))) 16))

(deftest test-md5-incremental
  "MD5 incremental matches one-shot"
  (let* ((data (string-to-bytes "hello world"))
         (one-shot (md5:md5 data))
         (state (md5:make-md5-state)))
    (md5:md5-update state (string-to-bytes "hello "))
    (md5:md5-update state (string-to-bytes "world"))
    (assert-true (equalp one-shot (md5:md5-finalize state)))))

(deftest test-md5-copy
  "MD5 copy produces independent state"
  (let ((state (md5:make-md5-state)))
    (md5:md5-update state (string-to-bytes "hello"))
    (let ((copy (md5:md5-copy state)))
      (md5:md5-update state (string-to-bytes " world"))
      (md5:md5-update copy (string-to-bytes " there"))
      (assert-not (equalp (md5:md5-finalize state)
                          (md5:md5-finalize copy))))))

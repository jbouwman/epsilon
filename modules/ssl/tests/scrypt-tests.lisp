;;;; Tests for scrypt (RFC 7914)

(defpackage epsilon.ssl.scrypt-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:scrypt #:epsilon.ssl.scrypt))
  (:enter t))

(in-package :epsilon.ssl.scrypt-tests)

;;; RFC 7914 Section 12 - Test Vector 1
;;; scrypt("", "", N=16, r=1, p=1, dkLen=64)

(deftest test-scrypt-rfc7914-vector1
  "RFC 7914 Section 12 Test Vector 1"
  (let* ((password (string-to-bytes ""))
         (salt (string-to-bytes ""))
         (dk (scrypt:scrypt password salt 16 1 1 64))
         (expected (hex-to-bytes "77d6576238657b203b19ca42c18a0497f16b4844e3074ae8dfdffa3fede21442fcd0069ded0948f8326a753a0fc81f17e8d3e0fb2e0d3628cf35e20c38d18906")))
    (assert-true (equalp dk expected))))

;;; RFC 7914 Section 12 - Test Vector 2
;;; scrypt("password", "NaCl", N=1024, r=8, p=16, dkLen=64)

(deftest test-scrypt-rfc7914-vector2
  "RFC 7914 Section 12 Test Vector 2"
  (let* ((password (string-to-bytes "password"))
         (salt (string-to-bytes "NaCl"))
         (dk (scrypt:scrypt password salt 1024 8 16 64))
         (expected (hex-to-bytes "fdbabe1c9d3472007856e7190d01e9fe7c6ad7cbc8237830e77376634b3731622eaf30d92e22a3886ff109279d9830dac727afb94a83ee6d8360cbdfa2cc0640")))
    (assert-true (equalp dk expected))))

;;; Basic functionality test with small parameters
(deftest test-scrypt-basic
  "scrypt produces correct-length output"
  (let* ((password (string-to-bytes "test"))
         (salt (string-to-bytes "salt"))
         (dk (scrypt:scrypt password salt 4 1 1 32)))
    (assert-= (length dk) 32)))

(deftest test-scrypt-deterministic
  "scrypt is deterministic"
  (let* ((password (string-to-bytes "test"))
         (salt (string-to-bytes "salt"))
         (dk1 (scrypt:scrypt password salt 4 1 1 32))
         (dk2 (scrypt:scrypt password salt 4 1 1 32)))
    (assert-true (equalp dk1 dk2))))

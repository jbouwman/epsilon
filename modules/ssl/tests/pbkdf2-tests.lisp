;;;; Tests for PBKDF2 (RFC 6070)

(defpackage epsilon.ssl.pbkdf2-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:pbkdf2 #:epsilon.ssl.pbkdf2))
  (:enter t))

(in-package :epsilon.ssl.pbkdf2-tests)

;;; RFC 6070 Test Vectors (PBKDF2-HMAC-SHA256)
;;; Note: RFC 6070 specifies SHA-1 but we test SHA-256 with known vectors

(deftest test-pbkdf2-sha256-vector1
  "PBKDF2-SHA256: 'password'/'salt', 1 iteration, 32 bytes"
  (let* ((password (string-to-bytes "password"))
         (salt (string-to-bytes "salt"))
         (dk (pbkdf2:pbkdf2 :sha256 password salt 1 32))
         (expected (hex-to-bytes "120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b")))
    (assert-true (equalp dk expected))))

(deftest test-pbkdf2-sha256-vector2
  "PBKDF2-SHA256: 'password'/'salt', 2 iterations, 32 bytes"
  (let* ((password (string-to-bytes "password"))
         (salt (string-to-bytes "salt"))
         (dk (pbkdf2:pbkdf2 :sha256 password salt 2 32))
         (expected (hex-to-bytes "ae4d0c95af6b46d32d0adff928f06dd02a303f8ef3c251dfd6e2d85a95474c43")))
    (assert-true (equalp dk expected))))

(deftest test-pbkdf2-sha256-vector3
  "PBKDF2-SHA256: 'password'/'salt', 4096 iterations, 32 bytes"
  (let* ((password (string-to-bytes "password"))
         (salt (string-to-bytes "salt"))
         (dk (pbkdf2:pbkdf2 :sha256 password salt 4096 32))
         (expected (hex-to-bytes "c5e478d59288c841aa530db6845c4c8d962893a001ce4e11a4963873aa98134a")))
    (assert-true (equalp dk expected))))

(deftest test-pbkdf2-sha256-long-password
  "PBKDF2-SHA256: long password"
  (let* ((password (string-to-bytes "passwordPASSWORDpassword"))
         (salt (string-to-bytes "saltSALTsaltSALTsaltSALTsaltSALTsalt"))
         (dk (pbkdf2:pbkdf2 :sha256 password salt 4096 40))
         (expected (hex-to-bytes "348c89dbcbd32b2f32d814b8116e84cf2b17347ebc1800181c4e2a1fb8dd53e1c635518c7dac47e9")))
    (assert-true (equalp dk expected))))

(deftest test-pbkdf2-output-length
  "PBKDF2 produces requested output length"
  (let* ((password (string-to-bytes "password"))
         (salt (string-to-bytes "salt")))
    (assert-= (length (pbkdf2:pbkdf2 :sha256 password salt 1 16)) 16)
    (assert-= (length (pbkdf2:pbkdf2 :sha256 password salt 1 64)) 64)))

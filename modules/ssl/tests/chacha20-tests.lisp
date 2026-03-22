;;;; Tests for ChaCha20 (RFC 8439)

(defpackage epsilon.ssl.chacha20-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:chacha #:epsilon.ssl.chacha20))
  (:enter t))

(in-package :epsilon.ssl.chacha20-tests)

;;; RFC 8439 Section 2.3.2 - ChaCha20 Block Function Test Vector
(deftest test-chacha20-block-rfc8439
  "RFC 8439 Section 2.3.2: ChaCha20 block function"
  (let* ((key (hex-to-bytes "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))
         (nonce (hex-to-bytes "000000090000004a00000000"))
         (counter 1)
         (block (chacha:chacha20-block key counter nonce))
         (expected (hex-to-bytes "10f1e7e4d13b5915500fdd1fa32071c4c7d1f4c733c068030422aa9ac3d46c4ed2826446079faa0914c2d705d98b02a2b5129cd1de164eb9cbd083e8a2503c4e")))
    (assert-true (equalp block expected))))

;;; RFC 8439 Section 2.4.2 - ChaCha20 Encryption Test Vector
(deftest test-chacha20-encrypt-rfc8439
  "RFC 8439 Section 2.4.2: ChaCha20 encryption"
  (let* ((key (hex-to-bytes "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))
         (nonce (hex-to-bytes "000000000000004a00000000"))
         (plaintext (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                         "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it."))
         (expected (hex-to-bytes "6e2e359a2568f98041ba0728dd0d6981e97e7aec1d4360c20a27afccfd9fae0bf91b65c5524733ab8f593dabcd62b3571639d624e65152ab8f530c359f0861d807ca0dbf500d6a6156a38e088a22b65e52bc514d16ccf806818ce91ab77937365af90bbf74a35be6b40b8eedf2785e42874d")))
    (let ((ciphertext (chacha:chacha20-encrypt plaintext key nonce :initial-counter 1)))
      (assert-true (equalp ciphertext expected)))))

;;; Round-trip
(deftest test-chacha20-roundtrip
  "ChaCha20: decrypt(encrypt(pt)) = pt"
  (let* ((key (hex-to-bytes "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))
         (nonce (hex-to-bytes "000000000000004a00000000"))
         (plaintext (make-array 200 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 200 do (setf (aref plaintext i) (mod (* i 7) 256)))
    (let* ((ct (chacha:chacha20-encrypt plaintext key nonce))
           (pt (chacha:chacha20-encrypt ct key nonce)))
      (assert-true (equalp pt plaintext)))))

;;; Empty plaintext
(deftest test-chacha20-empty
  "ChaCha20: empty plaintext"
  (let* ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
         (nonce (make-array 12 :element-type '(unsigned-byte 8) :initial-element 0))
         (plaintext (make-array 0 :element-type '(unsigned-byte 8)))
         (ct (chacha:chacha20-encrypt plaintext key nonce)))
    (assert-= (length ct) 0)))

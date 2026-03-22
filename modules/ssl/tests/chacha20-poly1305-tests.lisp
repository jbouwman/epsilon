;;;; Tests for ChaCha20-Poly1305 AEAD (RFC 8439)

(defpackage epsilon.ssl.chacha20-poly1305-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:aead #:epsilon.ssl.chacha20-poly1305))
  (:enter t))

(in-package :epsilon.ssl.chacha20-poly1305-tests)

;;; RFC 8439 Section 2.8.2 - AEAD Construction Test Vector
(deftest test-chacha20-poly1305-rfc8439
  "RFC 8439 Section 2.8.2: ChaCha20-Poly1305 AEAD"
  (let* ((key (hex-to-bytes "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"))
         (nonce (hex-to-bytes "070000004041424344454647"))
         (aad (hex-to-bytes "50515253c0c1c2c3c4c5c6c7"))
         (plaintext (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                         "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it."))
         (expected-ct "d31a8d34648e60db7b86afbc53ef7ec2a4aded51296e08fea9e2b5a736ee62d63dbea45e8ca9671282fafb69da92728b1a71de0a9e060b2905d6a5b67ecd3b3692ddbd7f2d778b8c9803aee328091b58fab324e4fad675945585808b4831d7bc3ff4def08e4b7a9de576d26586cec64b6116")
         (expected-tag "1ae10b594f09e26a7e902ecbd0600691"))
    (multiple-value-bind (ct tag)
        (aead:chacha20-poly1305-encrypt plaintext key nonce :aad aad)
      (assert-equal (bytes-to-hex ct) expected-ct)
      (assert-equal (bytes-to-hex tag) expected-tag))))

;;; Round-trip
(deftest test-chacha20-poly1305-roundtrip
  "ChaCha20-Poly1305 round-trip"
  (let* ((key (hex-to-bytes "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"))
         (nonce (hex-to-bytes "070000004041424344454647"))
         (aad (hex-to-bytes "50515253c0c1c2c3c4c5c6c7"))
         (plaintext (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                         "Hello, World!")))
    (multiple-value-bind (ct tag)
        (aead:chacha20-poly1305-encrypt plaintext key nonce :aad aad)
      (let ((recovered (aead:chacha20-poly1305-decrypt ct key nonce tag :aad aad)))
        (assert-true (equalp recovered plaintext))))))

;;; Bad tag
(deftest test-chacha20-poly1305-bad-tag
  "ChaCha20-Poly1305: bad tag signals error"
  (let* ((key (hex-to-bytes "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"))
         (nonce (hex-to-bytes "070000004041424344454647"))
         (plaintext (map '(simple-array (unsigned-byte 8) (*)) #'char-code "test")))
    (multiple-value-bind (ct tag)
        (aead:chacha20-poly1305-encrypt plaintext key nonce)
      (declare (ignore tag))
      (let ((bad-tag (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
        (assert-condition (error)
          (aead:chacha20-poly1305-decrypt ct key nonce bad-tag))))))

;;; Empty plaintext with AAD
(deftest test-chacha20-poly1305-empty-plaintext
  "ChaCha20-Poly1305: empty plaintext with AAD"
  (let* ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1))
         (nonce (make-array 12 :element-type '(unsigned-byte 8) :initial-element 2))
         (aad (map '(simple-array (unsigned-byte 8) (*)) #'char-code "some metadata"))
         (plaintext (make-array 0 :element-type '(unsigned-byte 8))))
    (multiple-value-bind (ct tag)
        (aead:chacha20-poly1305-encrypt plaintext key nonce :aad aad)
      (assert-= (length ct) 0)
      (assert-= (length tag) 16)
      (let ((recovered (aead:chacha20-poly1305-decrypt ct key nonce tag :aad aad)))
        (assert-= (length recovered) 0)))))

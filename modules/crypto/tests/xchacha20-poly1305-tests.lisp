;;;; Tests for XChaCha20-Poly1305 AEAD (draft-irtf-cfrg-xchacha-03)

(defpackage epsilon.crypto.xchacha20-poly1305-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import
   (epsilon.crypto.chacha20 chacha)
   (epsilon.crypto.xchacha20-poly1305 aead)))

(in-package :epsilon.crypto.xchacha20-poly1305-tests)

;;; ---------------------------------------------------------------------------
;;; HChaCha20 (draft-irtf-cfrg-xchacha-03 Section 2.2.1)
;;; ---------------------------------------------------------------------------

(deftest test-hchacha20-draft-vector
  "draft-irtf-cfrg-xchacha-03 Section 2.2.1: HChaCha20 test vector"
  (let* ((key (hex-to-bytes
               "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))
         (nonce16 (hex-to-bytes "000000090000004a0000000031415927"))
         (subkey (chacha:hchacha20 key nonce16))
         (expected (hex-to-bytes
                    "82413b4227b27bfed30e42508a877d73a0f9e4d58a74a853c12ec41326d3ecdc")))
    (assert-true (equalp subkey expected))))

;;; ---------------------------------------------------------------------------
;;; XChaCha20-Poly1305 AEAD test vector
;;; (draft-irtf-cfrg-xchacha-03 Appendix A.3.1)
;;; ---------------------------------------------------------------------------

(deftest test-xchacha20-poly1305-draft-vector
  "draft-irtf-cfrg-xchacha-03 A.3.1: XChaCha20-Poly1305 known-answer"
  (let* ((key (hex-to-bytes
               "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"))
         (nonce (hex-to-bytes
                 "404142434445464748494a4b4c4d4e4f5051525354555657"))
         (aad (hex-to-bytes "50515253c0c1c2c3c4c5c6c7"))
         (plaintext (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                         "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it."))
         (expected-ct
          (concatenate 'string
                       "bd6d179d3e83d43b9576579493c0e939"
                       "572a1700252bfaccbed2902c21396cbb"
                       "731c7f1b0b4aa6440bf3a82f4eda7e39"
                       "ae64c6708c54c216cb96b72e1213b452"
                       "2f8c9ba40db5d945b11b69b982c1bb9e"
                       "3f3fac2bc369488f76b2383565d3fff9"
                       "21f9664c97637da9768812f615c68b13"
                       "b52e"))
         (expected-tag "c0875924c1c7987947deafd8780acf49"))
    (multiple-value-bind (ct tag)
        (aead:xchacha20-poly1305-encrypt plaintext key nonce :aad aad)
      (assert-equal (bytes-to-hex ct) expected-ct)
      (assert-equal (bytes-to-hex tag) expected-tag))))

;;; ---------------------------------------------------------------------------
;;; Round-trip
;;; ---------------------------------------------------------------------------

(deftest test-xchacha20-poly1305-roundtrip
  "XChaCha20-Poly1305 encrypt/decrypt round-trip recovers plaintext"
  (let* ((key (hex-to-bytes
               "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"))
         (nonce (hex-to-bytes
                 "404142434445464748494a4b4c4d4e4f5051525354555657"))
         (aad (hex-to-bytes "50515253c0c1c2c3c4c5c6c7"))
         (plaintext (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                         "Hello, World!")))
    (multiple-value-bind (ct tag)
        (aead:xchacha20-poly1305-encrypt plaintext key nonce :aad aad)
      (let ((recovered (aead:xchacha20-poly1305-decrypt ct key nonce tag :aad aad)))
        (assert-true (equalp recovered plaintext))))))

(deftest test-xchacha20-poly1305-empty-aad
  "XChaCha20-Poly1305 round-trip with no AAD"
  (let* ((key (hex-to-bytes
               "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"))
         (nonce (hex-to-bytes
                 "404142434445464748494a4b4c4d4e4f5051525354555657"))
         (plaintext (map '(simple-array (unsigned-byte 8) (*)) #'char-code "abc")))
    (multiple-value-bind (ct tag)
        (aead:xchacha20-poly1305-encrypt plaintext key nonce)
      (let ((recovered (aead:xchacha20-poly1305-decrypt ct key nonce tag)))
        (assert-true (equalp recovered plaintext))))))

;;; ---------------------------------------------------------------------------
;;; Authentication failures
;;; ---------------------------------------------------------------------------

(deftest test-xchacha20-poly1305-bad-tag
  "XChaCha20-Poly1305: tampered tag is rejected"
  (let* ((key (hex-to-bytes
               "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"))
         (nonce (hex-to-bytes
                 "404142434445464748494a4b4c4d4e4f5051525354555657"))
         (plaintext (map '(simple-array (unsigned-byte 8) (*)) #'char-code "test")))
    (multiple-value-bind (ct tag)
        (aead:xchacha20-poly1305-encrypt plaintext key nonce)
      (declare (ignore tag))
      (let ((bad-tag (make-array 16 :element-type '(unsigned-byte 8)
                                 :initial-element 0)))
        (assert-condition (error)
          (aead:xchacha20-poly1305-decrypt ct key nonce bad-tag))))))

(deftest test-xchacha20-poly1305-aad-mismatch
  "XChaCha20-Poly1305: AAD mismatch is rejected"
  (let* ((key (hex-to-bytes
               "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"))
         (nonce (hex-to-bytes
                 "404142434445464748494a4b4c4d4e4f5051525354555657"))
         (plaintext (map '(simple-array (unsigned-byte 8) (*)) #'char-code "msg"))
         (aad-encrypt (hex-to-bytes "01020304"))
         (aad-decrypt (hex-to-bytes "0102ff04")))
    (multiple-value-bind (ct tag)
        (aead:xchacha20-poly1305-encrypt plaintext key nonce :aad aad-encrypt)
      (assert-condition (error)
        (aead:xchacha20-poly1305-decrypt ct key nonce tag :aad aad-decrypt)))))

;;; ---------------------------------------------------------------------------
;;; Nonce extension property: differing 24-byte nonces produce differing
;;; ciphertexts even when the first 16 bytes are identical, because
;;; HChaCha20 only consumes nonce[0..15] -- so the ChaCha core's 8-byte
;;; nonce comes from nonce[16..23] and propagates into the keystream.
;;; ---------------------------------------------------------------------------

(deftest test-xchacha20-poly1305-nonce-tail-affects-keystream
  "Differing tail bytes of a 24-byte nonce yield different ciphertexts"
  (let* ((key (hex-to-bytes
               "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"))
         (nonce-a (hex-to-bytes
                   "404142434445464748494a4b4c4d4e4f5051525354555657"))
         (nonce-b (hex-to-bytes
                   ;; Same first 16 bytes; tail differs.
                   "404142434445464748494a4b4c4d4e4f0000000000000000"))
         (plaintext (map '(simple-array (unsigned-byte 8) (*)) #'char-code "abc")))
    (multiple-value-bind (ct-a tag-a)
        (aead:xchacha20-poly1305-encrypt plaintext key nonce-a)
      (multiple-value-bind (ct-b tag-b)
          (aead:xchacha20-poly1305-encrypt plaintext key nonce-b)
        (assert-not (equalp ct-a ct-b))
        (assert-not (equalp tag-a tag-b))))))

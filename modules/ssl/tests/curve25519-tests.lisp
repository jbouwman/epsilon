;;;; Tests for Curve25519 (X25519) key exchange

(defpackage epsilon.ssl.curve25519-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:x25519 #:epsilon.ssl.curve25519))
  (:enter t))

(in-package :epsilon.ssl.curve25519-tests)

;;; RFC 7748 Section 6.1 Test Vector 1
(deftest test-x25519-rfc7748-vector1
  "RFC 7748 Section 6.1 - Test vector 1"
  (let* ((k (hex-to-bytes "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"))
         (u (hex-to-bytes "e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c"))
         (expected "c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552")
         (result (x25519:x25519 k u)))
    (assert-equal (bytes-to-hex result) expected)))

;;; RFC 7748 Section 6.1 Test Vector 2
(deftest test-x25519-rfc7748-vector2
  "RFC 7748 Section 6.1 - Test vector 2"
  (let* ((k (hex-to-bytes "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"))
         (u (hex-to-bytes "e5210f12786811d3f4b7959d0538ae2c31dbe7106fc03c3efc4cd549c715a493"))
         (expected "95cbde9476e8907d7aade45cb4b873f88b595a68799fa152e6f8f7647aac7957")
         (result (x25519:x25519 k u)))
    (assert-equal (bytes-to-hex result) expected)))

;;; RFC 7748 Section 6.1 Iterated test (1 iteration)
(deftest test-x25519-rfc7748-iter1
  "RFC 7748 Section 6.1 - After 1 iteration"
  (let* ((k (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
         (u (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref k 0) 9)
    (setf (aref u 0) 9)
    (let ((result (x25519:x25519 k u)))
      (assert-equal (bytes-to-hex result)
                    "422c8e7a6227d7bca1350b3e2bb7279f7897b87bb6854b783c60e80311ae3079"))))

;;; Base point multiplication
(deftest test-x25519-base
  "X25519 base point multiplication"
  (let* ((k (hex-to-bytes "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"))
         (result (x25519:x25519-base k)))
    ;; Just verify it produces a 32-byte result and doesn't crash
    (assert-= (length result) 32)))

;;; Key exchange round-trip
(deftest test-x25519-key-exchange
  "X25519 Diffie-Hellman key exchange"
  (let* (;; Alice's private key
         (alice-sk (hex-to-bytes "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"))
         ;; Bob's private key
         (bob-sk (hex-to-bytes "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"))
         ;; Alice's public key = X25519(alice_sk, 9)
         (alice-pk (x25519:x25519-base alice-sk))
         ;; Bob's public key = X25519(bob_sk, 9)
         (bob-pk (x25519:x25519-base bob-sk))
         ;; Shared secrets
         (alice-shared (x25519:x25519 alice-sk bob-pk))
         (bob-shared (x25519:x25519 bob-sk alice-pk)))
    ;; Both should derive the same shared secret
    (assert-equal (bytes-to-hex alice-shared) (bytes-to-hex bob-shared))
    ;; RFC 7748 Section 6.1: known shared secret
    (assert-equal (bytes-to-hex alice-shared)
                  "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742")))

;;; Low-order point check: all-zero u-coordinate
(deftest test-x25519-zero-u
  "X25519 with u=0 should produce all zeros"
  (let* ((k (hex-to-bytes "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"))
         (u (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
         (result (x25519:x25519 k u)))
    ;; u=0 is a low-order point, result should be all zeros
    (assert-true (every #'zerop result))))

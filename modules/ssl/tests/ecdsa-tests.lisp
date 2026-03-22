;;;; Tests for ECDSA over P-256

(defpackage epsilon.ssl.ecdsa-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:ecdsa #:epsilon.ssl.ecdsa)
   (#:ec #:epsilon.ssl.ec-p256))
  (:enter t))

(in-package :epsilon.ssl.ecdsa-tests)

;;; Sign/verify round-trip
(deftest test-ecdsa-sign-verify-roundtrip
  "ECDSA sign/verify round-trip"
  ;; Use a known private key
  (let* ((d #xc9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721)
         (pub (ecdsa:ecdsa-public-key-from-private d))
         (message (make-array 6 :element-type '(unsigned-byte 8)
                              :initial-contents '(115 97 109 112 108 101))))  ; "sample"
    (multiple-value-bind (r s) (ecdsa:ecdsa-sign d message)
      (assert-true (plusp r))
      (assert-true (plusp s))
      (assert-true (ecdsa:ecdsa-verify pub message r s)))))

;;; Deterministic signatures (RFC 6979)
(deftest test-ecdsa-deterministic
  "ECDSA signatures are deterministic (RFC 6979)"
  (let* ((d #xc9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721)
         (message (make-array 6 :element-type '(unsigned-byte 8)
                              :initial-contents '(115 97 109 112 108 101))))
    (multiple-value-bind (r1 s1) (ecdsa:ecdsa-sign d message)
      (multiple-value-bind (r2 s2) (ecdsa:ecdsa-sign d message)
        (assert-= r1 r2)
        (assert-= s1 s2)))))

;;; Wrong message
(deftest test-ecdsa-verify-wrong-message
  "ECDSA verification fails with wrong message"
  (let* ((d #xc9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721)
         (pub (ecdsa:ecdsa-public-key-from-private d))
         (message (make-array 5 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4 5)))
         (wrong-msg (make-array 5 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4 6))))
    (multiple-value-bind (r s) (ecdsa:ecdsa-sign d message)
      (assert-not (ecdsa:ecdsa-verify pub wrong-msg r s)))))

;;; Wrong key
(deftest test-ecdsa-verify-wrong-key
  "ECDSA verification fails with wrong public key"
  (let* ((d1 #xc9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721)
         (d2 #x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef)
         (pub2 (ecdsa:ecdsa-public-key-from-private d2))
         (message (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3))))
    (multiple-value-bind (r s) (ecdsa:ecdsa-sign d1 message)
      (assert-not (ecdsa:ecdsa-verify pub2 message r s)))))

;;; Invalid r/s values
(deftest test-ecdsa-verify-invalid-rs
  "ECDSA verification rejects invalid r/s"
  (let* ((d #xc9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721)
         (pub (ecdsa:ecdsa-public-key-from-private d)))
    ;; r = 0
    (assert-not (ecdsa:ecdsa-verify pub (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0) 0 1))
    ;; s = 0
    (assert-not (ecdsa:ecdsa-verify pub (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0) 1 0))
    ;; r >= n
    (assert-not (ecdsa:ecdsa-verify pub (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0) ec:+n+ 1))))

;;; Public key derivation produces on-curve point
(deftest test-ecdsa-pubkey-on-curve
  "Public key from private key is on curve"
  (let* ((d #xc9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721)
         (pub (ecdsa:ecdsa-public-key-from-private d)))
    (assert-true (ec:p256-on-curve-p pub))))

;;;; Tests for X25519+ML-KEM-768 hybrid KEM
;;;;
;;;; References:
;;;;   draft-kwiatkowski-tls-ecdhe-mlkem-02
;;;;   draft-ietf-tls-ecdhe-mlkem-00

(defpackage epsilon.crypto.ml-kem-hybrid-tests
  (:use :cl :epsilon.test :epsilon.crypto.test-support)
  (:import (epsilon.crypto.ml-kem-hybrid hybrid)
            (epsilon.crypto.ml-kem ml-kem)
            (epsilon.crypto.curve25519 x25519)))

(in-package :epsilon.crypto.ml-kem-hybrid-tests)

(defun deterministic-bytes (length seed)
  (let ((out (make-array length :element-type '(unsigned-byte 8)))
        (state (+ seed 1)))
    (loop for i from 0 below length do
      (setf state (mod (+ (* state 1103515245) 12345) (expt 2 32)))
      (setf (aref out i) (ldb (byte 8 16) state)))
    out))

;;; ---- Layout constants ----

(deftest test-hybrid-layout-constants
  "Verify the hybrid layout constants match the draft:
     public key  = 1184 + 32 = 1216 bytes
     private key = 2400 + 32 = 2432 bytes
     ciphertext  = 1088 + 32 = 1120 bytes
     shared secret = 32 + 32 = 64 bytes
     IANA codepoint = 0x11EC"
  (assert-= hybrid:+public-key-length+ 1216)
  (assert-= hybrid:+private-key-length+ 2432)
  (assert-= hybrid:+ciphertext-length+ 1120)
  (assert-= hybrid:+shared-secret-length+ 64)
  (assert-= hybrid:+tls-codepoint+ #x11EC))

;;; ---- Output shape ----

(deftest test-hybrid-keygen-internal-output-shape
  (let ((d (deterministic-bytes 32 1))
        (z (deterministic-bytes 32 2))
        (x (deterministic-bytes 32 3)))
    (multiple-value-bind (pk sk) (hybrid:hybrid-keygen-internal d z x)
      (assert-= (length pk) hybrid:+public-key-length+)
      (assert-= (length sk) hybrid:+private-key-length+))))

(deftest test-hybrid-encaps-internal-output-shape
  (let ((d (deterministic-bytes 32 10))
        (z (deterministic-bytes 32 11))
        (x (deterministic-bytes 32 12))
        (m (deterministic-bytes 32 13))
        (eph (deterministic-bytes 32 14)))
    (multiple-value-bind (pk sk) (hybrid:hybrid-keygen-internal d z x)
      (declare (ignore sk))
      (multiple-value-bind (ss ct) (hybrid:hybrid-encaps-internal pk m eph)
        (assert-= (length ss) hybrid:+shared-secret-length+)
        (assert-= (length ct) hybrid:+ciphertext-length+)))))

;;; ---- Wire layout: ML-KEM first, then X25519 ----

(deftest test-hybrid-public-key-layout
  "Public key layout per draft-kwiatkowski-tls-ecdhe-mlkem: the ML-KEM
   public key occupies the first 1184 bytes, and the remaining 32
   bytes are the X25519 public key. This test pins the layout
   directly by constructing the components from known seeds and
   comparing byte ranges."
  (let* ((d (deterministic-bytes 32 20))
         (z (deterministic-bytes 32 21))
         (x-priv (deterministic-bytes 32 22))
         (x-pub (x25519:x25519-base x-priv)))
    (multiple-value-bind (pk sk) (hybrid:hybrid-keygen-internal d z x-priv)
      (declare (ignore sk))
      ;; ML-KEM ek is the first 1184 bytes; it must equal what
      ;; ml-kem:keygen-internal returns independently.
      (multiple-value-bind (expected-ek expected-dk)
          (ml-kem:keygen-internal d z)
        (declare (ignore expected-dk))
        (assert-true (equalp (subseq pk 0 1184) expected-ek)))
      ;; The last 32 bytes are the X25519 public key.
      (assert-true (equalp (subseq pk 1184 1216) x-pub)))))

(deftest test-hybrid-ciphertext-layout
  "Ciphertext layout: ML-KEM ct (1088) || X25519 ephemeral pub (32)."
  (let* ((d (deterministic-bytes 32 30))
         (z (deterministic-bytes 32 31))
         (x-priv (deterministic-bytes 32 32))
         (m (deterministic-bytes 32 33))
         (eph-priv (deterministic-bytes 32 34))
         (eph-pub (x25519:x25519-base eph-priv)))
    (multiple-value-bind (pk sk) (hybrid:hybrid-keygen-internal d z x-priv)
      (declare (ignore sk))
      (multiple-value-bind (ss ct)
          (hybrid:hybrid-encaps-internal pk m eph-priv)
        (declare (ignore ss))
        ;; The first 1088 bytes are the ML-KEM ciphertext; the last 32
        ;; are the encapsulator's ephemeral X25519 public key.
        (assert-true (equalp (subseq ct 1088 1120) eph-pub))))))

(deftest test-hybrid-shared-secret-layout
  "Shared secret layout: ML-KEM ss (32) || X25519 ss (32)."
  (let* ((d (deterministic-bytes 32 40))
         (z (deterministic-bytes 32 41))
         (x-priv (deterministic-bytes 32 42))
         (m (deterministic-bytes 32 43))
         (eph-priv (deterministic-bytes 32 44)))
    (multiple-value-bind (pk sk) (hybrid:hybrid-keygen-internal d z x-priv)
      (declare (ignore sk))
      (let* ((mlkem-ek (subseq pk 0 1184))
             ;; What the raw ML-KEM encaps layer would return for the
             ;; same m -- the hybrid ss's first 32 bytes must match.
             (x-peer-pub (subseq pk 1184 1216)))
        (multiple-value-bind (expected-mlkem-ss expected-mlkem-ct)
            (ml-kem:encaps-internal mlkem-ek m)
          (declare (ignore expected-mlkem-ct))
          (multiple-value-bind (ss ct)
              (hybrid:hybrid-encaps-internal pk m eph-priv)
            (declare (ignore ct))
            (assert-true (equalp (subseq ss 0 32) expected-mlkem-ss))
            ;; The X25519 half of the shared secret is X25519(eph-priv, peer-pub).
            (assert-true (equalp (subseq ss 32 64)
                                 (x25519:x25519 eph-priv x-peer-pub)))))))))

;;; ---- The load-bearing round trip ----

(deftest test-hybrid-roundtrip
  "KeyGen, Encaps, Decaps: the decapsulator recovers the exact
   64-byte shared secret that encaps emitted."
  (let ((d (deterministic-bytes 32 50))
        (z (deterministic-bytes 32 51))
        (x (deterministic-bytes 32 52))
        (m (deterministic-bytes 32 53))
        (eph (deterministic-bytes 32 54)))
    (multiple-value-bind (pk sk) (hybrid:hybrid-keygen-internal d z x)
      (multiple-value-bind (ss ct) (hybrid:hybrid-encaps-internal pk m eph)
        (assert-true (equalp (hybrid:hybrid-decaps sk ct) ss))))))

(deftest test-hybrid-roundtrip-several-trials
  (dotimes (trial 5)
    (let ((d (deterministic-bytes 32 (+ 100 trial)))
          (z (deterministic-bytes 32 (+ 200 trial)))
          (x (deterministic-bytes 32 (+ 300 trial)))
          (m (deterministic-bytes 32 (+ 400 trial)))
          (eph (deterministic-bytes 32 (+ 500 trial))))
      (multiple-value-bind (pk sk) (hybrid:hybrid-keygen-internal d z x)
        (multiple-value-bind (ss ct) (hybrid:hybrid-encaps-internal pk m eph)
          (assert-true (equalp (hybrid:hybrid-decaps sk ct) ss)))))))

(deftest test-hybrid-random-keygen-roundtrip
  "The random `hybrid-keygen` and `hybrid-encaps` entry points round
   trip. Exercises the kernel entropy wiring at both steps."
  (multiple-value-bind (pk sk) (hybrid:hybrid-keygen)
    (multiple-value-bind (ss ct) (hybrid:hybrid-encaps pk)
      (assert-true (equalp (hybrid:hybrid-decaps sk ct) ss)))))

;;; ---- Tamper detection on the ML-KEM component ----

(deftest test-hybrid-mlkem-tamper-changes-secret
  "If an attacker tampers with the ML-KEM half of the ciphertext, the
   ML-KEM half of the decapsulated shared secret changes (via the
   implicit-rejection path); the X25519 half is unaffected because
   the ephemeral X25519 public key is untouched."
  (let ((d (deterministic-bytes 32 600))
        (z (deterministic-bytes 32 601))
        (x (deterministic-bytes 32 602))
        (m (deterministic-bytes 32 603))
        (eph (deterministic-bytes 32 604)))
    (multiple-value-bind (pk sk) (hybrid:hybrid-keygen-internal d z x)
      (multiple-value-bind (ss ct) (hybrid:hybrid-encaps-internal pk m eph)
        (let ((tampered (copy-seq ct)))
          ;; Flip a byte inside the ML-KEM ciphertext region (offset < 1088).
          (setf (aref tampered 500) (logxor (aref tampered 500) 1))
          (let ((recovered (hybrid:hybrid-decaps sk tampered)))
            (assert-= (length recovered) 64)
            ;; The ML-KEM halves differ (rejection branch).
            (assert-not (equalp (subseq recovered 0 32) (subseq ss 0 32)))
            ;; The X25519 halves still match (ephemeral pub is intact).
            (assert-true (equalp (subseq recovered 32 64)
                                 (subseq ss 32 64)))))))))

(deftest test-hybrid-x25519-tamper-changes-secret
  "If an attacker tampers with the X25519 ephemeral public key in the
   ciphertext, the X25519 half of the shared secret changes (because
   the ECDH input is now wrong). The ML-KEM half is unaffected."
  (let ((d (deterministic-bytes 32 700))
        (z (deterministic-bytes 32 701))
        (x (deterministic-bytes 32 702))
        (m (deterministic-bytes 32 703))
        (eph (deterministic-bytes 32 704)))
    (multiple-value-bind (pk sk) (hybrid:hybrid-keygen-internal d z x)
      (multiple-value-bind (ss ct) (hybrid:hybrid-encaps-internal pk m eph)
        (let ((tampered (copy-seq ct)))
          ;; Flip a byte in the X25519 ephemeral pub region (offset >= 1088).
          (setf (aref tampered 1100) (logxor (aref tampered 1100) 1))
          (let ((recovered (hybrid:hybrid-decaps sk tampered)))
            ;; The ML-KEM halves match (ML-KEM ciphertext is intact).
            (assert-true (equalp (subseq recovered 0 32) (subseq ss 0 32)))
            ;; The X25519 halves differ.
            (assert-not (equalp (subseq recovered 32 64) (subseq ss 32 64)))))))))

;;; ---- Input validation ----

(deftest test-hybrid-encaps-rejects-wrong-pk-length
  (let ((bad (make-array 100 :element-type '(unsigned-byte 8))))
    (assert-condition (hybrid:hybrid-error) (hybrid:hybrid-encaps bad))))

(deftest test-hybrid-decaps-rejects-wrong-sk-length
  (let ((bad (make-array 100 :element-type '(unsigned-byte 8)))
        (ct (make-array hybrid:+ciphertext-length+
                        :element-type '(unsigned-byte 8))))
    (assert-condition (hybrid:hybrid-error) (hybrid:hybrid-decaps bad ct))))

(deftest test-hybrid-decaps-rejects-wrong-ct-length
  (let* ((d (deterministic-bytes 32 800))
         (z (deterministic-bytes 32 801))
         (x (deterministic-bytes 32 802)))
    (multiple-value-bind (pk sk) (hybrid:hybrid-keygen-internal d z x)
      (declare (ignore pk))
      (let ((bad-ct (make-array 100 :element-type '(unsigned-byte 8))))
        (assert-condition (hybrid:hybrid-error)
                          (hybrid:hybrid-decaps sk bad-ct))))))

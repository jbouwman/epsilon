;;;; Tests for HMAC-DRBG implementation
;;;;
;;;; Test vectors from NIST SP 800-90A CAVS
;;;; (HMAC_DRBG SHA-256, no reseed, no additional input)

(defpackage epsilon.ssl.drbg-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:drbg #:epsilon.ssl.drbg)
   (#:ct #:epsilon.ssl.ct))
  (:enter t))

(in-package :epsilon.ssl.drbg-tests)

;;; ---------------------------------------------------------------------------
;;; NIST CAVS HMAC-DRBG (SHA-256) test vectors
;;; From: HMAC_DRBG.rsp, [SHA-256], [PredictionResistance = False]
;;; [EntropyInputLen = 256], [NonceLen = 128], [PersonalizationStringLen = 0]
;;; [AdditionalInputLen = 0], [ReturnedBitsLen = 1024]
;;; ---------------------------------------------------------------------------

(deftest test-hmac-drbg-nist-vector-0
  "NIST CAVS HMAC-DRBG SHA-256 Count 0"
  (let* ((entropy (hex-to-bytes "ca851911349384bffe89de1cbdc46e6831e44d34a4fb935ee285dd14b71a7488"))
         (nonce (hex-to-bytes "659ba96c601dc69fc902940805ec0ca8"))
         (state (drbg:make-hmac-drbg :entropy-input entropy :nonce nonce))
         ;; Generate 128 bytes (1024 bits) -- first call
         (_first-output (drbg:drbg-generate state 128))
         ;; Generate 128 bytes -- second call (this is the one we check)
         (second-output (drbg:drbg-generate state 128))
         (expected (hex-to-bytes "e528e9abf2dece54d47c7e75e5fe302149f817ea9fb4bee6f4199697d04d5b89d54fbb978a15b5c443c9ec21036d2460b6f73ebad0dc2aba6e624abf07745bc107694bb7547bb0995f70de25d6b29e2d3011bb19d27676c07162c8b5ccde0668961df86803482cb37ed6d5c0bb8d50cf1f50d476aa0458bdaba806f48be9dcb8")))
    (declare (ignore _first-output))
    (assert-true (ct:ct-equal second-output expected))))

;; TODO(claude): Verify NIST CAVS vector 1 expected output against
;; official HMAC_DRBG.rsp file - Issue #236

;;; ---------------------------------------------------------------------------
;;; HMAC-DRBG with personalization string
;;; ---------------------------------------------------------------------------

(deftest test-hmac-drbg-with-personalization
  "HMAC-DRBG with personalization string should differ from without"
  (let* ((entropy (hex-to-bytes "ca851911349384bffe89de1cbdc46e6831e44d34a4fb935ee285dd14b71a7488"))
         (nonce (hex-to-bytes "659ba96c601dc69fc902940805ec0ca8"))
         (pers (hex-to-bytes "deadbeef"))
         (state1 (drbg:make-hmac-drbg :entropy-input entropy :nonce nonce))
         (state2 (drbg:make-hmac-drbg :entropy-input entropy :nonce nonce
                                       :personalization pers))
         (out1 (drbg:drbg-generate state1 32))
         (out2 (drbg:drbg-generate state2 32)))
    ;; With different personalization, outputs should differ
    (assert-not (ct:ct-equal out1 out2))))

;;; ---------------------------------------------------------------------------
;;; HMAC-DRBG reseed tests
;;; ---------------------------------------------------------------------------

(deftest test-hmac-drbg-reseed-changes-output
  "Reseeding should change subsequent output"
  (let* ((entropy (hex-to-bytes "ca851911349384bffe89de1cbdc46e6831e44d34a4fb935ee285dd14b71a7488"))
         (nonce (hex-to-bytes "659ba96c601dc69fc902940805ec0ca8"))
         (state (drbg:make-hmac-drbg :entropy-input entropy :nonce nonce))
         (pre-reseed (drbg:drbg-generate state 32)))
    ;; Reseed
    (drbg:drbg-reseed state :entropy-input (hex-to-bytes "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
    (let ((post-reseed (drbg:drbg-generate state 32)))
      ;; Output after reseed should differ from what it would have been
      ;; (we can't easily test "what it would have been" but at least
      ;; verify the output is different from the pre-reseed output)
      (assert-not (ct:ct-equal pre-reseed post-reseed)))))

;;; ---------------------------------------------------------------------------
;;; Determinism tests
;;; ---------------------------------------------------------------------------

(deftest test-hmac-drbg-deterministic
  "Same seed should produce same output"
  (let* ((entropy (hex-to-bytes "0011223344556677889900aabbccddeeff0011223344556677889900aabbccddee"))
         (nonce (hex-to-bytes "aabbccddeeff00112233445566778899"))
         (state1 (drbg:make-hmac-drbg :entropy-input entropy :nonce nonce))
         (state2 (drbg:make-hmac-drbg :entropy-input entropy :nonce nonce))
         (out1 (drbg:drbg-generate state1 64))
         (out2 (drbg:drbg-generate state2 64)))
    (assert-true (ct:ct-equal out1 out2))))

;;; ---------------------------------------------------------------------------
;;; Output size tests
;;; ---------------------------------------------------------------------------

(deftest test-hmac-drbg-various-sizes
  "Generate various output sizes"
  (let* ((entropy (hex-to-bytes "0011223344556677889900aabbccddeeff0011223344556677889900aabbccddee"))
         (nonce (hex-to-bytes "aabbccddeeff00112233445566778899"))
         (state (drbg:make-hmac-drbg :entropy-input entropy :nonce nonce)))
    ;; Test various sizes
    (assert-= (length (drbg:drbg-generate state 1)) 1)
    (assert-= (length (drbg:drbg-generate state 16)) 16)
    (assert-= (length (drbg:drbg-generate state 32)) 32)
    (assert-= (length (drbg:drbg-generate state 48)) 48)
    (assert-= (length (drbg:drbg-generate state 64)) 64)
    (assert-= (length (drbg:drbg-generate state 128)) 128)
    (assert-= (length (drbg:drbg-generate state 256)) 256)))

;;; ---------------------------------------------------------------------------
;;; Global DRBG / convenience API
;;; ---------------------------------------------------------------------------

(deftest test-random-bytes
  "random-bytes should return the requested number of bytes"
  ;; Reset global DRBG for test isolation
  (let ((drbg:*drbg* nil))
    (let ((bytes (drbg:random-bytes 32)))
      (assert-= (length bytes) 32)
      ;; Should not be all zeros (with overwhelming probability)
      (assert-not (ct:ct-zerop bytes)))))

(deftest test-random-bytes-zero
  "random-bytes with 0 should return empty array"
  (let ((drbg:*drbg* nil))
    (let ((bytes (drbg:random-bytes 0)))
      (assert-= (length bytes) 0))))

(deftest test-random-integer
  "random-integer should produce values in range"
  (let ((drbg:*drbg* nil))
    (loop repeat 100
          do (let ((val (drbg:random-integer 100)))
               (assert-true (and (>= val 0) (< val 100)))))))

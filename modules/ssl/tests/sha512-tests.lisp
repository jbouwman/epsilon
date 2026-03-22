;;;; Tests for SHA-512 and SHA-384 implementations
;;;;
;;;; Test vectors from FIPS 180-4 and NIST CAVP

(defpackage epsilon.ssl.sha512-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:sha512 #:epsilon.ssl.sha512)
   (#:ct #:epsilon.ssl.ct))
  (:enter t))

(in-package :epsilon.ssl.sha512-tests)

;;; ---------------------------------------------------------------------------
;;; SHA-512 test vectors (FIPS 180-4)
;;; ---------------------------------------------------------------------------

(deftest test-sha512-empty
  "SHA-512 of empty input"
  (let* ((input (make-array 0 :element-type '(unsigned-byte 8)))
         (expected (hex-to-bytes "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"))
         (result (sha512:sha512 input)))
    (assert-true (ct:ct-equal result expected))))

(deftest test-sha512-abc
  "SHA-512 of 'abc' (FIPS 180-4 example)"
  (let* ((input (string-to-bytes "abc"))
         (expected (hex-to-bytes "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"))
         (result (sha512:sha512 input)))
    (assert-true (ct:ct-equal result expected))))

(deftest test-sha512-two-block
  "SHA-512 of two-block message (FIPS 180-4 example)"
  (let* ((input (string-to-bytes "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"))
         (expected (hex-to-bytes "8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909"))
         (result (sha512:sha512 input)))
    (assert-true (ct:ct-equal result expected))))

(deftest test-sha512-hex
  "SHA-512 hex output"
  (let ((result (sha512:sha512-hex (string-to-bytes "abc"))))
    (assert-equal result "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f")))

(deftest test-sha512-incremental
  "Incremental SHA-512 matches one-shot"
  (let* ((data (string-to-bytes "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"))
         (expected (sha512:sha512 data))
         (state (sha512:make-sha512-state)))
    ;; Feed in chunks
    (sha512:sha512-update state data :start 0 :end 50)
    (sha512:sha512-update state data :start 50)
    (let ((result (sha512:sha512-finalize state)))
      (assert-true (ct:ct-equal result expected)))))

;;; ---------------------------------------------------------------------------
;;; SHA-384 test vectors (FIPS 180-4)
;;; ---------------------------------------------------------------------------

(deftest test-sha384-empty
  "SHA-384 of empty input"
  (let* ((input (make-array 0 :element-type '(unsigned-byte 8)))
         (expected (hex-to-bytes "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b"))
         (result (sha512:sha384 input)))
    (assert-true (ct:ct-equal result expected))))

(deftest test-sha384-abc
  "SHA-384 of 'abc' (FIPS 180-4 example)"
  (let* ((input (string-to-bytes "abc"))
         (expected (hex-to-bytes "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7"))
         (result (sha512:sha384 input)))
    (assert-true (ct:ct-equal result expected))))

(deftest test-sha384-two-block
  "SHA-384 of two-block message (FIPS 180-4 example)"
  (let* ((input (string-to-bytes "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"))
         (expected (hex-to-bytes "09330c33f71147e83d192fc782cd1b4753111b173b3b05d22fa08086e3b0f712fcc7c71a557e2db966c3e9fa91746039"))
         (result (sha512:sha384 input)))
    (assert-true (ct:ct-equal result expected))))

(deftest test-sha384-hex
  "SHA-384 hex output"
  (let ((result (sha512:sha384-hex (string-to-bytes "abc"))))
    (assert-equal result "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7")))

(deftest test-sha384-incremental
  "Incremental SHA-384 matches one-shot"
  (let* ((data (string-to-bytes "Hello, World!"))
         (expected (sha512:sha384 data))
         (state (sha512:make-sha384-state)))
    (loop for i from 0 below (length data)
          do (sha512:sha384-update state data :start i :end (1+ i)))
    (let ((result (sha512:sha384-finalize state)))
      (assert-true (ct:ct-equal result expected)))))

;;; ---------------------------------------------------------------------------
;;; Output size verification
;;; ---------------------------------------------------------------------------

(deftest test-sha512-digest-size
  "SHA-512 should produce 64-byte digest"
  (assert-= (length (sha512:sha512 (make-array 0 :element-type '(unsigned-byte 8)))) 64))

(deftest test-sha384-digest-size
  "SHA-384 should produce 48-byte digest"
  (assert-= (length (sha512:sha384 (make-array 0 :element-type '(unsigned-byte 8)))) 48))

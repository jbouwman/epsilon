;;;; Tests for SHA-3 (Keccak)

(defpackage epsilon.ssl.sha3-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:sha3 #:epsilon.ssl.sha3))
  (:enter t))

(in-package :epsilon.ssl.sha3-tests)

;;; ---------------------------------------------------------------------------
;;; SHA3-256 (NIST FIPS 202 test vectors)
;;; ---------------------------------------------------------------------------

(deftest test-sha3-256-empty
  "SHA3-256 of empty input"
  (assert-equal (sha3:sha3-256-hex (string-to-bytes ""))
                "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a"))

(deftest test-sha3-256-abc
  "SHA3-256 of 'abc'"
  (assert-equal (sha3:sha3-256-hex (string-to-bytes "abc"))
                "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532"))

(deftest test-sha3-256-digest-length
  "SHA3-256 produces 32-byte digest"
  (assert-= (length (sha3:sha3-256 (string-to-bytes ""))) 32))

(deftest test-sha3-256-incremental
  "SHA3-256 incremental matches one-shot"
  (let* ((data (string-to-bytes "hello world"))
         (one-shot (sha3:sha3-256 data))
         (state (sha3:make-sha3-256-state)))
    (sha3:sha3-256-update state (string-to-bytes "hello "))
    (sha3:sha3-256-update state (string-to-bytes "world"))
    (assert-true (equalp one-shot (sha3:sha3-256-finalize state)))))

;;; ---------------------------------------------------------------------------
;;; SHA3-384
;;; ---------------------------------------------------------------------------

(deftest test-sha3-384-empty
  "SHA3-384 of empty input"
  (assert-equal (sha3:sha3-384-hex (string-to-bytes ""))
                "0c63a75b845e4f7d01107d852e4c2485c51a50aaaa94fc61995e71bbee983a2ac3713831264adb47fb6bd1e058d5f004"))

(deftest test-sha3-384-abc
  "SHA3-384 of 'abc'"
  (assert-equal (sha3:sha3-384-hex (string-to-bytes "abc"))
                "ec01498288516fc926459f58e2c6ad8df9b473cb0fc08c2596da7cf0e49be4b298d88cea927ac7f539f1edf228376d25"))

(deftest test-sha3-384-digest-length
  "SHA3-384 produces 48-byte digest"
  (assert-= (length (sha3:sha3-384 (string-to-bytes ""))) 48))

;;; ---------------------------------------------------------------------------
;;; SHA3-512
;;; ---------------------------------------------------------------------------

(deftest test-sha3-512-empty
  "SHA3-512 of empty input"
  (assert-equal (sha3:sha3-512-hex (string-to-bytes ""))
                "a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26"))

(deftest test-sha3-512-abc
  "SHA3-512 of 'abc'"
  (assert-equal (sha3:sha3-512-hex (string-to-bytes "abc"))
                "b751850b1a57168a5693cd924b6b096e08f621827444f70d884f5d0240d2712e10e116e9192af3c91a7ec57647e3934057340b4cf408d5a56592f8274eec53f0"))

(deftest test-sha3-512-digest-length
  "SHA3-512 produces 64-byte digest"
  (assert-= (length (sha3:sha3-512 (string-to-bytes ""))) 64))

;;; ---------------------------------------------------------------------------
;;; SHAKE128
;;; ---------------------------------------------------------------------------

(deftest test-shake128-empty-32
  "SHAKE128 of empty input, 32 bytes"
  (let* ((output (sha3:shake128 (string-to-bytes "") 32))
         (hex (with-output-to-string (s)
                (loop for b across output do (format s "~(~2,'0x~)" b)))))
    (assert-equal hex "7f9c2ba4e88f827d616045507605853ed73b8093f6efbc88eb1a6eacfa66ef26")))

(deftest test-shake128-output-length
  "SHAKE128 produces requested output length"
  (assert-= (length (sha3:shake128 (string-to-bytes "") 16)) 16)
  (assert-= (length (sha3:shake128 (string-to-bytes "") 100)) 100))

;;; ---------------------------------------------------------------------------
;;; SHAKE256
;;; ---------------------------------------------------------------------------

(deftest test-shake256-empty-32
  "SHAKE256 of empty input, 32 bytes"
  (let* ((output (sha3:shake256 (string-to-bytes "") 32))
         (hex (with-output-to-string (s)
                (loop for b across output do (format s "~(~2,'0x~)" b)))))
    (assert-equal hex "46b9dd2b0ba88d13233b3feb743eeb243fcd52ea62b81b82b50c27646ed5762f")))

(deftest test-shake256-output-length
  "SHAKE256 produces requested output length"
  (assert-= (length (sha3:shake256 (string-to-bytes "") 64)) 64))

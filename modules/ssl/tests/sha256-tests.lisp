;;;; Tests for SHA-256 implementation
;;;;
;;;; Test vectors from:
;;;; - NIST FIPS 180-4 examples
;;;; - NIST CAVP SHA byte vectors
;;;; - Additional edge cases

(defpackage epsilon.ssl.sha256-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:sha256 #:epsilon.ssl.sha256)
   (#:ct #:epsilon.ssl.ct))
  (:enter t))

(in-package :epsilon.ssl.sha256-tests)

;;; ---------------------------------------------------------------------------
;;; Helper functions
;;; ---------------------------------------------------------------------------

(defun bytes-equal-p (a b)
  "Check if two byte arrays are identical."
  (ct:ct-equal a b))

;;; ---------------------------------------------------------------------------
;;; FIPS 180-4 example test vectors
;;; ---------------------------------------------------------------------------

(deftest test-sha256-empty
  "SHA-256 of empty input"
  ;; SHA-256("") = e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
  (let* ((input (make-array 0 :element-type '(unsigned-byte 8)))
         (expected (hex-to-bytes "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
         (result (sha256:sha256 input)))
    (assert-true (bytes-equal-p result expected))))

(deftest test-sha256-abc
  "SHA-256 of 'abc' (FIPS 180-4 example B.1)"
  ;; SHA-256("abc") = ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad
  (let* ((input (string-to-bytes "abc"))
         (expected (hex-to-bytes "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
         (result (sha256:sha256 input)))
    (assert-true (bytes-equal-p result expected))))

(deftest test-sha256-two-block
  "SHA-256 of 448-bit message (FIPS 180-4 example B.2)"
  ;; SHA-256("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
  ;; = 248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1
  (let* ((input (string-to-bytes "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))
         (expected (hex-to-bytes "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"))
         (result (sha256:sha256 input)))
    (assert-true (bytes-equal-p result expected))))

(deftest test-sha256-long-message
  "SHA-256 of 896-bit message (FIPS 180-4 example B.3 equivalent)"
  ;; SHA-256("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu")
  ;; = cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1
  (let* ((input (string-to-bytes "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"))
         (expected (hex-to-bytes "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1"))
         (result (sha256:sha256 input)))
    (assert-true (bytes-equal-p result expected))))

;;; ---------------------------------------------------------------------------
;;; Additional NIST CAVP short message test vectors
;;; ---------------------------------------------------------------------------

(deftest test-sha256-single-byte
  "SHA-256 of single zero byte"
  ;; SHA-256(0x00) = 6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d
  (let* ((input (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0))
         (expected (hex-to-bytes "6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d"))
         (result (sha256:sha256 input)))
    (assert-true (bytes-equal-p result expected))))

(deftest test-sha256-ff-byte
  "SHA-256 of single 0xFF byte"
  ;; SHA-256(0xff) = a8100ae6aa1940d0b663bb31cd466142ebbdbd5187131b92d93818987832eb89
  (let* ((input (make-array 1 :element-type '(unsigned-byte 8) :initial-element #xFF))
         (expected (hex-to-bytes "a8100ae6aa1940d0b663bb31cd466142ebbdbd5187131b92d93818987832eb89"))
         (result (sha256:sha256 input)))
    (assert-true (bytes-equal-p result expected))))

;;; ---------------------------------------------------------------------------
;;; Incremental hashing tests
;;; ---------------------------------------------------------------------------

(deftest test-sha256-incremental-matches-oneshot
  "Incremental SHA-256 should produce same result as one-shot"
  (let* ((data (string-to-bytes "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))
         (oneshot (sha256:sha256 data))
         (state (sha256:make-sha256-state)))
    ;; Feed data byte by byte
    (loop for i from 0 below (length data)
          do (sha256:sha256-update state data :start i :end (1+ i)))
    (let ((incremental (sha256:sha256-finalize state)))
      (assert-true (bytes-equal-p oneshot incremental)))))

(deftest test-sha256-incremental-split
  "Incremental SHA-256 with data split at various points"
  (let* ((data (string-to-bytes "Hello, World! This is a test of incremental SHA-256."))
         (expected (sha256:sha256 data)))
    ;; Split at every possible point and verify
    (loop for split from 0 to (length data)
          do (let ((state (sha256:make-sha256-state)))
               (sha256:sha256-update state data :start 0 :end split)
               (sha256:sha256-update state data :start split)
               (let ((result (sha256:sha256-finalize state)))
                 (assert-true (bytes-equal-p result expected)))))))

(deftest test-sha256-copy
  "Copying SHA-256 state should produce independent hasher"
  (let* ((data1 (string-to-bytes "Hello"))
         (data2a (string-to-bytes ", World!"))
         (data2b (string-to-bytes ", Lisp!"))
         (state1 (sha256:make-sha256-state)))
    (sha256:sha256-update state1 data1)
    ;; Copy state after "Hello"
    (let ((state2 (sha256:sha256-copy state1)))
      ;; state1 gets "Hello, World!"
      (sha256:sha256-update state1 data2a)
      ;; state2 gets "Hello, Lisp!"
      (sha256:sha256-update state2 data2b)
      (let ((result1 (sha256:sha256-finalize state1))
            (result2 (sha256:sha256-finalize state2))
            (expected1 (sha256:sha256 (string-to-bytes "Hello, World!")))
            (expected2 (sha256:sha256 (string-to-bytes "Hello, Lisp!"))))
        (assert-true (bytes-equal-p result1 expected1))
        (assert-true (bytes-equal-p result2 expected2))
        ;; The two results should be different
        (assert-not (bytes-equal-p result1 result2))))))

;;; ---------------------------------------------------------------------------
;;; Hex output test
;;; ---------------------------------------------------------------------------

(deftest test-sha256-hex
  "sha256-hex should return correct hex string"
  (let* ((input (string-to-bytes "abc"))
         (result (sha256:sha256-hex input)))
    (assert-equal result "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")))

;;; ---------------------------------------------------------------------------
;;; Edge cases
;;; ---------------------------------------------------------------------------

(deftest test-sha256-exact-block-size
  "SHA-256 of exactly 64 bytes (one block before padding)"
  ;; 64 bytes of 'a'
  (let* ((input (make-array 64 :element-type '(unsigned-byte 8) :initial-element 97))
         (expected (hex-to-bytes "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb"))
         (result (sha256:sha256 input)))
    (assert-true (bytes-equal-p result expected))))

(deftest test-sha256-block-boundary
  "SHA-256 of 55 bytes (max that fits in one block with padding)"
  ;; 55 bytes of 'a'
  (let* ((input (make-array 55 :element-type '(unsigned-byte 8) :initial-element 97))
         (result1 (sha256:sha256 input))
         ;; 56 bytes of 'a' (needs two blocks for padding)
         (input2 (make-array 56 :element-type '(unsigned-byte 8) :initial-element 97))
         (result2 (sha256:sha256 input2)))
    ;; Just verify they're different and both 32 bytes
    (assert-= (length result1) 32)
    (assert-= (length result2) 32)
    (assert-not (bytes-equal-p result1 result2))))

(deftest test-sha256-large-input
  "SHA-256 of 1000 bytes"
  (let* ((input (make-array 1000 :element-type '(unsigned-byte 8) :initial-element 0))
         (result (sha256:sha256 input)))
    (assert-= (length result) 32)
    ;; Verify deterministic
    (let ((result2 (sha256:sha256 input)))
      (assert-true (bytes-equal-p result result2)))))

;;;; Tests for BLAKE3
;;;;
;;;; Uses official test vectors from the BLAKE3 specification.
;;;; Input pattern: byte[i] = i mod 251
;;;; Key: "whats the Elvish word for friend" (32 ASCII bytes)
;;;; Context: "BLAKE3 2019-12-27 16:29:52 test vectors context"

(defpackage epsilon.digest.blake3-tests
  (:use :cl :epsilon.test :epsilon.ssl.test-support)
  (:local-nicknames
   (#:blake3 #:epsilon.digest.blake3))
  (:enter t))

(in-package :epsilon.digest.blake3-tests)

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun make-test-input (len)
  "Create the standard BLAKE3 test input: byte[i] = i mod 251."
  (let ((data (make-array len :element-type '(unsigned-byte 8))))
    (loop for i from 0 below len
          do (setf (aref data i) (mod i 251)))
    data))

(defparameter +test-key+ (string-to-bytes "whats the Elvish word for friend"))
(defparameter +test-context+ "BLAKE3 2019-12-27 16:29:52 test vectors context")

;;; ---------------------------------------------------------------------------
;;; Hash mode test vectors
;;; ---------------------------------------------------------------------------

(deftest test-hash-empty
  "BLAKE3 hash: 0 bytes (empty)"
  (assert-equal (blake3:blake3-hex (make-test-input 0))
                "af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262"))

(deftest test-hash-1
  "BLAKE3 hash: 1 byte"
  (assert-equal (blake3:blake3-hex (make-test-input 1))
                "2d3adedff11b61f14c886e35afa036736dcd87a74d27b5c1510225d0f592e213"))

(deftest test-hash-2
  "BLAKE3 hash: 2 bytes"
  (assert-equal (blake3:blake3-hex (make-test-input 2))
                "7b7015bb92cf0b318037702a6cdd81dee41224f734684c2c122cd6359cb1ee63"))

(deftest test-hash-64
  "BLAKE3 hash: 64 bytes (1 block)"
  (assert-equal (blake3:blake3-hex (make-test-input 64))
                "4eed7141ea4a5cd4b788606bd23f46e212af9cacebacdc7d1f4c6dc7f2511b98"))

(deftest test-hash-65
  "BLAKE3 hash: 65 bytes (1 block + 1)"
  (assert-equal (blake3:blake3-hex (make-test-input 65))
                "de1e5fa0be70df6d2be8fffd0e99ceaa8eb6e8c93a63f2d8d1c30ecb6b263dee"))

(deftest test-hash-1023
  "BLAKE3 hash: 1023 bytes (chunk - 1)"
  (assert-equal (blake3:blake3-hex (make-test-input 1023))
                "10108970eeda3eb932baac1428c7a2163b0e924c9a9e25b35bba72b28f70bd11"))

(deftest test-hash-1024
  "BLAKE3 hash: 1024 bytes (1 chunk)"
  (assert-equal (blake3:blake3-hex (make-test-input 1024))
                "42214739f095a406f3fc83deb889744ac00df831c10daa55189b5d121c855af7"))

(deftest test-hash-1025
  "BLAKE3 hash: 1025 bytes (1 chunk + 1, triggers Merkle tree)"
  (assert-equal (blake3:blake3-hex (make-test-input 1025))
                "d00278ae47eb27b34faecf67b4fe263f82d5412916c1ffd97c8cb7fb814b8444"))

(deftest test-hash-2048
  "BLAKE3 hash: 2048 bytes (2 chunks)"
  (assert-equal (blake3:blake3-hex (make-test-input 2048))
                "e776b6028c7cd22a4d0ba182a8bf62205d2ef576467e838ed6f2529b85fba24a"))

(deftest test-hash-2049
  "BLAKE3 hash: 2049 bytes (2 chunks + 1)"
  (assert-equal (blake3:blake3-hex (make-test-input 2049))
                "5f4d72f40d7a5f82b15ca2b2e44b1de3c2ef86c426c95c1af0b6879522563030"))

(deftest test-hash-8192
  "BLAKE3 hash: 8192 bytes (8 chunks)"
  (assert-equal (blake3:blake3-hex (make-test-input 8192))
                "aae792484c8efe4f19e2ca7d371d8c467ffb10748d8a5a1ae579948f718a2a63"))

(deftest test-hash-8193
  "BLAKE3 hash: 8193 bytes (8 chunks + 1)"
  (assert-equal (blake3:blake3-hex (make-test-input 8193))
                "bab6c09cb8ce8cf459261398d2e7aef35700bf488116ceb94a36d0f5f1b7bc3b"))

(deftest test-hash-31744
  "BLAKE3 hash: 31744 bytes (31 chunks, deep tree)"
  (assert-equal (blake3:blake3-hex (make-test-input 31744))
                "62b6960e1a44bcc1eb1a611a8d6235b6b4b78f32e7abc4fb4c6cdcce94895c47"))

(deftest test-hash-102400
  "BLAKE3 hash: 102400 bytes (100 chunks)"
  (assert-equal (blake3:blake3-hex (make-test-input 102400))
                "bc3e3d41a1146b069abffad3c0d44860cf664390afce4d9661f7902e7943e085"))

;;; ---------------------------------------------------------------------------
;;; Keyed hash test vectors
;;; ---------------------------------------------------------------------------

(deftest test-keyed-hash-empty
  "BLAKE3 keyed hash: 0 bytes"
  (assert-equal (blake3:blake3-keyed-hex +test-key+ (make-test-input 0))
                "92b2b75604ed3c761f9d6f62392c8a9227ad0ea3f09573e783f1498a4ed60d26"))

(deftest test-keyed-hash-1
  "BLAKE3 keyed hash: 1 byte"
  (assert-equal (blake3:blake3-keyed-hex +test-key+ (make-test-input 1))
                "6d7878dfff2f485635d39013278ae14f1454b8c0a3a2d34bc1ab38228a80c95b"))

(deftest test-keyed-hash-64
  "BLAKE3 keyed hash: 64 bytes"
  (assert-equal (blake3:blake3-keyed-hex +test-key+ (make-test-input 64))
                "ba8ced36f327700d213f120b1a207a3b8c04330528586f414d09f2f7d9ccb7e6"))

(deftest test-keyed-hash-1024
  "BLAKE3 keyed hash: 1024 bytes"
  (assert-equal (blake3:blake3-keyed-hex +test-key+ (make-test-input 1024))
                "75c46f6f3d9eb4f55ecaaee480db732e6c2105546f1e675003687c31719c7ba4"))

(deftest test-keyed-hash-1025
  "BLAKE3 keyed hash: 1025 bytes"
  (assert-equal (blake3:blake3-keyed-hex +test-key+ (make-test-input 1025))
                "357dc55de0c7e382c900fd6e320acc04146be01db6a8ce7210b7189bd664ea69"))

(deftest test-keyed-hash-8192
  "BLAKE3 keyed hash: 8192 bytes"
  (assert-equal (blake3:blake3-keyed-hex +test-key+ (make-test-input 8192))
                "dc9637c8845a770b4cbf76b8daec0eebf7dc2eac11498517f08d44c8fc00d58a"))

(deftest test-keyed-hash-102400
  "BLAKE3 keyed hash: 102400 bytes"
  (assert-equal (blake3:blake3-keyed-hex +test-key+ (make-test-input 102400))
                "1c35d1a5811083fd7119f5d5d1ba027b4d01c0c6c49fb6ff2cf75393ea5db4a7"))

;;; ---------------------------------------------------------------------------
;;; Key derivation test vectors
;;; ---------------------------------------------------------------------------

(deftest test-derive-key-empty
  "BLAKE3 derive key: 0 bytes IKM"
  (let* ((result (blake3:blake3-derive-key +test-context+ (make-test-input 0)))
         (hex (with-output-to-string (s)
                (loop for b across result do (format s "~(~2,'0x~)" b)))))
    (assert-equal hex "2cc39783c223154fea8dfb7c1b1660f2ac2dcbd1c1de8277b0b0dd39b7e50d7d")))

(deftest test-derive-key-1
  "BLAKE3 derive key: 1 byte IKM"
  (let* ((result (blake3:blake3-derive-key +test-context+ (make-test-input 1)))
         (hex (with-output-to-string (s)
                (loop for b across result do (format s "~(~2,'0x~)" b)))))
    (assert-equal hex "b3e2e340a117a499c6cf2398a19ee0d29cca2bb7404c73063382693bf66cb06c")))

(deftest test-derive-key-64
  "BLAKE3 derive key: 64 bytes IKM"
  (let* ((result (blake3:blake3-derive-key +test-context+ (make-test-input 64)))
         (hex (with-output-to-string (s)
                (loop for b across result do (format s "~(~2,'0x~)" b)))))
    (assert-equal hex "a5c4a7053fa86b64746d4bb688d06ad1f02a18fce9afd3e818fefaa7126bf73e")))

(deftest test-derive-key-1024
  "BLAKE3 derive key: 1024 bytes IKM"
  (let* ((result (blake3:blake3-derive-key +test-context+ (make-test-input 1024)))
         (hex (with-output-to-string (s)
                (loop for b across result do (format s "~(~2,'0x~)" b)))))
    (assert-equal hex "7356cd7720d5b66b6d0697eb3177d9f8d73a4a5c5e968896eb6a689684302706")))

(deftest test-derive-key-102400
  "BLAKE3 derive key: 102400 bytes IKM"
  (let* ((result (blake3:blake3-derive-key +test-context+ (make-test-input 102400)))
         (hex (with-output-to-string (s)
                (loop for b across result do (format s "~(~2,'0x~)" b)))))
    (assert-equal hex "4652cff7a3f385a6103b5c260fc1593e13c778dbe608efb092fe7ee69df6e9c6")))

;;; ---------------------------------------------------------------------------
;;; Extended output (XOF) tests
;;; ---------------------------------------------------------------------------

(deftest test-xof-empty-131
  "BLAKE3 XOF: empty input, 131 bytes"
  (let* ((result (blake3:blake3 (make-test-input 0) :output-length 131))
         (hex (with-output-to-string (s)
                (loop for b across result do (format s "~(~2,'0x~)" b)))))
    (assert-equal hex
      "af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262e00f03e7b69af26b7faaf09fcd333050338ddfe085b8cc869ca98b206c08243a26f5487789e8f660afe6c99ef9e0c52b92e7393024a80459cf91f476f9ffdbda7001c22e159b402631f277ca96f2defdf1078282314e763699a31c5363165421cce14d")))

(deftest test-xof-1-byte-131
  "BLAKE3 XOF: 1-byte input, 131 bytes"
  (let* ((result (blake3:blake3 (make-test-input 1) :output-length 131))
         (hex (with-output-to-string (s)
                (loop for b across result do (format s "~(~2,'0x~)" b)))))
    (assert-equal hex
      "2d3adedff11b61f14c886e35afa036736dcd87a74d27b5c1510225d0f592e213c3a6cb8bf623e20cdb535f8d1a5ffb86342d9c0b64aca3bce1d31f60adfa137b358ad4d79f97b47c3d5e79f179df87a3b9776ef8325f8329886ba42f07fb138bb502f4081cbcec3195c5871e6c23e2cc97d3c69a613eba131e5f1351f3f1da786545e5")))

(deftest test-xof-prefix-consistency
  "BLAKE3 XOF: first 32 bytes of extended output match default output"
  (let ((short (blake3:blake3 (make-test-input 1024)))
        (long (blake3:blake3 (make-test-input 1024) :output-length 128)))
    (assert-true (equalp short (subseq long 0 32)))))

;;; ---------------------------------------------------------------------------
;;; Incremental hashing tests
;;; ---------------------------------------------------------------------------

(deftest test-incremental-byte-by-byte
  "BLAKE3 incremental: byte-by-byte matches one-shot for multi-chunk input"
  (let* ((data (make-test-input 2048))
         (one-shot (blake3:blake3 data))
         (state (blake3:make-blake3-state)))
    (loop for i from 0 below (length data)
          do (blake3:blake3-update state data :start i :end (1+ i)))
    (assert-true (equalp one-shot (blake3:blake3-finalize state)))))

(deftest test-incremental-split-positions
  "BLAKE3 incremental: split at every position for short input"
  (let* ((data (make-test-input 100))
         (one-shot (blake3:blake3 data)))
    (loop for split from 0 to (length data)
          do (let ((state (blake3:make-blake3-state)))
               (blake3:blake3-update state data :start 0 :end split)
               (blake3:blake3-update state data :start split)
               (assert-true (equalp one-shot (blake3:blake3-finalize state)))))))

(deftest test-incremental-keyed
  "BLAKE3 incremental keyed hash matches one-shot"
  (let* ((data (make-test-input 1025))
         (one-shot (blake3:blake3-keyed +test-key+ data))
         (state (blake3:make-blake3-keyed-state +test-key+)))
    (blake3:blake3-update state data :start 0 :end 512)
    (blake3:blake3-update state data :start 512)
    (assert-true (equalp one-shot (blake3:blake3-finalize state)))))

(deftest test-incremental-chunk-boundary
  "BLAKE3 incremental: exact chunk boundary (1024 bytes)"
  (let* ((data (make-test-input 1024))
         (one-shot (blake3:blake3 data))
         (state (blake3:make-blake3-state)))
    ;; Feed in 64-byte blocks (one block at a time)
    (loop for i from 0 below 1024 by 64
          do (blake3:blake3-update state data :start i :end (+ i 64)))
    (assert-true (equalp one-shot (blake3:blake3-finalize state)))))

;;; ---------------------------------------------------------------------------
;;; Copy tests
;;; ---------------------------------------------------------------------------

(deftest test-copy-independence
  "BLAKE3 copy: copied states produce independent results"
  (let* ((data (make-test-input 2048))
         (state (blake3:make-blake3-state)))
    ;; Feed first half
    (blake3:blake3-update state data :start 0 :end 1024)
    ;; Copy
    (let ((copy (blake3:blake3-copy state)))
      ;; Feed different suffixes
      (blake3:blake3-update state data :start 1024 :end 1536)
      (blake3:blake3-update copy data :start 1024 :end 2048)
      ;; Results should differ
      (let ((result-a (blake3:blake3-finalize state))
            (result-b (blake3:blake3-finalize copy)))
        (assert-true (not (equalp result-a result-b)))))))

(deftest test-copy-matches-one-shot
  "BLAKE3 copy: feeding same data to copy matches one-shot"
  (let* ((data (make-test-input 2048))
         (one-shot (blake3:blake3 data))
         (state (blake3:make-blake3-state)))
    (blake3:blake3-update state data :start 0 :end 1024)
    (let ((copy (blake3:blake3-copy state)))
      (blake3:blake3-update copy data :start 1024)
      (assert-true (equalp one-shot (blake3:blake3-finalize copy))))))

;;; ---------------------------------------------------------------------------
;;; Edge cases
;;; ---------------------------------------------------------------------------

(deftest test-default-digest-length
  "BLAKE3 default produces 32-byte digest"
  (assert-= (length (blake3:blake3 (make-test-input 0))) 32))

(deftest test-variable-output-lengths
  "BLAKE3 supports variable output lengths"
  (assert-= (length (blake3:blake3 (make-test-input 0) :output-length 16)) 16)
  (assert-= (length (blake3:blake3 (make-test-input 0) :output-length 32)) 32)
  (assert-= (length (blake3:blake3 (make-test-input 0) :output-length 64)) 64)
  (assert-= (length (blake3:blake3 (make-test-input 0) :output-length 128)) 128))

(deftest test-empty-all-modes
  "BLAKE3 empty input produces valid output in all three modes"
  (assert-= (length (blake3:blake3 (make-test-input 0))) 32)
  (assert-= (length (blake3:blake3-keyed +test-key+ (make-test-input 0))) 32)
  (assert-= (length (blake3:blake3-derive-key +test-context+ (make-test-input 0))) 32))

(deftest test-start-end-slicing
  "BLAKE3 start/end slicing matches sub-array hash"
  (let* ((data (make-test-input 200))
         (sub (subseq data 50 150))
         (full (blake3:blake3 data :start 50 :end 150))
         (direct (blake3:blake3 sub)))
    (assert-true (equalp full direct))))

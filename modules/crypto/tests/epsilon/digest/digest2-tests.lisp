;;;; epsilon.digest tests
;;;;
;;;; Tests for the digest module covering:
;;;; - BLAKE3 one-shot and streaming
;;;; - xxHash3 64-bit and 128-bit variants
;;;; - IO integration
;;;; - Edge cases

(defpackage epsilon.digest.tests
  (:use :cl :epsilon.test)
  (:require (epsilon.digest digest)
            (epsilon.digest.protocol proto)
            (epsilon.io.buffer buf)
            (epsilon.io.byte-stream byte-stream))
  (:enter t))

;;; ============================================================================
;;; Test Helpers
;;; ============================================================================

(defun make-test-data (size &optional (pattern 0))
  "Create test data of SIZE bytes with optional PATTERN."
  (let ((data (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size)
      (setf (aref data i) (mod (+ i pattern) 256)))
    data))

(defun bytes= (a b)
  "Compare two byte arrays for equality."
  (and (= (length a) (length b))
       (every #'= a b)))

(defun empty-bytes ()
  "Return an empty byte array of the proper type."
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun byte-array (&rest bytes)
  "Create a byte array from the given bytes."
  (make-array (length bytes)
              :element-type '(unsigned-byte 8)
              :initial-contents bytes))

;;; ============================================================================
;;; BLAKE3 Tests
;;; ============================================================================

(deftest blake3/empty-input ()
  "BLAKE3 hash of empty input matches known value."
  (let ((hash (digest:blake3 (empty-bytes))))
    (assert-equal 32 (length hash))
    ;; Known BLAKE3 hash of empty input
    (assert-equal "af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262"
              (digest:bytes-to-hex hash))))

(deftest blake3/hello-world ()
  "BLAKE3 hash of 'Hello, World!' matches known value."
  (let* ((data (sb-ext:string-to-octets "Hello, World!" :external-format :utf-8))
         (hash (digest:blake3 data)))
    (assert-equal 32 (length hash))
    ;; Known BLAKE3 hash
    (assert-equal "288a86a79f20a3d6dccdca7713beaed178798296bdfa7913fa2a62d9727bf8f8"
              (digest:bytes-to-hex hash))))

(deftest blake3/streaming-equals-oneshot ()
  "BLAKE3 streaming produces same result as one-shot."
  (let* ((data (make-test-data 10000))
         (oneshot-hash (digest:blake3 data))
         (h (digest:make-blake3-hasher)))
    ;; Feed in chunks
    (proto:hasher-update h data :start 0 :end 1000)
    (proto:hasher-update h data :start 1000 :end 5000)
    (proto:hasher-update h data :start 5000 :end 10000)
    (let ((streaming-hash (proto:hasher-finalize h)))
      (assert-true (bytes= oneshot-hash streaming-hash)))))

(deftest blake3/reset ()
  "BLAKE3 hasher can be reset and reused."
  (let* ((data1 (make-test-data 100 1))
         (data2 (make-test-data 100 2))
         (h (digest:make-blake3-hasher)))
    ;; Hash first data
    (proto:hasher-update h data1)
    (let ((hash1 (proto:hasher-finalize h)))
      ;; Reset and hash second data
      (proto:hasher-reset h)
      (proto:hasher-update h data2)
      (let ((hash2 (proto:hasher-finalize h)))
        ;; Hashes should be different
        (assert-not (bytes= hash1 hash2))
        ;; Reset and hash first data again
        (proto:hasher-reset h)
        (proto:hasher-update h data1)
        (let ((hash1-again (proto:hasher-finalize h)))
          (assert-true (bytes= hash1 hash1-again)))))))

(deftest blake3/variable-output-length ()
  "BLAKE3 supports variable output length (XOF mode)."
  (let ((data (make-test-data 100)))
    ;; Test various output lengths
    (dolist (len '(1 16 32 64 128 256))
      (let ((hash (digest:blake3 data :output-length len)))
        (assert-equal len (length hash))))
    ;; Longer outputs should be prefixes of even longer ones
    (let ((hash64 (digest:blake3 data :output-length 64))
          (hash128 (digest:blake3 data :output-length 128)))
      (assert-true (bytes= hash64 (subseq hash128 0 64))))))

(deftest blake3/keyed-mode ()
  "BLAKE3 keyed mode produces different hashes with different keys."
  (let ((data (make-test-data 100))
        (key1 (make-test-data 32 1))
        (key2 (make-test-data 32 2)))
    (let ((hash1 (digest:blake3-keyed key1 data))
          (hash2 (digest:blake3-keyed key2 data))
          (hash-unkeyed (digest:blake3 data)))
      ;; All should be different
      (assert-not (bytes= hash1 hash2))
      (assert-not (bytes= hash1 hash-unkeyed))
      (assert-not (bytes= hash2 hash-unkeyed)))))

(deftest blake3/derive-key ()
  "BLAKE3 key derivation produces consistent keys."
  (let ((ikm (make-test-data 64)))
    (let ((key1 (digest:blake3-derive-key "myapp:encryption-key" ikm))
          (key2 (digest:blake3-derive-key "myapp:mac-key" ikm)))
      ;; Different contexts produce different keys
      (assert-not (bytes= key1 key2))
      ;; Same context produces same key
      (assert-true (bytes= key1 (digest:blake3-derive-key "myapp:encryption-key" ikm))))))

(deftest blake3/xof-seek ()
  "BLAKE3 XOF seek produces correct output segments."
  (let ((data (make-test-data 100)))
    ;; Get full output and segments
    (let ((full (digest:blake3 data :output-length 128))
          (first-half (digest:blake3-xof data 64))
          (second-half (digest:blake3-xof-seek data 64 64)))
      (assert-true (bytes= first-half (subseq full 0 64)))
      (assert-true (bytes= second-half (subseq full 64 128))))))

;;; ============================================================================
;;; xxHash3 Tests
;;; ============================================================================

(deftest xxhash64/empty-input ()
  "xxHash64 of empty input matches known value."
  ;; Known XXH64 hash of empty input (seed 0)
  (let ((hash (digest:xxhash64 (empty-bytes))))
    (assert-equal #xEF46DB3751D8E999 hash)))

(deftest xxhash64/hello-world ()
  "xxHash64 of 'Hello, World!' produces consistent hash."
  (let* ((data (sb-ext:string-to-octets "Hello, World!" :external-format :utf-8))
         (hash1 (digest:xxhash64 data))
         (hash2 (digest:xxhash64 data)))
    (assert-equal hash1 hash2)
    (assert-true (typep hash1 '(unsigned-byte 64)))))

(deftest xxhash64/streaming-equals-oneshot ()
  "xxHash64 streaming produces same result as one-shot."
  (let* ((data (make-test-data 10000))
         (oneshot-hash (digest:xxhash64 data))
         (h (digest:make-xxhash64-hasher)))
    ;; Feed in chunks
    (proto:hasher-update h data :start 0 :end 1000)
    (proto:hasher-update h data :start 1000 :end 5000)
    (proto:hasher-update h data :start 5000 :end 10000)
    (let ((streaming-hash (proto:hasher-finalize h)))
      (assert-equal oneshot-hash streaming-hash))))

(deftest xxhash64/with-seed ()
  "xxHash64 with different seeds produces different hashes."
  (let ((data (make-test-data 100)))
    (let ((hash1 (digest:xxhash64 data :seed 0))
          (hash2 (digest:xxhash64 data :seed 12345))
          (hash3 (digest:xxhash64 data :seed 12345)))
      (assert-not (= hash1 hash2))
      (assert-equal hash2 hash3))))

(deftest xxhash128/basic ()
  "xxHash128 produces two 64-bit values."
    (let ((data (make-test-data 100)))
      (multiple-value-bind (low high)
          (digest:xxhash128 data)
        (assert-true (typep low '(unsigned-byte 64)))
        (assert-true (typep high '(unsigned-byte 64))))))

(deftest xxhash128/streaming-equals-oneshot ()
  "xxHash128 streaming produces same result as one-shot."
    (let* ((data (make-test-data 10000))
           (h (digest:make-xxhash128-hasher)))
      (multiple-value-bind (oneshot-low oneshot-high)
          (digest:xxhash128 data)
        ;; Feed in chunks
        (proto:hasher-update h data :start 0 :end 5000)
        (proto:hasher-update h data :start 5000 :end 10000)
        (multiple-value-bind (streaming-low streaming-high)
            (proto:hasher-finalize h)
          (assert-equal oneshot-low streaming-low)
          (assert-equal oneshot-high streaming-high)))))

(deftest xxhash64/bytes-format ()
  "xxHash64 can be returned as bytes."
    (let* ((data (make-test-data 100))
           (hash-int (digest:xxhash64 data))
           (hash-bytes (digest:xxhash64-bytes data)))
      (assert-equal 8 (length hash-bytes))
      ;; Verify little-endian encoding
      (assert-equal (ldb (byte 8 0) hash-int) (aref hash-bytes 0))
      (assert-equal (ldb (byte 8 56) hash-int) (aref hash-bytes 7))))

;;; ============================================================================
;;; IO Integration Tests
;;; ============================================================================

(deftest io/hash-reader-blake3 ()
  "hash-reader with BLAKE3 works correctly."
  (let* ((data (make-test-data 10000))
         (reader (byte-stream:make-byte-reader data))
         (expected (digest:blake3 data))
         (actual (digest:hash-reader-blake3 reader)))
    (assert-true (bytes= expected actual))))

(deftest io/hashing-reader ()
  "Hashing reader correctly hashes all data."
    (let* ((data (make-test-data 10000))
           (reader (byte-stream:make-byte-reader data))
           (hasher (digest:make-blake3-hasher))
           (hashing-reader (digest:make-hashing-reader reader hasher))
           (expected (digest:blake3 data)))
      ;; Read all data
      (epsilon.io.protocol:read-all hashing-reader)
      ;; Finalize hasher
      (let ((actual (proto:hasher-finalize hasher)))
        (assert-true (bytes= expected actual)))))

(deftest io/hashing-writer ()
  "Hashing writer correctly hashes all data."
  (let* ((data (make-test-data 10000))
         (writer (byte-stream:make-byte-writer))
         (hasher (digest:make-xxhash64-hasher))
         (hashing-writer (digest:make-hashing-writer writer hasher))
         (expected (digest:xxhash64 data)))
    ;; Write all data
    (epsilon.io.protocol:write-all hashing-writer data)
    (epsilon.io.protocol:flush hashing-writer)
    ;; Finalize hasher
    (let ((actual (proto:hasher-finalize hasher)))
      (assert-equal expected actual))))

(deftest io/buffer-hashing ()
  "Buffer hashing works correctly."
  (let* ((data (make-test-data 1000))
         (buffer (buf:buf-from-bytes data))
         (expected (digest:blake3 data))
         (actual (digest:hash-buffer-blake3 buffer)))
    (assert-true (bytes= expected actual))))

(deftest io/multi-hashing-reader ()
  "Multi-hashing reader updates multiple hashers."
  (let* ((data (make-test-data 10000))
         (reader (byte-stream:make-byte-reader data))
         (blake3-hasher (digest:make-blake3-hasher))
         (xxhash-hasher (digest:make-xxhash64-hasher))
         (multi-reader (digest:make-multi-hashing-reader reader blake3-hasher xxhash-hasher)))
    ;; Read all data
    (epsilon.io.protocol:read-all multi-reader)
    ;; Both hashers should have correct values
    (assert-true (bytes= (digest:blake3 data)
                     (proto:hasher-finalize blake3-hasher)))
    (assert-equal (digest:xxhash64 data)
              (proto:hasher-finalize xxhash-hasher))))

;;; ============================================================================
;;; Edge Cases
;;; ============================================================================

(deftest edge/single-byte ()
  "Hashing a single byte works correctly."
  (let ((data (byte-array 42)))
    (assert-equal 32 (length (digest:blake3 data)))))

(deftest edge/large-data ()
  "Hashing large data (1MB) works correctly."
  (let* ((data (make-test-data (* 1024 1024)))
         (hash (digest:blake3 data)))
    (assert-equal 32 (length hash))))

(deftest edge/alignment ()
  "Hashing with various alignments works correctly."
  (dolist (size '(1 2 3 4 7 8 15 16 31 32 63 64 127 128 255 256))
    (let* ((data (make-test-data size))
           (hash (digest:blake3 data)))
      (assert-equal 32 (length hash)))))

(deftest edge/subarray ()
  "Hashing a subarray works correctly."
  (let* ((data (make-test-data 1000))
         (full-hash (digest:blake3 (subseq data 100 500)))
         (sub-hash (digest:blake3 data :start 100 :end 500)))
    (assert-true (bytes= full-hash sub-hash))))

;;; ============================================================================
;;; Protocol Tests
;;; ============================================================================

(deftest protocol/hash-bytes ()
  "hash-bytes generic function dispatches correctly."
  (let* ((data (make-test-data 100))
         (expected (digest:blake3 data))
         (actual (proto:hash-bytes :blake3 data)))
    (assert-true (bytes= expected actual))))

(deftest protocol/hasher-p ()
  "hasher-p correctly identifies hashers."
  (assert-true (proto:hasher-p (digest:make-blake3-hasher)))
  (assert-not (proto:hasher-p "not a hasher")))

(deftest protocol/cryptographic-hasher-p ()
  "cryptographic-hasher-p correctly identifies crypto hashers."
  (assert-true (proto:cryptographic-hasher-p (digest:make-blake3-hasher)))
  (assert-not (proto:cryptographic-hasher-p (digest:make-xxhash64-hasher))))

(deftest protocol/make-hasher ()
  "make-hasher factory function works correctly."
    (let ((h (digest:make-hasher :blake3)))
      (assert-equal :blake3 (proto:hasher-algorithm h)))
    (let ((h (digest:make-hasher :xxhash64)))
      (assert-equal :xxhash64 (proto:hasher-algorithm h))))

;;; ============================================================================
;;; Utility Tests
;;; ============================================================================

(deftest util/bytes-to-hex ()
  "bytes-to-hex correctly converts bytes to hex string."
  (assert-equal "00" (digest:bytes-to-hex (byte-array 0)))
  (assert-equal "ff" (digest:bytes-to-hex (byte-array 255)))
  (assert-equal "deadbeef" (digest:bytes-to-hex (byte-array #xde #xad #xbe #xef))))

(deftest util/hex-to-bytes ()
  "hex-to-bytes correctly converts hex string to bytes."
  (assert-true (bytes= (byte-array 0) (digest:hex-to-bytes "00")))
  (assert-true (bytes= (byte-array 255) (digest:hex-to-bytes "ff")))
  (assert-true (bytes= (byte-array #xde #xad #xbe #xef) (digest:hex-to-bytes "deadbeef"))))

(deftest util/roundtrip ()
  "bytes-to-hex and hex-to-bytes are inverses."
  (let ((data (make-test-data 32)))
    (assert-true (bytes= data
                     (digest:hex-to-bytes (digest:bytes-to-hex data))))))

(deftest util/available-algorithms ()
  "available-algorithms returns correct list."
  (let ((algs (digest:available-algorithms)))
    (assert-not-null (member :blake3 algs))
    (assert-not-null (member :xxhash64 algs))))

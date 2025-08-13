(defpackage #:epsilon.digest.tests
  (:use #:cl #:epsilon.test)
  (:local-nicknames
   (:digest :epsilon.digest)))

(in-package #:epsilon.digest.tests)

;;; One-Shot API Tests (Rust-style)

(deftest test-sha256-string
  "Test SHA-256 with string input"
  (let ((result (digest:sha256 "hello")))
    (is (typep result '(vector (unsigned-byte 8))))
    (is-= 32 (length result))
    (is-equal "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
              (string-downcase (digest:bytes-to-hex result)))))

(deftest test-sha256-bytes
  "Test SHA-256 with byte array input"
  (let* ((input (make-array 5 :element-type '(unsigned-byte 8) 
                              :initial-contents '(104 101 108 108 111))) ; "hello"
         (result (digest:sha256 input)))
    (is (typep result '(vector (unsigned-byte 8))))
    (is-= 32 (length result))
    (is-equal "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
              (string-downcase (digest:bytes-to-hex result)))))

(deftest test-sha256-empty
  "Test SHA-256 with empty input"
  (let ((result (digest:sha256 "")))
    (is (typep result '(vector (unsigned-byte 8))))
    (is-= 32 (length result))))

(deftest test-sha1-string
  "Test SHA-1 with string input"
  (let ((result (digest:sha1 "hello")))
    (is (typep result '(vector (unsigned-byte 8))))
    (is-= 20 (length result))))

(deftest test-crc32-string
  "Test CRC-32 with string input"
  (let ((result (digest:crc32 "hello")))
    (is (typep result 'integer))
    (is (<= 0 result #xFFFFFFFF))))

(deftest test-crc32-known-value
  "Test CRC-32 with known value"
  (let ((result (digest:crc32 "123456789")))
    (is-= #xCBF43926 result)))

;;; Streaming API Tests (Go-style)

(deftest test-streaming-sha256-basic
  "Test basic streaming SHA-256"
  (let ((hasher (digest:make-sha256)))
    (digest:update hasher "hello")
    (let ((result (digest:finalize hasher)))
      (is (typep result '(vector (unsigned-byte 8))))
      (is-= 32 (length result))
      ;; Expect lowercase hex output
      (is-equal "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
                (string-downcase (digest:bytes-to-hex result))))))

(deftest test-streaming-sha256-multiple-updates
  "Test streaming SHA-256 with multiple updates"
  (let ((hasher (digest:make-sha256)))
    (digest:update hasher "hel")
    (digest:update hasher "lo")
    (let ((result (digest:finalize hasher)))
      (is (typep result '(vector (unsigned-byte 8))))
      (is-= 32 (length result))
      (is-equal "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
                (string-downcase (digest:bytes-to-hex result))))))

(deftest test-streaming-sha256-reset
  "Test streaming SHA-256 reset functionality"
  (let ((hasher (digest:make-sha256)))
    (digest:update hasher "hello")
    (digest:reset hasher)
    (digest:update hasher "world")
    (let ((result (digest:finalize hasher)))
      ;; Should be hash of "world", not "hello"
      (is (typep result '(vector (unsigned-byte 8))))
      (is-= 32 (length result)))))

(deftest test-streaming-crc32-basic
  "Test basic streaming CRC-32"
  (let ((hasher (digest:make-crc32)))
    (digest:update hasher "hello")
    (let ((result (digest:finalize hasher)))
      (is (typep result 'integer))
      (is (<= 0 result #xFFFFFFFF)))))

(deftest test-streaming-crc32-multiple-updates
  "Test streaming CRC-32 with multiple updates"
  (let ((hasher (digest:make-crc32)))
    (digest:update hasher "123")
    (digest:update hasher "456")
    (digest:update hasher "789")
    (let ((result (digest:finalize hasher)))
      (is-= #xCBF43926 result))))

;;; Input Type Tests

(deftest test-input-types-string
  "Test different input types work with strings"
  (let ((sha-result (digest:sha256 "test"))
        (crc-result (digest:crc32 "test")))
    (is (typep sha-result '(vector (unsigned-byte 8))))
    (is (typep crc-result 'integer))))

(deftest test-input-types-byte-vector
  "Test different input types work with byte vectors"
  (let* ((input #(116 101 115 116)) ; "test"
         (sha-result (digest:sha256 input))
         (crc-result (digest:crc32 input)))
    (is (typep sha-result '(vector (unsigned-byte 8))))
    (is (typep crc-result 'integer))))

(deftest test-input-types-byte-list
  "Test different input types work with byte lists"
  (let* ((input '(116 101 115 116)) ; "test"
         (sha-result (digest:sha256 input))
         (crc-result (digest:crc32 input)))
    (is (typep sha-result '(vector (unsigned-byte 8))))
    (is (typep crc-result 'integer))))

;;; Consistency Tests

(deftest test-consistency-one-shot-vs-streaming
  "Test one-shot and streaming APIs produce same results"
  (let* ((data "hello world")
         (one-shot (digest:sha256 data))
         (hasher (digest:make-sha256)))
    (digest:update hasher data)
    (let ((streaming (digest:finalize hasher)))
      (is (equalp one-shot streaming)))))

(deftest test-consistency-multiple-calls
  "Test multiple calls with same input produce same results"
  (let* ((data "consistency")
         (result1 (digest:sha256 data))
         (result2 (digest:sha256 data)))
    (is (equalp result1 result2))))

;;; Utility Tests

(deftest test-bytes-to-hex
  "Test bytes-to-hex utility function"
  (let ((bytes #(171 205 239)))
    (is-equal "ABCDEF" (digest:bytes-to-hex bytes))))

(deftest test-bytes-to-hex-empty
  "Test bytes-to-hex with empty input"
  (let ((bytes #()))
    (is-equal "" (digest:bytes-to-hex bytes))))

;;; Error Handling Tests

(deftest test-invalid-vector-input
  "Test error handling for invalid vector input"
  (is-thrown (simple-error) (digest:sha256 #(256 300 400))))

(deftest test-invalid-list-input
  "Test error handling for invalid list input"
  (is-thrown (simple-error) (digest:crc32 '(256 300 400))))

;;; Performance/Edge Cases

(deftest test-large-input
  "Test with large input"
  (let* ((size 10000)
         (data (make-string size :initial-element #\x))
         (result (digest:sha256 data)))
    (is (typep result '(vector (unsigned-byte 8))))
    (is-= 32 (length result))))

(deftest test-streaming-many-small-updates
  "Test streaming with many small updates"
  (let ((hasher (digest:make-sha256)))
    (loop for i from 0 below 100 do
      (digest:update hasher "x"))
    (let ((result (digest:finalize hasher)))
      (is (typep result '(vector (unsigned-byte 8))))
      (is-= 32 (length result)))))

;;; Legacy API Compatibility

(deftest test-legacy-sha1-digest
  "Test legacy SHA-1 function still works"
  (let ((result (digest:sha1-digest "hello")))
    (is (typep result '(vector (unsigned-byte 8))))
    (is-= 20 (length result))))
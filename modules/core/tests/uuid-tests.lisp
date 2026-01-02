;;;; UUID Tests
;;;;
;;;; Tests for UUID generation and manipulation, including UUIDv7.

(defpackage :epsilon.uuid.tests
  (:use :cl :epsilon.test :epsilon.uuid))

(in-package :epsilon.uuid.tests)

;;;; ==========================================================================
;;;; UUID v4 (Random) Tests
;;;; ==========================================================================

(deftest test-make-v4
  "Test creating a v4 UUID"
  (let ((uuid (make-v4)))
    (is (uuid-p uuid))
    (is (= 4 (uuid-version uuid)))
    (is (eq :rfc4122 (uuid-variant uuid)))))

(deftest test-make-v4-uniqueness
  "Test that v4 UUIDs are unique"
  (let ((uuid1 (make-v4))
        (uuid2 (make-v4)))
    (is-not (equal (to-string uuid1) (to-string uuid2)))))

;;;; ==========================================================================
;;;; UUID v7 (Time-ordered) Tests
;;;; ==========================================================================

(deftest test-make-v7
  "Test creating a v7 UUID"
  (let ((uuid (make-v7)))
    (is (uuid-p uuid))
    (is (= 7 (uuid-version uuid)))
    (is (eq :rfc4122 (uuid-variant uuid)))))

(deftest test-make-v7-uniqueness
  "Test that v7 UUIDs are unique"
  (let ((uuid1 (make-v7))
        (uuid2 (make-v7)))
    (is-not (equal (to-string uuid1) (to-string uuid2)))))

(deftest test-make-v7-with-timestamp
  "Test creating a v7 UUID with a specific timestamp"
  (let* ((timestamp 1704067200000) ; 2024-01-01T00:00:00Z
         (uuid (make-v7 timestamp)))
    (is (uuid-p uuid))
    (is (= 7 (uuid-version uuid)))
    (is (= timestamp (uuid-v7-timestamp-ms uuid)))))

(deftest test-v7-timestamp-extraction
  "Test extracting timestamp from v7 UUID"
  (let* ((uuid (make-v7))
         (extracted (uuid-v7-timestamp-ms uuid)))
    (is (integerp extracted))
    ;; Timestamp should be recent (within last minute)
    (let ((now-ms (epsilon.uuid::unix-timestamp-ms)))
      (is (< (abs (- now-ms extracted)) 60000)))))

(deftest test-v7-timestamp-ordering
  "Test that v7 UUIDs are time-ordered"
  (let* ((ts1 1704067200000)
         (ts2 1704067201000)
         (uuid1 (make-v7 ts1))
         (uuid2 (make-v7 ts2)))
    ;; UUID with later timestamp should sort after
    (is (string< (to-string uuid1) (to-string uuid2)))))

(deftest test-v7-timestamp-nil-for-other-versions
  "Test that uuid-v7-timestamp-ms returns nil for non-v7 UUIDs"
  (let ((v4-uuid (make-v4)))
    (is (null (uuid-v7-timestamp-ms v4-uuid)))))

;;;; ==========================================================================
;;;; UUID Conversion Tests
;;;; ==========================================================================

(deftest test-uuid-string-roundtrip
  "Test UUID string conversion roundtrip"
  (let* ((uuid (make-v4))
         (string (to-string uuid))
         (parsed (make-from-string string)))
    (is (= 36 (length string))) ; 8-4-4-4-12 with hyphens
    (is (equal (to-string uuid) (to-string parsed)))))

(deftest test-uuid-bytes-roundtrip
  "Test UUID bytes conversion roundtrip"
  (let* ((uuid (make-v4))
         (bytes (to-bytes uuid))
         (parsed (make-from-bytes bytes)))
    (is (= 16 (length bytes)))
    (is (equal (to-string uuid) (to-string parsed)))))

(deftest test-uuid-integer-roundtrip
  "Test UUID integer conversion roundtrip"
  (let* ((uuid (make-v4))
         (integer (to-integer uuid))
         (parsed (from-integer integer)))
    (is (integerp integer))
    (is (equal (to-string uuid) (to-string parsed)))))

(deftest test-v7-string-roundtrip
  "Test v7 UUID string conversion roundtrip"
  (let* ((uuid (make-v7))
         (string (to-string uuid))
         (parsed (make-from-string string)))
    (is (= 7 (uuid-version parsed)))
    (is (equal (to-string uuid) (to-string parsed)))
    ;; Timestamps should match after roundtrip
    (is (= (uuid-v7-timestamp-ms uuid) (uuid-v7-timestamp-ms parsed)))))

(deftest test-v7-bytes-roundtrip
  "Test v7 UUID bytes conversion roundtrip"
  (let* ((uuid (make-v7))
         (bytes (to-bytes uuid))
         (parsed (make-from-bytes bytes)))
    (is (= 7 (uuid-version parsed)))
    (is (equal (to-string uuid) (to-string parsed)))))

;;;; ==========================================================================
;;;; UUID Validation Tests
;;;; ==========================================================================

(deftest test-valid-uuid-p
  "Test UUID validation"
  (is (valid-uuid-p (make-v4)))
  (is (valid-uuid-p (make-v7)))
  (is (valid-uuid-p "550e8400-e29b-41d4-a716-446655440000"))
  (is-not (valid-uuid-p "invalid"))
  (is-not (valid-uuid-p "550e8400-e29b-41d4-a716-44665544"))
  (is-not (valid-uuid-p nil))
  (is-not (valid-uuid-p 12345)))

;;;; ==========================================================================
;;;; Nil UUID Tests
;;;; ==========================================================================

(deftest test-nil-uuid
  "Test nil UUID"
  (let ((nil-uuid (make-nil)))
    (is (uuid-p nil-uuid))
    (is (equal "00000000-0000-0000-0000-000000000000" (to-string nil-uuid)))
    (is (eq nil-uuid +nil-uuid+))))

;;;; ==========================================================================
;;;; UUID Components Tests
;;;; ==========================================================================

(deftest test-uuid-components
  "Test UUID component accessors"
  (let ((uuid (make-from-string "550e8400-e29b-41d4-a716-446655440000")))
    (is (= #x550e8400 (uuid-time-low uuid)))
    (is (= #xe29b (uuid-time-mid uuid)))
    (is (= #x41d4 (uuid-time-hi-and-version uuid)))
    (is (= #xa7 (uuid-clock-seq-hi-and-reserved uuid)))
    (is (= #x16 (uuid-clock-seq-low uuid)))
    (is (= #x446655440000 (uuid-node uuid)))))

(deftest test-uuid-version-detection
  "Test UUID version detection"
  (is (= 4 (uuid-version (make-v4))))
  (is (= 7 (uuid-version (make-v7))))
  (is (= 1 (uuid-version (make-v1)))))

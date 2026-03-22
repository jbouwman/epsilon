;;;; UUID Tests
;;;;
;;;; Tests for UUID generation and manipulation, including UUIDv7.

(defpackage epsilon.uuid-test
  (:use :cl :epsilon.test :epsilon.syntax :epsilon.uuid)
  (:enter t))

;;;; ==========================================================================
;;;; UUID v4 (Random) Tests
;;;; ==========================================================================

(deftest test-make-v4
  "Test creating a v4 UUID"
  (let ((uuid (make-v4)))
    (assert-true (uuid-p uuid))
    (assert-true (= 4 (uuid-version uuid)))
    (assert-true (eq :rfc4122 (uuid-variant uuid)))))

(deftest test-make-v4-uniqueness
  "Test that v4 UUIDs are unique"
  (let ((uuid1 (make-v4))
        (uuid2 (make-v4)))
    (assert-not (equal (to-string uuid1) (to-string uuid2)))))

;;;; ==========================================================================
;;;; UUID v7 (Time-ordered) Tests
;;;; ==========================================================================

(deftest test-make-v7
  "Test creating a v7 UUID"
  (let ((uuid (make-v7)))
    (assert-true (uuid-p uuid))
    (assert-true (= 7 (uuid-version uuid)))
    (assert-true (eq :rfc4122 (uuid-variant uuid)))))

(deftest test-make-v7-uniqueness
  "Test that v7 UUIDs are unique"
  (let ((uuid1 (make-v7))
        (uuid2 (make-v7)))
    (assert-not (equal (to-string uuid1) (to-string uuid2)))))

(deftest test-make-v7-with-timestamp
  "Test creating a v7 UUID with a specific timestamp"
  (let* ((timestamp 1704067200000) ; 2024-01-01T00:00:00Z
         (uuid (make-v7 timestamp)))
    (assert-true (uuid-p uuid))
    (assert-true (= 7 (uuid-version uuid)))
    (assert-true (= timestamp (uuid-v7-timestamp-ms uuid)))))

(deftest test-v7-timestamp-extraction
  "Test extracting timestamp from v7 UUID"
  (let* ((uuid (make-v7))
         (extracted (uuid-v7-timestamp-ms uuid)))
    (assert-true (integerp extracted))
    ;; Timestamp should be recent (within last minute)
    (let ((now-ms (epsilon.uuid::unix-timestamp-ms)))
      (assert-true (< (abs (- now-ms extracted)) 60000)))))

(deftest test-v7-timestamp-ordering
  "Test that v7 UUIDs are time-ordered"
  (let* ((ts1 1704067200000)
         (ts2 1704067201000)
         (uuid1 (make-v7 ts1))
         (uuid2 (make-v7 ts2)))
    ;; UUID with later timestamp should sort after
    (assert-true (string< (to-string uuid1) (to-string uuid2)))))

(deftest test-v7-timestamp-nil-for-other-versions
  "Test that uuid-v7-timestamp-ms returns nil for non-v7 UUIDs"
  (let ((v4-uuid (make-v4)))
    (assert-true (null (uuid-v7-timestamp-ms v4-uuid)))))

;;;; ==========================================================================
;;;; UUID Conversion Tests
;;;; ==========================================================================

(deftest test-uuid-string-roundtrip
  "Test UUID string conversion roundtrip"
  (let* ((uuid (make-v4))
         (string (to-string uuid))
         (parsed (make-from-string string)))
    (assert-true (= 36 (length string))) ; 8-4-4-4-12 with hyphens
    (assert-true (equal (to-string uuid) (to-string parsed)))))

(deftest test-uuid-bytes-roundtrip
  "Test UUID bytes conversion roundtrip"
  (let* ((uuid (make-v4))
         (bytes (to-bytes uuid))
         (parsed (make-from-bytes bytes)))
    (assert-true (= 16 (length bytes)))
    (assert-true (equal (to-string uuid) (to-string parsed)))))

(deftest test-uuid-integer-roundtrip
  "Test UUID integer conversion roundtrip"
  (let* ((uuid (make-v4))
         (integer (to-integer uuid))
         (parsed (from-integer integer)))
    (assert-true (integerp integer))
    (assert-true (equal (to-string uuid) (to-string parsed)))))

(deftest test-v7-string-roundtrip
  "Test v7 UUID string conversion roundtrip"
  (let* ((uuid (make-v7))
         (string (to-string uuid))
         (parsed (make-from-string string)))
    (assert-true (= 7 (uuid-version parsed)))
    (assert-true (equal (to-string uuid) (to-string parsed)))
    ;; Timestamps should match after roundtrip
    (assert-true (= (uuid-v7-timestamp-ms uuid) (uuid-v7-timestamp-ms parsed)))))

(deftest test-v7-bytes-roundtrip
  "Test v7 UUID bytes conversion roundtrip"
  (let* ((uuid (make-v7))
         (bytes (to-bytes uuid))
         (parsed (make-from-bytes bytes)))
    (assert-true (= 7 (uuid-version parsed)))
    (assert-true (equal (to-string uuid) (to-string parsed)))))

;;;; ==========================================================================
;;;; UUID Validation Tests
;;;; ==========================================================================

(deftest test-valid-uuid-p
  "Test UUID validation"
  (assert-true (valid-uuid-p (make-v4)))
  (assert-true (valid-uuid-p (make-v7)))
  (assert-true (valid-uuid-p "550e8400-e29b-41d4-a716-446655440000"))
  (assert-not (valid-uuid-p "invalid"))
  (assert-not (valid-uuid-p "550e8400-e29b-41d4-a716-44665544"))
  (assert-not (valid-uuid-p nil))
  (assert-not (valid-uuid-p 12345)))

;;;; ==========================================================================
;;;; Nil UUID Tests
;;;; ==========================================================================

(deftest test-nil-uuid
  "Test nil UUID"
  (let ((nil-uuid (make-nil)))
    (assert-true (uuid-p nil-uuid))
    (assert-true (equal "00000000-0000-0000-0000-000000000000" (to-string nil-uuid)))
    (assert-true (eq nil-uuid +nil-uuid+))))

;;;; ==========================================================================
;;;; UUID Components Tests
;;;; ==========================================================================

(deftest test-uuid-components
  "Test UUID component accessors"
  (let ((uuid (make-from-string "550e8400-e29b-41d4-a716-446655440000")))
    (assert-true (= #x550e8400 (uuid-time-low uuid)))
    (assert-true (= #xe29b (uuid-time-mid uuid)))
    (assert-true (= #x41d4 (uuid-time-hi-and-version uuid)))
    (assert-true (= #xa7 (uuid-clock-seq-hi-and-reserved uuid)))
    (assert-true (= #x16 (uuid-clock-seq-low uuid)))
    (assert-true (= #x446655440000 (uuid-node uuid)))))

(deftest test-uuid-version-detection
  "Test UUID version detection"
  (assert-true (= 4 (uuid-version (make-v4))))
  (assert-true (= 7 (uuid-version (make-v7))))
  (assert-true (= 1 (uuid-version (make-v1)))))

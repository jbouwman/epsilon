;;;; TypeID Tests
;;;;
;;;; Tests for TypeID generation, parsing, and validation.

(defpackage epsilon.typeid-test
  (:use :cl :epsilon.test :epsilon.syntax :epsilon.typeid)
  (:require (epsilon.uuid uuid))
  (:enter t))

;;;; ==========================================================================
;;;; Base32 Encoding/Decoding Tests
;;;; ==========================================================================

(deftest test-base32-encode
  "Test Crockford's base32 encoding"
  (let* ((bytes (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))
         (encoded (encode-base32 bytes)))
    (assert-true (= 26 (length encoded)))
    (assert-true (string= "00000000000000000000000000" encoded))))

(deftest test-base32-encode-max
  "Test base32 encoding with maximum value"
  (let* ((bytes (make-array 16 :element-type '(unsigned-byte 8) :initial-element 255))
         (encoded (encode-base32 bytes)))
    (assert-true (= 26 (length encoded)))
    (assert-true (string= "7zzzzzzzzzzzzzzzzzzzzzzzzz" encoded))))

(deftest test-base32-roundtrip
  "Test base32 encoding/decoding roundtrip"
  (let ((original (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (i 16)
      (setf (aref original i) (random 256)))
    (let* ((encoded (encode-base32 original))
           (decoded (decode-base32 encoded)))
      (assert-true (equalp original decoded)))))

(deftest test-base32-decode-invalid
  "Test base32 decoding with invalid input"
  (assert-true (null (decode-base32 "too-short")))
  (assert-true (null (decode-base32 "this-is-way-too-long-to-be-valid")))
  (assert-true (null (decode-base32 "invalid!chars!here!!!!!!!!")))
  ;; First character must be 0-7 (only 3 bits used)
  (assert-true (null (decode-base32 "z0000000000000000000000000"))))

(deftest test-base32-case-insensitive
  "Test base32 decoding is case-insensitive"
  (let ((lower "01h5xz0000000000000000000"))
    (assert-true (equalp (decode-base32 lower)
                (decode-base32 (string-upcase lower))))))

(deftest test-base32-confusable-chars
  "Test base32 handles confusable characters (i/l/o)"
  ;; i and l should decode as 1, o should decode as 0
  (let ((with-i "01h5xzi000000000000000000")
        (with-1 "01h5xz1000000000000000000"))
    (assert-true (equalp (decode-base32 with-i) (decode-base32 with-1))))
  (let ((with-o "01h5xz0o00000000000000000")
        (with-0 "01h5xz0000000000000000000"))
    (assert-true (equalp (decode-base32 with-o) (decode-base32 with-0)))))

;;;; ==========================================================================
;;;; Prefix Validation Tests
;;;; ==========================================================================

(deftest test-valid-prefix
  "Test valid TypeID prefixes"
  (assert-true (valid-prefix-p ""))
  (assert-true (valid-prefix-p "user"))
  (assert-true (valid-prefix-p "account"))
  (assert-true (valid-prefix-p "org_member"))
  (assert-true (valid-prefix-p "a"))
  (assert-true (valid-prefix-p (make-string 63 :initial-element #\a))))

(deftest test-invalid-prefix
  "Test invalid TypeID prefixes"
  (assert-not (valid-prefix-p "_user"))  ; starts with underscore
  (assert-not (valid-prefix-p "user_"))  ; ends with underscore
  (assert-not (valid-prefix-p "User"))   ; uppercase
  (assert-not (valid-prefix-p "user123")) ; numbers (not allowed in prefix)
  (assert-not (valid-prefix-p "user-id")) ; hyphen
  (assert-not (valid-prefix-p (make-string 64 :initial-element #\a))) ; too long
  (assert-not (valid-prefix-p nil)))

;;;; ==========================================================================
;;;; TypeID Creation Tests
;;;; ==========================================================================

(deftest test-make-typeid
  "Test creating a TypeID"
  (let ((tid (make-typeid "user")))
    (assert-true (typeid-p tid))
    (assert-true (string= "user" (typeid-prefix tid)))
    (assert-true (= 26 (length (typeid-suffix tid))))))

(deftest test-make-typeid-empty-prefix
  "Test creating a TypeID with empty prefix"
  (let ((tid (make-typeid "")))
    (assert-true (typeid-p tid))
    (assert-true (string= "" (typeid-prefix tid)))
    (assert-true (= 26 (length (typeid-suffix tid))))))

(deftest test-make-typeid-with-timestamp
  "Test creating a TypeID with specific timestamp"
  (let* ((timestamp 1704067200000)
         (tid (make-typeid "user" timestamp)))
    (assert-true (= timestamp (typeid-timestamp-ms tid)))))

(deftest test-make-typeid-uniqueness
  "Test that TypeIDs are unique"
  (let ((tid1 (make-typeid "user"))
        (tid2 (make-typeid "user")))
    (assert-not (string= (typeid-suffix tid1) (typeid-suffix tid2)))))

;;;; ==========================================================================
;;;; TypeID Parsing Tests
;;;; ==========================================================================

(deftest test-parse-typeid-with-prefix
  "Test parsing a TypeID with prefix"
  (let* ((original (make-typeid "user"))
         (string (to-string original))
         (parsed (make-from-string string)))
    (assert-true (typeid-p parsed))
    (assert-true (string= "user" (typeid-prefix parsed)))
    (assert-true (string= (typeid-suffix original) (typeid-suffix parsed)))))

(deftest test-parse-typeid-without-prefix
  "Test parsing a TypeID without prefix"
  (let* ((original (make-typeid ""))
         (string (to-string original))
         (parsed (make-from-string string)))
    (assert-true (typeid-p parsed))
    (assert-true (string= "" (typeid-prefix parsed)))
    (assert-true (string= (typeid-suffix original) (typeid-suffix parsed)))))

(deftest test-parse-typeid-invalid
  "Test parsing invalid TypeID strings"
  (assert-true (null (make-from-string "")))
  (assert-true (null (make-from-string "invalid")))
  (assert-true (null (make-from-string "user_tooshort")))
  (assert-true (null (make-from-string "_user_01h5xz0000000000000000000"))) ; invalid prefix
  (assert-true (null (make-from-string "user_01h5xz000000000000000000z"))) ; invalid base32
  (assert-true (null (make-from-string nil))))

(deftest test-parse-known-typeid
  "Test parsing a known valid TypeID"
  (let ((tid (make-from-string "user_00000000000000000000000000")))
    (assert-true (typeid-p tid))
    (assert-true (string= "user" (typeid-prefix tid)))
    (assert-true (string= "00000000000000000000000000" (typeid-suffix tid)))))

;;;; ==========================================================================
;;;; TypeID Conversion Tests
;;;; ==========================================================================

(deftest test-typeid-string-roundtrip
  "Test TypeID string conversion roundtrip"
  (let* ((tid (make-typeid "account"))
         (string (to-string tid))
         (parsed (make-from-string string)))
    (assert-true (string= (to-string tid) (to-string parsed)))))

(deftest test-typeid-uuid-roundtrip
  "Test TypeID UUID conversion"
  (let* ((tid (make-typeid "order"))
         (extracted-uuid (to-uuid tid)))
    (assert-true (uuid:uuid-p extracted-uuid))
    (assert-true (= 7 (uuid:uuid-version extracted-uuid)))))

(deftest test-make-from-uuid
  "Test creating TypeID from existing UUID"
  (let* ((uuid (uuid:make-v7))
         (tid (make-from-uuid "payment" uuid))
         (extracted (to-uuid tid)))
    (assert-true (string= "payment" (typeid-prefix tid)))
    (assert-true (string= (uuid:to-string uuid) (uuid:to-string extracted)))))

;;;; ==========================================================================
;;;; Timestamp Tests
;;;; ==========================================================================

(deftest test-typeid-timestamp
  "Test timestamp extraction from TypeID"
  (let* ((timestamp 1704067200000)
         (tid (make-typeid "event" timestamp)))
    (assert-true (= timestamp (typeid-timestamp-ms tid)))))

(deftest test-typeid-timestamp-ordering
  "Test that TypeIDs with different timestamps sort correctly"
  (let* ((ts1 1704067200000)
         (ts2 1704067201000)
         (tid1 (make-typeid "event" ts1))
         (tid2 (make-typeid "event" ts2)))
    ;; Later timestamp should result in lexicographically greater suffix
    (assert-true (string< (typeid-suffix tid1) (typeid-suffix tid2)))))

;;;; ==========================================================================
;;;; Validation Tests
;;;; ==========================================================================

(deftest test-valid-typeid-p
  "Test TypeID validation"
  (assert-true (valid-typeid-p (make-typeid "user")))
  (assert-true (valid-typeid-p (to-string (make-typeid "user"))))
  (assert-true (valid-typeid-p "user_00000000000000000000000000"))
  (assert-true (valid-typeid-p "00000000000000000000000000")) ; no prefix
  (assert-not (valid-typeid-p "invalid"))
  (assert-not (valid-typeid-p "User_00000000000000000000000000")) ; uppercase prefix
  (assert-not (valid-typeid-p nil))
  (assert-not (valid-typeid-p 12345)))

;;;; ==========================================================================
;;;; String Representation Tests
;;;; ==========================================================================

(deftest test-typeid-string-format
  "Test TypeID string format"
  (let ((tid-with-prefix (make-typeid "user"))
        (tid-no-prefix (make-typeid "")))
    ;; With prefix: prefix_suffix
    (assert-true (= (+ 4 1 26) (length (to-string tid-with-prefix)))) ; "user" + "_" + 26
    (assert-true (char= #\_ (char (to-string tid-with-prefix) 4)))
    ;; Without prefix: just suffix
    (assert-true (= 26 (length (to-string tid-no-prefix))))))

(deftest test-typeid-print
  "Test TypeID print representation"
  (let ((tid (make-typeid "user")))
    (assert-true (string= (to-string tid)
                 (format nil "~A" tid)))))

;;;; ==========================================================================
;;;; Edge Cases
;;;; ==========================================================================

(deftest test-typeid-with-underscores-in-prefix
  "Test TypeID with underscores in prefix"
  (let ((tid (make-typeid "org_member")))
    (assert-true (string= "org_member" (typeid-prefix tid)))
    ;; Parsing should use last underscore as separator
    (let* ((string (to-string tid))
           (parsed (make-from-string string)))
      (assert-true (string= "org_member" (typeid-prefix parsed))))))

(deftest test-typeid-max-prefix-length
  "Test TypeID with maximum prefix length (63 chars)"
  (let* ((prefix (make-string 63 :initial-element #\a))
         (tid (make-typeid prefix)))
    (assert-true (string= prefix (typeid-prefix tid)))
    (let* ((string (to-string tid))
           (parsed (make-from-string string)))
      (assert-true (string= prefix (typeid-prefix parsed))))))

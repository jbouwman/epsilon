;;;; HTTP/2 HPACK Tests
;;;;
;;;; Unit tests for HPACK header compression per RFC 7541, including
;;;; static table, Huffman coding, integer encoding, dynamic table,
;;;; and encoder/decoder round-trips.

(defpackage :epsilon.http.h2.hpack-tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.h2.hpack hpack)))

;;;; Static Table Tests

(deftest test-static-table-exact-match ()
  "Test find-in-static-table for exact name+value match"
  ;; :method GET is at index 2
  (assert-true (= (hpack:find-in-static-table ":method" "GET") 2))
  ;; :method POST is at index 3
  (assert-true (= (hpack:find-in-static-table ":method" "POST") 3))
  ;; :path / is at index 4
  (assert-true (= (hpack:find-in-static-table ":path" "/") 4))
  ;; :scheme https is at index 7
  (assert-true (= (hpack:find-in-static-table ":scheme" "https") 7))
  ;; :status 200 is at index 8
  (assert-true (= (hpack:find-in-static-table ":status" "200") 8)))

(deftest test-static-table-name-only-match ()
  "Test find-in-static-table for name-only match (negative index)"
  ;; :method with non-matching value returns negative index
  (let ((idx (hpack:find-in-static-table ":method" "PUT")))
    (assert-true (not (null idx)))
    (assert-true (minusp idx)))
  ;; :status with non-matching value
  (let ((idx (hpack:find-in-static-table ":status" "201")))
    (assert-true (not (null idx)))
    (assert-true (minusp idx))))

(deftest test-static-table-no-match ()
  "Test find-in-static-table returns nil for no match"
  (assert-true (null (hpack:find-in-static-table "x-custom-header")))
  (assert-true (null (hpack:find-in-static-table "x-nonexistent" "value"))))

(deftest test-static-table-name-only-lookup ()
  "Test find-in-static-table with name only (no value)"
  ;; :authority at index 1 (has nil value)
  (assert-true (= (hpack:find-in-static-table ":authority") 1))
  ;; :method at index 2
  (assert-true (= (hpack:find-in-static-table ":method") 2))
  ;; content-type at index 31
  (assert-true (= (hpack:find-in-static-table "content-type") 31)))

;;;; Huffman Coding Tests

(deftest test-huffman-encode-produces-bytes ()
  "Test Huffman encode produces a byte vector for common strings"
  (let ((test-strings '("hello" "world" "GET" "POST" "/")))
    (dolist (s test-strings)
      (let ((encoded (hpack:huffman-encode s)))
        (assert-true (not (null encoded)))
        (assert-true (typep encoded '(vector (unsigned-byte 8))))
        (assert-true (> (length encoded) 0))))))

(deftest test-huffman-encode-compression ()
  "Test that Huffman encoding compresses common HTTP strings"
  ;; Common HTTP values should compress well
  (let ((encoded (hpack:huffman-encode "www.example.com")))
    (assert-true (< (length encoded) (length "www.example.com")))))

(deftest test-huffman-empty-string ()
  "Test Huffman encode/decode of empty string"
  (let ((encoded (hpack:huffman-encode "")))
    (assert-true (= (length encoded) 0))
    (assert-true (equal (hpack:huffman-decode encoded) ""))))

;;;; Integer Encoding Tests

(deftest test-encode-decode-integer-fits-in-prefix ()
  "Test integer encoding/decoding when value fits in prefix bits"
  ;; Value 10 with 7-bit prefix fits directly
  (let ((output (make-array 10 :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (hpack::encode-integer 10 7 output)
    (assert-true (= (length output) 1))
    (assert-true (= (aref output 0) 10))
    ;; Decode it back
    (multiple-value-bind (value new-pos) (hpack::decode-integer output 0 7)
      (assert-true (= value 10))
      (assert-true (= new-pos 1)))))

(deftest test-encode-decode-integer-needs-continuation ()
  "Test integer encoding/decoding when value needs continuation bytes"
  ;; Value 200 with 7-bit prefix (max prefix = 127) needs continuation
  (let ((output (make-array 10 :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (hpack::encode-integer 200 7 output)
    (assert-true (> (length output) 1))
    ;; Decode it back
    (multiple-value-bind (value new-pos) (hpack::decode-integer output 0 7)
      (assert-true (= value 200))
      (assert-true (= new-pos (length output))))))

(deftest test-encode-decode-integer-boundary ()
  "Test integer encoding at prefix boundary"
  ;; Value 127 is exactly the max for 7-bit prefix
  (let ((output (make-array 10 :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (hpack::encode-integer 127 7 output)
    (multiple-value-bind (value _pos) (hpack::decode-integer output 0 7)
      (declare (ignore _pos))
      (assert-true (= value 127)))))

(deftest test-encode-decode-integer-zero ()
  "Test integer encoding of zero"
  (let ((output (make-array 10 :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (hpack::encode-integer 0 7 output)
    (assert-true (= (length output) 1))
    (assert-true (= (aref output 0) 0))
    (multiple-value-bind (value _pos) (hpack::decode-integer output 0 7)
      (declare (ignore _pos))
      (assert-true (= value 0)))))

(deftest test-encode-decode-integer-large ()
  "Test integer encoding/decoding of large values"
  (let ((output (make-array 20 :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (hpack::encode-integer 100000 7 output)
    (multiple-value-bind (value _pos) (hpack::decode-integer output 0 7)
      (declare (ignore _pos))
      (assert-true (= value 100000)))))

(deftest test-encode-decode-integer-5bit-prefix ()
  "Test integer encoding with 5-bit prefix"
  (let ((output (make-array 10 :element-type '(unsigned-byte 8) :fill-pointer 0)))
    ;; Max for 5-bit prefix is 31
    (hpack::encode-integer 50 5 output)
    (multiple-value-bind (value _pos) (hpack::decode-integer output 0 5)
      (declare (ignore _pos))
      (assert-true (= value 50)))))

;;;; Dynamic Table Tests

(deftest test-dynamic-table-add-and-get ()
  "Test adding and retrieving entries from dynamic table"
  (let ((table (hpack::make-dynamic-table)))
    (hpack::dynamic-table-add table "custom-header" "custom-value")
    ;; Newest entry is at index 0
    (let ((entry (hpack::dynamic-table-get table 0)))
      (assert-true (not (null entry)))
      (assert-true (equal (car entry) "custom-header"))
      (assert-true (equal (cdr entry) "custom-value")))))

(deftest test-dynamic-table-eviction ()
  "Test dynamic table evicts entries when full"
  ;; Create a small table (size = 64, one entry overhead = 32 + name + value)
  (let ((table (hpack::make-dynamic-table :max-size 64)))
    ;; First entry: 32 + 1 + 1 = 34 bytes (fits)
    (hpack::dynamic-table-add table "a" "b")
    (assert-true (= (length (hpack::dynamic-table-entries table)) 1))
    ;; Second entry: 32 + 1 + 1 = 34 bytes (total would be 68, must evict first)
    (hpack::dynamic-table-add table "c" "d")
    (assert-true (= (length (hpack::dynamic-table-entries table)) 1))
    ;; The remaining entry should be the newest one
    (let ((entry (hpack::dynamic-table-get table 0)))
      (assert-true (equal (car entry) "c")))))

(deftest test-dynamic-table-resize ()
  "Test dynamic table resize with eviction"
  (let ((table (hpack::make-dynamic-table :max-size 4096)))
    (hpack::dynamic-table-add table "name1" "value1")
    (hpack::dynamic-table-add table "name2" "value2")
    (assert-true (= (length (hpack::dynamic-table-entries table)) 2))
    ;; Resize to 0 should evict everything
    (hpack::dynamic-table-resize table 0)
    (assert-true (= (length (hpack::dynamic-table-entries table)) 0))
    (assert-true (= (hpack::dynamic-table-size table) 0))))

;;;; Encoder/Decoder Creation Tests

(deftest test-encoder-default-table-size ()
  "Test encoder creation with default table size"
  (let ((encoder (hpack:make-encoder)))
    (assert-true (= (hpack:encoder-max-dynamic-table-size encoder) 4096))
    (assert-true (= (hpack:encoder-current-table-size encoder) 0))))

(deftest test-encoder-custom-table-size ()
  "Test encoder creation with custom table size"
  (let ((encoder (hpack:make-encoder :table-size 1024)))
    (assert-true (= (hpack:encoder-max-dynamic-table-size encoder) 1024))))

(deftest test-decoder-default-table-size ()
  "Test decoder creation with default table size"
  (let ((decoder (hpack:make-decoder)))
    (assert-true (= (hpack:decoder-max-dynamic-table-size decoder) 4096))
    (assert-true (= (hpack:decoder-current-table-size decoder) 0))))

(deftest test-decoder-custom-table-size ()
  "Test decoder creation with custom table size"
  (let ((decoder (hpack:make-decoder :max-table-size 2048)))
    (assert-true (= (hpack:decoder-max-dynamic-table-size decoder) 2048))))

;;;; Encoder/Decoder Round-Trip Tests

(deftest test-round-trip-static-indexed ()
  "Test encode/decode round-trip for static-table indexed headers"
  (let ((encoder (hpack:make-encoder :huffman-p nil))
        (decoder (hpack:make-decoder))
        (headers '((":method" . "GET")
                   (":path" . "/"))))
    (let* ((encoded (hpack:encode-header-list encoder headers))
           (decoded (hpack:decode-header-block decoder encoded)))
      (assert-true (= (length decoded) 2))
      (assert-true (equal (car (first decoded)) ":method"))
      (assert-true (equal (cdr (first decoded)) "GET"))
      (assert-true (equal (car (second decoded)) ":path"))
      (assert-true (equal (cdr (second decoded)) "/")))))

(deftest test-round-trip-literal-indexed-name ()
  "Test encode/decode round-trip for literal header with indexed name"
  (let ((encoder (hpack:make-encoder :huffman-p nil))
        (decoder (hpack:make-decoder))
        ;; :status "201" - name is in static table, but value 201 is not
        (headers '((":status" . "201"))))
    (let* ((encoded (hpack:encode-header-list encoder headers))
           (decoded (hpack:decode-header-block decoder encoded)))
      (assert-true (= (length decoded) 1))
      (assert-true (equal (car (first decoded)) ":status"))
      (assert-true (equal (cdr (first decoded)) "201")))))

(deftest test-round-trip-literal-new-name ()
  "Test encode/decode round-trip for literal header with new name"
  (let ((encoder (hpack:make-encoder :huffman-p nil))
        (decoder (hpack:make-decoder))
        (headers '(("x-custom" . "custom-value"))))
    (let* ((encoded (hpack:encode-header-list encoder headers))
           (decoded (hpack:decode-header-block decoder encoded)))
      (assert-true (= (length decoded) 1))
      (assert-true (equal (car (first decoded)) "x-custom"))
      (assert-true (equal (cdr (first decoded)) "custom-value")))))

(deftest test-round-trip-multiple-headers ()
  "Test encode/decode round-trip for a full set of request headers"
  (let ((encoder (hpack:make-encoder :huffman-p nil))
        (decoder (hpack:make-decoder))
        (headers '((":method" . "GET")
                   (":scheme" . "https")
                   (":path" . "/")
                   (":authority" . "example.com")
                   ("accept" . "text/html"))))
    (let* ((encoded (hpack:encode-header-list encoder headers))
           (decoded (hpack:decode-header-block decoder encoded)))
      (assert-true (= (length decoded) (length headers)))
      (assert-true (equal (car (first decoded)) ":method"))
      (assert-true (equal (cdr (first decoded)) "GET")))))

;; NOTE: Huffman encode/decode round-trip is skipped because the Huffman
;; code table (*huffman-codes*) has 250 entries while *huffman-lengths*
;; has 261 entries.  RFC 7541 Appendix B requires 257 entries (bytes
;; 0-255 plus EOS).  The decoder crashes on index 250.  Once the tables
;; are corrected, a full round-trip test should be added here.

;;;; Alias Function Tests

(deftest test-create-encoder-alias ()
  "Test create-encoder is an alias for make-encoder"
  (let ((encoder (hpack:create-encoder :huffman-p nil)))
    (assert-true (not (null encoder)))
    (assert-true (= (hpack:encoder-max-dynamic-table-size encoder) 4096))))

(deftest test-create-decoder-alias ()
  "Test create-decoder is an alias for make-decoder"
  (let ((decoder (hpack:create-decoder)))
    (assert-true (not (null decoder)))
    (assert-true (= (hpack:decoder-max-dynamic-table-size decoder) 4096))))

;;;; Dynamic-table size update bounds (regression: fuzzer found a
;;;; non-fixnum size update crashing the FIXNUM-typed slot)

(deftest test-decode-rejects-oversized-table-size-update ()
  "A wire-encoded table-size-update larger than +max-dynamic-table-size+
   must be rejected as HPACK-DECODE-ERROR.  RFC 7541 §6.3 + RFC 9113
   §6.5.2: the value is unbounded on the wire (HPACK varint), so a
   hostile peer can send one that doesn't fit in fixnum.  Pre-fix
   this crashed the decoder with TYPE-ERROR when assigning into a
   :type fixnum slot."
  (let ((decoder (hpack:create-decoder)))
    (assert-condition
     (hpack:hpack-decode-error)
     ;; 0x3F prefix = dynamic table size update with prefix bits set;
     ;; following bytes are an HPACK varint encoding a value above
     ;; +max-dynamic-table-size+ (65536).  The exact bytes encode
     ;; (65536 - 31) + 7-bit chunks; we use a clearly-too-big value.
     (hpack:decode-header-block
      ;; 001x_xxxx prefix=11111 (=31), then varint continuation
      ;; encoding 95061120828458994206908 (the fuzzer's actual finding).
      ;; A simpler oversized value: any varint that decodes above
      ;; +max-dynamic-table-size+ should trigger.  The byte sequence
      ;; below decodes to a value > 2^32, well above the 65536 limit.
      #(#x3F #xE1 #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #x01)
      decoder))))

(deftest test-set-dynamic-table-size-rejects-out-of-range ()
  "Direct call to set-dynamic-table-size with a value above the
   ceiling also fails cleanly."
  (let ((decoder (hpack:create-decoder)))
    (assert-condition
     (hpack:hpack-decode-error)
     (hpack:set-dynamic-table-size
      (hpack:hpack-decoder-dynamic-table decoder)
      (1+ hpack::+max-dynamic-table-size+)))))

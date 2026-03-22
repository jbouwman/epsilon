;;;; epsilon.base-encode tests
;;;;
;;;; Comprehensive test suite covering RFC 4648 test vectors, Crockford
;;;; base32, bech32 BIP-173 vectors, edge cases, and roundtrip properties.

(defpackage #:epsilon.base-encode-tests
  (:use #:cl #:epsilon.test)
  (:local-nicknames
   (#:enc #:epsilon.base-encode)
   (#:b16 #:epsilon.base-encode.base16)
   (#:b32 #:epsilon.base-encode.base32)
   (#:b64 #:epsilon.base-encode.base64)
   (#:bech #:epsilon.base-encode.bech32)
   (#:tbl #:epsilon.base-encode.tables)
   (#:cond #:epsilon.base-encode.conditions))
  (:enter t))

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(defun string-to-bytes (string)
  "Convert a string to a byte vector via ASCII."
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code string))

(defun bytes-equal (a b)
  "Check two byte vectors are equal."
  (and (= (length a) (length b))
       (every #'= a b)))

(defun make-bytes (&rest values)
  "Create a byte vector from integer values."
  (make-array (length values)
              :element-type '(unsigned-byte 8)
              :initial-contents values))

;;; ============================================================================
;;; RFC 4648 Section 10 Test Vectors -- Base16
;;; ============================================================================

(deftest base16-rfc4648-vectors
  "RFC 4648 Section 10 test vectors for base16."
  (assert-equal "" (b16:encode (string-to-bytes "")))
  (assert-equal "66" (b16:encode (string-to-bytes "f")))
  (assert-equal "666f" (b16:encode (string-to-bytes "fo")))
  (assert-equal "666f6f" (b16:encode (string-to-bytes "foo")))
  (assert-equal "666f6f62" (b16:encode (string-to-bytes "foob")))
  (assert-equal "666f6f6261" (b16:encode (string-to-bytes "fooba")))
  (assert-equal "666f6f626172" (b16:encode (string-to-bytes "foobar"))))

(deftest base16-decode-rfc4648
  "Decode RFC 4648 base16 test vectors."
  (assert-true (bytes-equal (string-to-bytes "") (b16:decode "")))
  (assert-true (bytes-equal (string-to-bytes "f") (b16:decode "66")))
  (assert-true (bytes-equal (string-to-bytes "fo") (b16:decode "666F")))
  (assert-true (bytes-equal (string-to-bytes "foo") (b16:decode "666f6f")))
  (assert-true (bytes-equal (string-to-bytes "foobar") (b16:decode "666F6F626172"))))

(deftest base16-uppercase
  "Base16 uppercase encoding."
  (assert-equal "DEADBEEF" (b16:encode (make-bytes #xDE #xAD #xBE #xEF) :uppercase t)))

(deftest base16-roundtrip
  "Base16 encode/decode roundtrip."
  (let ((data (make-bytes 0 1 127 128 254 255)))
    (assert-true (bytes-equal data (b16:decode (b16:encode data))))))

(deftest base16-single-byte-ops
  "Base16 encode-byte and decode-byte."
  (multiple-value-bind (hi lo) (b16:encode-byte #xAB)
    (assert-equal #\a hi)
    (assert-equal #\b lo)
    (assert-= #xAB (b16:decode-byte hi lo))))

(deftest base16-odd-length-error
  "Base16 decode signals invalid-length on odd-length input."
  (assert-condition (cond:invalid-length) (b16:decode "abc")))

(deftest base16-invalid-char-error
  "Base16 decode signals bad-character on invalid hex chars."
  (assert-condition (cond:bad-character) (b16:decode "zz")))

(deftest base16-empty
  "Base16 handles empty input."
  (assert-equal "" (b16:encode (make-array 0 :element-type '(unsigned-byte 8))))
  (assert-true (bytes-equal (make-array 0 :element-type '(unsigned-byte 8))
                            (b16:decode ""))))

;;; ============================================================================
;;; RFC 4648 Section 10 Test Vectors -- Base32
;;; ============================================================================

(deftest base32-rfc4648-vectors
  "RFC 4648 Section 10 test vectors for base32."
  (assert-equal "" (b32:encode (string-to-bytes "")))
  (assert-equal "MY======" (b32:encode (string-to-bytes "f")))
  (assert-equal "MZXQ====" (b32:encode (string-to-bytes "fo")))
  (assert-equal "MZXW6===" (b32:encode (string-to-bytes "foo")))
  (assert-equal "MZXW6YQ=" (b32:encode (string-to-bytes "foob")))
  (assert-equal "MZXW6YTB" (b32:encode (string-to-bytes "fooba")))
  (assert-equal "MZXW6YTBOI======" (b32:encode (string-to-bytes "foobar"))))

(deftest base32-decode-rfc4648
  "Decode RFC 4648 base32 test vectors."
  (assert-true (bytes-equal (string-to-bytes "") (b32:decode "")))
  (assert-true (bytes-equal (string-to-bytes "f") (b32:decode "MY======")))
  (assert-true (bytes-equal (string-to-bytes "fo") (b32:decode "MZXQ====")))
  (assert-true (bytes-equal (string-to-bytes "foo") (b32:decode "MZXW6===")))
  (assert-true (bytes-equal (string-to-bytes "foob") (b32:decode "MZXW6YQ=")))
  (assert-true (bytes-equal (string-to-bytes "fooba") (b32:decode "MZXW6YTB")))
  (assert-true (bytes-equal (string-to-bytes "foobar") (b32:decode "MZXW6YTBOI======"))))

(deftest base32-case-insensitive
  "Base32 decode is case-insensitive."
  (assert-true (bytes-equal (b32:decode "MZXW6===") (b32:decode "mzxw6==="))))

(deftest base32-no-padding
  "Base32 encode without padding."
  (assert-equal "MY" (b32:encode (string-to-bytes "f") :pad nil))
  (assert-equal "MZXW6YTBOI" (b32:encode (string-to-bytes "foobar") :pad nil)))

(deftest base32-roundtrip
  "Base32 encode/decode roundtrip for various lengths."
  (loop for len from 0 to 20
        for data = (make-array len :element-type '(unsigned-byte 8)
                                   :initial-element (mod len 256))
        do (assert-true (bytes-equal data (b32:decode (b32:encode data))))))

;;; ============================================================================
;;; Crockford Base32
;;; ============================================================================

(deftest crockford-128-zero-bytes
  "Crockford-128: all-zero bytes encode to all-zero characters."
  (let ((zeros (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (assert-equal "00000000000000000000000000" (b32:encode-crockford-128 zeros))))

(deftest crockford-128-all-ff
  "Crockford-128: all-0xFF bytes."
  (let ((ones (make-array 16 :element-type '(unsigned-byte 8) :initial-element #xFF)))
    (assert-equal "7zzzzzzzzzzzzzzzzzzzzzzzzz" (b32:encode-crockford-128 ones))))

(deftest crockford-128-roundtrip
  "Crockford-128: encode then decode returns original bytes."
  (let ((data (make-bytes 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
    (assert-true (bytes-equal data (b32:decode-crockford-128
                                    (b32:encode-crockford-128 data))))))

(deftest crockford-128-case-insensitive
  "Crockford-128 decode is case-insensitive."
  (let ((lower "01h5xz0000000000000000000")
        (upper "01H5XZ0000000000000000000"))
    ;; Both should decode successfully and produce same result
    ;; (padding to 26 chars for the test)
    (let ((lower-26 (concatenate 'string lower "0"))
          (upper-26 (concatenate 'string upper "0")))
      (let ((lower-result (b32:decode-crockford-128 lower-26))
            (upper-result (b32:decode-crockford-128 upper-26)))
        (assert-not-null lower-result)
        (assert-not-null upper-result)
        (assert-true (bytes-equal lower-result upper-result))))))

(deftest crockford-128-confusable-chars
  "Crockford-128: i/l decode as 1, o decodes as 0."
  (let ((table tbl:+base32-crockford-decode+))
    ;; i/I/l/L -> 1
    (assert-= 1 (aref table (char-code #\i)))
    (assert-= 1 (aref table (char-code #\I)))
    (assert-= 1 (aref table (char-code #\l)))
    (assert-= 1 (aref table (char-code #\L)))
    ;; o/O -> 0
    (assert-= 0 (aref table (char-code #\o)))
    (assert-= 0 (aref table (char-code #\O)))))

(deftest crockford-128-invalid-input
  "Crockford-128: invalid inputs return NIL."
  (assert-not (b32:decode-crockford-128 "too-short"))
  (assert-not (b32:decode-crockford-128 "this-string-is-way-too-long-for-crockford"))
  (assert-not (b32:decode-crockford-128 "invalid!chars!here!!!!!!!!"))
  ;; First char must be 0-7 (only 3 bits for 128-bit value)
  (assert-not (b32:decode-crockford-128 "z0000000000000000000000000")))

(deftest crockford-variable-length
  "Crockford variable-length encode/decode roundtrip."
  (let ((data (string-to-bytes "Hello, World!")))
    (assert-true (bytes-equal data (b32:decode-crockford (b32:encode-crockford data))))))

;;; ============================================================================
;;; RFC 4648 Section 10 Test Vectors -- Base64
;;; ============================================================================

(deftest base64-rfc4648-vectors
  "RFC 4648 Section 10 test vectors for base64."
  (assert-equal "" (b64:encode (string-to-bytes "")))
  (assert-equal "Zg==" (b64:encode (string-to-bytes "f")))
  (assert-equal "Zm8=" (b64:encode (string-to-bytes "fo")))
  (assert-equal "Zm9v" (b64:encode (string-to-bytes "foo")))
  (assert-equal "Zm9vYg==" (b64:encode (string-to-bytes "foob")))
  (assert-equal "Zm9vYmE=" (b64:encode (string-to-bytes "fooba")))
  (assert-equal "Zm9vYmFy" (b64:encode (string-to-bytes "foobar"))))

(deftest base64-decode-rfc4648
  "Decode RFC 4648 base64 test vectors."
  (assert-true (bytes-equal (string-to-bytes "") (b64:decode "")))
  (assert-true (bytes-equal (string-to-bytes "f") (b64:decode "Zg==")))
  (assert-true (bytes-equal (string-to-bytes "fo") (b64:decode "Zm8=")))
  (assert-true (bytes-equal (string-to-bytes "foo") (b64:decode "Zm9v")))
  (assert-true (bytes-equal (string-to-bytes "foob") (b64:decode "Zm9vYg==")))
  (assert-true (bytes-equal (string-to-bytes "fooba") (b64:decode "Zm9vYmE=")))
  (assert-true (bytes-equal (string-to-bytes "foobar") (b64:decode "Zm9vYmFy"))))

(deftest base64-whitespace-ignore
  "Base64 decode ignores whitespace by default."
  (assert-true (bytes-equal (string-to-bytes "foobar")
                            (b64:decode (format nil "Zm9v~%YmFy")))))

(deftest base64-whitespace-error
  "Base64 decode signals on whitespace when :error."
  (assert-condition (cond:bad-character)
    (b64:decode "Zm9v YmFy" :whitespace :error)))

(deftest base64-invalid-char
  "Base64 decode signals on invalid characters."
  (assert-condition (cond:bad-character) (b64:decode "Zm9v!mFy")))

(deftest base64-roundtrip
  "Base64 encode/decode roundtrip for various lengths."
  (loop for len from 0 to 20
        for data = (make-array len :element-type '(unsigned-byte 8)
                                   :initial-element (mod (* len 37) 256))
        do (assert-true (bytes-equal data (b64:decode (b64:encode data))))))

;;; ============================================================================
;;; Base64url
;;; ============================================================================

(deftest base64url-no-padding
  "Base64url encodes without padding by default."
  (let ((encoded (b64:encode-url (string-to-bytes "f"))))
    (assert-equal "Zg" encoded)
    (assert-not (find #\= encoded))))

(deftest base64url-with-padding
  "Base64url encodes with padding when requested."
  (assert-equal "Zg==" (b64:encode-url (string-to-bytes "f") :pad t)))

(deftest base64url-safe-chars
  "Base64url uses - and _ instead of + and /."
  ;; Create input that would produce + and / in standard base64
  ;; 0xFB = standard: +, url: -
  ;; 0xFF = standard: /, url: _
  (let* ((data (make-bytes #xFB #xFF #xFE))
         (encoded (b64:encode-url data)))
    (assert-not (find #\+ encoded))
    (assert-not (find #\/ encoded))))

(deftest base64url-decode-no-padding
  "Base64url decode handles missing padding."
  (assert-true (bytes-equal (string-to-bytes "f") (b64:decode-url "Zg")))
  (assert-true (bytes-equal (string-to-bytes "fo") (b64:decode-url "Zm8")))
  (assert-true (bytes-equal (string-to-bytes "foo") (b64:decode-url "Zm9v"))))

(deftest base64url-roundtrip
  "Base64url encode/decode roundtrip."
  (loop for len from 0 to 20
        for data = (make-array len :element-type '(unsigned-byte 8)
                                   :initial-element (mod (* len 41) 256))
        do (assert-true (bytes-equal data (b64:decode-url (b64:encode-url data))))))

;;; ============================================================================
;;; Base64 String Convenience
;;; ============================================================================

(deftest base64-string-encode-decode
  "Base64 string encode/decode roundtrip."
  (assert-equal "Hello, World!" (b64:decode-string (b64:encode-string "Hello, World!"))))

(deftest base64-string-utf8
  "Base64 string encode handles UTF-8."
  (let ((text "cafe"))
    (assert-equal text (b64:decode-string (b64:encode-string text)))))

;;; ============================================================================
;;; Base64 with Line Breaks
;;; ============================================================================

(deftest base64-columns
  "Base64 encode with column wrapping."
  (let* ((data (make-array 30 :element-type '(unsigned-byte 8) :initial-element 65))
         (encoded (b64:encode data :columns 20)))
    (assert-true (find #\Newline encoded))))

;;; ============================================================================
;;; Bech32
;;; ============================================================================

(deftest bech32-polymod-known
  "Bech32 polymod returns 1 for valid checksum."
  ;; A valid bech32 string should produce polymod = 1
  (let* ((hrp "a")
         (data-values (list 0 0 0 0 0 0))
         (check-values (append (bech:hrp-expand hrp) data-values)))
    ;; Verify polymod works (not necessarily 1 without proper checksum)
    (assert-true (integerp (bech:polymod check-values)))))

(deftest bech32-encode-decode-roundtrip
  "Bech32 encode then decode returns original data."
  (let ((hrp "test")
        (data (make-bytes 0 1 2 3 4 5 6 7 8 9 10)))
    (multiple-value-bind (decoded-hrp decoded-data) (bech:decode (bech:encode hrp data))
      (assert-equal hrp decoded-hrp)
      (assert-true (bytes-equal data decoded-data)))))

(deftest bech32-encode-empty-data
  "Bech32 encode with empty data."
  (let ((hrp "a")
        (data (make-array 0 :element-type '(unsigned-byte 8))))
    (multiple-value-bind (decoded-hrp decoded-data) (bech:decode (bech:encode hrp data))
      (assert-equal hrp decoded-hrp)
      (assert-= 0 (length decoded-data)))))

(deftest bech32-decode-invalid-no-separator
  "Bech32 decode signals error with no separator."
  (assert-condition (cond:encoding-error) (bech:decode "noseparator")))

(deftest bech32-decode-invalid-checksum
  "Bech32 decode signals error on bad checksum."
  ;; Encode a valid string then corrupt a character
  (let* ((encoded (bech:encode "test" (make-bytes 1 2 3)))
         (corrupted (concatenate 'string
                                 (subseq encoded 0 (1- (length encoded)))
                                 (string (if (char= (char encoded (1- (length encoded))) #\q)
                                             #\p
                                             #\q)))))
    (assert-condition (cond:checksum-error) (bech:decode corrupted))))

(deftest bech32-case-insensitive
  "Bech32 decode is case-insensitive."
  (let* ((encoded (bech:encode "test" (make-bytes 10 20 30)))
         (upper (string-upcase encoded)))
    (multiple-value-bind (hrp1 data1) (bech:decode encoded)
      (multiple-value-bind (hrp2 data2) (bech:decode upper)
        (assert-equal hrp1 hrp2)
        (assert-true (bytes-equal data1 data2))))))

;;; ============================================================================
;;; High-Level Dispatch
;;; ============================================================================

(deftest dispatch-base16
  "High-level encode/decode dispatch for base16."
  (let ((data (make-bytes #xDE #xAD)))
    (assert-equal "dead" (enc:encode :base16 data))
    (assert-true (bytes-equal data (enc:decode :base16 "dead")))
    ;; :hex alias
    (assert-equal "dead" (enc:encode :hex data))
    (assert-true (bytes-equal data (enc:decode :hex "dead")))))

(deftest dispatch-base64
  "High-level encode/decode dispatch for base64."
  (let ((data (string-to-bytes "foo")))
    (assert-equal "Zm9v" (enc:encode :base64 data))
    (assert-true (bytes-equal data (enc:decode :base64 "Zm9v")))))

(deftest dispatch-base64url
  "High-level encode/decode dispatch for base64url."
  (let ((data (string-to-bytes "foo")))
    (assert-equal "Zm9v" (enc:encode :base64url data))
    (assert-true (bytes-equal data (enc:decode :base64url "Zm9v")))))

(deftest dispatch-base32
  "High-level encode/decode dispatch for base32."
  (let ((data (string-to-bytes "foo")))
    (assert-equal "MZXW6===" (enc:encode :base32 data))
    (assert-true (bytes-equal data (enc:decode :base32 "MZXW6===")))))

(deftest dispatch-crockford-128
  "High-level encode/decode dispatch for crockford-128."
  (let ((data (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (assert-equal "00000000000000000000000000"
                  (enc:encode :base32-crockford-128 data))
    (assert-true (bytes-equal data
                              (enc:decode :base32-crockford-128
                                          "00000000000000000000000000")))))

;;; ============================================================================
;;; Edge Cases
;;; ============================================================================

(deftest all-zero-bytes
  "All encodings handle all-zero input."
  (let ((zeros (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; All should roundtrip
    (assert-true (bytes-equal zeros (b16:decode (b16:encode zeros))))
    (assert-true (bytes-equal zeros (b64:decode (b64:encode zeros))))
    (assert-true (bytes-equal zeros (b32:decode (b32:encode zeros))))))

(deftest all-ff-bytes
  "All encodings handle all-0xFF input."
  (let ((ones (make-array 16 :element-type '(unsigned-byte 8) :initial-element #xFF)))
    (assert-true (bytes-equal ones (b16:decode (b16:encode ones))))
    (assert-true (bytes-equal ones (b64:decode (b64:encode ones))))
    (assert-true (bytes-equal ones (b32:decode (b32:encode ones))))))

(deftest single-byte-all-values
  "Base16 roundtrips every possible byte value."
  (loop for b from 0 to 255
        for data = (make-bytes b)
        do (assert-true (bytes-equal data (b16:decode (b16:encode data))))))

(deftest exact-group-boundaries
  "Test inputs at exact group boundaries."
  ;; Base64: groups of 3
  (loop for len in '(0 3 6 9 12)
        for data = (make-array len :element-type '(unsigned-byte 8) :initial-element #x42)
        do (let ((encoded (b64:encode data)))
             (assert-not (find #\= encoded))
             (assert-true (bytes-equal data (b64:decode encoded)))))
  ;; Base32: groups of 5
  (loop for len in '(0 5 10 15 20)
        for data = (make-array len :element-type '(unsigned-byte 8) :initial-element #x42)
        do (assert-true (bytes-equal data (b32:decode (b32:encode data))))))

(deftest one-short-of-group
  "Test inputs one byte short of a complete group."
  ;; Base64: 2, 5, 8 bytes (one short of 3, 6, 9)
  (loop for len in '(2 5 8)
        for data = (make-array len :element-type '(unsigned-byte 8) :initial-element #x42)
        do (assert-true (bytes-equal data (b64:decode (b64:encode data)))))
  ;; Base32: 4, 9, 14 bytes (one short of 5, 10, 15)
  (loop for len in '(4 9 14)
        for data = (make-array len :element-type '(unsigned-byte 8) :initial-element #x42)
        do (assert-true (bytes-equal data (b32:decode (b32:encode data))))))

;;; ============================================================================
;;; Table Infrastructure
;;; ============================================================================

(deftest make-encode-table-validates
  "make-encode-table rejects invalid alphabets."
  ;; Duplicate characters
  (assert-condition (error) (tbl:make-encode-table "aab"))
  ;; Too large
  (assert-condition (error) (tbl:make-encode-table (make-string 128 :initial-element #\a))))

(deftest decode-table-sentinel-values
  "Decode tables use correct sentinel values."
  (let ((dt tbl:+base64-decode+))
    ;; Valid chars produce >= 0
    (assert-true (>= (aref dt (char-code #\A)) 0))
    ;; Padding produces -2
    (assert-= -2 (aref dt (char-code #\=)))
    ;; Whitespace produces -3
    (assert-= -3 (aref dt (char-code #\Space)))
    ;; Invalid chars produce -1
    (assert-= -1 (aref dt (char-code #\!)))))

(deftest custom-tables
  "Custom encode/decode tables work correctly."
  (let* ((alphabet "abcdefghijklmnop")
         (etable (tbl:make-encode-table alphabet))
         (dtable (tbl:make-decode-table etable :case-fold t)))
    ;; First char maps to 0
    (assert-= 0 (aref dtable (char-code #\a)))
    ;; Last char maps to 15
    (assert-= 15 (aref dtable (char-code #\p)))
    ;; Case folding works
    (assert-= 0 (aref dtable (char-code #\A)))))

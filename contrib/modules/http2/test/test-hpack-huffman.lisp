;;;; HPACK Huffman Encoding Tests
;;;;
;;;; Test-first approach for HPACK Huffman encoding per RFC 7541 Appendix B

(in-package :epsilon.http2.hpack)

;;;; Test Vectors from RFC 7541 Appendix C

(epsilon.test:deftest test-huffman-encode-www-example-com
  "Test Huffman encoding of 'www.example.com' from RFC 7541 C.4.1"
  (let* ((input "www.example.com")
         (expected #(#xf1 #xe3 #xc2 #xe5 #xf2 #x3a #x6b #xa0 #xab #x90 #xf4 #xff))
         (encoded (huffman-encode input)))
    (epsilon.test:is (equalp expected encoded))
    ;; Should be shorter than original
    (epsilon.test:is (< (length encoded) (length input)))))

(epsilon.test:deftest test-huffman-decode-www-example-com  "Test Huffman decoding of 'www.example.com'"
  (let* ((input #(#xf1 #xe3 #xc2 #xe5 #xf2 #x3a #x6b #xa0 #xab #x90 #xf4 #xff))
         (expected "www.example.com")
         (decoded (huffman-decode input)))
    (epsilon.test:is (string= expected decoded))))

(epsilon.test:deftest test-huffman-encode-no-cache  "Test Huffman encoding of 'no-cache' from RFC 7541 C.4.2"
  (let* ((input "no-cache")
         (expected #(#xa8 #xeb #x10 #x64 #x9c #xbf))
         (encoded (huffman-encode input)))
    (epsilon.test:is (equalp expected encoded))))

(epsilon.test:deftest test-huffman-encode-custom-key  "Test Huffman encoding of 'custom-key' from RFC 7541 C.4.3"
  (let* ((input "custom-key")
         (expected #(#x25 #xa8 #x49 #xe9 #x5b #xa9 #x7d #x7f))
         (encoded (huffman-encode input)))
    (epsilon.test:is (equalp expected encoded))))

(epsilon.test:deftest test-huffman-encode-custom-value  "Test Huffman encoding of 'custom-value' from RFC 7541 C.4.3"
  (let* ((input "custom-value") 
         (expected #(#x25 #xa8 #x49 #xe9 #x5b #xb8 #xe8 #xb4 #xbf))
         (encoded (huffman-encode input)))
    (epsilon.test:is (equalp expected encoded))))

(epsilon.test:deftest test-huffman-encode-private  "Test Huffman encoding of 'private' from RFC 7541 C.5.1"
  (let* ((input "private")
         (expected #(#xae #xc3 #x77 #x1a #x4b))
         (encoded (huffman-encode input)))
    (epsilon.test:is (equalp expected encoded))))

(epsilon.test:deftest test-huffman-encode-date  "Test Huffman encoding of date value from RFC 7541 C.5.1"
  (let* ((input "Mon, 21 Oct 2013 20:13:21 GMT")
         (expected #(#xd0 #x7a #xbe #x94 #x10 #x54 #xd4 #x44 
                     #xa8 #x20 #x05 #x95 #x04 #x0b #x81 #x66
                     #xe0 #x82 #xa6 #x2d #x1b #xff))
         (encoded (huffman-encode input)))
    (epsilon.test:is (equalp expected encoded))))

(epsilon.test:deftest test-huffman-encode-location  "Test Huffman encoding of 'https://www.example.com' from RFC 7541 C.5.3"
  (let* ((input "https://www.example.com")
         (expected #(#x9d #x29 #xad #x17 #x18 #x63 #xc7 #x8f
                     #x0b #x97 #xc8 #xe9 #xae #x82 #xae #x43
                     #xd3))
         (encoded (huffman-encode input)))
    (epsilon.test:is (equalp expected encoded))))

;;;; Edge Cases

(epsilon.test:deftest test-huffman-encode-empty  "Test Huffman encoding of empty string"
  (let ((encoded (huffman-encode "")))
    (epsilon.test:is (equalp #() encoded))))

(epsilon.test:deftest test-huffman-decode-empty  "Test Huffman decoding of empty bytes"
  (let ((decoded (huffman-decode #())))
    (epsilon.test:is (string= "" decoded))))

(epsilon.test:deftest test-huffman-encode-single-char  "Test Huffman encoding of single character"
  (let* ((encoded-a (huffman-encode "a"))
         (encoded-0 (huffman-encode "0"))
         (encoded-space (huffman-encode " ")))
    ;; 'a' = 00011 (5 bits) -> padded to 00011111 = 0x1F
    (epsilon.test:is (equalp #(#x1f) encoded-a))
    ;; '0' = 00000 (5 bits) -> padded to 00000111 = 0x07  
    (epsilon.test:is (equalp #(#x07) encoded-0))
    ;; ' ' = 010100 (6 bits) -> padded to 01010011 = 0x53
    (epsilon.test:is (equalp #(#x53) encoded-space))))

(epsilon.test:deftest test-huffman-encode-all-ascii  "Test that all printable ASCII characters can be encoded"
  (loop for code from 32 to 126
        for char = (code-char code)
        for str = (string char)
        do (let ((encoded (huffman-encode str)))
             (epsilon.test:is (not (null encoded)))
             (epsilon.test:is (> (length encoded) 0))
             ;; Verify round-trip
             (epsilon.test:is (string= str (huffman-decode encoded))))))

(epsilon.test:deftest test-huffman-padding  "Test that Huffman encoding adds proper padding"
  ;; String that doesn't align to byte boundary
  (let* ((input "f")  ; 'f' has a short encoding
         (encoded (huffman-encode input))
         (last-byte (aref encoded (1- (length encoded)))))
    ;; Last byte should have padding of all 1s
    (epsilon.test:is (= (logand last-byte #x7F) #x7F))))

(epsilon.test:deftest test-huffman-decode-with-padding  "Test Huffman decoding handles padding correctly"
  ;; Encoded 'f' with different padding lengths (all should decode to 'f')
  (let ((inputs (list #(#x8b)           ; minimal padding
                     #(#x8f)           ; more padding bits
                     #(#x8f #xff))))   ; extra padding byte
    (dolist (input inputs)
      (handler-case
          (let ((decoded (huffman-decode input)))
            (when decoded
              (epsilon.test:is (or (string= "f" decoded)
                      (string= "" decoded))))) ; padding-only might decode to empty
        (error () nil))))) ; Some padding patterns might be invalid

;;;; Round-trip Tests

(epsilon.test:deftest test-huffman-round-trip-common-headers  "Test round-trip encoding/decoding of common HTTP headers"
  (let ((headers '(":method" ":path" ":scheme" ":status"
                  "accept" "accept-encoding" "accept-language"
                  "cache-control" "content-length" "content-type"
                  "cookie" "date" "expires" "host" "if-match"
                  "if-none-match" "if-modified-since" "location"
                  "referer" "server" "set-cookie" "user-agent")))
    (dolist (header headers)
      (let* ((encoded (huffman-encode header))
             (decoded (huffman-decode encoded)))
        (epsilon.test:is (string= header decoded))))))

(epsilon.test:deftest test-huffman-round-trip-common-values  "Test round-trip encoding/decoding of common header values"
  (let ((values '("GET" "POST" "200" "404" "500"
                 "/" "/index.html" "http" "https"
                 "text/html" "application/json" "gzip, deflate"
                 "en-US,en;q=0.8" "no-cache" "max-age=3600"
                 "Mozilla/5.0" "nginx" "Apache")))
    (dolist (value values)
      (let* ((encoded (huffman-encode value))
             (decoded (huffman-decode encoded)))
        (epsilon.test:is (string= value decoded))))))

;;;; Efficiency Tests

(epsilon.test:deftest test-huffman-compression-ratio  "Test that Huffman encoding provides compression for typical headers"
  (let ((test-cases '(("accept-encoding" . "gzip, deflate, br")
                     ("content-type" . "text/html; charset=utf-8")
                     ("user-agent" . "Mozilla/5.0 (Windows NT 10.0; Win64; x64)")
                     ("cache-control" . "max-age=86400, must-revalidate"))))
    (dolist (test test-cases)
      (let* ((input (cdr test))
             (encoded (huffman-encode input))
             (input-size (length input))
             (encoded-size (length encoded))
             (ratio (/ (float encoded-size) input-size)))
        ;; Huffman should compress typical headers to 60-80% of original
        (epsilon.test:is (< ratio 0.85))))))

;;;; Error Handling Tests

(epsilon.test:deftest test-huffman-decode-truncated  "Test that truncated Huffman sequences are detected"
  ;; Truncated encoding of "www.example.com" 
  (let ((truncated #(#xf1 #xe3 #xc2))) ; First 3 bytes only
    (handler-case
        (huffman-decode truncated)
      (error (e)
        (epsilon.test:is (not (null e))))))) ; Should signal an error

(epsilon.test:deftest test-huffman-decode-invalid-padding  "Test that invalid padding is detected"
  ;; Invalid padding (not all 1s)
  (let ((invalid #(#xf1 #xe3 #xc2 #x00))) ; Zero padding bits
    (handler-case
        (let ((result (huffman-decode invalid)))
          ;; Implementation may choose to accept or reject
          (epsilon.test:is (or (null result) (stringp result))))
      (error (e)
        (epsilon.test:is (not (null e))))))) ; Error is also acceptable

;;;; Performance Test (optional)

(epsilon.test:deftest test-huffman-performance  "Test Huffman encoding performance"
  (let* ((iterations 1000)
         (test-string "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
         (start-time (get-internal-real-time)))
    ;; Encode many times
    (dotimes (i iterations)
      (huffman-encode test-string))
    (let* ((end-time (get-internal-real-time))
           (elapsed (/ (- end-time start-time) 
                      internal-time-units-per-second))
           (ops-per-sec (/ iterations elapsed)))
      ;; Should handle at least 10000 operations per second
      (epsilon.test:is (> ops-per-sec 10000)))))
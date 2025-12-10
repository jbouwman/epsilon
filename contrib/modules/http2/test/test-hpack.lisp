;;;; HPACK Tests
;;;;
;;;; Tests for HTTP/2 header compression

(in-package :epsilon.http2.hpack)

(epsilon.test:deftest test-integer-encoding
  "Test HPACK integer encoding/decoding"
  
  ;; Test small values that fit in prefix
  (let ((output (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0)))
    (encode-integer 10 5 output)
    (epsilon.test:is (= (aref output 0) 10)))
  
  ;; Test larger values requiring continuation
  (let ((output (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0)))
    (encode-integer 1337 5 output)
    (epsilon.test:is (> (length output) 1))
    
    ;; Decode it back
    (multiple-value-bind (value offset)
        (decode-integer output 0 5)
      (epsilon.test:is (= value 1337)))))

(epsilon.test:deftest test-string-encoding
  "Test HPACK string encoding/decoding"
  
  (let* ((test-string "www.example.com")
         (output (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0)))
    
    ;; Encode string
    (encode-string test-string output)
    
    ;; Decode it back
    (multiple-value-bind (decoded-string offset)
        (decode-string output 0)
      (epsilon.test:is (string= decoded-string test-string))
      (epsilon.test:is (= offset (length output))))))

(epsilon.test:deftest test-static-table-lookup
  "Test static table lookups"
  
  ;; Test exact match
  (let ((index (find-in-static-table ":method" "GET")))
    (epsilon.test:is (= index 2)))
  
  ;; Test name-only match (returns negative)
  (let ((index (find-in-static-table ":method" "DELETE")))
    (epsilon.test:is (< index 0))
    (epsilon.test:is (= (abs index) 2)))
  
  ;; Test no match
  (let ((index (find-in-static-table "x-custom-header" "value")))
    (epsilon.test:is (null index))))

(epsilon.test:deftest test-header-encoding-decoding
  "Test complete header encoding and decoding"
  
  (let* ((encoder (create-encoder))
         (decoder (create-decoder))
         (headers '((":method" . "GET")
                   (":path" . "/")
                   (":scheme" . "https")
                   ("host" . "example.com")
                   ("custom-header" . "custom-value"))))
    
    ;; Encode headers
    (let ((encoded (encode-headers encoder headers)))
      (epsilon.test:is (> (length encoded) 0))
      
      ;; Decode headers
      (let ((decoded (decode-headers decoder encoded)))
        (epsilon.test:is (= (length decoded) (length headers)))
        
        ;; Check each header
        (dolist (header headers)
          (let ((found (find (car header) decoded 
                           :key #'car 
                           :test #'string-equal)))
            (epsilon.test:is found)
            (when found
              (epsilon.test:is (string= (cdr found) (cdr header))))))))))

(epsilon.test:deftest test-indexed-header
  "Test indexed header field encoding"
  
  (let* ((encoder (create-encoder))
         (decoder (create-decoder))
         ;; :method GET is index 2 in static table
         (headers '((":method" . "GET"))))
    
    (let ((encoded (encode-headers encoder headers)))
      ;; Should be just one byte for indexed header
      (epsilon.test:is (= (length encoded) 1))
      (epsilon.test:is (= (aref encoded 0) #x82)) ; 0x80 | 2
      
      ;; Decode it
      (let ((decoded (decode-headers decoder encoded)))
        (epsilon.test:is (equal decoded headers))))))

(epsilon.test:deftest test-literal-header
  "Test literal header field encoding"
  
  (let* ((encoder (create-encoder))
         (decoder (create-decoder))
         (headers '(("x-custom" . "value"))))
    
    (let ((encoded (encode-headers encoder headers)))
      ;; Should be longer than indexed
      (epsilon.test:is (> (length encoded) 1))
      
      ;; First byte should indicate literal without indexing
      (epsilon.test:is (= (logand (aref encoded 0) #xf0) 0))
      
      ;; Decode it
      (let ((decoded (decode-headers decoder encoded)))
        (epsilon.test:is (equal decoded headers))))))

(epsilon.test:deftest test-encoder-decoder-state
  "Test that encoder/decoder maintain state"
  
  (let* ((encoder (create-encoder :max-table-size 4096))
         (decoder (create-decoder :max-table-size 4096)))
    
    ;; Check initial state
    (epsilon.test:is (= (encoder-max-dynamic-table-size encoder) 4096))
    (epsilon.test:is (= (decoder-max-dynamic-table-size decoder) 4096))
    (epsilon.test:is (= (encoder-current-table-size encoder) 0))
    (epsilon.test:is (= (decoder-current-table-size decoder) 0))))
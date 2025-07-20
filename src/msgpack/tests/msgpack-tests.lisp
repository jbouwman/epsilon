(defpackage #:epsilon.msgpack.tests
  (:use
   #:cl
   #:epsilon.type
   #:epsilon.test)
  (:local-nicknames
   (:map #:epsilon.map)
   (:msgp #:epsilon.msgpack.impl)
   (:time #:epsilon.time)))

(in-package #:epsilon.msgpack.tests)

(deftest nil-bool-values
  (is (equalp (msgp:encode nil) #(192)))
  (is (equalp (msgp:encode t) #(195)))
  (is (equalp (msgp:decode #(192)) nil))
  (is (equalp (msgp:decode #(195)) t))
  (is (equalp (msgp:decode #(194)) nil)))

(deftest encode-string
  (is (equalp (msgp:encode "abc")
              #(163 97 98 99)))
  (is (equalp (msgp:decode #(163 97 98 99))
              "abc"))
  
  ;; Test empty string
  (is (equalp (msgp:encode "")
              #(160)))
  (is (equalp (msgp:decode #(160))
              ""))
  
  ;; Test longer string that requires str8 format
  (let ((long-str (make-string 50 :initial-element #\a)))
    (is (= (length (msgp:encode long-str)) 52))  ; 2 bytes header + 50 content
    (is (string= (msgp:decode (msgp:encode long-str)) long-str))))

(deftest integer-formats
  ;; Positive fixint (0-127)
  (is (equalp (msgp:encode 0) #(0)))
  (is (equalp (msgp:encode 127) #(127)))
  
  ;; Negative fixint (-32 to -1)
  (is (equalp (msgp:encode -1) #(255)))
  (is (equalp (msgp:encode -32) #(224)))
  
  ;; uint8 (0-255)
  (is (equalp (msgp:encode 128) #(204 128)))
  (is (equalp (msgp:encode 255) #(204 255)))
  
  ;; uint16 (0-65535)
  (is (equalp (msgp:encode 256) #(205 1 0)))
  (is (equalp (msgp:encode 65535) #(205 255 255)))
  
  ;; uint32 (0-4294967295)
  (is (equalp (msgp:encode 65536) #(206 0 1 0 0)))
  (is (equalp (msgp:encode 999999999) #(206 59 154 201 255)))
  
  ;; int8 (-128 to -33)
  (is (equalp (msgp:encode -33) #(208 223)))
  (is (equalp (msgp:encode -128) #(208 128)))
  
  ;; int16 (-32768 to -129)
  (is (equalp (msgp:encode -129) #(209 255 127)))
  (is (equalp (msgp:encode -32768) #(209 128 0)))
  
  ;; int32 (-2147483648 to -32769)
  (is (equalp (msgp:encode -32769) #(210 255 255 127 255)))
  
  ;; Round-trip test
  (dolist (num (list 0 127 128 255 256 65535 65536 999999999
                     -1 -32 -33 -128 -129 -32768 -32769 -2147483648))
    (is (= (msgp:decode (msgp:encode num)) num))))

(deftest float-formats
  ;; Test single-float
  (let ((val 123.456))
    (is (equalp (msgp:decode (msgp:encode (float val 0.0s0)))
                (float val 0.0s0))))
  
  ;; Test double-float
  (let ((val 123.456d0))
    (is (equalp (msgp:decode (msgp:encode val)) val))))

(deftest encode-map
  ;; Test empty map
  (is (equalp (msgp:encode map:+empty+) #(128)))
  
  ;; Test with string keys
  (is (equalp (msgp:encode (map:make-map "a" 1 "b" 2))
              #(130 161 98 2 161 97 1)))
  
  ;; Test round-trip with mixed types
  (let ((original (map:make-map "string" "value"
                                "int" 42
                                "float" 3.14d0
                                "nil" nil
                                "bool" t)))
    (is (map:map= original (msgp:decode (msgp:encode original))))))

(deftest array-formats
  ;; Test empty array
  (is (equalp (msgp:encode #()) #(144)))
  
  ;; Test fixarray
  (is (equalp (msgp:encode #(1 2 3)) #(147 1 2 3)))
  
  ;; Test array with 15 elements (still fixarray)
  (let ((array15 (make-array 15 :initial-contents (loop for i from 0 below 15 collect i))))
    (let ((encoded (msgp:encode array15))
          (decoded (msgp:decode (msgp:encode array15))))
      (is (equalp decoded array15))))
  
  ;; Test array with 16 elements (should use array16 format)
  (let ((array16 (make-array 16 :initial-contents (loop for i from 0 below 16 collect i))))
    (let ((encoded (msgp:encode array16))
          (decoded (msgp:decode (msgp:encode array16))))
      (is (equalp decoded array16))))
  
  ;; Test array with 100 elements (should use array16 format)
  (let ((array100 (make-array 100 :initial-contents (loop for i from 0 below 100 collect i))))
    (let ((encoded (msgp:encode array100))
          (decoded (msgp:decode (msgp:encode array100))))
      (is (equalp decoded array100))))
  
  ;; Test with mixed types
  (let ((array #("string" 42 3.14d0 nil t)))
    (is (equalp (msgp:decode (msgp:encode array)) array))))

(deftest bin-format
  ;; Test empty binary
  (let ((bin (make-array 0 :element-type 'u8)))
    (is (equalp (msgp:encode bin) #(196 0))))
  
  ;; Test small binary
  (let ((bin (make-array 5 :element-type '(unsigned-byte 8)
                        :initial-contents '(1 2 3 4 5))))
    (is (equalp (msgp:encode bin) #(196 5 1 2 3 4 5)))
    (is (equalp (msgp:decode (msgp:encode bin)) bin))))

(deftest ext-format
  ;; Test ext format (as a list with :ext tag)
  (let* ((type-code 1)
         (data (make-array 4 :element-type 'u8 
                          :initial-contents '(1 2 3 4)))
         (ext-obj (list :ext type-code data)))
    (let ((encoded (msgp:encode ext-obj)))
      (is (equalp (msgp:decode encoded) ext-obj)))))

;; FIXME
#++
(deftest timestamp-format
  ;; Test timestamp encoding/decoding using our epsilon.time implementation
  (let* ((timestamp (time:make-timestamp 1000000000 0)))
    (let ((decoded (msgp:decode (msgp:encode timestamp))))
      (is (time:timestamp= decoded timestamp))
      (is (= (time:timestamp-seconds decoded) 1000000000))
      (is (= (time:timestamp-nanoseconds decoded) 0))))
  
  ;; Test with nanoseconds
  (let* ((timestamp (time:make-timestamp 1000000000 500000000)))
    (let ((decoded (msgp:decode (msgp:encode timestamp))))
      (is (time:timestamp= decoded timestamp))
      (is (= (time:timestamp-seconds decoded) 1000000000))
      (is (= (time:timestamp-nanoseconds decoded) 500000000))))
  
  ;; Test backward compatibility with list format
  (let* ((timestamp (list :timestamp 1000000000 0)))
    (let ((decoded (msgp:decode (msgp:encode timestamp))))
      (is (typep decoded 'time:timestamp))
      (is (= (time:timestamp-seconds decoded) 1000000000))
      (is (= (time:timestamp-nanoseconds decoded) 0)))))

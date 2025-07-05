(defpackage #:epsilon.lib.binary.struct.tests
  (:use #:cl #:epsilon.tool.test)
  (:local-nicknames (#:binary #:epsilon.lib.binary)))

(in-package #:epsilon.lib.binary.struct.tests)

(deftest test-simple-struct ()
  "Test basic struct definition and operations"
  ;; Define a simple point structure
  (binary:define-binary-struct point ()
    (x :s32)
    (y :s32))
  
  ;; Test structure creation
  (let ((p (make-point-struct :x 10 :y 20)))
    (is-equal 10 (point-struct-x p))
    (is-equal 20 (point-struct-y p)))
  
  ;; Test binary encoding/decoding
  (let* ((p (make-point-struct :x 42 :y -17))
         (bytes (binary:to-bytes p 'point))
         (decoded (binary:from-bytes bytes 'point)))
    (is-equal 42 (point-struct-x decoded))
    (is-equal -17 (point-struct-y decoded)))
  
  ;; Test size calculation
  (is-equal 8 (binary:size 'point)))

(deftest test-string-fields ()
  "Test structures with string fields"
  ;; Pascal-style string with length prefix
  (binary:define-binary-struct pascal-string ()
    (length :u8)
    (data :string :size length))
  
  ;; Test encoding/decoding
  (let* ((str (make-pascal-string-struct :length 5 :data "hello"))
         (bytes (binary:to-bytes str 'pascal-string))
         (decoded (binary:from-bytes bytes 'pascal-string)))
    (is-equal 5 (pascal-string-struct-length decoded))
    (is-equal "hello" (pascal-string-struct-data decoded))))

(deftest test-bytes-fields ()
  "Test structures with byte array fields"
  (binary:define-binary-struct packet ()
    (type :u8)
    (size :u16)
    (payload :bytes :size size))
  
  ;; Test with binary payload
  (let* ((payload #(1 2 3 4 5))
         (pkt (make-packet-struct :type 42 :size 5 :payload payload))
         (bytes (binary:to-bytes pkt 'packet))
         (decoded (binary:from-bytes bytes 'packet)))
    (is-equal 42 (packet-struct-type decoded))
    (is-equal 5 (packet-struct-size decoded))
    (is-equalp payload (packet-struct-payload decoded))))

(deftest test-constant-fields ()
  "Test fields with constant values for validation"
  (binary:define-binary-struct header ()
    (magic :u32 :value #x12345678)
    (version :u16)
    (flags :u16))
  
  ;; Test successful validation
  (let* ((h (make-header-struct :magic #x12345678 :version 1 :flags 0))
         (bytes (binary:to-bytes h 'header))
         (decoded (binary:from-bytes bytes 'header)))
    (is-equal #x12345678 (header-struct-magic decoded))
    (is-equal 1 (header-struct-version decoded)))
  
  ;; Test validation failure
  (let ((bad-bytes #(#x87 #x65 #x43 #x21 0 1 0 0))) ; wrong magic
    (is-thrown (error) (binary:from-bytes bad-bytes 'header))))

(deftest test-endianness ()
  "Test endianness handling"
  (binary:define-binary-struct mixed-endian ()
    (little-val :u32 :endian :little-endian)
    (big-val :u32 :endian :big-endian))
  
  ;; Test that different endianness produces different byte patterns
  (let* ((s (make-mixed-endian-struct :little-val #x12345678 :big-val #x12345678))
         (bytes (binary:to-bytes s 'mixed-endian)))
    ;; Little-endian: 78 56 34 12, Big-endian: 12 34 56 78
    (is-equal #x78 (aref bytes 0))  ; little-endian low byte
    (is-equal #x56 (aref bytes 1))
    (is-equal #x34 (aref bytes 2))
    (is-equal #x12 (aref bytes 3))  ; little-endian high byte
    (is-equal #x12 (aref bytes 4))  ; big-endian high byte
    (is-equal #x34 (aref bytes 5))
    (is-equal #x56 (aref bytes 6))
    (is-equal #x78 (aref bytes 7)))) ; big-endian low byte

(deftest test-nested-structures ()
  "Test structures containing other structures"
  ;; Define inner structure
  (binary:define-binary-struct color ()
    (r :u8)
    (g :u8)
    (b :u8))
  
  ;; Define outer structure
  (binary:define-binary-struct pixel ()
    (x :u16)
    (y :u16)
    (color color))
  
  ;; Test nested encoding/decoding
  (let* ((c (make-color-struct :r 255 :g 128 :b 64))
         (p (make-pixel-struct :x 100 :y 200 :color c))
         (bytes (binary:to-bytes p 'pixel))
         (decoded (binary:from-bytes bytes 'pixel)))
    (is-equal 100 (pixel-struct-x decoded))
    (is-equal 200 (pixel-struct-y decoded))
    (is-equal 255 (color-struct-r (pixel-struct-color decoded)))
    (is-equal 128 (color-struct-g (pixel-struct-color decoded)))
    (is-equal 64 (color-struct-b (pixel-struct-color decoded)))))

(deftest test-variable-size ()
  "Test structures with variable size"
  (binary:define-binary-struct message ()
    (type :u8)
    (length :u16)
    (data :bytes :size length))
  
  ;; Test different sizes
  (let* ((small-msg (make-message-struct :type 1 :length 3 :data #(1 2 3)))
         (large-msg (make-message-struct :type 2 :length 10 :data #(0 1 2 3 4 5 6 7 8 9)))
         (small-bytes (binary:to-bytes small-msg 'message))
         (large-bytes (binary:to-bytes large-msg 'message)))
    
    ;; Verify sizes
    (is-equal 6 (length small-bytes))  ; 1 + 2 + 3
    (is-equal 13 (length large-bytes)) ; 1 + 2 + 10
    
    ;; Verify round-trip
    (let ((decoded-small (binary:from-bytes small-bytes 'message))
          (decoded-large (binary:from-bytes large-bytes 'message)))
      (is-equal 1 (message-struct-type decoded-small))
      (is-equal 3 (message-struct-length decoded-small))
      (is-equalp #(1 2 3) (message-struct-data decoded-small))
      
      (is-equal 2 (message-struct-type decoded-large))
      (is-equal 10 (message-struct-length decoded-large))
      (is-equalp #(0 1 2 3 4 5 6 7 8 9) (message-struct-data decoded-large)))))

(deftest test-stream-operations ()
  "Test reading/writing structures from/to streams"
  (binary:define-binary-struct record ()
    (id :u32)
    (value :f32))
  
  ;; Test stream writing
  (let ((stream (make-instance 'binary:binary-output-stream))
        (r1 (make-record-struct :id 1 :value 3.14))
        (r2 (make-record-struct :id 2 :value 2.71)))
    
    (binary:write stream r1 'record)
    (binary:write stream r2 'record)
    
    ;; Test stream reading
    (let* ((bytes (binary:get-output-stream-bytes stream))
           (input-stream (make-instance 'binary:binary-input-stream :data bytes))
           (decoded-r1 (binary:read input-stream 'record))
           (decoded-r2 (binary:read input-stream 'record)))
      
      (is-equal 1 (record-struct-id decoded-r1))
      (is-equal 3.14 (record-struct-value decoded-r1) 0.001)
      (is-equal 2 (record-struct-id decoded-r2))
      (is-equal 2.71 (record-struct-value decoded-r2) 0.001))))

(deftest test-error-handling ()
  "Test error conditions"
  (binary:define-binary-struct test-struct ()
    (magic :u32 :value #xdeadbeef)
    (data :u32))
  
  ;; Test insufficient data
  (let ((short-bytes #(#xef #xbe)))
    (is-thrown (error) (binary:from-bytes short-bytes 'test-struct)))
  
  ;; Test magic value mismatch
  (let ((bad-magic #(#x00 #x00 #x00 #x00 #x12 #x34 #x56 #x78)))
    (is-thrown (error) (binary:from-bytes bad-magic 'test-struct))))

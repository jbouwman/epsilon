(defpackage epsilon.lib.binary.tests
  (:use
   cl
   epsilon.test)
  (:local-nicknames
   (binary epsilon.lib.binary)
   (test epsilon.test)
   (fs epsilon.sys.fs)))

(in-package epsilon.lib.binary.tests)

(deftest basic-integer-conversion
  "Basic integer conversions"
  (test:with-label "Unsigned integers"
    (is-equalp #(42) (binary:to-bytes 42 :u8))
    (is-equalp #(0 42) (binary:to-bytes 42 :u16 :big-endian))
    (is-equalp #(42 0) (binary:to-bytes 42 :u16 :little-endian))
    (is-equalp #(0 0 0 42) (binary:to-bytes 42 :u32 :big-endian))
    (is-equalp #(42 0 0 0) (binary:to-bytes 42 :u32 :little-endian)))
  
  (test:with-label "Signed integers"
    (is-equalp #(255) (binary:to-bytes -1 :s8))
    (is-equalp #(255 255) (binary:to-bytes -1 :s16 :big-endian))
    (is-equalp #(255 255) (binary:to-bytes -1 :s16 :little-endian))))

(deftest round-trip-conversion
  "Test that we can convert to bytes and back"
  (dolist (type '(:u8 :u16 :u32 :u64 :s8 :s16 :s32 :s64))
    (dolist (endian '(:little-endian :big-endian))
      (let* ((value (case type
                      ((:u8 :s8) 42)
                      ((:u16 :s16) 1234)
                      ((:u32 :s32) 123456)
                      ((:u64 :s64) 123456789)))
             (bytes (binary:to-bytes value type endian))
             (recovered (binary:from-bytes bytes type endian)))
        (is-equal value recovered)))))

(deftest stream-operations
  "Test reading and writing to streams"
  (let ((data '((42 :u8)
                (1234 :u16 :little-endian)
                (5678 :u16 :big-endian)
                (123456 :u32 :little-endian)
                (-42 :s8)
                (-1234 :s16 :big-endian))))
    
    ;; Write data
    (fs:with-temp-file (filename)
      (with-open-file (stream filename
                              :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
        (dolist (spec data)
          (destructuring-bind (value type &optional (endian :native)) spec
            (binary:write stream value type endian))))
      
      ;; Read it back
      (with-open-file (stream filename
                              :direction :input
                              :element-type '(unsigned-byte 8))
        (dolist (spec data)
          (destructuring-bind (expected type &optional (endian :native)) spec
            (let ((actual (binary:read stream type endian)))
              (is-equal expected actual))))))))

(deftest sequence-operations
  "Test reading/writing sequences"
  (let ((values #(1 2 3 4 5)))
    (fs:with-temp-file (filename)
      (with-open-file (stream filename
                              :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
        (binary:write-sequence stream values :u16 :big-endian))
      
      (with-open-file (stream filename
                              :direction :input
                              :element-type '(unsigned-byte 8))
        (let ((result (binary:read-sequence stream :u16 5 :big-endian)))
          (is-equalp values result))))))

(deftest size-calculation
  "Test size calculation for different types"
  (is-equal 1 (binary:size :u8))
  (is-equal 2 (binary:size :u16))
  (is-equal 4 (binary:size :u32))
  (is-equal 8 (binary:size :u64))
  (is-equal 4 (binary:size :f32))
  (is-equal 8 (binary:size :f64)))

(deftest endianness-matters
  "Verify that endianness actually changes the output"
  (let ((value #x1234))
    (let ((be (binary:to-bytes value :u16 :big-endian))
          (le (binary:to-bytes value :u16 :little-endian)))
      (is-equalp #(#x12 #x34) be)
      (is-equalp #(#x34 #x12) le)
      (is-not (equalp be le)))))

(deftest with-endian-macro
  "Test the with-endian macro"
  (let ((value #x1234))
    (binary:with-endian (:big-endian)
      (is-equalp #(#x12 #x34) (binary:to-bytes value :u16)))
    (binary:with-endian (:little-endian)
      (is-equalp #(#x34 #x12) (binary:to-bytes value :u16)))))

;; TODO: Add tests for:
;; - Custom type definitions
;; - Binary struct definitions
;; - Error conditions
;; - Float types (when properly implemented)

(defpackage epsilon.lib.msgpack.binary.benchmark
  (:use cl)
  (:local-nicknames
   (bench epsilon.tool.benchmark)
   (msgpack epsilon.lib.msgpack)
   (msgpack-binary epsilon.lib.msgpack.binary)
   (map epsilon.lib.map)
   (time epsilon.lib.time)))

(in-package :epsilon.lib.msgpack.binary.benchmark)

;;;; Benchmarks comparing original MessagePack vs binary structure implementation

;;; Test data generators

(defun make-test-integers ()
  "Generate test integers of various sizes"
  (list 0 1 127 128 255 256 65535 65536 #xFFFFFFFF #x100000000
        -1 -32 -33 -128 -129 -32768 -32769 #x-80000000 #x-80000001))

(defun make-test-strings ()
  "Generate test strings of various lengths"
  (list ""
        "a"
        "hello"
        "this is a 31 character string!"
        "this is a longer string that exceeds 31 characters and should use str8 format"
        (make-string 256 :initial-element #\x)
        (make-string 65536 :initial-element #\y)))

(defun make-test-arrays ()
  "Generate test arrays of various sizes"
  (list #()
        #(1 2 3)
        (make-array 15 :initial-contents (loop for i from 0 below 15 collect i))
        (make-array 16 :initial-contents (loop for i from 0 below 16 collect i))
        (make-array 100 :initial-contents (loop for i from 0 below 100 collect i))))

(defun make-test-maps ()
  "Generate test maps of various sizes"
  (list map:+empty+
        (let ((m map:+empty+))
          (setf m (map:assoc m "a" 1))
          (setf m (map:assoc m "b" 2))
          (setf m (map:assoc m "c" 3))
          m)
        (let ((m map:+empty+))
          (loop for i from 0 below 15
                do (setf m (map:assoc m (format nil "key~D" i) i)))
          m)
        (let ((m map:+empty+))
          (loop for i from 0 below 100
                do (setf m (map:assoc m (format nil "key~D" i) i)))
          m)))

(defun make-test-binary-data ()
  "Generate test binary data of various sizes"
  (list #()
        #(1 2 3 4)
        (make-array 255 :element-type '(unsigned-byte 8) 
                    :initial-contents (loop for i from 0 below 255 collect (mod i 256)))
        (make-array 65536 :element-type '(unsigned-byte 8)
                    :initial-contents (loop for i from 0 below 65536 collect (mod i 256)))))

(defun make-complex-nested-data ()
  "Generate complex nested test data"
  (list
    ;; Nested array
    #(1 "hello" #(2 3 4) map:+empty+)
    
    ;; Nested map
    (let ((inner-map map:+empty+))
      (setf inner-map (map:assoc inner-map "x" 10))
      (setf inner-map (map:assoc inner-map "y" 20))
      (let ((outer-map map:+empty+))
        (setf outer-map (map:assoc outer-map "numbers" #(1 2 3 4 5)))
        (setf outer-map (map:assoc outer-map "strings" #("a" "b" "c")))
        (setf outer-map (map:assoc outer-map "nested" inner-map))
        outer-map))
    
    ;; Mixed data structure
    (let ((mixed-map map:+empty+))
      (setf mixed-map (map:assoc mixed-map "key1" "value1"))
      (setf mixed-map (map:assoc mixed-map "key2" 42))
      #((1 2 3) 
        "test string"
        mixed-map
        #(255 128 64 32)))))

;;; Individual benchmarks

(bench:defbenchmark encode-integers-original ()
  (let ((integers '(0 1 42 127 255 256 65535 -1 -32)))
    (dolist (int integers)
      (msgpack:encode int))))

(bench:defbenchmark encode-integers-binary ()
  (let ((integers '(0 1 42 127 255 256 65535 -1 -32)))
    (dolist (int integers)
      (msgpack-binary:encode-with-binary-structs int))))

(bench:defbenchmark encode-strings-original ()
  (let ((strings '("" "a" "hello" "test string")))
    (dolist (str strings)
      (msgpack:encode str))))

(bench:defbenchmark encode-strings-binary ()
  (let ((strings '("" "a" "hello" "test string")))
    (dolist (str strings)
      (msgpack-binary:encode-with-binary-structs str))))

(bench:defbenchmark encode-arrays-original ()
  (let ((arrays (make-test-arrays)))
    (dolist (arr arrays)
      (msgpack:encode arr))))

(bench:defbenchmark encode-arrays-binary ()
  (let ((arrays (make-test-arrays)))
    (dolist (arr arrays)
      (msgpack-binary:encode-with-binary-structs arr))))

(bench:defbenchmark encode-binary-data-original ()
  (let ((data (make-test-binary-data)))
    (dolist (bin data)
      (msgpack:encode bin))))

(bench:defbenchmark encode-binary-data-binary ()
  (let ((data (make-test-binary-data)))
    (dolist (bin data)
      (msgpack-binary:encode-with-binary-structs bin))))

(bench:defbenchmark encode-complex-original ()
  (let ((data (make-complex-nested-data)))
    (dolist (obj data)
      (msgpack:encode obj))))

(bench:defbenchmark encode-complex-binary ()
  (let ((data (make-complex-nested-data)))
    (dolist (obj data)
      (msgpack-binary:encode-with-binary-structs obj))))

;;; Round-trip benchmarks

(bench:defbenchmark roundtrip-integers-original ()
  (let ((integers (make-test-integers)))
    (dolist (int integers)
      (let ((encoded (msgpack:encode int)))
        (msgpack:decode encoded)))))

(bench:defbenchmark roundtrip-strings-original ()
  (let ((strings (make-test-strings)))
    (dolist (str strings)
      (let ((encoded (msgpack:encode str)))
        (msgpack:decode encoded)))))

;;; Benchmark runner

(defun run-msgpack-benchmarks ()
  "Run all MessagePack benchmarks and return comparison results"
  (format t "~&Running MessagePack benchmarks...~%~%")
  
  ;; Integer encoding comparison
  (let ((orig-int (bench:run-benchmark 
                   (bench:get-benchmark 'encode-integers-original)
                   :name "Original Integers"))
        (bin-int (bench:run-benchmark 
                  (bench:get-benchmark 'encode-integers-binary)
                  :name "Binary Struct Integers")))
    
    (format t "Integer Encoding Comparison:~%")
    (bench:format-benchmark-result orig-int)
    (bench:format-benchmark-result bin-int)
    (bench:format-comparison 
     (bench:compare-benchmarks orig-int bin-int))
    (format t "~%"))
  
  ;; String encoding comparison
  (let ((orig-str (bench:run-benchmark 
                   (bench:get-benchmark 'encode-strings-original)
                   :name "Original Strings"))
        (bin-str (bench:run-benchmark 
                  (bench:get-benchmark 'encode-strings-binary)
                  :name "Binary Struct Strings")))
    
    (format t "String Encoding Comparison:~%")
    (bench:format-benchmark-result orig-str)
    (bench:format-benchmark-result bin-str)
    (bench:format-comparison 
     (bench:compare-benchmarks orig-str bin-str))
    (format t "~%"))
  
  ;; Array encoding comparison
  (let ((orig-arr (bench:run-benchmark 
                   (bench:get-benchmark 'encode-arrays-original)
                   :name "Original Arrays"))
        (bin-arr (bench:run-benchmark 
                  (bench:get-benchmark 'encode-arrays-binary)
                  :name "Binary Struct Arrays")))
    
    (format t "Array Encoding Comparison:~%")
    (bench:format-benchmark-result orig-arr)
    (bench:format-benchmark-result bin-arr)
    (bench:format-comparison 
     (bench:compare-benchmarks orig-arr bin-arr))
    (format t "~%"))
  
  ;; Binary data encoding comparison
  (let ((orig-bin (bench:run-benchmark 
                   (bench:get-benchmark 'encode-binary-data-original)
                   :name "Original Binary"))
        (bin-bin (bench:run-benchmark 
                  (bench:get-benchmark 'encode-binary-data-binary)
                  :name "Binary Struct Binary")))
    
    (format t "Binary Data Encoding Comparison:~%")
    (bench:format-benchmark-result orig-bin)
    (bench:format-benchmark-result bin-bin)
    (bench:format-comparison 
     (bench:compare-benchmarks orig-bin bin-bin))
    (format t "~%"))
  
  ;; Complex data encoding comparison
  (let ((orig-complex (bench:run-benchmark 
                       (bench:get-benchmark 'encode-complex-original)
                       :name "Original Complex"))
        (bin-complex (bench:run-benchmark 
                      (bench:get-benchmark 'encode-complex-binary)
                      :name "Binary Struct Complex")))
    
    (format t "Complex Data Encoding Comparison:~%")
    (bench:format-benchmark-result orig-complex)
    (bench:format-benchmark-result bin-complex)
    (bench:format-comparison 
     (bench:compare-benchmarks orig-complex bin-complex))
    (format t "~%"))
  
  (format t "Benchmark suite completed.~%"))

;;; Memory usage analysis

(defun analyze-memory-usage ()
  "Analyze memory usage patterns between implementations"
  (format t "~&Memory Usage Analysis:~%~%")
  
  (let* ((test-data (append (make-test-integers)
                           (make-test-strings)
                           (make-test-arrays)
                           (make-test-binary-data)))
         (orig-start (sb-ext:get-bytes-consed))
         (orig-results (mapcar #'msgpack:encode test-data))
         (orig-end (sb-ext:get-bytes-consed))
         (orig-memory (- orig-end orig-start))
         
         (bin-start (sb-ext:get-bytes-consed))
         (bin-results (mapcar #'msgpack-binary:encode-with-binary-structs test-data))
         (bin-end (sb-ext:get-bytes-consed))
         (bin-memory (- bin-end bin-start)))
    
    (format t "Original implementation: ~:D bytes allocated~%" orig-memory)
    (format t "Binary struct implementation: ~:D bytes allocated~%" bin-memory)
    (format t "Difference: ~:D bytes (~,1F% ~A)~%~%" 
            (abs (- bin-memory orig-memory))
            (* 100.0 (/ (abs (- bin-memory orig-memory)) orig-memory))
            (if (< bin-memory orig-memory) "less" "more"))
    
    ;; Verify results are equivalent
    (let ((mismatches 0))
      (loop for orig in orig-results
            for bin in bin-results
            unless (equalp orig bin)
            do (incf mismatches))
      
      (if (zerop mismatches)
          (format t "All results match - implementations are equivalent.~%")
          (format t "WARNING: ~D mismatches found between implementations!~%" mismatches)))))

;;; Validation functions

(defun validate-encoding-equivalence ()
  "Validate that both implementations produce equivalent results"
  (format t "~&Validating encoding equivalence...~%")
  
  ;; Start with simpler test cases to isolate issues
  (let ((simple-test-cases '(42 "hello" #(1 2 3)))
        (mismatches 0))
    
    (dolist (test-case simple-test-cases)
      (handler-case
          (let ((orig-result (msgpack:encode test-case)))
            (handler-case
                (let ((bin-result (msgpack-binary:encode-with-binary-structs test-case)))
                  (unless (equalp orig-result bin-result)
                    (incf mismatches)
                    (format t "MISMATCH for ~A:~%" test-case)
                    (format t "  Original: ~A~%" orig-result)
                    (format t "  Binary:   ~A~%" bin-result)))
              (error (e)
                (incf mismatches)
                (format t "ERROR in binary implementation for ~A: ~A~%" test-case e))))
        (error (e)
          (incf mismatches)
          (format t "ERROR in original implementation for ~A: ~A~%" test-case e))))
    
    (if (zerop mismatches)
        (format t "All ~D simple test cases passed.~%~%" 
                (length simple-test-cases))
        (format t "~D out of ~D simple test cases failed!~%~%" 
                mismatches (length simple-test-cases)))
    
    (zerop mismatches)))

;;; Main benchmark suite

(defun run-complete-benchmark-suite ()
  "Run the complete benchmark and validation suite"
  (format t "=== MessagePack Binary Structure Benchmark Suite ===~%~%")
  
  ;; First validate equivalence
  (unless (validate-encoding-equivalence)
    (format t "WARNING: Implementations are not equivalent - benchmark results may not be meaningful!~%~%"))
  
  ;; Run performance benchmarks
  (run-msgpack-benchmarks)
  
  ;; Analyze memory usage
  (analyze-memory-usage)
  
  (format t "~%=== Benchmark Suite Complete ===~%"))
(defpackage epsilon.msgpack.benchmark
  (:use cl)
  (:local-nicknames
   (bench epsilon.tool.benchmark)
   (msgpack epsilon.msgpack)
   (map epsilon.map)
   (time epsilon.time)))

(in-package :epsilon.msgpack.benchmark)

;;;; Benchmarks

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

(bench:defbenchmark encode-integers ()
  (let ((integers '(0 1 42 127 255 256 65535 -1 -32)))
    (dolist (int integers)
      (msgpack:encode int))))

(bench:defbenchmark encode-strings ()
  (let ((strings '("" "a" "hello" "test string")))
    (dolist (str strings)
      (msgpack:encode str))))

(bench:defbenchmark encode-arrays ()
  (let ((arrays (make-test-arrays)))
    (dolist (arr arrays)
      (msgpack:encode arr))))

(bench:defbenchmark encode-binary-data ()
  (let ((data (make-test-binary-data)))
    (dolist (bin data)
      (msgpack:encode bin))))

(bench:defbenchmark encode-complex ()
  (let ((data (make-complex-nested-data)))
    (dolist (obj data)
      (msgpack:encode obj))))

;;; Round-trip benchmarks

(bench:defbenchmark roundtrip-integers ()
  (let ((integers (make-test-integers)))
    (dolist (int integers)
      (let ((encoded (msgpack:encode int)))
        (msgpack:decode encoded)))))

(bench:defbenchmark roundtrip-strings ()
  (let ((strings (make-test-strings)))
    (dolist (str strings)
      (let ((encoded (msgpack:encode str)))
        (msgpack:decode encoded)))))

;;; Benchmark runner

(defun run-msgpack-benchmarks ()
  "Run all MessagePack benchmarks and return comparison results"
  (format t "~&Running MessagePack benchmarks...~%~%")
  
  ;; Integer encoding comparison
  (let ((int (bench:run-benchmark 
                   (bench:get-benchmark 'encode-integers)
                   :name "Original Integers")))
    
    (format t "Integer Encoding Comparison:~%")
    (bench:format-benchmark-result int)
    (format t "~%"))
  
  ;; String encoding comparison
  (let ((str (bench:run-benchmark 
                   (bench:get-benchmark 'encode-strings)
                   :name "Original Strings")))
    
    (format t "String Encoding Comparison:~%")
    (bench:format-benchmark-result str)
    (format t "~%"))
  
  ;; Array encoding comparison
  (let ((arr (bench:run-benchmark 
                   (bench:get-benchmark 'encode-arrays)
                   :name "Original Arrays")))
    
    (format t "Array Encoding Comparison:~%")
    (bench:format-benchmark-result arr)
    (format t "~%"))
  
  ;; Binary data encoding comparison
  (let ((bin (bench:run-benchmark 
                   (bench:get-benchmark 'encode-binary-data)
                   :name "Original Binary")))
    
    (format t "Binary Data Encoding Comparison:~%")
    (bench:format-benchmark-result bin)
    (format t "~%"))
  
  ;; Complex data encoding comparison
  (let ((complex (bench:run-benchmark 
                       (bench:get-benchmark 'encode-complex)
                       :name "Original Complex")))
    
    (format t "Complex Data Encoding Comparison:~%")
    (bench:format-benchmark-result complex)
    (bench:format-benchmark-result bin-complex)
    (bench:format-comparison 
     (bench:compare-benchmarks complex bin-complex))
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
         (start (sb-ext:get-bytes-consed))
         (results (mapcar #'msgpack:encode test-data))
         (end (sb-ext:get-bytes-consed))
         (memory (- end start)))
    (format t "~:D bytes allocated~%" memory)))

;;; Main benchmark suite

(defun run-complete-benchmark-suite ()
  "Run the complete benchmark and validation suite"
  (format t "=== MessagePack Benchmark Suite ===~%~%") ; TODO suite should know its own name
  
  ;; Run performance benchmarks
  (run-msgpack-benchmarks)
  
  ;; Analyze memory usage
  (analyze-memory-usage)
  
  (format t "~%=== Benchmark Suite Complete ===~%"))

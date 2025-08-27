;;;; Performance Benchmark Tests for epsilon.foreign
;;;;
;;;; Compare old vs new implementation performance

(defpackage epsilon.foreign.benchmark-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (#:old epsilon.foreign)      ; Current implementation
   (#:new epsilon.foreign.core)  ; New implementation
   (#:fw epsilon.foreign.test-framework)))

(in-package epsilon.foreign.benchmark-tests)

;;;; Test Functions

;; Define same functions in both old and new APIs
(defun setup-test-functions ()
  ;; Old API
  (old:defshared old-strlen "strlen" "libc" :unsigned-long ((str :string)))
  (old:defshared old-getpid "getpid" "libc" :int ())
  (old:defshared old-memcmp "memcmp" "libc" :int 
    ((s1 :pointer) (s2 :pointer) (n :unsigned-long)))
  
  ;; New API
  (new:defshared new-strlen "strlen" "libc" :unsigned-long ((str :string)))
  (new:defshared new-getpid "getpid" "libc" :int ())
  (new:defshared new-memcmp "memcmp" "libc" :int
    ((s1 :pointer) (s2 :pointer) (n :unsigned-long))))

;;;; Micro-benchmarks

(deftest benchmark-simple-call ()
  "Benchmark simple function call (no arguments)"
  (setup-test-functions)
  
  (let ((old-result (fw:benchmark-operation "old-getpid"
                      (lambda () (old-getpid))
                      :iterations 100000
                      :warmup-iterations 1000))
        (new-result (fw:benchmark-operation "new-getpid"
                      (lambda () (new-getpid))
                      :iterations 100000
                      :warmup-iterations 1000)))
    
    (fw:report-benchmark-comparison old-result new-result)
    
    ;; New should be at least 2x faster
    (fw:assert-better-performance 
     (fw:benchmark-result-average-time new-result)
     (fw:benchmark-result-average-time old-result)
     :threshold 0.5)))

(deftest benchmark-string-argument ()
  "Benchmark function with string argument"
  (setup-test-functions)
  
  (let ((test-string "Hello, World! This is a test string."))
    (let ((old-result (fw:benchmark-operation "old-strlen"
                        (lambda () (old-strlen test-string))
                        :iterations 50000))
          (new-result (fw:benchmark-operation "new-strlen"  
                        (lambda () (new-strlen test-string))
                        :iterations 50000)))
      
      (fw:report-benchmark-comparison old-result new-result)
      
      ;; String handling should be optimized
      (fw:assert-better-performance
       (fw:benchmark-result-average-time new-result)
       (fw:benchmark-result-average-time old-result)
       :threshold 0.3))))

(deftest benchmark-pointer-operations ()
  "Benchmark pointer argument functions"
  (setup-test-functions)
  
  (old:with-foreign-memory (old-buf1 128)
    (old:with-foreign-memory (old-buf2 128)
      (new:with-foreign-memory ((new-buf1 128))
        (new:with-foreign-memory ((new-buf2 128))
          ;; Initialize buffers
          (dotimes (i 128)
            (setf (sb-sys:sap-ref-8 old-buf1 i) (mod i 256))
            (setf (sb-sys:sap-ref-8 old-buf2 i) (mod i 256))
            (setf (sb-sys:sap-ref-8 new-buf1 i) (mod i 256))
            (setf (sb-sys:sap-ref-8 new-buf2 i) (mod i 256)))
          
          (let ((old-result (fw:benchmark-operation "old-memcmp"
                              (lambda () (old-memcmp old-buf1 old-buf2 128))
                              :iterations 50000))
                (new-result (fw:benchmark-operation "new-memcmp"
                              (lambda () (new-memcmp new-buf1 new-buf2 128))
                              :iterations 50000)))
            
            (fw:report-benchmark-comparison old-result new-result)
            
            ;; Should be comparable or better
            (is (<= (fw:benchmark-result-average-time new-result)
                    (* 1.1 (fw:benchmark-result-average-time old-result))))))))))

;;;; Memory Management Benchmarks

(deftest benchmark-memory-allocation ()
  "Benchmark memory allocation and deallocation"
  (let ((old-result (fw:benchmark-operation "old-alloc/free"
                      (lambda ()
                        (let ((ptr (old:foreign-alloc 1024)))
                          (old:foreign-free ptr)))
                      :iterations 10000))
        (new-result (fw:benchmark-operation "new-alloc/free"
                      (lambda ()
                        (let ((ptr (new:foreign-alloc 1024)))
                          (new:foreign-free ptr)))
                      :iterations 10000)))
    
    (fw:report-benchmark-comparison old-result new-result)
    
    ;; Memory operations should be optimized
    (fw:assert-better-performance
     (fw:benchmark-result-average-time new-result)
     (fw:benchmark-result-average-time old-result)
     :threshold 0.2)))

(deftest benchmark-with-foreign-memory ()
  "Benchmark RAII memory management"
  (let ((old-result (fw:benchmark-operation "old-with-foreign-memory"
                      (lambda ()
                        (old:with-foreign-memory (ptr 256)
                          (setf (sb-sys:sap-ref-64 ptr 0) 42)))
                      :iterations 10000))
        (new-result (fw:benchmark-operation "new-with-foreign-memory"
                      (lambda ()
                        (new:with-foreign-memory (ptr 256)
                          (setf (sb-sys:sap-ref-64 ptr 0) 42)))
                      :iterations 10000)))
    
    (fw:report-benchmark-comparison old-result new-result)
    
    ;; Should have minimal overhead
    (is (<= (fw:benchmark-result-average-time new-result)
            (* 1.05 (fw:benchmark-result-average-time old-result))))))

;;;; Type Conversion Benchmarks

(deftest benchmark-type-conversions ()
  "Benchmark type conversion overhead"
  ;; Integer conversions
  (let ((old-int-result 
         (fw:benchmark-operation "old-int-conversion"
           (lambda () (old:convert-to-foreign 42 :int))
           :iterations 100000))
        (new-int-result
         (fw:benchmark-operation "new-int-conversion"
           (lambda () (new:convert-to-foreign 42 :int))
           :iterations 100000)))
    
    (fw:report-benchmark-comparison old-int-result new-int-result)
    
    ;; Should be essentially free
    (is (< (fw:benchmark-result-average-time new-int-result) 10)))
  
  ;; String conversions (more expensive)
  (let ((old-str-result
         (fw:benchmark-operation "old-string-conversion"
           (lambda ()
             (let ((ptr (old:convert-to-foreign "test" :string)))
               (when ptr
                 (old:free-converted-object ptr :string))))
           :iterations 10000))
        (new-str-result
         (fw:benchmark-operation "new-string-conversion"
           (lambda ()
             (new:with-c-string (ptr "test")
               ptr))
           :iterations 10000)))
    
    (fw:report-benchmark-comparison old-str-result new-str-result)
    
    ;; String handling should be optimized
    (fw:assert-better-performance
     (fw:benchmark-result-average-time new-str-result)
     (fw:benchmark-result-average-time old-str-result)
     :threshold 0.3)))

;;;; Cache Effectiveness Tests

(deftest benchmark-function-caching ()
  "Test that function lookups are cached effectively"
  (setup-test-functions)
  
  ;; First call (cache miss)
  (let ((first-call-time
         (fw:benchmark-operation "first-call"
           (lambda () (new-strlen "test"))
           :iterations 1)))
    
    ;; Subsequent calls (cache hit)
    (let ((cached-call-time
           (fw:benchmark-operation "cached-calls"
             (lambda () (new-strlen "test"))
             :iterations 10000)))
      
      ;; Cached calls should be much faster
      (is (< (fw:benchmark-result-average-time cached-call-time)
             (* 0.1 (fw:benchmark-result-average-time first-call-time)))))))

;;;; Stress Tests

(deftest stress-test-concurrent-calls ()
  "Stress test with concurrent FFI calls"
  (setup-test-functions)
  
  (let* ((thread-count 10)
         (iterations-per-thread 1000)
         (start-time (get-internal-real-time))
         (threads
          (loop for i below thread-count
                collect (sb-thread:make-thread
                         (lambda ()
                           (dotimes (j iterations-per-thread)
                             (new-strlen "concurrent test")
                             (new-getpid)))))))
    
    ;; Wait for all threads
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    
    (let* ((end-time (get-internal-real-time))
           (total-time (/ (- end-time start-time) 
                          internal-time-units-per-second))
           (total-calls (* thread-count iterations-per-thread 2))
           (calls-per-second (/ total-calls total-time)))
      
      (format t "~%Concurrent stress test: ~:D calls/second~%" 
              (round calls-per-second))
      
      ;; Should handle at least 100k calls/second
      (is (> calls-per-second 100000)))))

;;;; Memory Leak Tests

(deftest test-no-memory-leaks ()
  "Verify no memory leaks in common operations"
  ;; String conversion leak test
  (fw:detect-memory-leak
   (lambda ()
     (new:with-c-string (ptr "memory leak test")
       (new-strlen ptr)))
   :iterations 1000
   :threshold 0.05)
  
  ;; Memory allocation leak test
  (fw:detect-memory-leak
   (lambda ()
     (new:with-foreign-memory (ptr 1024)
       (setf (sb-sys:sap-ref-32 ptr 0) 42)))
   :iterations 1000
   :threshold 0.05)
  
  ;; Function call leak test
  (fw:detect-memory-leak
   (lambda ()
     (new-getpid))
   :iterations 10000
   :threshold 0.01))
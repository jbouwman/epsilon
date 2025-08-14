;;;; performance-tests.lisp - Performance benchmarks for FFI optimizations
;;;;
;;;; This module provides benchmarks to measure the performance improvements
;;;; from Phase 5 optimizations: inline expansion, memory pooling, and batching.

(defpackage epsilon.foreign.performance-tests
  (:use cl epsilon.syntax epsilon.test)
  (:local-nicknames
   (lib epsilon.foreign)
   (inline epsilon.foreign.inline)
   (pool epsilon.foreign.memory-pool)
   (batch epsilon.foreign.batch)))

(in-package epsilon.foreign.performance-tests)

;;; Test configuration

(defparameter *benchmark-iterations* 100000
  "Number of iterations for benchmarks")

(defparameter *warmup-iterations* 1000
  "Number of warmup iterations")

;;; Timing utilities

(defmacro measure-time ((&key (iterations *benchmark-iterations*)) &body body)
  "Measure execution time in milliseconds"
  `(progn
     ;; Warmup
     (dotimes (i *warmup-iterations*)
       ,@body)
     ;; Actual measurement
     (let ((start (get-internal-real-time)))
       (dotimes (i ,iterations)
         ,@body)
       (/ (- (get-internal-real-time) start)
          (/ internal-time-units-per-second 1000.0)))))

(defun report-speedup (baseline optimized name)
  "Report speedup from optimization"
  (format t "~%~A:~%" name)
  (format t "  Baseline:  ~,3f ms~%" baseline)
  (format t "  Optimized: ~,3f ms~%" optimized)
  (format t "  Speedup:   ~,2fx~%" (/ baseline optimized))
  (format t "  Improvement: ~,1f%~%" (* 100 (- 1 (/ optimized baseline)))))

;;; Inline expansion benchmarks

(deftest benchmark-inline-expansion
  "Benchmark inline expansion optimization"
  ;; Regular strlen call
  (let ((baseline-time
         (measure-time ()
           (lib:shared-call '("strlen" "libc") :unsigned-long '(:string) "hello world")))
        ;; Inline strlen call
        (inline-time
         (measure-time ()
           (inline:%strlen "hello world"))))
    
    (report-speedup baseline-time inline-time "Inline strlen")
    (is (< inline-time baseline-time) "Inline should be faster")))

(deftest benchmark-inline-multiple-args
  "Benchmark inline expansion with multiple arguments"
  ;; Regular memcpy
  (let* ((src (lib:foreign-alloc :char :count 100))
         (dst (lib:foreign-alloc :char :count 100))
         (baseline-time
          (measure-time ()
            (lib:shared-call '("memcpy" "libc") :pointer 
                            '(:pointer :pointer :unsigned-long)
                            dst src 100)))
         ;; Inline memcpy
         (inline-time
          (measure-time ()
            (inline:%memcpy dst src 100))))
    
    (lib:foreign-free src)
    (lib:foreign-free dst)
    
    (report-speedup baseline-time inline-time "Inline memcpy")
    (is (< inline-time baseline-time) "Inline memcpy should be faster")))

;;; Memory pooling benchmarks

(deftest benchmark-memory-pooling
  "Benchmark memory pool allocation vs regular allocation"
  ;; Regular allocation
  (let ((baseline-time
         (measure-time ()
           (let ((ptr (lib:foreign-alloc :char :count 256)))
             (lib:foreign-free ptr))))
        ;; Pooled allocation
        (pool (pool:create-pool :block-size 256 :max-blocks 1000))
        (pooled-time
         (measure-time ()
           (pool:with-pooled-memory (ptr 256 :pool pool)
             ;; Use memory to prevent optimization
             (setf (sb-sys:sap-ref-8 ptr 0) 42)))))
    
    (pool:pool-destroy pool)
    
    (report-speedup baseline-time pooled-time "Memory pooling (256 bytes)")
    (is (< pooled-time baseline-time) "Pooled allocation should be faster")))

(deftest benchmark-pool-reuse
  "Benchmark memory pool reuse efficiency"
  (let ((pool (pool:create-pool :block-size 1024 :max-blocks 10)))
    ;; First allocation wave (cold)
    (let ((cold-time
           (measure-time (:iterations 1000)
             (pool:with-pooled-memory (ptr 1024 :pool pool)
               (setf (sb-sys:sap-ref-32 ptr 0) 42))))
          ;; Second allocation wave (warm, reusing)
          (warm-time
           (measure-time (:iterations 1000)
             (pool:with-pooled-memory (ptr 1024 :pool pool)
               (setf (sb-sys:sap-ref-32 ptr 0) 42)))))
      
      (report-speedup cold-time warm-time "Pool reuse benefit")
      
      ;; Check pool statistics
      (let ((stats (pool:pool-statistics pool)))
        (format t "~%Pool statistics:~%")
        (format t "  Reuse ratio: ~,1f%~%" (* 100 (getf stats :reuse-ratio)))
        (is (> (getf stats :reuse-ratio) 0.5) "Pool should have good reuse ratio"))
      
      (pool:pool-destroy pool))))

;;; Batch operations benchmarks

(deftest benchmark-batch-operations
  "Benchmark batched FFI calls vs individual calls"
  ;; Individual calls
  (let ((strings '("hello" "world" "lisp" "foreign" "function" "interface"))
        (baseline-time
         (measure-time (:iterations 10000)
           (dolist (s strings)
             (lib:shared-call '("strlen" "libc") :unsigned-long '(:string) s))))
        ;; Batched calls
        (batched-time
         (measure-time (:iterations 10000)
           (batch:with-foreign-batch ()
             (dolist (s strings)
               (batch:batch-call '("strlen" "libc") :unsigned-long '(:string) s))))))
    
    (report-speedup baseline-time batched-time "Batch string operations")
    (is (<= batched-time baseline-time) "Batched calls should not be slower")))

(deftest benchmark-batch-memory-operations
  "Benchmark batched memory operations"
  (let* ((size 100)
         (copies 10)
         (src-list (loop repeat copies collect (lib:foreign-alloc :char :count size)))
         (dst-list (loop repeat copies collect (lib:foreign-alloc :char :count size))))
    
    ;; Individual memory copies
    (let ((baseline-time
           (measure-time (:iterations 1000)
             (loop for src in src-list
                   for dst in dst-list
                   do (lib:shared-call '("memcpy" "libc") :pointer
                                      '(:pointer :pointer :unsigned-long)
                                      dst src size))))
          ;; Batched memory copies
          (batched-time
           (measure-time (:iterations 1000)
             (batch:with-foreign-batch ()
               (loop for src in src-list
                     for dst in dst-list
                     do (batch:batch-call '("memcpy" "libc") :pointer
                                         '(:pointer :pointer :unsigned-long)
                                         dst src size))))))
      
      (report-speedup baseline-time batched-time "Batch memory copies")
      
      ;; Cleanup
      (mapc #'lib:foreign-free src-list)
      (mapc #'lib:foreign-free dst-list))))

;;; Combined optimizations benchmark

(deftest benchmark-combined-optimizations
  "Benchmark all optimizations combined"
  ;; Scenario: Process array of strings
  (let ((strings (loop repeat 100 collect "test string for processing"))
        (pool (pool:create-pool :block-size 256 :max-blocks 100)))
    
    ;; Baseline: No optimizations
    (let ((baseline-time
           (measure-time (:iterations 100)
             (dolist (s strings)
               (let* ((len (lib:shared-call '("strlen" "libc") :unsigned-long '(:string) s))
                      (buf (lib:foreign-alloc :char :count (1+ len))))
                 (lib:shared-call '("strcpy" "libc") :pointer '(:pointer :string) buf s)
                 (lib:foreign-free buf)))))
          
          ;; Optimized: All techniques
          (optimized-time
           (measure-time (:iterations 100)
             (batch:with-foreign-batch (:pool pool)
               (dolist (s strings)
                 (let* ((len (inline:%strlen s))
                        (buf (pool:pool-allocate (1+ len) pool)))
                   (batch:batch-call '("strcpy" "libc") :pointer '(:pointer :string) buf s)
                   (pool:pool-free buf pool)))))))
      
      (report-speedup baseline-time optimized-time "Combined optimizations")
      (pool:pool-destroy pool)
      
      ;; Expect significant improvement
      (is (< (* optimized-time 1.5) baseline-time) 
          "Combined optimizations should provide significant speedup"))))

;;; Trampoline performance (from Phase 1)

(deftest benchmark-trampoline-vs-eval
  "Compare compiled trampolines vs eval-based calls"
  ;; This validates Phase 1 improvements
  (let* ((strlen-trampoline (lib::get-or-create-trampoline :unsigned-long '(:string)))
         (trampoline-time
          (measure-time ()
            (funcall strlen-trampoline 
                    (sb-alien:extern-alien "strlen" 
                                          (sb-alien:function sb-alien:unsigned-long
                                                            sb-alien:c-string))
                    "test string")))
         ;; Simulate old eval-based approach
         (eval-time
          (measure-time ()
            (eval `(sb-alien:alien-funcall
                   (sb-alien:extern-alien "strlen"
                                         (sb-alien:function sb-alien:unsigned-long
                                                           sb-alien:c-string))
                   "test string")))))
    
    (report-speedup eval-time trampoline-time "Compiled trampolines (Phase 1)")
    (is (< trampoline-time eval-time) "Trampolines should be faster than eval")))

;;; Performance regression tests

(deftest test-no-performance-regression
  "Ensure optimizations don't cause regression"
  ;; Simple baseline test
  (let ((baseline (measure-time (:iterations 10000)
                   (lib:shared-call '("abs" "libc") :int '(:int) -42))))
    ;; Should complete in reasonable time
    (is (< baseline 1000) "Basic call should complete in < 1 second for 10k iterations")))

;;; Run all benchmarks

(defun run-performance-benchmarks ()
  "Run all performance benchmarks and generate report"
  (format t "~%========================================~%")
  (format t "FFI Performance Benchmarks~%")
  (format t "========================================~%")
  (format t "Iterations: ~D~%" *benchmark-iterations*)
  
  (run-tests 'benchmark-inline-expansion
             'benchmark-inline-multiple-args
             'benchmark-memory-pooling
             'benchmark-pool-reuse
             'benchmark-batch-operations
             'benchmark-batch-memory-operations
             'benchmark-combined-optimizations
             'benchmark-trampoline-vs-eval
             'test-no-performance-regression)
  
  (format t "~%========================================~%")
  (format t "Benchmark Summary Complete~%")
  (format t "========================================~%"))
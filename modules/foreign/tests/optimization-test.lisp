;;;; optimization-test.lisp - Test FFI compiler optimizations

(defpackage epsilon.foreign.optimization-test
  (:use cl epsilon.test)
  (:local-nicknames
   (ffi epsilon.foreign)
   (opt epsilon.foreign.optimize)))

(in-package epsilon.foreign.optimization-test)

(deftest test-direct-optimization
  "Test that simple libc calls get optimized"
  ;; Test strlen optimization
  (let ((result (ffi:shared-call '("strlen" "libc") :unsigned-long '(:string) "hello")))
    (is (= result 5) "strlen should return correct length"))
  
  ;; Test with optimization explicitly
  (let ((result (opt:%strlen-optimized "hello world")))
    (is (= result 11) "Optimized strlen should work")))

(deftest test-optimization-control
  "Test enabling/disabling optimizations"
  (opt:enable-ffi-optimizations)
  (is opt:*optimization-enabled* "Optimizations should be enabled")
  
  (opt:disable-ffi-optimizations)
  (is (not opt:*optimization-enabled*) "Optimizations should be disabled")
  
  ;; Re-enable for other tests
  (opt:enable-ffi-optimizations))

(deftest test-compile-time-optimization
  "Test that compiler macros work at compile time"
  ;; This function should be optimized at compile time
  (flet ((test-strlen (s)
           (ffi:shared-call '("strlen" "libc") :unsigned-long '(:string) s)))
    (is (= (test-strlen "test") 4) "Compile-time optimized call should work")
    (is (= (test-strlen "longer string") 13) "Another optimized call")))

(deftest test-performance-improvement
  "Verify optimization provides performance benefit"
  (skip)
  (let ((test-string "This is a test string for performance measurement")
        (iterations 10000))
    
    ;; Time regular call (with trampolines)
    (let ((start (get-internal-real-time)))
      (dotimes (i iterations)
        (ffi:shared-call-fast '("strlen" "libc") :unsigned-long '(:string) test-string))
      (let ((regular-time (- (get-internal-real-time) start)))
        
        ;; Time optimized call
        (setf start (get-internal-real-time))
        (dotimes (i iterations)
          (opt:%strlen-optimized test-string))
        (let ((optimized-time (- (get-internal-real-time) start)))
          
          ;; Optimized should be faster (or at least not slower)
          (is (<= optimized-time (* regular-time 1.1))
              "Optimized call should not be significantly slower than regular")
          
          ;; Report speedup
          (when (> regular-time 0)
            (format t "~%  Speedup: ~,2fx (regular: ~D, optimized: ~D)~%"
                    (/ regular-time (max 1 optimized-time))
                    regular-time optimized-time)))))))

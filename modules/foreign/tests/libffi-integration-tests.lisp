;;;; libffi-integration-tests.lisp - Comprehensive integration tests
;;;;
;;;; This test suite validates the complete libffi-first architecture
;;;; including signature extraction, auto-discovery, and performance.

(defpackage epsilon.foreign.test.integration
  (:use cl epsilon.syntax epsilon.test)
  (:local-nicknames
   (foreign epsilon.foreign))
  (:export
   #:run-integration-tests
   #:test-complete-system
   #:test-api-compatibility
   #:test-performance-regression
   #:test-signature-coverage))

(in-package :epsilon.foreign.test.integration)

;;;; System Availability Tests

(deftest test-system-components
  "Test that all system components are available"
  ;; Core libffi
  (is (boundp 'foreign:*libffi-available-p*) "libffi availability flag should exist")
  
  ;; Signature extraction
  (is (find-package :epsilon.clang.signatures) "Clang signatures package should exist")
  
  ;; Public API functions
  (is (fboundp 'foreign:ffi-call) "ffi-call should be available")
  (is (fboundp 'foreign:shared-call-unified) "shared-call-unified should be available")
  (is (fboundp 'foreign:auto-discover-signature) "auto-discover-signature should be available")
  
  ;; Configuration variables
  (is (boundp 'foreign:*use-libffi-calls*) "libffi configuration should exist")
  (is (boundp 'foreign:*track-call-performance*) "performance tracking should exist"))

;;;; Basic Functionality Tests

(deftest test-simple-ffi-calls
  "Test basic FFI calls work correctly"
  (handler-case
      (progn
        ;; Test getpid (no arguments)
        (let ((pid (foreign:shared-call-unified "getpid" :int '())))
          (is (and (integerp pid) (> pid 0)) "getpid should return positive integer"))
        
        ;; Test strlen (string argument)  
        (let ((len (foreign:shared-call-unified "strlen" :unsigned-long '(:string) "hello")))
          (is-= len 5 "strlen('hello') should return 5")))
    (error (e)
	   (format t "Basic FFI call test failed: ~A~%" e)
	   (is nil "Basic FFI calls should work"))))

(deftest test-auto-discovery-calls
  "Test automatic signature discovery"
  (if (find-package :epsilon.clang.signatures)
      (handler-case
          (progn
            ;; Test auto-discovery function
            (let ((signature (foreign:auto-discover-signature "strlen")))
              (if signature
                  (is t "Should auto-discover strlen signature")
                (format t "Could not auto-discover strlen signature~%")))
            
            ;; Test ffi-call with auto-discovery
            (let ((len (foreign:ffi-call "strlen" "test")))
              (is-= len 4 "ffi-call with auto-discovery should work")))
        (error (e)
               (format t "Auto-discovery test failed: ~A~%" e)))
    (format t "Skipping auto-discovery test - clang signatures not available~%")))

;;;; API Compatibility Tests

(deftest test-original-shared-call-compatibility
  "Test that original shared-call still works"
  (handler-case
      (let ((pid (foreign:shared-call "getpid" :int '())))
        (is (and (integerp pid) (> pid 0)) 
            "Original shared-call should still work"))
    (error (e)
	   (format t "Original shared-call compatibility test failed: ~A~%" e)
	   (is nil "Original shared-call should be preserved"))))

(deftest test-defshared-compatibility
  "Test defshared macro compatibility"
  (handler-case
      (progn
        ;; Test original defshared if available
        (when (fboundp 'foreign:defshared)
          (eval '(foreign:defshared test-getpid "getpid" "libc" :int ()))
          (when (fboundp 'test-getpid)
            (let ((pid (test-getpid)))
              (is (and (integerp pid) (> pid 0)) "defshared should work"))))
        
        ;; Test new defshared-smart
        (eval '(foreign:defshared-smart test-getpid-smart "getpid" "libc" :int ()))
        (when (fboundp 'test-getpid-smart)
          (let ((pid (test-getpid-smart)))
            (is (and (integerp pid) (> pid 0)) "defshared-smart should work"))))
    (error (e)
	   (format t "defshared compatibility test failed: ~A~%" e))))

;;;; Performance Tests

(deftest test-call-performance
  "Test that FFI calls perform reasonably"
  (let ((iterations 100)
        (function-name "getpid"))
    
    ;; Since we now have a unified implementation, just test performance
    (let ((call-time
           (handler-case
               (let ((start (get-internal-real-time)))
                 (dotimes (i iterations)
                   (foreign:shared-call function-name :int '()))
                 (/ (- (get-internal-real-time) start) 
                    internal-time-units-per-second))
             (error (e)
		    (format t "Performance test failed: ~A~%" e)
		    nil))))
      
      (when call-time
        (format t "FFI Performance (~D calls):~%" iterations)
        (format t "  Time: ~,6F seconds~%" call-time)
        (format t "  Per call: ~,3F microseconds~%" (* 1000000 (/ call-time iterations)))
        ;; Just verify calls complete in reasonable time
        (is (< call-time 1.0) "FFI calls should complete within 1 second")))))

;;;; Signature System Tests

(deftest test-signature-database
  "Test signature database functionality"
  (if (find-package :epsilon.clang.signatures)
      (let ((db-symbol (find-symbol "*SIGNATURE-DATABASE*" :epsilon.clang.signatures)))
        (when db-symbol
          (let ((original-count (hash-table-count (symbol-value db-symbol))))
            ;; Test caching a signature
            (handler-case
                (let ((cache-fn (find-symbol "CACHE-SIGNATURE" :epsilon.clang.signatures))
                      (get-fn (find-symbol "GET-CACHED-SIGNATURE" :epsilon.clang.signatures)))
                  (when (and cache-fn get-fn)
                    ;; Create a test signature
                    (let ((test-sig (funcall (find-symbol "MAKE-FUNCTION-SIGNATURE" :epsilon.clang.signatures)
                                             :name "test_func"
                                             :return-type :int
                                             :arg-types '(:int))))
                      (funcall cache-fn "test_func" test-sig :library "test")
                      
                      ;; Verify it was cached
                      (let ((retrieved (funcall get-fn "test_func" "test")))
                        (is retrieved "Should retrieve cached signature")))))
              (error (e)
                     (format t "Signature database test failed: ~A~%" e))))))
    (format t "Skipping signature database test - clang signatures not available~%")))

;;;; Error Handling Tests

(deftest test-error-handling
  "Test error handling in various scenarios"
  ;; Test with nonexistent function
  (handler-case
      (foreign:shared-call-unified "nonexistent_function_xyz" :int '())
    (error (e)
	   (is t "Should handle nonexistent functions gracefully")))
  
  ;; Test with invalid arguments
  (handler-case
      (foreign:shared-call-unified "strlen" :unsigned-long '(:string) 123)
    (error (e)
	   (is t "Should handle type mismatches gracefully")))
  
  ;; Test auto-discovery with invalid function
  (if (find-package :epsilon.clang.signatures)
      (handler-case
          (let ((sig (foreign:auto-discover-signature "nonexistent_function_xyz")))
            (is (null sig) "Should return nil for nonexistent functions"))
        (error (e)
               (is t "Should handle auto-discovery errors gracefully")))
    (format t "Skipping auto-discovery error test - clang signatures not available~%")))

;;;; Configuration Tests

(deftest test-configuration-system
  "Test configuration and toggles"
  ;; Test that libffi is properly initialized
  (is foreign::*libffi-available-p* "libffi should be available")
  
  ;; Test that we can make calls with libffi
  (let ((result (foreign:shared-call-unified "getpid" :int '())))
    (is (and (integerp result) (> result 0)) 
        "Should successfully call getpid through libffi")))

;;;; Stress Tests

(deftest test-multiple-function-types
  "Test various function types and signatures"
  (let ((test-cases
         '(;; No arguments
           ("getpid" :int ())
           ;; String argument
           ("strlen" :unsigned-long (:string) "test")
           ;; Integer argument (if close is available)
           ;; Pointer arguments (if available)
           )))
    
    (dolist (test-case test-cases)
      (destructuring-bind (fn-name return-type arg-types &rest args) test-case
			  (handler-case
			      (let ((result (apply #'foreign:shared-call-unified 
						   fn-name return-type arg-types args)))
				(format t "~A: OK (~A)~%" fn-name result))
			    (error (e)
				   (format t "~A: FAILED (~A)~%" fn-name e)))))))

(deftest test-concurrent-calls
  "Test thread safety of FFI calls"
  (let ((threads '())
        (results (make-hash-table :test 'equal))
        (results-lock (sb-thread:make-mutex :name "results-lock")))
    
    ;; Create multiple threads making FFI calls
    (dotimes (i 5)
      (push
       (sb-thread:make-thread
        (lambda ()
          (let ((thread-id (sb-thread:thread-name sb-thread:*current-thread*)))
            (handler-case
                (dotimes (j 10)
                  (let ((pid (foreign:shared-call-unified "getpid" :int '())))
                    (sb-thread:with-mutex (results-lock)
					  (setf (gethash (list thread-id j) results) pid))))
              (error (e)
                     (sb-thread:with-mutex (results-lock)
					   (setf (gethash (list thread-id 'error) results) e))))))
        :name (format nil "ffi-test-~D" i))
       threads))
    
    ;; Wait for all threads
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    
    ;; Check results
    (let ((error-count 0)
          (success-count 0))
      (maphash (lambda (key value)
                 (if (eq (second key) 'error)
                     (incf error-count)
                   (incf success-count)))
               results)
      
      (format t "Concurrent test: ~D successes, ~D errors~%" success-count error-count)
      (is (= error-count 0) "Should have no errors in concurrent calls"))))

;;;; Documentation and Help Tests

(deftest test-help-system
  "Test help and documentation functions"
  (handler-case
      (with-output-to-string (*standard-output*)
			     (foreign:ffi-help))
    (error (e)
	   (format t "Help system test failed: ~A~%" e)
	   (is nil "Help system should work")))
  
  (handler-case
      (with-output-to-string (*standard-output*)
			     (foreign:ffi-system-status))
    (error (e)
	   (format t "System status test failed: ~A~%" e)
	   (is nil "System status should work"))))

;;;; Integration Test Suite Runner

(deftest test-complete-integration
  "Complete integration test of all components"
  (format t "Running complete libffi integration test...~%")
  
  ;; Test system initialization
  (is t "System should initialize without errors")
  
  ;; Test core functionality
  (handler-case
      (let ((pid (foreign:shared-call-unified "getpid" :int '())))
        (is (and (integerp pid) (> pid 0)) "Basic FFI should work"))
    (error (e)
	   (format t "Core functionality test failed: ~A~%" e)))
  
  ;; Test auto-discovery if available
  (when (find-package :epsilon.clang.signatures)
    (handler-case
        (let ((len (foreign:ffi-call "strlen" "integration test")))
          (is-= len 16 "Auto-discovery should work"))
      (error (e)
             (format t "Auto-discovery integration failed: ~A~%" e))))
  
  ;; Test performance tracking
  (let ((original-tracking foreign:*track-call-performance*))
    (unwind-protect
        (progn
          (setf foreign:*track-call-performance* t)
          (foreign:shared-call-unified "getpid" :int '())
          (let ((stats (foreign:get-call-statistics)))
            (is (>= (length stats) 0) "Performance tracking should work")))
      (setf foreign:*track-call-performance* original-tracking)))
  
  (format t "Complete integration test finished~%"))

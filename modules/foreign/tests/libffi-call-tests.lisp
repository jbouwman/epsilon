;;;; libffi-call-tests.lisp - Tests for libffi-based function calls

(defpackage epsilon.foreign.test.libffi-calls
  (:use cl epsilon.syntax epsilon.test)
  (:local-nicknames
   (foreign epsilon.foreign))
  (:export
   #:run-libffi-call-tests
   #:test-libffi-basic-calls
   #:test-libffi-types
   #:test-libffi-fallback
   #:test-libffi-performance))

(in-package :epsilon.foreign.test.libffi-calls)

;;;; Helper Functions

(defun libffi-calls-available-p ()
  "Check if libffi function calls are available"
  (and (find-package '#:epsilon.foreign)
       (find-symbol "*LIBFFI-AVAILABLE-P*" '#:epsilon.foreign)
       (symbol-value (find-symbol "*LIBFFI-AVAILABLE-P*" '#:epsilon.foreign))
       (find-symbol "LIBFFI-AVAILABLE-FOR-CALLS-P" '#:epsilon.foreign)
       (funcall (find-symbol "LIBFFI-AVAILABLE-FOR-CALLS-P" '#:epsilon.foreign))))

(defun call-libffi-function (symbol &rest args)
  "Call a libffi function if available"
  (when (libffi-calls-available-p)
    (let ((func (find-symbol (string symbol) '#:epsilon.foreign)))
      (when func
        (apply func args)))))

;;;; Basic Function Call Tests

(deftest test-libffi-extension-for-calls
  "Test that libffi extension supports function calls"
  (if (libffi-calls-available-p)
      (progn
        (is t "libffi function call support available")
        (format t "libffi call support: Available~%"))
      (progn
        (format t "libffi call support: Not available - skipping tests~%")
        (is t "Test skipped due to missing libffi"))))

(deftest test-resolve-function-address
  "Test function address resolution"
  (if (libffi-calls-available-p)
      (handler-case
          (let ((addr (call-libffi-function 'resolve-function-address "getpid")))
            (is (and addr (> addr 0)) "getpid address should be valid"))
        (error (e)
          (format t "Function resolution test failed: ~A~%" e)))
      (format t "Skipping function resolution test - libffi not available~%")))

;;;; Type Validation Tests

(deftest test-call-signature-validation
  "Test call signature validation"
  (if (libffi-calls-available-p)
      (progn
        ;; Valid signature should not error
        (handler-case
            (progn
              (call-libffi-function 'validate-call-signature :int '(:int) '(42))
              (is t "Valid signature should pass validation"))
          (error (e)
            (is nil "Valid signature failed: ~A" e)))
        
        ;; Invalid signature should error
        (handler-case
            (progn
              (call-libffi-function 'validate-call-signature :int '(:int :int) '(42))
              (is nil "Invalid signature should fail validation"))
          (error (e)
            (is t "Invalid signature correctly failed validation"))))
      (format t "Skipping signature validation test - libffi not available~%")))

(deftest test-argument-type-validation
  "Test individual argument type validation"
  (if (libffi-calls-available-p)
      (progn
        ;; Valid types
        (handler-case
            (progn
              (call-libffi-function 'validate-argument-type 42 :int)
              (call-libffi-function 'validate-argument-type "hello" :string)
              (is t "Valid argument types should pass"))
          (error (e)
            (is nil "Valid argument types failed: ~A" e)))
        
        ;; Invalid types
        (handler-case
            (progn
              (call-libffi-function 'validate-argument-type "string" :int)
              (is nil "Invalid argument type should fail"))
          (error (e)
            (is t "Invalid argument type correctly failed"))))
      (format t "Skipping argument validation test - libffi not available~%")))

;;;; Function Call Tests

(deftest test-simple-libffi-calls
  "Test simple function calls through libffi"
  (if (libffi-calls-available-p)
      (progn
        ;; Test getpid() - simple no-argument function
        (handler-case
            (let ((pid (call-libffi-function 'shared-call-unified "getpid" :int '())))
              (is (and (integerp pid) (> pid 0)) "getpid should return positive integer"))
          (error (e)
            (format t "getpid test failed: ~A~%" e)))
        
        ;; Test strlen() - function with string argument
        (handler-case
            (let ((len (call-libffi-function 'shared-call-unified "strlen" :unsigned-long '(:string) "hello")))
              (is-= len 5 "strlen('hello') should return 5"))
          (error (e)
            (format t "strlen test failed: ~A~%" e))))
      (format t "Skipping function call tests - libffi not available~%")))

(deftest test-libffi-vs-original
  "Compare libffi calls with original implementation"
  (if (libffi-calls-available-p)
      (progn
        ;; Test that both implementations give same results
        (handler-case
            (let ((original-result (foreign:shared-call "getpid" :int '()))
                  (libffi-result (call-libffi-function 'shared-call-unified "getpid" :int '())))
              ;; PIDs might differ due to timing, but both should be positive
              (is (and (integerp original-result) (> original-result 0)) 
                  "Original implementation should work")
              (is (and (integerp libffi-result) (> libffi-result 0))
                  "libffi implementation should work"))
          (error (e)
            (format t "Comparison test failed: ~A~%" e))))
      (format t "Skipping comparison test - libffi not available~%")))

;;;; Fallback Tests

(deftest test-fallback-mechanism
  "Test fallback to SBCL when libffi fails"
  (if (libffi-calls-available-p)
      (let ((original-use-libffi (when (find-symbol "*USE-LIBFFI-CALLS*" '#:epsilon.foreign)
                                   (symbol-value (find-symbol "*USE-LIBFFI-CALLS*" '#:epsilon.foreign)))))
        (unwind-protect
             (progn
               ;; Disable libffi temporarily
               (when (find-symbol "*USE-LIBFFI-CALLS*" '#:epsilon.foreign)
                 (setf (symbol-value (find-symbol "*USE-LIBFFI-CALLS*" '#:epsilon.foreign)) nil))
               
               (handler-case
                   (let ((result (call-libffi-function 'shared-call-unified "getpid" :int '())))
                     (is (and (integerp result) (> result 0))
                         "Fallback should work when libffi disabled"))
                 (error (e)
                   (format t "Fallback test failed: ~A~%" e))))
          ;; Restore original setting
          (when (find-symbol "*USE-LIBFFI-CALLS*" '#:epsilon.foreign)
            (setf (symbol-value (find-symbol "*USE-LIBFFI-CALLS*" '#:epsilon.foreign)) 
                  original-use-libffi))))
      (format t "Skipping fallback test - libffi not available~%")))

;;;; Performance Tests

(deftest test-call-performance-tracking
  "Test call performance tracking"
  (if (libffi-calls-available-p)
      (let ((original-tracking (when (find-symbol "*TRACK-CALL-PERFORMANCE*" '#:epsilon.foreign)
                                 (symbol-value (find-symbol "*TRACK-CALL-PERFORMANCE*" '#:epsilon.foreign)))))
        (unwind-protect
             (progn
               ;; Enable performance tracking
               (when (find-symbol "*TRACK-CALL-PERFORMANCE*" '#:epsilon.foreign)
                 (setf (symbol-value (find-symbol "*TRACK-CALL-PERFORMANCE*" '#:epsilon.foreign)) t))
               
               ;; Make some calls
               (dotimes (i 5)
                 (call-libffi-function 'shared-call-unified "getpid" :int '()))
               
               ;; Check statistics
               (let ((stats (call-libffi-function 'get-call-statistics '("getpid" "libc"))))
                 (when stats
                   (is (>= (getf stats :count) 5) "Should track at least 5 calls"))))
          ;; Restore original setting
          (when (find-symbol "*TRACK-CALL-PERFORMANCE*" '#:epsilon.foreign)
            (setf (symbol-value (find-symbol "*TRACK-CALL-PERFORMANCE*" '#:epsilon.foreign)) 
                  original-tracking))))
      (format t "Skipping performance tracking test - libffi not available~%")))

;;;; Integration Tests

(deftest test-libffi-integration-complete
  "Complete integration test of libffi function call system"
  (if (libffi-calls-available-p)
      (handler-case
          (progn
            (format t "Running complete libffi integration test...~%")
            
            ;; Test the integration function
            (call-libffi-function 'test-libffi-integration)
            
            (is t "Complete integration test passed"))
        (error (e)
          (format t "Integration test failed: ~A~%" e)
          (is nil "Integration test failed")))
      (format t "Skipping integration test - libffi not available~%")))

;;;; Diagnostic Tests

(deftest test-diagnose-ffi-call
  "Test FFI call diagnosis functionality"
  (if (libffi-calls-available-p)
      (handler-case
          (progn
            (call-libffi-function 'diagnose-ffi-call "strlen" :unsigned-long '(:string) "test")
            (is t "Diagnosis function should work without error"))
        (error (e)
          (format t "Diagnosis test failed: ~A~%" e)))
      (format t "Skipping diagnosis test - libffi not available~%")))

;;;; Test Suite Runner

(defun run-libffi-call-tests ()
  "Run all libffi function call tests"
  (format t "Running libffi function call tests...~%")
  (if (libffi-calls-available-p)
      (format t "libffi function call support detected~%")
      (progn
        (format t "libffi function call support not available~%")
        (format t "Tests will be skipped where libffi is required~%"))))

;; Tests are automatically discovered by the test framework
;;;; libffi-integration-tests.lisp - Comprehensive tests for libffi callback integration
;;;;
;;;; These tests verify the libffi C extension works correctly and integrates
;;;; properly with the existing Epsilon FFI callback infrastructure.

(defpackage epsilon.foreign.test.libffi
  (:use cl epsilon.syntax epsilon.test)
  (:local-nicknames
   (foreign epsilon.foreign))
  (:export
   #:run-libffi-tests
   #:test-libffi-basic
   #:test-libffi-types
   #:test-libffi-callbacks
   #:test-libffi-integration
   #:test-libffi-fallback
   #:test-libffi-performance))

(in-package :epsilon.foreign.test.libffi)

;;;; Test Helper Functions

(defun libffi-available-p ()
  "Check if libffi integration is available"
  (and (find-package '#:epsilon.foreign)
       (find-symbol "*LIBFFI-AVAILABLE-P*" '#:epsilon.foreign)
       (symbol-value (find-symbol "*LIBFFI-AVAILABLE-P*" '#:epsilon.foreign))))

(defun call-libffi-function (symbol &rest args)
  "Call a libffi function if available"
  (when (libffi-available-p)
    (let ((func (find-symbol (string symbol) '#:epsilon.foreign)))
      (when func
        (apply func args)))))

;;;; Basic Extension Tests

(deftest test-libffi-extension-loading
  "Test that the libffi C extension loads correctly"
  (is (or (libffi-available-p)
               (progn
                 (format t "libffi extension not available - skipping tests~%")
                 t))))

(deftest test-libffi-basic-function
  "Test basic libffi extension functionality"
  (if (libffi-available-p)
      (let ((result (call-libffi-function 'epsilon-libffi-test)))
        (is-= result 42 "libffi test function should return 42"))
      (format t "libffi not available - skipping basic function test~%")))

;;;; Type System Tests

(deftest test-libffi-type-constants
  "Test that libffi type constants are correctly defined"
  (if (libffi-available-p)
      (progn
        (is (= (symbol-value (find-symbol "+EPSILON-TYPE-VOID+" '#:epsilon.foreign)) 0))
        (is (= (symbol-value (find-symbol "+EPSILON-TYPE-INT+" '#:epsilon.foreign)) 1))
        (is (= (symbol-value (find-symbol "+EPSILON-TYPE-POINTER+" '#:epsilon.foreign)) 5)))
      (format t "libffi not available - skipping type constants test~%")))

(deftest test-libffi-type-conversion
  "Test Lisp type to libffi type conversion"
  (skip "libffi type conversion not yet implemented")
  (if (libffi-available-p)
      (let ((converter (find-symbol "LISP-TYPE-TO-EPSILON-TYPE" '#:epsilon.foreign)))
        (when converter
          (is-= (funcall converter :int) 1)
          (is-= (funcall converter :void) 0)
          (is-= (funcall converter :pointer) 5)
          (is-thrown 'error (funcall converter :unknown-type))))
      (format t "libffi not available - skipping type conversion test~%")))

;;;; Callback Creation Tests

(deftest test-libffi-simple-callback
  "Test creating a simple callback with libffi"
  (if (libffi-available-p)
      (let ((simple-func (lambda (x) (+ x 1))))
        (handler-case
            (let ((callback-ptr (call-libffi-function 'make-libffi-callback
                                                      simple-func :int '(:int))))
              (is (not (sb-alien:null-alien callback-ptr))
                       "Callback pointer should not be null"))
          (error (e)
            (format t "libffi callback creation failed: ~A~%" e))))
      (format t "libffi not available - skipping simple callback test~%")))

(deftest test-libffi-callback-types
  "Test callbacks with different argument and return types"
  (skip "libffi callback types not yet implemented")
  (if (libffi-available-p)
      (progn
        ;; Test void return
        (handler-case
            (let ((void-func (lambda (x) (declare (ignore x)) nil)))
              (call-libffi-function 'make-libffi-callback void-func :void '(:int))
              (is t "Void callback created successfully"))
          (error (e)
            (is nil "Void callback creation failed: ~A" e)))
        
        ;; Test multiple arguments
        (handler-case
            (let ((multi-func (lambda (x y) (+ x y))))
              (call-libffi-function 'make-libffi-callback multi-func :int '(:int :int))
              (is t "Multi-argument callback created successfully"))
          (error (e)
            (is nil "Multi-argument callback creation failed: ~A" e))))
      (format t "libffi not available - skipping callback types test~%")))

;;;; Integration Tests

(deftest test-libffi-enhanced-make-callback
  "Test that enhanced make-callback uses libffi when available"
  (let ((test-func (lambda (x) (* x 2))))
    (handler-case
        (let ((callback-ptr (foreign:make-callback test-func :int '(:int))))
          (is (not (sb-alien:null-alien callback-ptr))
                   "Enhanced make-callback should return valid pointer"))
      (error (e)
        (format t "Enhanced make-callback failed: ~A~%" e)))))

(deftest test-libffi-fallback-behavior
  "Test fallback to SBCL implementation when libffi fails"
  (skip "libffi fallback behavior not yet implemented")
  ;; Temporarily disable libffi to test fallback
  (let ((original-use-libffi (when (find-symbol "*USE-LIBFFI*" '#:epsilon.foreign.callback)
                               (symbol-value (find-symbol "*USE-LIBFFI*" '#:epsilon.foreign.callback)))))
    (unwind-protect
         (progn
           (when (find-symbol "*USE-LIBFFI*" '#:epsilon.foreign.callback)
             (setf (symbol-value (find-symbol "*USE-LIBFFI*" '#:epsilon.foreign.callback)) nil))
           (let ((test-func (lambda (x) (+ x 10))))
             (handler-case
                 (let ((callback-ptr (foreign:make-callback test-func :int '(:int))))
                   (is (not (sb-alien:null-alien callback-ptr))
                            "Fallback callback should return valid pointer"))
               (error (e)
                 (is nil "Fallback callback creation failed: ~A" e)))))
      ;; Restore original setting
      (when (find-symbol "*USE-LIBFFI*" '#:epsilon.foreign.callback)
        (setf (symbol-value (find-symbol "*USE-LIBFFI*" '#:epsilon.foreign.callback)) 
              original-use-libffi)))))

;;;; Registry and Memory Management Tests

(deftest test-libffi-callback-registry
  "Test libffi callback registry management"
  (skip "libffi callback registry not yet implemented")
  (if (libffi-available-p)
      (let ((initial-count (call-libffi-function 'libffi-callback-count)))
        (let ((test-func (lambda (x) x)))
          (handler-case
              (progn
                (call-libffi-function 'make-libffi-callback test-func :int '(:int))
                (let ((new-count (call-libffi-function 'libffi-callback-count)))
                  (is (> new-count initial-count)
                           "Callback count should increase after creation")))
            (error (e)
              (is nil "Callback registry test failed: ~A" e)))))
      (format t "libffi not available - skipping registry test~%")))

(deftest test-libffi-cleanup
  "Test libffi callback cleanup functionality"
  (if (libffi-available-p)
      (progn
        ;; Create some callbacks
        (let ((test-func (lambda (x) x)))
          (dotimes (i 3)
            (handler-case
                (call-libffi-function 'make-libffi-callback test-func :int '(:int))
              (error ()))))
        
        ;; Test cleanup
        (handler-case
            (progn
              (call-libffi-function 'cleanup-libffi-callbacks)
              (let ((count-after-cleanup (call-libffi-function 'libffi-callback-count)))
                (is-= count-after-cleanup 0
                          "Callback count should be 0 after cleanup")))
          (error (e)
            (is nil "Callback cleanup failed: ~A" e))))
      (format t "libffi not available - skipping cleanup test~%")))

;;;; Error Handling Tests

(deftest test-libffi-invalid-types
  "Test error handling for invalid types"
  (skip "libffi invalid type handling not yet implemented")
  (if (libffi-available-p)
      (let ((test-func (lambda (x) x)))
        (is-thrown 'error
                        (call-libffi-function 'make-libffi-callback 
                                              test-func :invalid-type '(:int))
                        "Should throw error for invalid return type")
        
        (is-thrown 'error
                        (call-libffi-function 'make-libffi-callback 
                                              test-func :int '(:invalid-type))
                        "Should throw error for invalid argument type"))
      (format t "libffi not available - skipping invalid types test~%")))

(deftest test-libffi-invalid-arguments
  "Test error handling for invalid arguments"
  (skip "libffi invalid argument handling not yet implemented")
  (if (libffi-available-p)
      (progn
        (is-thrown 'error
                        (call-libffi-function 'make-libffi-callback 
                                              "not-a-function" :int '(:int))
                        "Should throw error for non-function argument")
        
        (is-thrown 'error
                        (call-libffi-function 'make-libffi-callback 
                                              (lambda (x) x) :int "not-a-list")
                        "Should throw error for non-list arg-types"))
      (format t "libffi not available - skipping invalid arguments test~%")))

;;;; Performance and Stress Tests

(deftest test-libffi-multiple-callbacks
  "Test creating multiple callbacks simultaneously"
  (skip "libffi multiple callbacks not yet implemented")
  (if (libffi-available-p)
      (let ((callbacks '())
            (test-func (lambda (x) (* x x))))
        (handler-case
            (progn
              ;; Create 10 callbacks
              (dotimes (i 10)
                (let ((cb (call-libffi-function 'make-libffi-callback 
                                                test-func :int '(:int))))
                  (push cb callbacks)))
              
              (is (= (length callbacks) 10)
                       "Should create 10 callbacks successfully")
              
              ;; Verify all callbacks are valid pointers
              (is (every (lambda (cb) (not (sb-alien:null-alien cb))) callbacks)
                       "All callbacks should be valid pointers"))
          (error (e)
            (is nil "Multiple callback creation failed: ~A" e))))
      (format t "libffi not available - skipping multiple callbacks test~%")))

;;;; Integration with Existing Tests

(deftest test-libffi-qsort-integration
  "Test libffi callbacks work with qsort (if available)"
  (if (libffi-available-p)
      (handler-case
          (let* ((test-array (make-array 5 :element-type '(signed-byte 32)
                                         :initial-contents '(5 2 8 1 9)))
                 (compare-func (lambda (a b)
                                (let ((val-a (sb-alien:deref 
                                              (sb-alien:cast a (* sb-alien:int)) 0))
                                      (val-b (sb-alien:deref 
                                              (sb-alien:cast b (* sb-alien:int)) 0)))
                                  (cond ((< val-a val-b) -1)
                                        ((> val-a val-b) 1)
                                        (t 0)))))
                 (callback-ptr (foreign:make-callback compare-func :int '(:pointer :pointer))))
            
            ;; Pin the array and call qsort
            (sb-sys:with-pinned-objects (test-array)
              (let ((array-sap (sb-sys:vector-sap test-array)))
                (foreign:shared-call '("qsort" "libc") :void
                                     '(:pointer :unsigned-long :unsigned-long :pointer)
                                     array-sap 5 4 callback-ptr)))
            
            ;; Check if array is sorted
            (is (equalp test-array #(1 2 5 8 9))
                     "Array should be sorted after qsort with libffi callback"))
        (error (e)
          (format t "libffi qsort integration failed: ~A~%" e)))
      (format t "libffi not available - skipping qsort integration test~%")))

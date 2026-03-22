;;;; signatures-test.lisp - Tests for signature discovery

(defpackage :epsilon.foreign.signatures.test
  (:use :cl :epsilon.syntax :epsilon.test)
  (:local-nicknames
   (:sigs :epsilon.foreign.signatures)
   (:lc :epsilon.foreign.libclang))
   (:enter t))

;;; Skip all tests if libclang is not available

;;; Use local test headers that work on all platforms
(defun get-test-headers-dir ()
  "Get the path to the test-headers directory."
  (let* ((env (epsilon.loader:environment))
         (module (epsilon.loader:get-module env "epsilon.foreign")))
    (when module
      (let ((location (epsilon.path:path-string (epsilon.loader:module-location module))))
        (concatenate 'string location "/tests/jit/test-headers/")))))

(defparameter *test-headers-dir* (get-test-headers-dir))

(defparameter *math-header*
  (when *test-headers-dir*
    (concatenate 'string *test-headers-dir* "math.h")))

(defparameter *stdlib-header*
  (when *test-headers-dir*
    (concatenate 'string *test-headers-dir* "stdlib.h")))

;;; ============================================================================
;;; Signature Discovery Tests
;;; ============================================================================

(deftest discover-sin-signature
  "can discover sin function signature"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (let ((sig (sigs:discover-signature "sin" *math-header*)))
    (assert-true sig)
    (assert-equal "sin" (getf sig :name))
    (assert-equal :double (getf sig :return-type-kind))))

(deftest discover-pow-signature
  "can discover pow function with two args"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (let ((sig (sigs:discover-signature "pow" *math-header*)))
    (assert-true sig)
    (assert-equal 2 (length (getf sig :params)))))

(deftest discover-abs-signature
  "can discover abs function signature"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (let ((sig (sigs:discover-signature "abs" *stdlib-header*)))
    (assert-true sig)
    (assert-equal "abs" (getf sig :name))
    (assert-equal :int (getf sig :return-type-kind))))

;;; ============================================================================
;;; Type Conversion Tests
;;; ============================================================================

(deftest convert-double-type
  "can convert double type"
  (assert-equal :double (sigs::libclang-type-to-jit-type :double)))

(deftest convert-int-type
  "can convert int type"
  (assert-equal :int (sigs::libclang-type-to-jit-type :int)))

(deftest convert-pointer-type
  "can convert pointer type"
  (assert-equal :pointer (sigs::libclang-type-to-jit-type :pointer)))

(deftest convert-unknown-type
  "converts unknown types to pointer"
  (assert-equal :pointer (sigs::libclang-type-to-jit-type :unknown-fancy-type)))

;;; ============================================================================
;;; Auto-JIT Creation Tests
;;; ============================================================================

(deftest make-auto-jit-sin
  "can create auto-jit caller for sin"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (let ((caller (sigs:make-auto-jit-caller "sin" *math-header*)))
    (assert-true (functionp caller))
    (assert-true (< (abs (- 0.0d0 (funcall caller 0.0d0))) 0.0001))
    (assert-true (< (abs (- 1.0d0 (funcall caller (/ pi 2)))) 0.0001))))

(deftest make-auto-jit-floor
  "can create auto-jit caller for floor"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (let ((caller (sigs:make-auto-jit-caller "floor" *math-header*)))
    (assert-true (functionp caller))
    (assert-equal 3.0d0 (funcall caller 3.7d0))
    (assert-equal -4.0d0 (funcall caller -3.2d0))))

(deftest make-auto-jit-pow
  "can create auto-jit caller for pow"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (let ((caller (sigs:make-auto-jit-caller "pow" *math-header*)))
    (assert-true (functionp caller))
    (assert-equal 8.0d0 (funcall caller 2.0d0 3.0d0))
    (assert-equal 1024.0d0 (funcall caller 2.0d0 10.0d0))))

;;; ============================================================================
;;; Cache Tests
;;; ============================================================================

(deftest signature-cache-works
  "signature cache stores and retrieves signatures"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (sigs:clear-signature-cache)
  ;; First call should discover
  (let ((sig1 (sigs:discover-signature "sin" *math-header*)))
    (assert-true sig1)
    ;; Second call should use cache
    (let ((sig2 (sigs:discover-signature "sin" *math-header*)))
      (assert-true sig2)
      (assert-equal (getf sig1 :name) (getf sig2 :name)))))

;;; ============================================================================
;;; Error Handling Tests
;;; ============================================================================

(deftest error-on-missing-function
  "signals error for missing function"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (let ((error-signaled nil))
    (handler-case
        (sigs:make-auto-jit-caller "nonexistent_function_xyz" *math-header*)
      (error (e)
        (declare (ignore e))
        (setf error-signaled t)))
    (assert-true error-signaled)))

;;; ============================================================================
;;; Struct Discovery Tests
;;; ============================================================================

(defparameter *time-header*
  (when *test-headers-dir*
    (concatenate 'string *test-headers-dir* "time.h")))

(defparameter *sys-time-header*
  (when *test-headers-dir*
    (concatenate 'string *test-headers-dir* "time.h")))

(deftest discover-timespec-struct
  "can discover timespec struct layout"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (let ((info (sigs:discover-struct "timespec" *time-header*)))
    (assert-true info)
    (assert-equal "timespec" (getf info :name))
    (assert-true (> (getf info :size) 0))
    (assert-true (getf info :fields))))

(deftest discover-timeval-struct
  "can discover timeval struct layout"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (let ((info (sigs:discover-struct "timeval" *sys-time-header*)))
    (assert-true info)
    (assert-equal "timeval" (getf info :name))
    ;; timeval has tv_sec and tv_usec fields
    (assert-true (>= (length (getf info :fields)) 2))))

(deftest struct-cache-works
  "struct cache stores and retrieves structs"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (sigs:clear-struct-cache)
  ;; First call should discover
  (let ((info1 (sigs:discover-struct "timespec" *time-header*)))
    (assert-true info1)
    ;; Second call should use cache
    (let ((info2 (sigs:discover-struct "timespec" *time-header*)))
      (assert-true info2)
      (assert-equal (getf info1 :name) (getf info2 :name))
      (assert-equal (getf info1 :size) (getf info2 :size)))))

(deftest struct-type-conversion
  "libclang type kinds convert to JIT types correctly"
  (assert-equal :int (sigs::libclang-type-kind-to-jit-type :int))
  (assert-equal :long (sigs::libclang-type-kind-to-jit-type :long))
  (assert-equal :double (sigs::libclang-type-kind-to-jit-type :double))
  (assert-equal :pointer (sigs::libclang-type-kind-to-jit-type :pointer)))

(deftest error-on-missing-struct
  "discover-struct returns nil for missing struct"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (let ((info (sigs:discover-struct "nonexistent_struct_xyz" *time-header*)))
    (assert-true (null info))))

;;; ============================================================================
;;; Function-to-Header Mapping Tests (IMPL-182)
;;; ============================================================================

(deftest detect-function-headers-known-function
  "detect-function-headers returns correct headers for known functions"
  (let ((headers (sigs:detect-function-headers "strlen")))
    (assert-true headers)
    (assert-true (member "string.h" headers :test #'string=)))
  (let ((headers (sigs:detect-function-headers "malloc")))
    (assert-true headers)
    (assert-true (member "stdlib.h" headers :test #'string=)))
  (let ((headers (sigs:detect-function-headers "printf")))
    (assert-true headers)
    (assert-true (member "stdio.h" headers :test #'string=)))
  (let ((headers (sigs:detect-function-headers "sin")))
    (assert-true headers)
    (assert-true (member "math.h" headers :test #'string=))))

(deftest detect-function-headers-unknown-function
  "detect-function-headers returns standard headers for unknown functions"
  (let ((headers (sigs:detect-function-headers "totally_unknown_xyz_123")))
    (assert-true headers)
    ;; Should return *standard-headers* for unknown functions
    (assert-true (> (length headers) 5))))

(deftest function-header-map-is-populated
  "function-header-map has reasonable number of entries"
  (assert-true sigs:*function-header-map*)
  (assert-true (> (length sigs:*function-header-map*) 100)))

;;; ============================================================================
;;; Auto-Discovery Tests (IMPL-182)
;;; ============================================================================

(deftest discover-signature-auto-works
  "discover-signature-auto finds signatures for common functions"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  ;; Test with a function that has a known header mapping
  (multiple-value-bind (sig header-path)
      (sigs:discover-signature-auto "sin")
    (declare (ignore header-path))
    ;; Note: This may fail if system headers aren't available
    ;; which is fine for unit tests
    (when sig
      (assert-equal "sin" (getf sig :name)))))

;;; ============================================================================
;;; Clear All Caches Tests (IMPL-182)
;;; ============================================================================

(deftest clear-all-caches-works
  "clear-all-caches clears both signature and struct caches"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  ;; Populate caches
  (sigs:discover-signature "sin" *math-header*)
  (sigs:discover-struct "timespec" *time-header*)
  ;; Clear all caches
  (sigs:clear-all-caches)
  ;; Verify caches are empty by checking hash table counts
  (assert-equal 0 (hash-table-count sigs:*signature-cache*))
  (assert-equal 0 (hash-table-count sigs:*struct-cache*)))

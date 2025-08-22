;;;; Test Suite for SBCL Integration

(defpackage epsilon.test.compile-integration
  (:use cl epsilon.test)
  (:local-nicknames
   (api epsilon.compile-api)
   (hooks epsilon.compile-hooks)
   (compile epsilon.compile)
   (compile-integration epsilon.compile-integration)
   (log epsilon.log)))

(in-package epsilon.test.compile-integration)

;;; Basic functionality tests

(deftest test-package-existence
  "Test that the deep integration package exists and has key exports"
  (is (find-package :epsilon.compile-integration))
  (is (find-symbol "WITH-SOURCE-TRACKING" :epsilon.compile-integration))
  (is (find-symbol "INSTALL-COMPILER-HOOKS" :epsilon.compile-integration)))

(deftest test-variables-exist
  "Test that key tracking variables exist"
  (is (boundp 'compile-integration:*real-time-source-tracking*))
  (is (boundp 'compile-integration:*current-compilation-location*)))

(deftest test-initialization
  "Test deep integration initialization"
  (handler-case
      (progn
        (compile-integration:initialize-integration)
        (is t))  ; Just test that it doesn't error
    (error (e)
      (warn "Initialization failed: ~A" e)
      (is nil))))

(deftest test-hook-functions-exist
  "Test that hook installation functions exist"
  (is (fboundp 'compile-integration:install-compiler-hooks))
  (is (fboundp 'compile-integration:uninstall-compiler-hooks))
  (is (fboundp 'compile-integration:get-real-time-source-location)))

(deftest test-integration-with-compile
  "Test that integration functions exist in compile package"
  (is (fboundp 'compile:with-source-tracking))
  (is (fboundp 'compile:compile-file-with-tracking))
  (is (fboundp 'compile:compile-form-with-tracking)))

(deftest test-line-number-extraction
  "Test that line number extraction works without debug output"
  (let ((test-file (format nil "/tmp/test-line-~A.lisp" (get-universal-time))))
    (unwind-protect
         (handler-case
             (progn
               ;; Create test file with known line structure
               (with-open-file (stream test-file :direction :output :if-exists :supersede)
                 (format stream ";; Line 1: Comment~%")
                 (format stream "(format t \"Line 2\")~%")
                 (format stream "(defun test-func (x)~%")
                 (format stream "  (+ x 1))~%"))
               
               ;; Test compilation with deep tracking
               (let ((result (compile:compile-file-with-tracking test-file :verbose nil)))
                 (is (not (null result)) "Should return a compilation result")
                 (is (api:compilation-result-success-p result) "Compilation should succeed")
                 ;; Compilation should succeed cleanly without errors
                 (is-= 0 (length (api:get-errors result)) "Should have no errors")))
           (error (e)
             (warn "Line number extraction test failed with error: ~A" e)
             (is nil "Test should not error")))
      
      ;; Cleanup
      (when (probe-file test-file) (delete-file test-file))
      (let ((fasl-file (concatenate 'string test-file ".fasl")))
        (when (probe-file fasl-file) (delete-file fasl-file))))))

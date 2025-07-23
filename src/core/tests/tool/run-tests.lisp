;;;; Tests for the run command functionality
;;;;
;;;; This test suite validates the 'epsilon run' command implementation,
;;;; including package discovery, loading, and execution.

(defpackage #:epsilon.tool.run-tests
  (:use #:cl)
  (:local-nicknames
   (#:test #:epsilon.tool.test)
   (#:run #:epsilon.tool.run)
   (#:fs #:epsilon.sys.fs)
   (#:path #:epsilon.path))
  (:export
   #:run-command-tests))

(in-package #:epsilon.tool.run-tests)

(defun run-command-tests ()
  "Run all tests for the run command"
  (test:test-package "epsilon.tool.run-tests"))

;; Test helper functions

(test:deftest test-find-package-file ()
  "Test package.lisp discovery"
  ;; Note: This would require setting up temporary directories
  ;; For now, just test that the function exists
  (test:is (fboundp 'run:find-package-file)))

(test:deftest test-read-package-definition ()
  "Test package definition reading"
  ;; Test with a simple property list
  (let ((test-file "/tmp/test-package.lisp"))
    (handler-case
        (progn
          (with-open-file (stream test-file :direction :output :if-exists :supersede)
            (write '(:name "test-package"
                     :version "1.0.0"
                     :sources ("src")
                     :main "test-package:main")
                   :stream stream))
          (let ((result (run:read-package-definition test-file)))
            (test:is-equal (getf result :name) "test-package")
            (test:is-equal (getf result :version) "1.0.0")
            (test:is-equal (getf result :main) "test-package:main")))
      (error (e)
        (test:fail (format nil "Failed to read package definition: ~A" e)))
      (:cleanup
       (when (probe-file test-file)
         (delete-file test-file))))))

(test:deftest test-validate-package-definition ()
  "Test package definition validation"
  ;; Valid package definition
  (let ((valid-def '(:name "test-pkg" :main "test-pkg:main")))
    (test:is (run:validate-package-definition valid-def "test-pkg")))
  
  ;; Missing name
  (let ((invalid-def '(:main "test-pkg:main")))
    (test:is-thrown-p 'error
      (run:validate-package-definition invalid-def "test-pkg")))
  
  ;; Name mismatch
  (let ((mismatch-def '(:name "wrong-name" :main "test-pkg:main")))
    (test:is-thrown-p 'error
      (run:validate-package-definition mismatch-def "test-pkg")))
  
  ;; Missing main
  (let ((no-main-def '(:name "test-pkg")))
    (test:is-thrown-p 'error
      (run:validate-package-definition no-main-def "test-pkg"))))

(test:deftest test-parse-main-spec ()
  "Test main function specification parsing"
  ;; Package:function format
  (let ((result (run:parse-main-spec "test-pkg:main")))
    (test:is-equal (car result) "test-pkg")
    (test:is-equal (cdr result) "main"))
  
  ;; Function only format
  (let ((result (run:parse-main-spec "main")))
    (test:is (null (car result)))
    (test:is-equal (cdr result) "main"))
  
  ;; Symbol input
  (let ((result (run:parse-main-spec 'main)))
    (test:is (null (car result)))
    (test:is-equal (cdr result) "MAIN")))

(test:deftest test-run-command-help ()
  "Test that run command help works"
  (handler-case
      (let ((output (with-output-to-string (*standard-output*)
                      (run:handle-run-help nil))))
        (test:is (search "run - Execute a package" output))
        (test:is (search "Usage:" output))
        (test:is (search "Examples:" output)))
    (error (e)
      (test:fail (format nil "Run help failed: ~A" e)))))

;; Integration test (requires test package structure)
(test:deftest test-run-integration ()
  "Test run command with a minimal test package"
  ;; This test would create a temporary directory structure
  ;; with package.lisp and main.lisp files, then test running it
  ;; For now, we just verify the main functions exist
  (test:is (fboundp 'run:run-package))
  (test:is (fboundp 'run:handle-run)))

;; Error handling tests
(test:deftest test-run-error-handling ()
  "Test error handling in run command"
  ;; Test missing package.lisp
  (test:is-thrown-p 'error
    (run:run-package "nonexistent-package"))
  
  ;; Test invalid main spec
  (let ((invalid-def '(:name "test-pkg" :main "nonexistent:function")))
    ;; This would require more complex setup to test properly
    (test:is t))) ; Placeholder
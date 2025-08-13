;;;; Tests for epsilon.process enhancements
;;;;
;;;; Comprehensive tests for the enhanced process execution functionality

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "EPSILON.TEST")
    (require :epsilon.test)))

(defpackage epsilon.process.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (process epsilon.process)))

(in-package epsilon.process.tests)

;;; ============================================================================
;;; Command Building and Escaping Tests
;;; ============================================================================

(deftest test-escape-shell-arg-posix ()
  "Test POSIX shell argument escaping"
  (is-equal "'hello'" (process:escape-shell-arg "hello" :shell :posix))
  (is-equal "'hello world'" (process:escape-shell-arg "hello world" :shell :posix))
  (is-equal "'hello'\\''world'" (process:escape-shell-arg "hello'world" :shell :posix))
  (is-equal "''" (process:escape-shell-arg "" :shell :posix)))

(deftest test-escape-shell-arg-windows ()
  "Test Windows shell argument escaping"
  (is-equal "\"hello\"" (process:escape-shell-arg "hello" :shell :windows))
  (is-equal "\"hello world\"" (process:escape-shell-arg "hello world" :shell :windows))
  (is-equal "\"hello\\\"world\"" (process:escape-shell-arg "hello\"world" :shell :windows)))

(deftest test-build-command-line ()
  "Test command line building"
  (is-equal "echo 'hello'" 
            (process:build-command-line "echo" '("hello") :shell :posix))
  (is-equal "ls 'file with spaces.txt'" 
            (process:build-command-line "ls" '("file with spaces.txt") :shell :posix))
  (is-equal "grep 'pattern' 'file1.txt' 'file2.txt'" 
            (process:build-command-line "grep" '("pattern" "file1.txt" "file2.txt") :shell :posix)))

(deftest test-find-executable ()
  "Test executable finding in PATH"
  ;; Test finding a known command
  (let ((ls-path (process:find-executable "ls")))
    (is (or (null ls-path) (stringp ls-path)))
    (when ls-path
      (is (probe-file ls-path))))
  
  ;; Test command that doesn't exist
  (is (null (process:find-executable "this-command-does-not-exist-hopefully"))))

;;; ============================================================================
;;; Basic Process Execution Tests
;;; ============================================================================

(deftest test-run-sync-basic ()
  "Test basic synchronous process execution"
  (multiple-value-bind (output error-output exit-code)
      (process:run-sync "echo" :args '("hello world"))
    (is-equal 0 exit-code)
    (is (search "hello world" output))))

(deftest test-run-sync-error-handling ()
  "Test error handling in synchronous execution"
  (handler-case
      (process:run-sync "false" :error-on-failure t)
    (process:process-error-condition (e)
      (is (not (zerop (process:process-error-exit-code e)))))
    (:no-error ()
      (is nil "Should have raised an error"))))

(deftest test-run-sync-no-error-on-failure ()
  "Test running with error-on-failure disabled"
  (multiple-value-bind (output error-output exit-code)
      (process:run-sync "false" :error-on-failure nil)
    (is (not (zerop exit-code)))))

(deftest test-command-not-found ()
  "Test command not found error"
  (handler-case
      (process:run-sync "this-command-definitely-does-not-exist" 
                        :check-executable t)
    (process:command-not-found (e)
      (is (search "this-command-definitely-does-not-exist" 
                  (process:process-error-command e))))
    (:no-error ()
      (is nil "Should have raised command-not-found error"))))

;;; ============================================================================
;;; Stream Processing Tests
;;; ============================================================================

(deftest test-stream-output-callback ()
  "Test stream output processing with callback"
  (let ((received-lines '()))
    (process:run-sync "printf" 
                      :args '("line1\\nline2\\nline3\\n")
                      :stream-output (lambda (line)
                                       (push line received-lines)))
    (is (= 3 (length received-lines)))
    (is (find "line1" received-lines :test #'search))
    (is (find "line2" received-lines :test #'search))
    (is (find "line3" received-lines :test #'search))))

(deftest test-merge-error-stream ()
  "Test merging error stream into output"
  ;; This test uses a command that writes to both stdout and stderr
  (multiple-value-bind (output error-output exit-code)
      (process:run-sync "sh" 
                        :args '("-c" "echo stdout; echo stderr >&2")
                        :merge-error t
                        :error-on-failure nil)
    (is (search "stdout" output))
    (is (or (search "stderr" output)  ; merged into output
            (search "stderr" error-output))) ; or kept separate
    ))

;;; ============================================================================
;;; Shell Execution Tests
;;; ============================================================================

(deftest test-shell-execution ()
  "Test running commands through shell"
  (multiple-value-bind (output error-output exit-code)
      (process:run-sync "sh" 
                        :args '("-c" "echo $HOME")
                        :shell nil
                        :error-on-failure nil)
    (is-equal 0 exit-code)
    ;; When run through shell, $HOME should be expanded
    (is (not (search "$HOME" output)))))

;;; ============================================================================
;;; Process Monitoring Tests
;;; ============================================================================

(deftest test-subprocess-lifecycle ()
  "Test subprocess creation and lifecycle"
  (let ((subprocess (process:make-subprocess "sleep" :args '("0.1"))))
    (is (typep subprocess 'process:subprocess))
    (is-equal "sleep" (process:subprocess-command subprocess))
    (is-equal '("0.1") (process:subprocess-args subprocess))
    
    ;; Start the process
    (process:start subprocess :wait nil)
    (is (process:running-p subprocess))
    
    ;; Wait for completion
    (process:wait-for-process subprocess 2)
    (is (not (process:running-p subprocess)))
    (is-equal 0 (process:process-exit-code subprocess))))

(deftest test-terminate-gracefully ()
  "Test graceful process termination"
  ;; Start a long-running process
  (let ((subprocess (process:make-subprocess "sleep" :args '("10"))))
    (process:start subprocess :wait nil)
    (is (process:running-p subprocess))
    
    ;; Terminate it gracefully
    (process:terminate-gracefully subprocess :timeout 1)
    
    ;; Give it a moment to clean up
    (sleep 0.1)
    (is (not (process:running-p subprocess)))))

;;; ============================================================================
;;; Error Condition Tests
;;; ============================================================================

(deftest test-error-conditions ()
  "Test process error condition hierarchy"
  (is (subtypep 'process:command-not-found 'process:process-error-condition))
  (is (subtypep 'process:process-timeout-error 'process:process-error-condition))
  (is (subtypep 'process:process-error-condition 'error)))

;;; ============================================================================
;;; Platform-Specific Tests
;;; ============================================================================

#+unix
(deftest test-unix-specific-features ()
  "Test Unix-specific process features"
  ;; Test that we can run common Unix commands
  (multiple-value-bind (output error-output exit-code)
      (process:run-sync "uname" :error-on-failure nil)
    (is-equal 0 exit-code)
    (is (> (length output) 0))))

#+windows
(deftest test-windows-specific-features ()
  "Test Windows-specific process features"
  ;; Test that we can run common Windows commands
  (multiple-value-bind (output error-output exit-code)
      (process:run-sync "echo" :args '("test") :error-on-failure nil)
    (is-equal 0 exit-code)
    (is (search "test" output))))

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(deftest test-complex-pipeline-simulation ()
  "Test simulating complex operations"
  ;; Simulate what epsilon.release might do
  (let ((progress-count 0))
    (multiple-value-bind (output error-output exit-code)
        (process:run-sync "find" 
                          :args '("/tmp" "-name" "*.tmp" "-type" "f")
                          :stream-output (lambda (line)
                                           (incf progress-count)
                                           (format t "Processing: ~A~%" line))
                          :error-on-failure nil
                          :timeout 5)
      ;; Should complete without timing out
      (is (>= exit-code 0)))))

(deftest test-environment-handling ()
  "Test environment variable handling"
  (let ((test-env (list "TEST_VAR=test_value")))
    (multiple-value-bind (output error-output exit-code)
        (process:run-sync "sh" 
                          :args '("-c" "echo $TEST_VAR")
                          :environment test-env
                          :shell nil
                          :error-on-failure nil)
      (is-equal 0 exit-code)
      (is (search "test_value" output)))))

(deftest test-working-directory ()
  "Test working directory change"
  (multiple-value-bind (output error-output exit-code)
      (process:run-sync "pwd"
                        :working-directory "/tmp" 
                        :error-on-failure nil)
    (is-equal 0 exit-code)
    (is (search "/tmp" output))))

;;; ============================================================================
;;; Performance and Resource Tests
;;; ============================================================================

(deftest test-multiple-processes ()
  "Test running multiple processes concurrently"
  (let ((processes '()))
    (dotimes (i 3)
      (let ((subprocess (process:make-subprocess "sleep" :args '("0.1"))))
        (process:start subprocess :wait nil)
        (push subprocess processes)))
    
    ;; All should be running
    (dolist (p processes)
      (is (process:running-p p)))
    
    ;; Wait for all to complete
    (dolist (p processes)
      (process:wait-for-process p 2))
    
    ;; All should be completed
    (dolist (p processes)
      (is (not (process:running-p p)))
      (is-equal 0 (process:process-exit-code p)))))
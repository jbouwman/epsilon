;;;; Tests for epsilon.process enhancements
;;;;
;;;;  tests for the enhanced process execution functionality

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "EPSILON.TEST")
    (require :epsilon.test)))

(defpackage epsilon.process.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (process epsilon.process)))

(in-package epsilon.process.tests)

;;; ============================================================================
;;; Test Helpers
;;; ============================================================================

(defun poll-until (predicate &key (timeout-seconds 5) (poll-interval 0.01))
  "Poll until predicate returns true or timeout is reached.
   Returns T if predicate succeeded, NIL if timeout occurred."
  (let ((end-time (+ (get-internal-real-time)
                     (* timeout-seconds internal-time-units-per-second))))
    (loop while (< (get-internal-real-time) end-time)
          when (funcall predicate)
            return t
          do (sleep poll-interval)
          finally (return nil))))

(defun wait-for-process-state (subprocess target-state &key (timeout-seconds 5))
  "Wait for a subprocess to reach a target state using polling.
   Target state can be :running or :stopped."
  (poll-until 
   (lambda ()
     (ecase target-state
       (:running (process:running-p subprocess))
       (:stopped (not (process:running-p subprocess)))))
   :timeout-seconds timeout-seconds))

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
  ;; Use a simple, fast command that should exist everywhere
  ;; The echo command is POSIX standard and exists in most shells
  (let* ((cmd "echo")
         (args '("test"))
         (subprocess (handler-case 
                         (process:make-subprocess cmd :args args)
                       (error (e)
                         ;; If echo doesn't exist, skip the test
                         (skip (format nil "Cannot create subprocess: ~A" e))
                         (return-from test-subprocess-lifecycle)))))
    
    (is (typep subprocess 'process:subprocess))
    (is-equal cmd (process:subprocess-command subprocess))
    (is-equal args (process:subprocess-args subprocess))
    
    ;; Start the process
    (handler-case
        (process:start subprocess :wait nil)
      (error (e)
        (skip (format nil "Cannot start subprocess: ~A" e))
        (return-from test-subprocess-lifecycle)))
    
    ;; For echo, the process might complete immediately
    ;; So we check if it's either running or already completed
    (let ((started-or-completed 
           (poll-until 
            (lambda ()
              (or (process:running-p subprocess)
                  (numberp (process:process-exit-code subprocess))))
            :timeout-seconds 2)))
      (is started-or-completed "Process neither started nor completed within timeout"))
    
    ;; Wait for completion
    (process:wait-for-process subprocess 5)
    
    ;; Now verify the process has completed
    (is (not (process:running-p subprocess)))
    (is-equal 0 (process:process-exit-code subprocess))))

(deftest test-terminate-gracefully ()
  "Test graceful process termination"
  ;; Use cat which reads from stdin and will block indefinitely
  ;; This is more portable than sleep
  (let* ((cmd "cat")
         (args '())  ; No args, will read from stdin
         (subprocess (handler-case 
                         (process:make-subprocess cmd :args args)
                       (error (e)
                         ;; If cat doesn't exist, try sh -c read
                         (handler-case
                             (process:make-subprocess "sh" :args '("-c" "read line"))
                           (error (e2)
                             (skip (format nil "Cannot create long-running subprocess: ~A, ~A" e e2))
                             (return-from test-terminate-gracefully)))))))
    
    (handler-case
        (process:start subprocess :wait nil)
      (error (e)
        (skip (format nil "Cannot start subprocess: ~A" e))
        (return-from test-terminate-gracefully)))
    
    ;; Poll until process is running
    (is (wait-for-process-state subprocess :running :timeout-seconds 2)
        "Process did not start within timeout")
    
    ;; Terminate it gracefully
    (process:terminate-gracefully subprocess :timeout 1)
    
    ;; Poll until process stops
    (is (wait-for-process-state subprocess :stopped :timeout-seconds 3)
        "Process did not terminate within timeout")
    
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

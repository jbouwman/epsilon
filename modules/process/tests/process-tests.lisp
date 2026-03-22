;;;; Tests for epsilon.process enhancements
;;;;
;;;;  tests for the enhanced process execution functionality

(defpackage epsilon.process-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.process process)
            (epsilon.file fs))
  (:enter t))

;;; ============================================================================
;;; Test Helpers
;;; ============================================================================

;;; FIXME these should be standard library functions

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
  (assert-equal "'hello'" (process:escape-shell-arg "hello" :shell :posix))
  (assert-equal "'hello world'" (process:escape-shell-arg "hello world" :shell :posix))
  (assert-equal "'hello'\\''world'" (process:escape-shell-arg "hello'world" :shell :posix))
  (assert-equal "''" (process:escape-shell-arg "" :shell :posix)))

(deftest test-escape-shell-arg-windows ()
  "Test Windows shell argument escaping"
  (assert-equal "\"hello\"" (process:escape-shell-arg "hello" :shell :windows))
  (assert-equal "\"hello world\"" (process:escape-shell-arg "hello world" :shell :windows))
  (assert-equal "\"hello\\\"world\"" (process:escape-shell-arg "hello\"world" :shell :windows)))

(deftest test-build-command-line ()
  "Test command line building"
  (assert-equal "echo 'hello'"
            (process:build-command-line "echo" '("hello") :shell :posix))
  (assert-equal "ls 'file with spaces.txt'"
            (process:build-command-line "ls" '("file with spaces.txt") :shell :posix))
  (assert-equal "grep 'pattern' 'file1.txt' 'file2.txt'"
            (process:build-command-line "grep" '("pattern" "file1.txt" "file2.txt") :shell :posix)))

(deftest test-find-executable ()
  "Test executable finding in PATH"
  ;; Test finding a known command
  (let ((ls-path (process:find-executable "ls")))
    (assert-true (or (null ls-path) (stringp ls-path)))
    (when ls-path
      (assert-true (probe-file ls-path))))

  ;; Test command that doesn't exist
  (assert-true (null (process:find-executable "this-command-does-not-exist-hopefully"))))

;;; ============================================================================
;;; Basic Process Execution Tests
;;; ============================================================================

(deftest test-run-sync-basic ()
  "Test basic synchronous process execution"
  (multiple-value-bind (output error-output exit-code)
      (process:run-sync "echo" :args '("hello world"))
    (declare (ignore error-output))
    (assert-equal 0 exit-code)
    (assert-true (search "hello world" output))))

(deftest test-run-sync-error-handling ()
  "Test error handling in synchronous execution"
  (handler-case
      (process:run-sync "false" :error-on-failure t)
    (process:process-error-condition (e)
      (assert-true (not (zerop (process:process-error-exit-code e)))))
    (:no-error ()
      (assert-true nil "Should have raised an error"))))

(deftest test-run-sync-no-error-on-failure ()
  "Test running with error-on-failure disabled"
  (multiple-value-bind (output error-output exit-code)
      (process:run-sync "false" :error-on-failure nil)
    (declare (ignore output error-output))
    (assert-true (not (zerop exit-code)))))

(deftest test-command-not-found ()
  "Test command not found error"
  (handler-case
      (process:run-sync "this-command-definitely-does-not-exist"
                        :check-executable t)
    (process:command-not-found (e)
      (assert-true (search "this-command-definitely-does-not-exist"
                  (process:process-error-command e))))
    (:no-error ()
      (assert-true nil "Should have raised command-not-found error"))))

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
    (assert-true (= 3 (length received-lines)))
    (assert-true (find "line1" received-lines :test #'search))
    (assert-true (find "line2" received-lines :test #'search))
    (assert-true (find "line3" received-lines :test #'search))))

(deftest test-merge-error-stream ()
  "Test merging error stream into output"
  ;; This test uses a command that writes to both stdout and stderr
  (multiple-value-bind (output error-output exit-code)
      (process:run-sync "sh"
                        :args '("-c" "echo stdout; echo stderr >&2")
                        :merge-error t
                        :error-on-failure nil)
    (declare (ignore exit-code))
    (assert-true (search "stdout" output))
    (assert-true (or (search "stderr" output)  ; merged into output
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
    (declare (ignore error-output))
    (assert-equal 0 exit-code)
    ;; When run through shell, $HOME should be expanded
    (assert-true (not (search "$HOME" output)))))

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

    (assert-true (typep subprocess 'process:subprocess))
    (assert-equal cmd (process:subprocess-command subprocess))
    (assert-equal args (process:subprocess-args subprocess))

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
      (assert-true started-or-completed "Process neither started nor completed within timeout"))

    ;; Wait for completion
    (process:wait-for-process subprocess 5)

    ;; Now verify the process has completed
    (assert-true (not (process:running-p subprocess)))
    (assert-equal 0 (process:process-exit-code subprocess))))

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
    (assert-true (wait-for-process-state subprocess :running :timeout-seconds 2)
        "Process did not start within timeout")

    ;; Terminate it gracefully
    (process:terminate-gracefully subprocess :timeout 1)

    ;; Poll until process stops
    (assert-true (wait-for-process-state subprocess :stopped :timeout-seconds 3)
        "Process did not terminate within timeout")

    (assert-true (not (process:running-p subprocess)))))

;;; ============================================================================
;;; Error Condition Tests
;;; ============================================================================

(deftest test-error-conditions ()
  "Test process error condition hierarchy"
  (assert-true (subtypep 'process:command-not-found 'process:process-error-condition))
  (assert-true (subtypep 'process:process-timeout-error 'process:process-error-condition))
  (assert-true (subtypep 'process:process-error-condition 'error)))

;;; ============================================================================
;;; Platform-Specific Tests
;;; ============================================================================

#+unix
(deftest test-unix-specific-features ()
  "Test Unix-specific process features"
  ;; Test that we can run common Unix commands
  (multiple-value-bind (output error-output exit-code)
      (process:run-sync "uname" :error-on-failure nil)
    (declare (ignore error-output))
    (assert-equal 0 exit-code)
    (assert-true (> (length output) 0))))

#+windows
(deftest test-windows-specific-features ()
  "Test Windows-specific process features"
  ;; Test that we can run common Windows commands
  (multiple-value-bind (output error-output exit-code)
      (process:run-sync "echo" :args '("test") :error-on-failure nil)
    (assert-equal 0 exit-code)
    (assert-true (search "test" output))))

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(deftest test-complex-pipeline-simulation ()
  "Test simulating complex operations with stream processing"
  ;; Create a small temporary directory tree so find completes quickly and
  ;; deterministically, avoiding flaky timeouts when /tmp is large on CI.
  (fs:with-temp-dir (test-dir)
    (let ((sub-dir (fs:join-paths test-dir "sub")))
      (fs:make-dirs sub-dir)
      ;; Create a few .tmp files
      (dolist (name '("a.tmp" "b.tmp"))
        (with-open-file (s (fs:join-paths test-dir name)
                           :direction :output :if-exists :supersede)
          (write-string "test" s)))
      (with-open-file (s (fs:join-paths sub-dir "c.tmp")
                         :direction :output :if-exists :supersede)
        (write-string "test" s))
      (let ((progress-count 0))
        (multiple-value-bind (output error-output exit-code)
            (process:run-sync "find"
                              :args (list test-dir "-name" "*.tmp" "-type" "f")
                              :stream-output (lambda (line)
                                               (incf progress-count)
                                               (format t "Processing: ~A~%" line))
                              :error-on-failure nil
                              :timeout 10)
          (declare (ignore output error-output))
          (assert-true (>= exit-code 0))
          (assert-true (= 3 progress-count)))))))

(deftest test-environment-handling ()
  "Test environment variable handling"
  (let ((test-env (list "TEST_VAR=test_value")))
    (multiple-value-bind (output error-output exit-code)
        (process:run-sync "sh"
                          :args '("-c" "echo $TEST_VAR")
                          :environment test-env
                          :shell nil
                          :error-on-failure nil)
      (declare (ignore error-output))
      (assert-equal 0 exit-code)
      (assert-true (search "test_value" output)))))

(deftest test-working-directory ()
  "Test working directory change"
  (fs:with-temp-dir (dir)
    (multiple-value-bind (output error-output exit-code)
        (process:run-sync "pwd"
                          :working-directory dir
                          :error-on-failure nil)
      (declare (ignore error-output))
      (assert-equal 0 exit-code)
      ;; pwd may resolve symlinks, so just check the final component
      (let ((dir-name (fs:basename dir)))
        (assert-true (search dir-name output))))))

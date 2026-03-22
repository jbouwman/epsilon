;;;; Basic test for epsilon.process enhancements

(defpackage epsilon.basic-process-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.process proc))
  (:enter t))

(deftest basic-command-execution
  "Test basic command execution"
  (multiple-value-bind (output error-output exit-code)
      (proc:run-sync "echo" :args '("hello world"))
    (declare (ignore error-output))
    (assert-true (= exit-code 0) "Echo command should succeed")
    (assert-true (search "hello world" output) "Output should contain 'hello world'")))

(deftest command-not-found-detection
  "Test command not found detection"
  (assert-condition (proc:command-not-found)
    (proc:run-sync "this-command-does-not-exist"
                   :check-executable t)))

(deftest shell-argument-escaping
  "Test shell argument escaping"
  (let ((escaped (proc:escape-shell-arg "hello world" :shell :posix)))
    (assert-true (string= escaped "'hello world'") "Should escape with single quotes")))

(deftest find-executable
  "Test executable finding"
  (let ((echo-path (proc:find-executable "echo")))
    (assert-true echo-path "Should find echo executable")
    (when echo-path
      (assert-true (probe-file echo-path) "Found executable should exist"))))

(deftest stream-output-processing
  "Test stream output processing"
  (let ((lines-received '()))
    (proc:run-sync "printf"
                   :args '("line1\\nline2\\nline3\\n")
                   :stream-output (lambda (line)
                                    (push line lines-received)))
    (assert-true (>= (length lines-received) 3) "Should receive multiple lines")))

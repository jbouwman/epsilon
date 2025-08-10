;;;; Basic test for epsilon.process enhancements

(defun test-basic-process-functionality ()
  "Test basic process functionality"
  
  ;; Test 1: Basic command execution
  (format t "Testing basic command execution...~%")
  (multiple-value-bind (output error-output exit-code)
      (epsilon.process:run-sync "echo" :args '("hello world"))
    (assert (= 0 exit-code) nil "Echo command should succeed")
    (assert (search "hello world" output) nil "Output should contain 'hello world'")
    (format t "  ✓ Basic execution works~%"))
  
  ;; Test 2: Command not found detection
  (format t "Testing command not found detection...~%")
  (handler-case
      (epsilon.process:run-sync "this-command-does-not-exist" 
                                :check-executable t)
    (epsilon.process:command-not-found (e)
      (format t "  ✓ Command not found error properly raised~%"))
    (:no-error ()
      (error "Should have raised command-not-found error")))
  
  ;; Test 3: Shell argument escaping
  (format t "Testing shell argument escaping...~%")
  (let ((escaped (epsilon.process:escape-shell-arg "hello world" :shell :posix)))
    (assert (string= escaped "'hello world'") nil "Should escape with single quotes")
    (format t "  ✓ Shell escaping works~%"))
  
  ;; Test 4: Find executable
  (format t "Testing executable finding...~%")
  (let ((echo-path (epsilon.process:find-executable "echo")))
    (when echo-path
      (assert (probe-file echo-path) nil "Found executable should exist")
      (format t "  ✓ Found echo at: ~A~%" echo-path)))
  
  ;; Test 5: Stream output processing
  (format t "Testing stream output processing...~%")
  (let ((lines-received '()))
    (epsilon.process:run-sync "printf" 
                              :args '("line1\\nline2\\nline3\\n")
                              :stream-output (lambda (line)
                                               (push line lines-received)))
    (assert (>= (length lines-received) 3) nil "Should receive multiple lines")
    (format t "  ✓ Stream processing works, received ~D lines~%" 
            (length lines-received)))
  
  (format t "~%All basic process functionality tests passed!~%"))

;; Run the test
(test-basic-process-functionality)
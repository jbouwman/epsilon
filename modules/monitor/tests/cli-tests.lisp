(defpackage epsilon.monitor.cli.tests
  (:use
   cl
   epsilon.test)
  (:local-nicknames
   (monitor epsilon.monitor)
   (cli epsilon.monitor.cli)
   (time epsilon.time)))

(in-package epsilon.monitor.cli.tests)

;;;; CLI Renderer Tests

(deftest cli-renderer-creation
  "Test creating CLI renderer with various options"
  (let ((renderer1 (cli:make-cli-renderer))
        (renderer2 (cli:make-cli-renderer :show-progress nil :progress-width 40)))
    
    (is (typep renderer1 'cli:cli-renderer))
    (is (cli:cli-show-progress renderer1))
    (is-equal 20 (cli:cli-progress-width renderer1))
    
    (is (not (cli:cli-show-progress renderer2)))
    (is-equal 40 (cli:cli-progress-width renderer2))))

(deftest progress-bar-generation
  "Test progress bar string generation"
  ;; Test various progress values
  (is-equal "██████████          " (cli:make-progress-bar 0.5 20))
  (is-equal "████████████████████" (cli:make-progress-bar 1.0 20))
  (is-equal "                    " (cli:make-progress-bar 0.0 20))
  
  ;; Test edge cases
  (is-equal "████████████████████" (cli:make-progress-bar 1.1 20)) ; Over 100%
  (is-equal "                    " (cli:make-progress-bar -0.1 20))) ; Negative

(deftest duration-formatting
  "Test human-readable duration formatting"
  ;; Test sub-second durations
  (let ((d1 (time:make-duration 0 500000000))) ; 0.5 seconds
    (is-equal "0.50s" (cli:format-duration d1)))
  
  ;; Test seconds
  (let ((d2 (time:make-duration 45))) ; 45 seconds
    (is-equal "45.0s" (cli:format-duration d2)))
  
  ;; Test minutes
  (let ((d3 (time:make-duration 125))) ; 2m 5s
    (is-equal "2m 5s" (cli:format-duration d3)))
  
  ;; Test hours
  (let ((d4 (time:make-duration 7325))) ; 2h 2m
    (is-equal "2h 2m" (cli:format-duration d4))))

(deftest percentage-formatting
  "Test percentage formatting"
  (is-equal " 50%" (cli:format-percentage 0.5))
  (is-equal "100%" (cli:format-percentage 1.0))
  (is-equal "  0%" (cli:format-percentage 0.0))
  (is-equal "   " (cli:format-percentage '(:items 5 10)))) ; Non-numeric progress

(deftest indentation-strings
  "Test indentation string generation for nested contexts"
  (is-equal "" (cli:get-indent-string 0))
  (is-equal "├─ " (cli:get-indent-string 1))
  (is-equal "│├─ " (cli:get-indent-string 2))
  (is-equal "││├─ " (cli:get-indent-string 3)))

(deftest context-depth-calculation
  "Test calculating context nesting depth"
  (let* ((ctx1 (monitor:create-process-context :operation "Level 1"))
         (ctx2 (monitor:create-process-context :operation "Level 2" :parent ctx1))
         (ctx3 (monitor:create-process-context :operation "Level 3" :parent ctx2)))
    
    (is-equal 0 (cli:get-context-depth ctx1))
    (is-equal 1 (cli:get-context-depth ctx2))
    (is-equal 2 (cli:get-context-depth ctx3))
    (is-equal 0 (cli:get-context-depth nil))))

(deftest cli-output-capture
  "Test CLI renderer output generation"
  (let ((output (make-string-output-stream)))
    ;; Redirect output to capture it
    (let ((*standard-output* output)
          (monitor (monitor:make-process-monitor))
          (renderer (cli:make-cli-renderer)))
      
      (monitor:add-renderer monitor renderer)
      (monitor:set-default-monitor monitor)
      (monitor:start-monitor monitor)
      
      ;; Generate some events
      (monitor:with-process-context ("Test Output")
        (monitor:process-log :info "Test message")
        (monitor:process-warning "Test warning"))
      
      ;; Check output
      (let ((output-string (get-output-stream-string output)))
        (is (search "Test Output" output-string))
        (is (search "Test message" output-string))
        (is (search "WARNING: Test warning" output-string))))))

(deftest cli-error-formatting
  "Test error message formatting with structured data"
  (let ((output (make-string-output-stream)))
    (let ((*standard-output* output)
          (monitor (monitor:make-process-monitor))
          (renderer (cli:make-cli-renderer)))
      
      (monitor:add-renderer monitor renderer)
      (monitor:set-default-monitor monitor)
      (monitor:start-monitor monitor)
      
      ;; Generate error with structured data
      (monitor:with-process-context ("Error Test")
        (monitor:process-error "Test error"
                              :error-type 'test-error-type
                              :suggestions '("Try this" "Or that")
                              :related-contexts '("ctx1" "ctx2")))
      
      ;; Check formatted output
      (let ((output-string (get-output-stream-string output)))
        (is (search "ERROR: Test error" output-string))
        (is (search "Type: TEST-ERROR-TYPE" output-string))
        (is (search "Suggestions:" output-string))
        (is (search "- Try this" output-string))
        (is (search "- Or that" output-string))
        (is (search "Related: ctx1, ctx2" output-string))))))

(deftest cli-progress-updates
  "Test progress update rendering"
  (let ((output (make-string-output-stream)))
    (let ((*standard-output* output)
          (monitor (monitor:make-process-monitor))
          (renderer (cli:make-cli-renderer :show-progress t)))
      
      (monitor:add-renderer monitor renderer)
      (monitor:set-default-monitor monitor)
      (monitor:start-monitor monitor)
      
      ;; Generate progress updates
      (monitor:with-process-context ("Progress Test")
        (monitor:process-progress :percentage 50 :message "Halfway there"))
      
      ;; Check output contains progress information
      (let ((output-string (get-output-stream-string output)))
        (is (search "50%" output-string))
        (is (search "Halfway there" output-string))))))

(deftest cli-completion-symbols
  "Test completion symbols rendering"
  (let ((output (make-string-output-stream)))
    (let ((*standard-output* output)
          (monitor (monitor:make-process-monitor))
          (renderer (cli:make-cli-renderer)))
      
      (monitor:add-renderer monitor renderer)
      (monitor:set-default-monitor monitor)
      (monitor:start-monitor monitor)
      
      ;; Test successful completion
      (let ((ctx (monitor:create-process-context :operation "Success Test")))
        (setf (monitor:process-context-state ctx) :completed)
        (monitor:render-event renderer 
                            (monitor:make-process-event
                             :context-id :test
                             :type :complete
                             :timestamp (time:now)
                             :message "Success")
                            ctx))
      
      ;; Test failed completion
      (let ((ctx (monitor:create-process-context :operation "Failure Test")))
        (setf (monitor:process-context-state ctx) :failed)
        (monitor:render-event renderer
                            (monitor:make-process-event
                             :context-id :test
                             :type :complete
                             :timestamp (time:now)
                             :message "Failed")
                            ctx))
      
      (let ((output-string (get-output-stream-string output)))
        (is (search "✓" output-string)) ; Success symbol
        (is (search "✗" output-string)))))) ; Failure symbol
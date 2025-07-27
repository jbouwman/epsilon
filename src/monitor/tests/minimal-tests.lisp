(defpackage epsilon.monitor.minimal.tests
  (:use
   cl
   epsilon.test)
  (:local-nicknames
   (monitor epsilon.monitor)
   (minimal epsilon.monitor.minimal)
   (time epsilon.time)))

(in-package epsilon.monitor.minimal.tests)

;;;; Minimal Renderer Tests

(deftest minimal-renderer-creation
  "Test creating minimal renderer with various options"
  (let ((r1 (minimal:make-minimal-renderer))
        (r2 (minimal:make-minimal-renderer 
             :error-only nil 
             :show-completion t
             :show-failures nil)))
    
    (is (typep r1 'minimal:minimal-renderer))
    (is (minimal:minimal-error-only r1))
    (is (not (minimal:minimal-show-completion r1)))
    (is (minimal:minimal-show-failures r1))
    
    (is (not (minimal:minimal-error-only r2)))
    (is (minimal:minimal-show-completion r2))
    (is (not (minimal:minimal-show-failures r2)))))

(deftest minimal-error-only-behavior
  "Test that minimal renderer only shows errors when configured"
  (let ((output (make-string-output-stream)))
    (let ((monitor (monitor:make-process-monitor))
          (renderer (minimal:make-minimal-renderer :error-only t :stream output)))
      
      (monitor:add-renderer monitor renderer)
      (monitor:set-default-monitor monitor)
      (monitor:start-monitor monitor)
      
      ;; Generate various events
      (monitor:with-process-context ("Minimal Test")
        (monitor:process-log :info "This should not appear")
        (monitor:process-progress :percentage 50)
        (monitor:process-log :debug "Neither should this")
        (monitor:process-warning "This warning should appear")
        (monitor:process-error "This error should definitely appear"))
      
      ;; Check output
      (let ((output-string (get-output-stream-string output)))
        (is (not (search "This should not appear" output-string)))
        (is (not (search "Neither should this" output-string)))
        (is (search "This warning should appear" output-string))
        (is (search "This error should definitely appear" output-string))))))

(deftest minimal-completion-messages
  "Test completion message behavior"
  (let ((output (make-string-output-stream)))
    (let ((monitor (monitor:make-process-monitor)))
      
      ;; Test with show-completion enabled
      (let ((renderer (minimal:make-minimal-renderer 
                      :show-completion t
                      :show-failures t
                      :stream output)))
        (monitor:add-renderer monitor renderer)
        (monitor:set-default-monitor monitor)
        (monitor:start-monitor monitor)
        
        ;; Successful completion
        (let ((ctx (monitor:create-process-context :operation "Success")))
          (setf (monitor:process-context-state ctx) :completed)
          (monitor:render-event renderer
                              (monitor:make-process-event
                               :context-id :test
                               :type :complete
                               :timestamp (time:now)
                               :message "Operation succeeded")
                              ctx))
        
        ;; Failed completion
        (let ((ctx (monitor:create-process-context :operation "Failure")))
          (setf (monitor:process-context-state ctx) :failed)
          (monitor:render-event renderer
                              (monitor:make-process-event
                               :context-id :test
                               :type :complete
                               :timestamp (time:now)
                               :message "Operation failed")
                              ctx)))
      
      (let ((output-string (get-output-stream-string output)))
        (is (search "COMPLETED: Operation succeeded" output-string))
        (is (search "FAILED: Operation failed" output-string))))))

(deftest minimal-error-suggestions
  "Test that error suggestions are shown"
  (let ((output (make-string-output-stream)))
    (let ((monitor (monitor:make-process-monitor))
          (renderer (minimal:make-minimal-renderer :stream output)))
      
      (monitor:add-renderer monitor renderer)
      (monitor:set-default-monitor monitor)
      (monitor:start-monitor monitor)
      
      ;; Error with suggestions
      (monitor:with-process-context ("Error Context")
        (monitor:process-error "Something went wrong"
                              :suggestions '("Check configuration"
                                           "Restart the service")))
      
      (let ((output-string (get-output-stream-string output)))
        (is (search "ERROR [Error Context]: Something went wrong" output-string))
        (is (search "Suggestions:" output-string))
        (is (search "- Check configuration" output-string))
        (is (search "- Restart the service" output-string))))))

(deftest minimal-selective-completion
  "Test selective completion reporting"
  (let ((output (make-string-output-stream)))
    (let ((monitor (monitor:make-process-monitor))
          (renderer (minimal:make-minimal-renderer 
                    :show-completion nil
                    :show-failures t
                    :stream output)))
      
      (monitor:add-renderer monitor renderer)
      (monitor:set-default-monitor monitor)
      (monitor:start-monitor monitor)
      
      ;; Successful completion (should not show)
      (let ((ctx (monitor:create-process-context :operation "Success")))
        (setf (monitor:process-context-state ctx) :completed)
        (monitor:render-event renderer
                            (monitor:make-process-event
                             :context-id :test
                             :type :complete
                             :timestamp (time:now)
                             :message "Success")
                            ctx))
      
      ;; Failed completion (should show)
      (let ((ctx (monitor:create-process-context :operation "Failure")))
        (setf (monitor:process-context-state ctx) :failed)
        (monitor:render-event renderer
                            (monitor:make-process-event
                             :context-id :test
                             :type :complete
                             :timestamp (time:now)
                             :message "Failed")
                            ctx))
      
      (let ((output-string (get-output-stream-string output)))
        (is (not (search "Success" output-string)))
        (is (search "FAILED: Failed" output-string))))))

(deftest minimal-timing-information
  "Test that timing information is included when available"
  (let ((output (make-string-output-stream)))
    (let ((monitor (monitor:make-process-monitor))
          (renderer (minimal:make-minimal-renderer :show-completion t :stream output)))
      
      (monitor:add-renderer monitor renderer)
      (monitor:set-default-monitor monitor)
      (monitor:start-monitor monitor)
      
      ;; Create context with known start time
      (let ((ctx (monitor:create-process-context :operation "Timed Op")))
        ;; Simulate some elapsed time by manually adjusting the start time
        (setf (monitor:process-context-state ctx) :completed)
        (monitor:render-event renderer
                            (monitor:make-process-event
                             :context-id :test
                             :type :complete
                             :timestamp (time:now)
                             :message "Timed operation")
                            ctx))
      
      (let ((output-string (get-output-stream-string output)))
        ;; Should show timing information
        (is (search "COMPLETED: Timed operation" output-string))
        (is (search "s)" output-string)))))) ; Look for seconds marker

(deftest minimal-final-summary
  "Test final summary generation"
  (let ((output (make-string-output-stream)))
    (let ((renderer (minimal:make-minimal-renderer :show-failures t :stream output)))
      ;; Test summary with failures
      (minimal:render-final-summary renderer 3 10)
      
      (let ((output-string (get-output-stream-string output)))
        (is (search "SUMMARY: 3 of 10 operations failed" output-string)))))
  
  ;; Test summary with no failures (should not appear)
  (let ((output2 (make-string-output-stream)))
    (let ((renderer (minimal:make-minimal-renderer :show-failures t :stream output2)))
      (minimal:render-final-summary renderer 0 10)
      
      (let ((output-string (get-output-stream-string output2)))
        (is (string= "" output-string)))))) ; No output for zero failures

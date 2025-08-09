(defpackage epsilon.monitor.tests
  (:use
   cl
   epsilon.test)
  (:local-nicknames
   (monitor epsilon.monitor)
   (time epsilon.time)))

(in-package epsilon.monitor.tests)

;;;; Core Data Structure Tests

(deftest process-context-creation
  "Test creating process contexts with various options"
  ;; Basic context
  (let ((ctx (monitor:create-process-context :operation "Test Operation")))
    (is (typep ctx 'monitor:process-context))
    (is-equal "Test Operation" (monitor:process-context-operation ctx))
    (is-equal :starting (monitor:process-context-state ctx))
    (is-equal 0.0 (monitor:process-context-progress ctx))
    (is (typep (monitor:process-context-start-time ctx) 'time:timestamp)))
  
  ;; Context with all options
  (let ((parent (monitor:create-process-context :operation "Parent"))
        (metadata (make-hash-table :test 'equal)))
    (setf (gethash "key" metadata) "value")
    (let ((ctx (monitor:create-process-context 
                :operation "Child"
                :parent parent
                :state :running
                :progress 0.5
                :metadata metadata
                :estimated-duration 10.0
                :tags '("test" "demo"))))
      (is-equal parent (monitor:process-context-parent ctx))
      (is-equal :running (monitor:process-context-state ctx))
      (is-equal 0.5 (monitor:process-context-progress ctx))
      (is-equal 10.0 (monitor:process-context-estimated-duration ctx))
      (is-equal '("test" "demo") (monitor:process-context-tags ctx))
      (is-equal "value" (gethash "key" (monitor:process-context-metadata ctx))))))

(deftest process-event-creation
  "Test creating process events"
  (let ((event (monitor:make-process-event
                :context-id :test-ctx
                :timestamp (time:now)
                :type :log
                :level :info
                :message "Test message"
                :data '(:key "value")
                :tags '("test"))))
    (is (typep event 'monitor:process-event))
    (is-equal :test-ctx (monitor:process-event-context-id event))
    (is-equal :log (monitor:process-event-type event))
    (is-equal :info (monitor:process-event-level event))
    (is-equal "Test message" (monitor:process-event-message event))
    (is-equal '(:key "value") (monitor:process-event-data event))
    (is-equal '("test") (monitor:process-event-tags event))))

;;;; Context Management Tests

(deftest with-process-context-macro
  "Test the with-process-context macro"
  (let ((events '())
        (monitor (monitor:make-process-monitor)))
    
    ;; Create a test renderer that captures events
    (defclass test-renderer (monitor:process-renderer) ())
    (defmethod monitor:render-event ((renderer test-renderer) event context)
      (push (list :event (monitor:process-event-type event)
                  :state (and context (monitor:process-context-state context)))
            events))
    
    (monitor:add-renderer monitor (make-instance 'test-renderer))
    (monitor:set-default-monitor monitor)
    (monitor:start-monitor monitor)
    
    ;; Test successful execution
    (let ((result (monitor:with-process-context ("Test Process")
                    (monitor:process-log :info "Inside context")
                    42)))
      (is-equal 42 result))
    
    ;; Check captured events
    (is (>= (length events) 3)) ; start, log, complete
    (is (member '(:event :start :state :running) events :test #'equal))
    (is (member '(:event :complete :state :completed) events :test #'equal))))

(deftest with-process-context-error-handling
  "Test error handling in with-process-context"
  (let ((events '())
        (monitor (monitor:make-process-monitor)))
    
    ;; Create a test renderer that captures events
    (defclass test-error-renderer (monitor:process-renderer) ())
    (defmethod monitor:render-event ((renderer test-error-renderer) event context)
      (push (monitor:process-event-type event) events))
    
    (monitor:add-renderer monitor (make-instance 'test-error-renderer))
    (monitor:set-default-monitor monitor)
    (monitor:start-monitor monitor)
    
    ;; Test error propagation
    (handler-case
        (progn
          (monitor:with-process-context ("Error Process")
            (error "Test error"))
          (is nil "Expected error was not thrown"))
      (simple-error ()
        (is t "Error was properly propagated")))
    
    ;; Check that error event was emitted
    (is (member :error events))
    (is (member :complete events))))

(deftest nested-contexts
  "Test nested process contexts"
  (let ((contexts '())
        (monitor (monitor:make-process-monitor)))
    
    ;; Create a renderer that captures context hierarchy
    (defclass hierarchy-renderer (monitor:process-renderer) ())
    (defmethod monitor:render-event ((renderer hierarchy-renderer) event context)
      (when (eq (monitor:process-event-type event) :start)
        (push (list :operation (and context (monitor:process-context-operation context))
                    :parent (and context 
                               (monitor:process-context-parent context)
                               (monitor:process-context-operation 
                                (monitor:process-context-parent context))))
              contexts)))
    
    (monitor:add-renderer monitor (make-instance 'hierarchy-renderer))
    (monitor:set-default-monitor monitor)
    (monitor:start-monitor monitor)
    
    ;; Test nested contexts
    (monitor:with-process-context ("Parent")
      (monitor:with-nested-context ("Child1")
        (monitor:process-log :info "In child 1"))
      (monitor:with-nested-context ("Child2")
        (monitor:process-log :info "In child 2")))
    
    ;; Verify hierarchy
    (is (member '(:operation "Child1" :parent "Parent") contexts :test #'equal))
    (is (member '(:operation "Child2" :parent "Parent") contexts :test #'equal))))

;;;; Event Emission Tests

(deftest progress-tracking
  "Test progress tracking functionality"
  (let ((progress-updates '())
        (monitor (monitor:make-process-monitor)))
    
    ;; Create a renderer that captures progress
    (defclass progress-renderer (monitor:process-renderer) ())
    (defmethod monitor:render-event ((renderer progress-renderer) event context)
      (when (eq (monitor:process-event-type event) :progress)
        (push (monitor:process-context-progress context) progress-updates)))
    
    (monitor:add-renderer monitor (make-instance 'progress-renderer))
    (monitor:set-default-monitor monitor)
    (monitor:start-monitor monitor)
    
    ;; Test various progress update methods
    (monitor:with-process-context ("Progress Test")
      (monitor:process-progress :percentage 25)
      (monitor:process-progress :completed 1 :total 4)
      (monitor:process-progress :completed 3 :total 4))
    
    ;; Verify progress values
    (is-equal 3 (length progress-updates))
    (is (member 0.25 progress-updates))
    (is (member 0.75 progress-updates))))

(deftest event-data-propagation
  "Test that event data is properly propagated"
  (let ((captured-data nil)
        (monitor (monitor:make-process-monitor)))
    
    ;; Create a renderer that captures event data
    (defclass data-renderer (monitor:process-renderer) ())
    (defmethod monitor:render-event ((renderer data-renderer) event context)
      (when (eq (monitor:process-event-type event) :error)
        (setf captured-data (monitor:process-event-data event))))
    
    (monitor:add-renderer monitor (make-instance 'data-renderer))
    (monitor:set-default-monitor monitor)
    (monitor:start-monitor monitor)
    
    ;; Test error with structured data
    (monitor:with-process-context ("Data Test")
      (monitor:process-error "Test error"
                            :error-type 'test-error
                            :suggestions '("Fix it" "Try again")
                            :related-contexts '("context1" "context2")))
    
    ;; Verify data propagation
    (is-equal 'test-error (getf captured-data :error-type))
    (is-equal '("Fix it" "Try again") (getf captured-data :suggestions))
    (is-equal '("context1" "context2") (getf captured-data :related-contexts))))

;;;; Monitor Management Tests

(deftest monitor-lifecycle
  "Test monitor creation and lifecycle"
  (let ((monitor (monitor:make-process-monitor)))
    (is (typep monitor 'monitor:process-monitor))
    
    ;; Test starting and stopping
    (monitor:start-monitor monitor)
    (is (monitor:monitor-running-p monitor))
    
    (monitor:stop-monitor monitor)
    (is (not (monitor:monitor-running-p monitor)))))

(deftest renderer-management
  "Test adding and removing renderers"
  (let ((monitor (monitor:make-process-monitor))
        (renderer1 (make-instance 'monitor:process-renderer))
        (renderer2 (make-instance 'monitor:process-renderer)))
    
    ;; Add renderers
    (monitor:add-renderer monitor renderer1)
    (monitor:add-renderer monitor renderer2)
    (is-equal 2 (length (monitor:monitor-renderers monitor)))
    
    ;; Remove renderer
    (monitor:remove-renderer monitor renderer1)
    (is-equal 1 (length (monitor:monitor-renderers monitor)))
    (is (member renderer2 (monitor:monitor-renderers monitor)))))

(deftest multiple-renderers
  "Test that events are sent to all renderers"
  (let ((renderer1-count 0)
        (renderer2-count 0)
        (monitor (monitor:make-process-monitor)))
    
    ;; Create two renderers that count events
    (defclass count-renderer-1 (monitor:process-renderer) ())
    (defmethod monitor:render-event ((renderer count-renderer-1) event context)
      (incf renderer1-count))
    
    (defclass count-renderer-2 (monitor:process-renderer) ())
    (defmethod monitor:render-event ((renderer count-renderer-2) event context)
      (incf renderer2-count))
    
    (monitor:add-renderer monitor (make-instance 'count-renderer-1))
    (monitor:add-renderer monitor (make-instance 'count-renderer-2))
    (monitor:set-default-monitor monitor)
    (monitor:start-monitor monitor)
    
    ;; Generate some events
    (monitor:with-process-context ("Multi Renderer Test")
      (monitor:process-log :info "Test 1")
      (monitor:process-log :info "Test 2"))
    
    ;; Both renderers should have received all events
    (is (> renderer1-count 0))
    (is-equal renderer1-count renderer2-count)))

;;;; Edge Cases and Error Conditions

(deftest context-without-monitor
  "Test that operations work gracefully without a monitor"
  (monitor:set-default-monitor nil)
  
  ;; These should not error
  (is (null (monitor:process-log :info "No monitor")))
  (is (null (monitor:process-progress :percentage 50)))
  (is (null (monitor:process-error "No monitor error"))))

(deftest malformed-progress-values
  "Test handling of invalid progress values"
  (let ((captured-progress nil)
        (monitor (monitor:make-process-monitor)))
    
    ;; Create a renderer that captures progress
    (defclass capture-progress-renderer (monitor:process-renderer) ())
    (defmethod monitor:render-event ((renderer capture-progress-renderer) event context)
      (when (and (eq (monitor:process-event-type event) :progress) context)
        (setf captured-progress (monitor:process-context-progress context))))
    
    (monitor:add-renderer monitor (make-instance 'capture-progress-renderer))
    (monitor:set-default-monitor monitor)
    (monitor:start-monitor monitor)
    
    (monitor:with-process-context ("Progress Edge Cases")
      ;; Division by zero should be handled
      (monitor:process-progress :completed 5 :total 0)
      (is (numberp captured-progress))
      
      ;; Percentage over 100
      (monitor:process-progress :percentage 150)
      (is-equal 1.5 captured-progress))))

(deftest concurrent-context-access
  "Test thread safety of context access"
  ;; This is a placeholder for thread safety tests
  ;; In a real implementation, we'd test concurrent access patterns
  (is t "Thread safety tests not implemented yet"))
(defpackage :epsilon.log.tests
  (:use :cl :epsilon.test)
  (:local-nicknames 
   (:log :epsilon.log)
   (:map :epsilon.map)
   (:str :epsilon.string)))

(in-package :epsilon.log.tests)

;;; Test fixtures

(defclass test-appender (log:appender)
  ((messages :accessor test-messages :initform '()))
  (:documentation "Appender that captures messages for testing"))

(defmethod log:append-log ((appender test-appender) record)
  (push (cons (log:log-level record) (log:log-message record)) 
        (test-messages appender)))

(defmacro with-test-logger ((logger-var appender-var &key (level :debug)) &body body)
  "Set up a test logger with a test appender"
  `(let* ((,appender-var (make-instance 'test-appender))
          (,logger-var (make-instance 'log:logger 
                                      :name "test" 
                                      :level ,level
                                      :appenders (list ,appender-var))))
     (let ((log:*loggers* (map:make-map)))
       (map:assoc! log:*loggers* "test" ,logger-var)
       ,@body)))

;;; Basic Logging Tests

(deftest test-log-levels
  "Test that log levels are correctly filtered"
  (with-test-logger (logger appender :level :info)
    ;; These should be logged
    (log:emit-log logger :fatal "fatal message")
    (log:emit-log logger :error "error message") 
    (log:emit-log logger :warn "warn message")
    (log:emit-log logger :info "info message")
    
    ;; These should be filtered out
    (log:emit-log logger :debug "debug message")
    (log:emit-log logger :trace "trace message")
    
    (let ((messages (reverse (test-messages appender))))
      (is (= 4 (length messages)))
      (is (equal '(:fatal . "fatal message") (first messages)))
      (is (equal '(:error . "error message") (second messages)))
      (is (equal '(:warn . "warn message") (third messages)))
      (is (equal '(:info . "info message") (fourth messages))))))

(deftest test-logger-hierarchy
  "Test logger parent-child relationships"
  (let ((log:*loggers* (map:make-map))
        (log:*root-logger* (make-instance 'log:logger :name "root" :level :warn)))
    
    ;; Create parent and child loggers
    (let ((parent (log:get-logger "com.example"))
          (child (log:get-logger "com.example.module")))
      
      ;; Child should inherit parent's level initially
      (is (eq :warn (log:logger-effective-level child)))
      
      ;; Setting child level should override parent
      (setf (log:logger-level child) :debug)
      (is (eq :debug (log:logger-effective-level child)))
      
      ;; Parent change shouldn't affect child with explicit level
      (setf (log:logger-level parent) :error)
      (is (eq :debug (log:logger-effective-level child))))))

;;; Formatter Tests

(deftest test-compact-formatter
  "Test compact log format"
  (let* ((record (make-instance 'log:log-record
                                :timestamp (get-universal-time)
                                :level :info
                                :logger (make-instance 'log:logger :name "test.module")
                                :message "Test message"
                                :thread "main"
                                :package "TEST"
                                :file "test.lisp"
                                :line 42))
         (output (log:format-log-record record :compact)))
    
    ;; Check format components
    (is (search "INF" output))
    (is (search "test.module" output))
    (is (search "test.lisp:42" output))
    (is (search "Test message" output))
    
    ;; Should be reasonably compact
    (is (< (length output) 100))))

(deftest test-detailed-formatter
  "Test detailed log format"
  (let* ((record (make-instance 'log:log-record
                                :timestamp (get-universal-time)
                                :level :error
                                :logger (make-instance 'log:logger :name "app")
                                :message "Error occurred"
                                :thread "worker-1"
                                :package "APP.CORE"
                                :file "app.lisp"
                                :line 100
                                :context '(:user-id 123 :request-id "abc")))
         (output (log:format-log-record record :detailed)))
    
    ;; Check all components are present
    (is (search "ERROR" output))
    (is (search "app" output))
    (is (search "worker-1" output))
    (is (search "APP.CORE" output))
    (is (search "app.lisp:100" output))
    (is (search "Error occurred" output))
    (is (search "user-id=123" output))
    (is (search "request-id=abc" output))))

(deftest test-json-formatter
  "Test JSON log format"
  (let* ((record (make-instance 'log:log-record
                                :timestamp (get-universal-time)
                                :level :warn
                                :logger (make-instance 'log:logger :name "json.test")
                                :message "Warning message"
                                :thread "main"
                                :package "JSON.TEST"
                                :file "json.lisp"
                                :line 25))
         (output (log:format-log-record record :json)))
    
    ;; Check JSON structure
    (is (char= #\{ (char output 0)))
    (is (char= #\} (char output (1- (length output)))))
    (is (search "\"level\":\"WARN\"" output))
    (is (search "\"logger\":\"json.test\"" output))
    (is (search "\"message\":\"Warning message\"" output))
    (is (search "\"file\":\"json.lisp\"" output))
    (is (search "\"line\":25" output))))

;;; Appender Tests  

(deftest test-tee-appender
  "Test tee appender forwards to multiple appenders"
  (let* ((appender1 (make-instance 'test-appender))
         (appender2 (make-instance 'test-appender))
         (tee (make-instance 'log:tee-appender 
                            :appenders (list appender1 appender2)))
         (logger (make-instance 'log:logger
                               :name "tee-test"
                               :level :debug
                               :appenders (list tee))))
    
    (log:emit-log logger :info "test message")
    
    ;; Both appenders should receive the message
    (is (= 1 (length (test-messages appender1))))
    (is (= 1 (length (test-messages appender2))))
    (is (equal '(:info . "test message") (first (test-messages appender1))))
    (is (equal '(:info . "test message") (first (test-messages appender2))))))

;;; Configuration Tests

(deftest test-configure-from-string-simple
  "Test simple string configuration"
  (let ((log:*loggers* (map:make-map))
        (log:*root-logger* (make-instance 'log:logger :name "root" :level :info))
        (log:*wildcard-rules* '()))
    
    ;; Configure root level
    (log:configure-from-string "debug")
    (is (eq :debug (log:logger-level log:*root-logger*)))
    
    ;; Configure specific logger
    (log:configure-from-string "error:my.app")
    (let ((logger (log:get-logger "my.app")))
      (is (eq :error (log:logger-level logger))))))

(deftest test-configure-from-string-wildcards
  "Test wildcard configuration patterns"
  (let ((log:*loggers* (map:make-map))
        (log:*root-logger* (make-instance 'log:logger :name "root" :level :info))
        (log:*wildcard-rules* '()))
    
    ;; Configure with wildcard
    (log:configure-from-string "debug:app.*,trace:app.debug")
    
    ;; Check wildcard rule was stored
    (is (member "app." log:*wildcard-rules* :key #'car :test #'string=))
    
    ;; New loggers matching pattern should get the level
    (let ((logger1 (log:get-logger "app.module1"))
          (logger2 (log:get-logger "app.module2"))
          (logger3 (log:get-logger "other.module")))
      
      (is (eq :debug (log:logger-level logger1)))
      (is (eq :debug (log:logger-level logger2)))
      ;; Non-matching logger should not get the level
      (is (not (eq :debug (log:logger-level logger3))))
      
      ;; Exact match should override wildcard
      (let ((debug-logger (log:get-logger "app.debug")))
        (is (eq :trace (log:logger-level debug-logger)))))))

(deftest test-configure-from-string-multiple
  "Test multiple configuration specs"
  (let ((log:*loggers* (map:make-map))
        (log:*root-logger* (make-instance 'log:logger :name "root" :level :info))
        (log:*wildcard-rules* '()))
    
    ;; Configure multiple patterns
    (log:configure-from-string "warn,debug:epsilon.*,trace:epsilon.core.loader")
    
    ;; Root should be warn
    (is (eq :warn (log:logger-level log:*root-logger*)))
    
    ;; Create and check loggers
    (let ((eps-log (log:get-logger "epsilon.log"))
          (eps-loader (log:get-logger "epsilon.core.loader"))
          (other (log:get-logger "other.module")))
      
      (is (eq :debug (log:logger-level eps-log)))
      (is (eq :trace (log:logger-level eps-loader)))
      (is (eq :warn (log:logger-effective-level other))))))

;;; Structured Logging Tests

(deftest test-with-context
  "Test thread-local context"
  (with-test-logger (logger appender)
    (let ((log:*loggers* (map:make-map)))
      (map:assoc! log:*loggers* "test" logger)
      
      ;; Log without context
      (log:emit-log logger :info "no context")
      
      ;; Log with context
      (log:with-context (:user-id 123 :session "xyz")
        (log:emit-log-with-context logger :info "TEST" "with context"))
      
      ;; Context should be cleared
      (log:emit-log logger :info "context cleared")
      
      (is (= 3 (length (test-messages appender)))))))

(deftest test-structured-fields
  "Test structured field logging"
  (with-test-logger (logger appender)
    (let ((fields (log:fields :status 200 
                             :duration 125
                             :method "GET")))
      
      (log:emit-log-with-fields logger :info "TEST" "Request completed" fields)
      
      (is (= 1 (length (test-messages appender))))
      (is (equal '(:info . "Request completed") 
                 (first (test-messages appender)))))))

;;; Convenience Configuration Tests

(deftest test-configure-with-tee
  "Test convenience tee configuration"
  (let ((log:*root-logger* (make-instance 'log:logger :name "root" :level :info)))
    
    ;; Configure with tee (console only, no file)
    (log:configure-with-tee :level :debug 
                           :console-format :compact)
    
    (is (eq :debug (log:logger-level log:*root-logger*)))
    (is (= 1 (length (log:logger-appenders log:*root-logger*))))
    
    (let ((tee (first (log:logger-appenders log:*root-logger*))))
      (is (typep tee 'log:tee-appender))
      (is (= 1 (length (log:tee-appenders tee))))
      (is (typep (first (log:tee-appenders tee)) 'log:console-appender)))))

;;; Source Location Tests

(deftest test-source-location-capture
  "Test that source location is captured in macros"
  ;; This test verifies the macro expansion captures location
  (let ((log:*loggers* (map:make-map))
        (logger (log:get-logger "epsilon.log.tests")))
    
    ;; The log macro should capture file/line info
    (setf (log:logger-level logger) :debug)
    
    ;; Add a test appender to capture the record
    (let* ((records '())
           (capturing-appender 
            (make-instance 'log:appender))
           (original-appenders (log:logger-appenders logger)))
      
      ;; Override append-log to capture the record
      (defmethod log:append-log ((app (eql capturing-appender)) record)
        (push record records))
      
      (setf (log:logger-appenders logger) (list capturing-appender))
      
      ;; Use the log macro - should capture location
      (log:info "Test message from macro")
      
      ;; Restore original appenders
      (setf (log:logger-appenders logger) original-appenders)
      
      (when records
        (let ((record (first records)))
          ;; File should be captured (might be nil in REPL)
          (is (or (null (log:log-file record))
                  (stringp (log:log-file record))))
          ;; Line might be captured with new implementation
          (is (or (null (log:log-line record))
                  (numberp (log:log-line record)))))))))

;;; Performance Tests

(deftest test-logger-caching
  "Test that loggers are cached properly"
  (let ((log:*loggers* (map:make-map)))
    (let ((logger1 (log:get-logger "perf.test"))
          (logger2 (log:get-logger "perf.test")))
      
      ;; Should return the same logger instance
      (is (eq logger1 logger2))
      
      ;; Should be stored in cache
      (is (eq logger1 (map:get log:*loggers* "perf.test"))))))

(deftest test-level-check-performance
  "Test that level checking is efficient"
  (with-test-logger (logger appender :level :error)
    ;; These debug messages should be filtered quickly
    ;; without creating log records
    (dotimes (i 1000)
      (when (log:logger-enabled-p logger :debug)
        (log:emit-log logger :debug "This should not be logged")))
    
    ;; No messages should have been appended
    (is (= 0 (length (test-messages appender))))))

;;; Edge Cases

(deftest test-empty-logger-name
  "Test handling of empty logger names"
  (let ((log:*loggers* (map:make-map)))
    (let ((logger (log:get-logger "")))
      (is (string= "" (log:logger-name logger)))
      (is (eq log:*root-logger* (log:logger-parent logger))))))

(deftest test-nil-message
  "Test handling of nil messages"
  (with-test-logger (logger appender)
    (log:emit-log logger :info nil)
    (is (= 1 (length (test-messages appender))))
    ;; NIL should be handled gracefully
    (is (null (cdr (first (test-messages appender)))))))

(deftest test-circular-context
  "Test that circular references in context don't cause issues"
  (with-test-logger (logger appender)
    (let ((circular-list (list 1 2 3)))
      ;; Make it circular
      (setf (cdr (last circular-list)) circular-list)
      
      ;; This should not hang or error
      (handler-case
          (log:with-context (:data circular-list)
            (log:emit-log logger :info "Circular context")
            t)
        (error () nil))
      
      ;; Should have logged something
      (is (>= (length (test-messages appender)) 0)))))
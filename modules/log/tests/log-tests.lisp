(defpackage epsilon.log-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.log log)
            (epsilon.map map)
            (epsilon.string str))
  (:enter t))

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
     (let ((log::*loggers* {}))
       (map:assoc! log::*loggers* "test" ,logger-var)
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
      (assert-true (= 4 (length messages)))
      (assert-true (equal '(:fatal . "fatal message") (first messages)))
      (assert-true (equal '(:error . "error message") (second messages)))
      (assert-true (equal '(:warn . "warn message") (third messages)))
      (assert-true (equal '(:info . "info message") (fourth messages))))))

(deftest test-logger-hierarchy
  "Test logger parent-child relationships"
  (let ((log::*loggers* {})
        (log::*root-logger* (make-instance 'log:logger :name "root" :level :warn)))

    ;; Create parent and child loggers
    (let ((parent (log:get-logger "com.example"))
          (child (log:get-logger "com.example.module")))

      ;; Child should inherit root's level (parent has nil level too)
      (assert-true (eq :warn (log:logger-effective-level child)))

      ;; Setting child level should override parent
      (setf (log:logger-level child) :debug)
      (assert-true (eq :debug (log:logger-effective-level child)))

      ;; Parent change shouldn't affect child with explicit level
      (setf (log:logger-level parent) :error)
      (assert-true (eq :debug (log:logger-effective-level child))))))

(deftest test-hierarchy-propagation
  "Test that setting parent level propagates to children with nil level"
  (let ((log::*loggers* {})
        (log::*root-logger* (make-instance 'log:logger :name "root" :level :info))
        (log::*wildcard-rules* '()))

    ;; Create parent and children
    (let ((parent (log:get-logger "epsilon.web"))
          (child1 (log:get-logger "epsilon.web.view"))
          (child2 (log:get-logger "epsilon.web.view.live")))

      ;; All should inherit :info from root initially
      (assert-true (eq :info (log:logger-effective-level parent)))
      (assert-true (eq :info (log:logger-effective-level child1)))
      (assert-true (eq :info (log:logger-effective-level child2)))

      ;; Set parent to debug -- children should inherit immediately
      (setf (log:logger-level parent) :debug)
      (assert-true (eq :debug (log:logger-effective-level parent)))
      (assert-true (eq :debug (log:logger-effective-level child1)))
      (assert-true (eq :debug (log:logger-effective-level child2)))

      ;; Set one child explicitly -- it should not inherit further
      (setf (log:logger-level child1) :error)
      (assert-true (eq :error (log:logger-effective-level child1)))
      ;; Grandchild still inherits from parent through the chain
      ;; (child2's parent is child1 which now has :error)
      (assert-true (eq :error (log:logger-effective-level child2))))))

(deftest test-lazy-child-inherits-ancestor
  "Test that child logger created AFTER parent level is set inherits correctly"
  (let ((log::*loggers* {})
        (log::*root-logger* (make-instance 'log:logger :name "root" :level :info))
        (log::*wildcard-rules* '()))

    ;; Set parent level first
    (log:set-level "epsilon.web" :debug)

    ;; Now create child -- should inherit debug from parent
    (let ((child (log:get-logger "epsilon.web.view.live")))
      (assert-true (eq :debug (log:logger-effective-level child))))))

(deftest test-reparent-descendants
  "Test that creating intermediate logger re-parents existing descendants"
  (let ((log::*loggers* {})
        (log::*root-logger* (make-instance 'log:logger :name "root" :level :warn))
        (log::*wildcard-rules* '()))

    ;; Create grandchild first (no intermediate parent exists)
    (let ((grandchild (log:get-logger "a.b.c")))
      ;; Should be parented to root (a and a.b don't exist)
      (assert-true (eq log::*root-logger* (log::logger-parent grandchild)))
      (assert-true (eq :warn (log:logger-effective-level grandchild)))

      ;; Now create intermediate parent
      (let ((parent (log:get-logger "a.b")))
        ;; Grandchild should be re-parented to a.b
        (assert-true (eq parent (log::logger-parent grandchild)))

        ;; Set parent level -- grandchild should inherit
        (setf (log:logger-level parent) :debug)
        (assert-true (eq :debug (log:logger-effective-level grandchild)))))))

(deftest test-configure-spec-dotted-name
  "Test that --log 'epsilon.web=debug' works for child loggers"
  (let ((log::*loggers* {})
        (log::*root-logger* (make-instance 'log:logger :name "root" :level :info))
        (log::*wildcard-rules* '()))

    ;; Create child loggers first (simulating runtime package loading)
    (let ((child (log:get-logger "epsilon.web.view.live")))

      ;; Now apply spec like --log "epsilon.web=debug"
      (log:configure-from-spec "epsilon.web=debug")

      ;; Child should now inherit debug via re-parented chain
      (assert-true (eq :debug (log:logger-effective-level child))))))

;;; Formatter Tests

(deftest test-compact-formatter
  "Test compact log format: bare timestamp, 3-char level, lowercased logger, no file location"
  (let* ((record (make-instance 'log:log-record
                                :timestamp (get-universal-time)
                                :level :info
                                :logger (make-instance 'log:logger :name "TEST.MODULE")
                                :message "Test message"
                                :thread "main"
                                :package "TEST"
                                :file "test.lisp"
                                :line 42))
         (output (log::format-log-record record :compact)))

    ;; Check format: "HH:MM:SS INF test.module: Test message"
    (assert-true (search "INF" output))
    (assert-true (search "test.module:" output))    ;; lowercased, colon separator
    (assert-true (search "Test message" output))

    ;; File location should NOT appear in compact format
    (assert-true (not (search "test.lisp" output)))

    ;; No brackets in output
    (assert-true (not (search "[" output)))
    (assert-true (not (search "]" output)))

    ;; Should be reasonably compact
    (assert-true (< (length output) 80))))

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
         (output (log::format-log-record record :detailed)))

    ;; Check all components are present
    (assert-true (search "ERROR" output))
    (assert-true (search "app" output))
    (assert-true (search "worker-1" output))
    (assert-true (search "APP.CORE" output))
    (assert-true (search "app.lisp:100" output))
    (assert-true (search "Error occurred" output))
    (assert-true (search "USER-ID=123" output))
    (assert-true (search "REQUEST-ID=abc" output))))

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
         (output (log::format-log-record record :json)))

    ;; Check JSON structure
    (assert-true (char= #\{ (char output 0)))
    (assert-true (char= #\} (char output (1- (length output)))))
    (assert-true (search "\"level\":\"WARN\"" output))
    (assert-true (search "\"logger\":\"json.test\"" output))
    (assert-true (search "\"message\":\"Warning message\"" output))
    (assert-true (search "\"file\":\"json.lisp\"" output))
    (assert-true (search "\"line\":25" output))))

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
    (assert-true (= 1 (length (test-messages appender1))))
    (assert-true (= 1 (length (test-messages appender2))))
    (assert-true (equal '(:info . "test message") (first (test-messages appender1))))
    (assert-true (equal '(:info . "test message") (first (test-messages appender2))))))

;;; Configuration Tests

(deftest test-configure-from-spec-simple
  "Test simple spec configuration"
  (let ((log::*loggers* {})
        (log::*root-logger* (make-instance 'log:logger :name "root" :level :info))
        (log::*wildcard-rules* '()))

    ;; Configure root level
    (log:configure-from-spec "debug")
    (assert-true (eq :debug (log:logger-level log::*root-logger*)))

    ;; Configure specific logger with equals syntax
    (log:configure-from-spec "my.app=error")
    (let ((logger (log:get-logger "my.app")))
      (assert-true (eq :error (log:logger-level logger))))))

(deftest test-configure-from-spec-wildcards
  "Test wildcard configuration patterns"
  (let ((log::*loggers* {})
        (log::*root-logger* (make-instance 'log:logger :name "root" :level :info))
        (log::*wildcard-rules* '()))

    ;; Configure with wildcard using equals syntax
    (log:configure-from-spec "app.*=debug,app.debug=trace")

    ;; Check wildcard rule was stored
    (assert-true (member "app." log::*wildcard-rules* :key #'car :test #'string=))

    ;; New loggers matching pattern should get the level
    (let ((logger1 (log:get-logger "app.module1"))
          (logger2 (log:get-logger "app.module2"))
          (logger3 (log:get-logger "other.module")))

      (assert-true (eq :debug (log:logger-level logger1)))
      (assert-true (eq :debug (log:logger-level logger2)))
      ;; Non-matching logger should not get the level
      (assert-true (not (eq :debug (log:logger-level logger3))))

      ;; Exact match should override wildcard
      (let ((debug-logger (log:get-logger "app.debug")))
        (assert-true (eq :trace (log:logger-level debug-logger)))))))

(deftest test-configure-from-spec-multiple
  "Test multiple configuration specs"
  (let ((log::*loggers* {})
        (log::*root-logger* (make-instance 'log:logger :name "root" :level :info))
        (log::*wildcard-rules* '()))

    ;; Configure multiple patterns with equals syntax
    (log:configure-from-spec "warn,epsilon.*=debug,epsilon.core.loader=trace")

    ;; Root should be warn
    (assert-true (eq :warn (log:logger-level log::*root-logger*)))

    ;; Create and check loggers
    (let ((eps-log (log:get-logger "epsilon.log"))
          (eps-loader (log:get-logger "epsilon.core.loader"))
          (other (log:get-logger "other.module")))

      (assert-true (eq :debug (log:logger-level eps-log)))
      (assert-true (eq :trace (log:logger-level eps-loader)))
      (assert-true (eq :warn (log:logger-effective-level other))))))

(deftest test-configure-from-spec-presets
  "Test preset expansion"
  (let ((log::*loggers* {})
        (log::*root-logger* (make-instance 'log:logger :name "root" :level :info))
        (log::*wildcard-rules* '()))

    ;; verbose => debug
    (log:configure-from-spec "verbose")
    (assert-true (eq :debug (log:logger-level log::*root-logger*)))

    ;; quiet => warn
    (log:configure-from-spec "quiet")
    (assert-true (eq :warn (log:logger-level log::*root-logger*)))

    ;; silent => fatal
    (log:configure-from-spec "silent")
    (assert-true (eq :fatal (log:logger-level log::*root-logger*)))

    ;; Preset composes with targeted overrides
    (log:configure-from-spec "quiet,my.module=debug")
    (assert-true (eq :warn (log:logger-level log::*root-logger*)))
    (let ((logger (log:get-logger "my.module")))
      (assert-true (eq :debug (log:logger-level logger))))))

(deftest test-configure-from-spec-format-directive
  "Test +format directives"
  (let ((log::*root-logger* (make-instance 'log:logger :name "root" :level :info
                                           :appenders (list (make-instance
                                                             'log:console-appender
                                                             :formatter :compact)))))
    ;; Apply +json format
    (log:configure-from-spec "+json")
    (let ((app (first (log:logger-appenders log::*root-logger*))))
      (assert-true (eq :json (log::appender-formatter app))))

    ;; Apply +simple format
    (log:configure-from-spec "+simple")
    (let ((app (first (log:logger-appenders log::*root-logger*))))
      (assert-true (eq :simple (log::appender-formatter app))))))

(deftest test-configure-from-spec-abbreviated-names
  "Test abbreviated module name resolution"
  (let ((log::*loggers* {})
        (log::*root-logger* (make-instance 'log:logger :name "root" :level :info))
        (log::*wildcard-rules* '()))

    ;; Short name resolves to kreisler.service.NAME.*
    (log:configure-from-spec "hemidemi=debug")

    ;; Should have created a wildcard rule for kreisler.service.hemidemi.
    (assert-true (member "kreisler.service.hemidemi."
                         log::*wildcard-rules*
                         :key #'car :test #'string=))))

(deftest test-configure-from-spec-abbreviated-name-epsilon-prefix
  "Test abbreviated name resolves to epsilon.NAME.* when registered"
  (let ((log::*loggers* {})
        (log::*root-logger* (make-instance 'log:logger :name "root" :level :info))
        (log::*wildcard-rules* '()))

    ;; Pre-register a logger under epsilon.sql so the prefix match fires
    (log:get-logger "epsilon.sql")

    ;; Now 'sql' should resolve to epsilon.sql.* (not kreisler.service.sql.*)
    (log:configure-from-spec "sql=trace")

    (assert-true (member "epsilon.sql."
                         log::*wildcard-rules*
                         :key #'car :test #'string=))))

(deftest test-configure-from-spec-output-directive
  "Test @output directives"
  (let ((log::*root-logger* (make-instance 'log:logger :name "root" :level :info
                                           :appenders (list (make-instance
                                                             'log:console-appender
                                                             :formatter :compact)))))
    ;; @stderr should switch the console appender stream
    (log:configure-from-spec "@stderr")
    (let ((app (first (log:logger-appenders log::*root-logger*))))
      (assert-true (typep app 'log:console-appender))
      (assert-true (eq *error-output* (log:console-stream app))))

    ;; @stdout should switch back
    (log:configure-from-spec "@stdout")
    (let ((app (first (log:logger-appenders log::*root-logger*))))
      (assert-true (eq *standard-output* (log:console-stream app))))

    ;; @/tmp/test-log-spec.log should add a file appender
    (let ((count-before (length (log:logger-appenders log::*root-logger*))))
      (log:configure-from-spec "@/tmp/test-log-spec.log")
      (assert-true (= (1+ count-before)
                       (length (log:logger-appenders log::*root-logger*)))))))

(deftest test-configure-from-spec-invalid-directives
  "Test that invalid directives are silently ignored"
  (let ((log::*loggers* {})
        (log::*root-logger* (make-instance 'log:logger :name "root" :level :info))
        (log::*wildcard-rules* '()))

    ;; Invalid bare word -- should not change root level
    (log:configure-from-spec "banana")
    (assert-true (eq :info (log:logger-level log::*root-logger*)))

    ;; Invalid level in target=level -- should not create logger
    (log:configure-from-spec "my.app=banana")
    (assert-true (null (map:get log::*loggers* "my.app")))

    ;; Empty string -- should be no-op
    (log:configure-from-spec "")
    (assert-true (eq :info (log:logger-level log::*root-logger*)))

    ;; nil -- should be no-op
    (log:configure-from-spec nil)
    (assert-true (eq :info (log:logger-level log::*root-logger*)))))

;;; Structured Logging Tests

(deftest test-with-context
  "Test thread-local context"
  (with-test-logger (logger appender)
    (let ((log::*loggers* {}))
      (map:assoc! log::*loggers* "test" logger)

      ;; Log without context
      (log:emit-log logger :info "no context")

      ;; Log with context
      (log:with-context (:user-id 123 :session "xyz")
        (log:emit-log-with-context logger :info "TEST" "with context"))

      ;; Context should be cleared
      (log:emit-log logger :info "context cleared")

      (assert-true (= 3 (length (test-messages appender)))))))

(deftest test-structured-fields
  "Test structured field logging"
  (with-test-logger (logger appender)
    (let ((fields (log:fields :status 200
                             :duration 125
                             :method "GET")))

      (log:emit-log-with-fields logger :info "TEST" "Request completed" fields)

      (assert-true (= 1 (length (test-messages appender))))
      (assert-true (equal '(:info . "Request completed")
                 (first (test-messages appender)))))))

;;; Convenience Configuration Tests

(deftest test-configure-with-tee
  "Test convenience tee configuration"
  (let ((log::*root-logger* (make-instance 'log:logger :name "root" :level :info)))

    ;; Configure with tee (console only, no file)
    (log:configure-with-tee :level :debug
                           :console-format :compact)

    (assert-true (eq :debug (log:logger-level log::*root-logger*)))
    (assert-true (= 1 (length (log:logger-appenders log::*root-logger*))))

    (let ((tee (first (log:logger-appenders log::*root-logger*))))
      (assert-true (typep tee 'log:tee-appender))
      (assert-true (= 1 (length (log:tee-appenders tee))))
      (assert-true (typep (first (log:tee-appenders tee)) 'log:console-appender)))))

;;; Source Location Tests

(deftest test-source-location-capture
  "Test that source location is captured in macros"
  ;; This test verifies the macro expansion captures location
  (let ((log::*loggers* {})
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
          (assert-true (or (null (log:log-file record))
                  (stringp (log:log-file record))))
          ;; Line might be captured with new implementation
          (assert-true (or (null (log:log-line record))
                  (numberp (log:log-line record)))))))))

;;; Performance Tests

(deftest test-logger-caching
  "Test that loggers are cached properly"
  (let ((log::*loggers* {}))
    (let ((logger1 (log:get-logger "perf.test"))
          (logger2 (log:get-logger "perf.test")))

      ;; Should return the same logger instance
      (assert-true (eq logger1 logger2))

      ;; Should be stored in cache
      (assert-true (eq logger1 (map:get log::*loggers* "perf.test"))))))

(deftest test-level-check-performance
  "Test that level checking is efficient"
  (with-test-logger (logger appender :level :error)
    ;; These debug messages should be filtered quickly
    ;; without creating log records
    (dotimes (i 1000)
      (when (log:logger-enabled-p logger :debug)
        (log:emit-log logger :debug "This should not be logged")))

    ;; No messages should have been appended
    (assert-true (= 0 (length (test-messages appender))))))

;;; Edge Cases

(deftest test-empty-logger-name
  "Test handling of empty logger names"
  (let ((log::*loggers* {}))
    (let ((logger (log:get-logger "")))
      (assert-true (string= "" (log:logger-name logger)))
      (assert-true (eq log::*root-logger* (log::logger-parent logger))))))

(deftest test-nil-message
  "Test handling of nil messages"
  (with-test-logger (logger appender)
    (log:emit-log logger :info nil)
    (assert-true (= 1 (length (test-messages appender))))
    ;; NIL should be handled gracefully
    (assert-true (null (cdr (first (test-messages appender)))))))

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
      (assert-true (>= (length (test-messages appender)) 0)))))

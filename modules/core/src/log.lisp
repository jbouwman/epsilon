(defpackage epsilon.log
  (:use cl)
  (:shadow
   error log trace debug warn)
  (:local-nicknames
   (map epsilon.map)
   (str epsilon.string)
   (seq epsilon.sequence))
  (:export
   ;; Main logging macros and log levels
   log trace debug info warn error fatal
   
   ;; Structured logging macros
   log-with-fields info-with-fields debug-with-fields 
   warn-with-fields error-with-fields
   
   ;; Structured logging helpers
   fields
   
   ;; Configuration
   configure configure-from-string reset-configuration
   set-level get-level logger-enabled-p logger-level
   
   ;; Appenders
   add-appender remove-appender console-appender file-appender
   tee-appender logger-appenders appender append-log
   tee-appenders
   
   ;; Convenience configuration
   configure-with-tee
   
   ;; Internal types for testing
   logger log-record logger-name
   log-level log-message log-timestamp log-context
   log-fields log-thread log-package log-file log-line
   emit-log emit-log-with-context emit-log-with-fields
   logger-effective-level
   
   ;; Structured logging context
   with-context add-context remove-context
   
   ;; Loggers
   get-logger make-logger
   
   ;; Utilities
   format-timestamp current-thread-name
   
   ;; Source location
   with-source-location))

(in-package epsilon.log)

;;; Log Levels

(defparameter *log-levels* 
  (map:make-map :trace 0
                :debug 1  
                :info 2
                :warn 3
                :error 4
                :fatal 5))

(defun level-value (level)
  "Get numeric value for log level"
  (map:get *log-levels* level))

(defun level-enabled-p (logger-level message-level)
  "Check if message level is enabled for logger level"
  (>= (level-value message-level) (level-value logger-level)))

;;; Logger Hierarchy

(defclass logger ()
  ((name :initarg :name :reader logger-name)
   (level :initarg :level :accessor logger-level :initform :info)
   (appenders :initarg :appenders :accessor logger-appenders :initform '())
   (parent :initarg :parent :accessor logger-parent :initform nil)
   (inherit-appenders :initarg :inherit-appenders 
                      :accessor logger-inherit-appenders 
                      :initform t)))

(defparameter *loggers* (map:make-map))
(defparameter *root-logger* (make-instance 'logger :name "root" :level :info))

(defun get-logger (&optional (name (package-name *package*)))
  "Get or create logger for given name (defaults to current package)"
  (let ((name-str (if (stringp name) name (string name))))
    (or (map:get *loggers* name-str)
        (let ((logger (make-logger name-str)))
          (map:assoc! *loggers* name-str logger)
          logger))))

(defun make-logger (name)
  "Create a new logger with proper parent hierarchy"
  (let* ((parent (find-parent-logger name))
         (logger (make-instance 'logger 
				:name name 
				:parent parent
				:level (if parent (logger-level parent) :info))))
    ;; Apply any matching wildcard rules
    (apply-wildcard-rules logger)
    logger))

(defun find-parent-logger (name)
  "Find the closest parent logger in the hierarchy"
  (let ((parts (seq:realize (str:split #\. name))))
    (loop for i from (1- (length parts)) downto 1
          for parent-name = (str:join #\. (seq:seq (subseq parts 0 i)))
          for parent = (map:get *loggers* parent-name)
          when parent return parent
          finally (return *root-logger*))))

(defun logger-effective-level (logger)
  "Get effective level, walking up parent chain if needed"
  (if (logger-level logger)
      (logger-level logger)
    (if (logger-parent logger)
        (logger-effective-level (logger-parent logger))
      :info)))

(defun logger-enabled-p (logger level)
  "Check if logger is enabled for given level"
  (level-enabled-p (logger-effective-level logger) level))

;;; Context (Thread-local key-value pairs)

(defvar *log-context* nil
  "Thread-local logging context")

(defmacro with-context ((&rest context-pairs) &body body)
  "Execute body with additional logging context"
  `(let ((*log-context* (append (list ,@context-pairs) *log-context*)))
     ,@body))

(defun add-context (key value)
  "Add a key-value pair to current logging context"
  (push value *log-context*)
  (push key *log-context*))

(defun remove-context (key)
  "Remove a key from current logging context"
  (setf *log-context* 
        (loop for (k v) on *log-context* by #'cddr
              unless (eql k key)
              collect k and collect v)))

;;; Log Records

(defclass log-record ()
  ((timestamp :initarg :timestamp :reader log-timestamp)
   (level :initarg :level :reader log-level)
   (logger :initarg :logger :reader log-logger)
   (message :initarg :message :reader log-message)
   (context :initarg :context :reader log-context :initform nil)
   (fields :initarg :fields :reader log-fields :initform nil)
   (thread :initarg :thread :reader log-thread :initform "unknown")
   (package :initarg :package :reader log-package :initform "UNKNOWN")
   (file :initarg :file :reader log-file :initform nil)
   (line :initarg :line :reader log-line :initform nil)))

(defun make-log-record (logger level message)
  "Create a log record"
  (make-instance 'log-record
                 :timestamp (get-universal-time)
                 :level level
                 :logger logger
                 :message message
                 :context (copy-list *log-context*)
                 :thread (current-thread-name)
                 :package (package-name *package*)))

(defun make-log-record-with-context (logger level package-name message &key file line)
  "Create a log record with explicit package context"
  (make-instance 'log-record
                 :timestamp (get-universal-time)
                 :level level
                 :logger logger
                 :message message
                 :context (copy-list *log-context*)
                 :thread (current-thread-name)
                 :package package-name
                 :file file
                 :line line))

(defun make-log-record-with-fields (logger level package-name message fields &key file line)
  "Create a log record with structured fields"
  (make-instance 'log-record
                 :timestamp (get-universal-time)
                 :level level
                 :logger logger
                 :message message
                 :context (copy-list *log-context*)
                 :fields fields
                 :thread (current-thread-name)
                 :package package-name
                 :file file
                 :line line))

;;; Appenders

(defclass appender ()
  ((formatter :initarg :formatter :accessor appender-formatter :initform :simple)))

(defclass console-appender (appender)
  ((stream :initarg :stream :accessor console-stream :initform *standard-output*)))

(defclass file-appender (appender)
  ((filename :initarg :filename :accessor file-appender-filename)
   (stream :accessor file-appender-stream :initform nil)))

(defclass tee-appender (appender)
  ((appenders :initarg :appenders :accessor tee-appenders :initform '()))
  (:documentation "Appender that forwards to multiple child appenders"))

(defgeneric append-log (appender record)
  (:documentation "Write log record to appender"))

(defmethod append-log ((appender console-appender) record)
  (format (console-stream appender) "~A~%" 
          (format-log-record record (appender-formatter appender)))
  (force-output (console-stream appender)))

(defmethod append-log ((appender file-appender) record)
  (let ((filename (file-appender-filename appender)))
    (unless filename
      (cl:error "file-appender filename is nil"))
    (unless (file-appender-stream appender)
      (setf (file-appender-stream appender)
            (open filename
                  :direction :output
                  :if-exists :append
                  :if-does-not-exist :create)))
    (format (file-appender-stream appender) "~A~%" 
            (format-log-record record (appender-formatter appender)))
    (force-output (file-appender-stream appender))))

(defmethod append-log ((appender tee-appender) record)
  (dolist (child-appender (tee-appenders appender))
    (append-log child-appender record)))

;;; Formatters

(defun format-log-record (record formatter)
  "Format a log record using the given formatter"
  (case formatter
	(:compact (format-compact record))
	(:simple (format-simple record))
	(:detailed (format-detailed record))
	(:json (format-json record))
	(t (format-simple record))))

(defun format-compact (record)
  "Compact one-line format for console output"
  (let* ((time-only (multiple-value-bind (sec min hour)
					 (decode-universal-time (log-timestamp record))
					 (format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)))
         (level-abbrev (case (log-level record)
                             (:trace "TRC")
                             (:debug "DBG")
                             (:info "INF")
                             (:warn "WRN")
                             (:error "ERR")
                             (:fatal "FTL")
                             (t (subseq (string-upcase (log-level record)) 0 3))))
         (logger-short (let ((name (logger-name (log-logger record))))
                         (if (> (length name) 20)
                             (concatenate 'string (subseq name 0 17) "...")
                           name)))
         (location (cond
                    ((and (log-file record) (log-line record))
                     (format nil "~A:~A" 
                             (if (> (length (log-file record)) 20)
                                 (concatenate 'string "..." 
                                              (subseq (log-file record) 
                                                      (- (length (log-file record)) 17)))
                               (log-file record))
                             (log-line record)))
                    ((log-file record)
                     (if (> (length (log-file record)) 20)
                         (concatenate 'string "..." 
                                      (subseq (log-file record) 
                                              (- (length (log-file record)) 17)))
                       (log-file record)))
                    (t "")))
         (fields-str (when (log-fields record)
                       (format nil " {~{~A:~A~^ ~}}" 
                               (mapcan (lambda (pair)
                                         (if (consp pair)
                                             (list (car pair) (cdr pair))
                                           (list pair "true")))
                                       (log-fields record))))))
    (format nil "[~A] ~A [~A~@[ ~A~]] ~A~A"
            time-only
            level-abbrev
            logger-short
            (when (not (string= location "")) location)
            (log-message record)
            (or fields-str ""))))

(defun format-simple (record)
  "Simple one-line format"
  (let ((fields-str (when (log-fields record)
                      (format nil " ~{~A=~A~^ ~}" 
                              (mapcan (lambda (pair)
                                        (if (consp pair)
                                            (list (car pair) (cdr pair))
                                          (list pair "true")))
                                      (log-fields record)))))
        (location-str (cond
                       ((and (log-file record) (log-line record))
                        (format nil " [~A:~A]" 
                                (log-file record) 
                                (log-line record)))
                       ((log-file record)
                        (format nil " [~A]" (log-file record)))
                       (t ""))))
    (format nil "[~A] ~A ~A~A: ~A~A" 
            (format-timestamp (log-timestamp record))
            (string-upcase (log-level record))
            (logger-name (log-logger record))
            location-str
            (log-message record)
            (or fields-str ""))))

(defun format-detailed (record)
  "Detailed format with context and fields"
  (let ((context-str (when (log-context record)
                       (format nil " ctx:{~{~A=~A~^ ~}}" (log-context record))))
        (fields-str (when (log-fields record)
                      (format nil " fields:{~{~A=~A~^ ~}}" 
                              (mapcan (lambda (pair)
                                        (if (consp pair)
                                            (list (car pair) (cdr pair))
                                          (list pair "true")))
                                      (log-fields record)))))
        (location-str (cond
                       ((and (log-file record) (log-line record))
                        (format nil " ~A:~A" 
                                (log-file record) 
                                (log-line record)))
                       ((log-file record)
                        (format nil " ~A" (log-file record)))
                       (t ""))))
    (format nil "[~A] ~A ~A [~A@~A~A]: ~A~A~A"
            (format-timestamp (log-timestamp record))
            (string-upcase (log-level record))
            (logger-name (log-logger record))
            (log-thread record)
            (log-package record)
            location-str
            (log-message record)
            (or context-str "")
            (or fields-str ""))))

(defun format-json (record)
  "JSON format for structured logging"
  (let ((context-json (when (log-context record)
                        (format nil ",\"context\":{~{~S:~S~^,~}}" (log-context record))))
        (fields-json (when (log-fields record)
                       (format nil ",\"fields\":{~{\"~A\":~S~^,~}}" 
                               (mapcan (lambda (pair)
                                         (if (consp pair)
                                             (list (car pair) (cdr pair))
                                           (list pair "true")))
                                       (log-fields record)))))
        (file-json (when (log-file record)
                     (format nil ",\"file\":~S" (log-file record))))
        (line-json (when (log-line record)
                     (format nil ",\"line\":~D" (log-line record)))))
    (format nil "{\"timestamp\":~S,\"level\":~S,\"logger\":~S,\"message\":~S,\"thread\":~S,\"package\":~S~A~A~A~A}"
            (format-timestamp (log-timestamp record))
            (string (log-level record))
            (logger-name (log-logger record))
            (log-message record)
            (log-thread record)
            (log-package record)
            (or file-json "")
            (or line-json "")
            (or context-json "")
            (or fields-json ""))))

;;; Core Logging Function

(defun emit-log (logger level message)
  "Emit a log message if level is enabled"
  (when (logger-enabled-p logger level)
    (let ((record (make-log-record logger level message)))
      (emit-to-appenders logger record))))

(defun emit-log-with-context (logger level package-name message &key file line)
  "Emit a log message with explicit package context"
  (when (logger-enabled-p logger level)
    (let ((record (make-log-record-with-context logger level package-name message :file file :line line)))
      (emit-to-appenders logger record))))

(defun emit-log-with-fields (logger level package-name message fields &key file line)
  "Emit a log message with structured fields"
  (when (logger-enabled-p logger level)
    (let ((record (make-log-record-with-fields logger level package-name message fields :file file :line line)))
      (emit-to-appenders logger record))))

(defun emit-to-appenders (logger record)
  "Send record to all applicable appenders"
  (dolist (appender (logger-appenders logger))
    (append-log appender record))
  (when (and (logger-inherit-appenders logger) 
             (logger-parent logger))
    (emit-to-appenders (logger-parent logger) record)))

;;; Public Logging Macros

(defmacro with-source-location ((file-var line-var) &body body)
  "Capture source location at macro expansion time"
  `(let ((,file-var ,(or (when *compile-file-pathname* 
                           (enough-namestring *compile-file-pathname*))
                         (when *load-pathname*
                           (enough-namestring *load-pathname*))
                         nil))
         ;; Try to get line number from deep integration if available
         (,line-var (when (find-package :epsilon.compile-integration)
                      (let ((deep-pkg (find-package :epsilon.compile-integration))
                            (api-pkg (find-package :epsilon.compile-api)))
                        (when (and deep-pkg api-pkg)
                          (let ((location-var (find-symbol "*CURRENT-COMPILATION-LOCATION*" deep-pkg))
                                (line-fn (find-symbol "SOURCE-LOCATION-LINE" api-pkg)))
                            (when (and location-var line-fn 
                                       (boundp location-var)
                                       (symbol-value location-var))
                              (funcall line-fn (symbol-value location-var)))))))))
     ,@body))

(defmacro log (level format-string &rest args)
  "Log a message at the specified level"
  (let ((package-name (package-name *package*)))
    `(with-source-location (file line)
			   (let ((logger (get-logger ,package-name)))
			     (when (logger-enabled-p logger ,level)
			       (emit-log-with-context logger ,level ,package-name
						      ,(if args
							   `(format nil ,format-string ,@args)
							 format-string)
						      :file file
						      :line line))))))

(defmacro trace (format-string &rest args)
  `(log :trace ,format-string ,@args))

(defmacro debug (format-string &rest args)
  `(log :debug ,format-string ,@args))

(defmacro info (format-string &rest args)
  `(log :info ,format-string ,@args))

(defmacro warn (format-string-or-condition &rest args)
  "Log a warning message. If first argument is a condition, logs it with context."
  (let ((first-arg (gensym "FIRST-ARG")))
    `(let ((,first-arg ,format-string-or-condition))
       (if (typep ,first-arg 'condition)
           (log :warn "~A" ,first-arg ,@args)
         (log :warn ,first-arg ,@args)))))

(defmacro error (format-string-or-condition &rest args)
  "Log an error message. If first argument is a condition, logs it with context."
  (let ((first-arg (gensym "FIRST-ARG")))
    `(let ((,first-arg ,format-string-or-condition))
       (if (typep ,first-arg 'condition)
           (log :error "~A" ,first-arg ,@args)
         (log :error ,first-arg ,@args)))))

(defmacro fatal (format-string &rest args)
  `(log :fatal ,format-string ,@args))

;;; Structured Logging Macros

(defmacro log-with-fields (level format-string fields &rest args)
  "Log a message with structured fields (key-value pairs)"
  (let ((package-name (package-name *package*)))
    `(with-source-location (file line)
			   (let ((logger (get-logger ,package-name)))
			     (when (logger-enabled-p logger ,level)
			       (emit-log-with-fields logger ,level ,package-name
						     ,(if args
							  `(format nil ,format-string ,@args)
							format-string)
						     ,fields
						     :file file
						     :line line))))))

(defmacro info-with-fields (format-string fields &rest args)
  `(log-with-fields :info ,format-string ,fields ,@args))

(defmacro debug-with-fields (format-string fields &rest args)
  `(log-with-fields :debug ,format-string ,fields ,@args))

(defmacro warn-with-fields (format-string fields &rest args)
  `(log-with-fields :warn ,format-string ,fields ,@args))

(defmacro error-with-fields (format-string fields &rest args)
  `(log-with-fields :error ,format-string ,fields ,@args))

;;; Structured Logging Helpers

(defun fields (&rest key-value-pairs)
  "Create a field list from alternating keys and values"
  (when (oddp (length key-value-pairs))
    (error "fields requires an even number of arguments (key-value pairs)"))
  (loop for (key value) on key-value-pairs by #'cddr
        collect (cons key value)))

;;; Configuration

(defparameter *wildcard-rules* '()
  "Wildcard rules for logger configuration")

(defun configure-from-string (config-string)
  "Configure logging from string like 'debug:epsilon.*,trace:epsilon.yaml'"
  (let ((specs (seq:realize (str:split #\, config-string))))
    (dolist (spec specs)
      (when (and spec (not (str:empty-p spec)))
        (let* ((parts (seq:realize (str:split #\: spec)))
               (level-str (first parts))
               (pattern (second parts)))
          (when level-str
            (let ((level (intern (string-upcase level-str) :keyword)))
              (cond
               ;; No pattern means root logger
               ((or (null pattern) (string= pattern ""))
                (setf (logger-level *root-logger*) level))
               
               ;; Wildcard pattern
               ((position #\* pattern)
                (let ((prefix (subseq pattern 0 (position #\* pattern))))
                  ;; Apply to all matching loggers
                  (map:each (lambda (name logger)
                              (when (and (>= (length name) (length prefix))
                                         (string= prefix name :end2 (length prefix)))
                                (setf (logger-level logger) level)))
                            *loggers*)
                  ;; Also apply to new loggers that match (store the rule)
                  (push (cons prefix level) *wildcard-rules*)))
               
               ;; Exact match
               (t
                (let ((logger (get-logger pattern)))
                  (setf (logger-level logger) level)))))))))))

(defun apply-wildcard-rules (logger)
  "Apply any matching wildcard rules to a logger"
  (let ((name (logger-name logger)))
    (dolist (rule *wildcard-rules*)
      (let ((prefix (car rule))
            (level (cdr rule)))
        (when (and (>= (length name) (length prefix))
                   (string= prefix name :end2 (length prefix)))
          (setf (logger-level logger) level)
          (return))))))

(defun set-level (logger-name level)
  "Set level for specific logger"
  (let ((logger (get-logger logger-name)))
    (setf (logger-level logger) level)))

(defun get-level (logger-name)
  "Get level for specific logger"
  (logger-effective-level (get-logger logger-name)))

(defun reset-configuration ()
  "Reset all logging configuration"
  (setf *loggers* (map:make-map))
  (setf *root-logger* (make-instance 'logger :name "root" :level :info))
  (add-appender *root-logger* (make-instance 'console-appender)))

(defun configure (&key (level :info) (format :compact) (file nil))
  "Configure root logger with common options"
  (setf (logger-level *root-logger*) level)
  (setf (logger-appenders *root-logger*) '())
  (add-appender *root-logger* (make-instance 'console-appender 
                                             :formatter format))
  (when file
    (add-appender *root-logger* (make-instance 'file-appender
					       :filename file
					       :formatter format))))

(defun configure-with-tee (&key (level :info) 
                                (console-format :compact)
                                (file nil)
                                (file-format :detailed))
  "Configure root logger with tee appender for dual console/file output"
  (setf (logger-level *root-logger*) level)
  (setf (logger-appenders *root-logger*) '())
  
  (let ((appenders (list (make-instance 'console-appender 
                                        :formatter console-format))))
    (when file
      (push (make-instance 'file-appender
                           :filename file
                           :formatter file-format)
            appenders))
    
    (add-appender *root-logger* 
                  (make-instance 'tee-appender 
                                 :appenders appenders))))

(defun add-appender (logger appender)
  "Add appender to logger"
  (push appender (logger-appenders logger)))

(defun remove-appender (logger appender)
  "Remove appender from logger"
  (setf (logger-appenders logger) 
        (remove appender (logger-appenders logger))))

;;; Utilities

(defun format-timestamp (timestamp)
  "Format universal time as ISO 8601"
  (multiple-value-bind (sec min hour date month year)
		       (decode-universal-time timestamp)
		       (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
			       year month date hour min sec)))

(defun current-thread-name ()
  "Get current thread name"
  (sb-thread:thread-name sb-thread:*current-thread*))

;;; Initialize default configuration
(add-appender *root-logger* (make-instance 'console-appender :formatter :compact))

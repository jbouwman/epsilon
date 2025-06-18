(defpackage #:epsilon.tool.test
  (:use
   #:cl)
  (:local-nicknames
   (#:map #:epsilon.lib.map)
   (#:seq #:epsilon.lib.sequence)
   (#:str #:epsilon.lib.string)
   (#:pkg #:epsilon.sys.pkg)
   (#:build #:epsilon.tool.build)
   (#:re #:epsilon.lib.regex)
   (#:uri #:epsilon.lib.uri))
  (:export #:deftest
           #:is
           #:is-p
           #:is-equal
           #:is-equalp
           #:is-thrown-p
           #:project-file
           #:run-tests
           #:run-success-p
           #:log-test-result
           #:skip))

(in-package #:epsilon.tool.test)

(defvar *test-result* nil
  "Dynamically bound to current test result during execution")

;; fixme clean up error handling

(defun project-file (project-name relative-path)
  (uri:path (uri:merge (or (map:get build::*known-projects* project-name)
                           (error "unknown project"))
                       relative-path)))

(defclass test-node ()
  ((children :initform map:+empty+
             :accessor children-of
             :initarg :children
             :documentation "Map containing nested test suites")))

(defclass test-root (test-node)
  ())

(defvar *test-root* (make-instance 'test-root))

(defclass test-suite (test-node)
  ((name :accessor name-of
         :initarg :name
         :type string
         :documentation "Single segment of hierarchical package name")
   (tests :initform map:+empty+
          :accessor tests-of
          :documentation "Map containing test cases, keyed by bound function symbol")))

(defclass test ()
  ((symbol :accessor test-symbol
           :initarg :symbol
           :type symbol
           :documentation "Symbol naming the test function")))

(defmethod make-load-form ((self test) &optional environment)
  "Define how to save and load test objects in compiled files.
Ensures tests are re-registered in their suites when loaded from fasl files."
  (declare (ignore environment))
  (with-slots (symbol) self
    (values
     `(make-instance ',(class-name (class-of self))
                     :symbol ',symbol)
     `(ensure-test ',symbol))))

(defun ensure-test-suite (package-name)
  "Ensure package hierarchy exists, creating nodes as needed.
Returns the leaf node for package-name."
  (seq:reduce (lambda (current-node name)
                (let ((existing (map:get (children-of current-node) name)))
                  (unless existing
                    (setf existing (make-instance 'test-suite :name name))
                    (setf (children-of current-node)
                          (map:assoc (children-of current-node)
                                     name existing)))
                  existing))
              (pkg:parse package-name)
              :initial-value *test-root*))

;;; Metric collection

(defgeneric start-metric (metric-type test-result)
  (:documentation "Start collecting a metric for a test"))

(defgeneric end-metric (metric-type test-result)
  (:documentation "Stop collecting a metric and store result"))

(defgeneric format-metric (metric-type value stream)
  (:documentation "Format metric value for reports"))

(defclass metric ()
  ((name :initarg :name :reader metric-name)
   (value :initform nil :accessor metric-value)))

(defclass cpu-time-metric (metric)
  ((start-cpu :initform nil :accessor start-cpu)))

;;; Test results

(defclass test-result ()
  ((test :initarg :test
         :reader test-result-test)
   (name :initarg :name 
         :reader test-name)
   (status :initform :not-run 
           :accessor test-status)
   (condition :initform nil 
              :accessor test-condition)
   (stack-trace :initform nil 
                :accessor stack-trace)
   (assertions :initform nil 
               :accessor test-assertions)
   (start-time :initform nil
               :accessor start-time)
   (end-time :initform nil
             :accessor end-time)
   (metrics :initform map:+empty+ 
            :accessor test-metrics)
   (stdout-output :initform nil
                  :accessor stdout-output)
   (stderr-output :initform nil
                  :accessor stderr-output)
   (return-value :initform nil
                 :accessor return-value)))

(defclass test-run ()
  ((start-time :initform (get-internal-real-time)
               :reader start-time)
   (end-time :initform nil 
             :accessor end-time)
   (tests :initform map:+empty+
          :accessor tests)
   (formatter :initarg :formatter
              :initform (error "specify test formatter")
              :reader test-run-formatter)
   (log-file :initform nil
             :accessor log-file
             :initarg :log-file)
   (max-failures :initform 10
                 :accessor max-failures
                 :initarg :max-failures)
   (abort-on-failure :initform nil
                     :accessor abort-on-failure
                     :initarg :abort-on-failure)))

(defun test-failures (run)
  "Return list of failed test results"
  (remove-if-not (lambda (r) (eq :failure (test-status r)))
                 (map:vals (tests run))))

(defun test-errors (run)
  "Return list of errored test results"
  (remove-if-not (lambda (r) (eq :error (test-status r)))
                 (map:vals (tests run))))

(defun test-skips (run)
  "Return list of skipped test results"
  (remove-if-not (lambda (r) (eq :skip (test-status r)))
                 (map:vals (tests run))))

;;; Metric implementations

(defun get-metric (result name)
  (map:get (test-metrics result) name))

(defun set-metric (result name metric)
  (setf (test-metrics result)
        (map:assoc (test-metrics result) name metric)))

(defmethod start-metric ((type (eql :wall-time)) result)
  (let ((metric (make-instance 'timing-metric :name :wall-time)))
    (setf (start-time metric) (get-internal-real-time))
    (set-metric result :wall-time metric)))

(defmethod end-metric ((type (eql :wall-time)) result)
  (let ((metric (get-metric result :wall-time)))
    (setf (metric-value metric)
          (/ (- (get-internal-real-time) (start-time metric))
             internal-time-units-per-second))))

(defmethod format-metric ((type (eql :wall-time)) value stream)
  (format stream "~,3F seconds" value))

(defmethod start-metric ((type (eql :cpu-time)) result)
  (let ((metric (make-instance 'cpu-time-metric :name :cpu-time)))
    (setf (start-cpu metric) (get-internal-run-time))
    (set-metric result :cpu-time metric)))

(defmethod end-metric ((type (eql :cpu-time)) result)
  (let ((metric (get-metric result :cpu-time)))
    (setf (metric-value metric)
          (/ (- (get-internal-run-time) (start-cpu metric))
             internal-time-units-per-second))))

(defmethod format-metric ((type (eql :cpu-time)) value stream)
  (format stream "~,3F seconds" value))

;;; Test execution

(defun run-test-body (test args result)
  (let ((metrics '(:cpu-time))  ; removed :wall-time
        (stdout-stream (make-string-output-stream))
        (stderr-stream (make-string-output-stream))
        (return-value nil))
    (unwind-protect
         (progn
           (setf (start-time result) (get-internal-real-time))
           (dolist (metric-type metrics)
             (start-metric metric-type result))
           (let ((*test-result* result)
                 (*standard-output* (make-broadcast-stream *standard-output* stdout-stream))
                 (*error-output* (make-broadcast-stream *error-output* stderr-stream)))
             (setf return-value (apply (test-symbol test) args))))
      (setf (end-time result) (get-internal-real-time)
            (stdout-output result) (get-output-stream-string stdout-stream)
            (stderr-output result) (get-output-stream-string stderr-stream)
            (return-value result) return-value)
      (dolist (metric-type metrics)
        (end-metric metric-type result)))))

(defun elapsed-time (result)
  "Return elapsed time in seconds for a test result"
  (when (and (start-time result) (end-time result))
    (/ (- (end-time result) (start-time result))
       internal-time-units-per-second)))

(defun find-test-package (path node)
  "Find all tests defined in package and its subpackages."
  (seq:reduce (lambda (node name)
                (when node
                  (map:get (children-of node) name)))
              path
              :initial-value node))

(defun format-package-header (package-name)
  "Format a package header with standard indentation"
  (format t "~&;; ~A~%" package-name))

(defun format-status-field (result)
  "Format the timing and status field, returning its string representation"
  (when result
    (format nil "~,2fs~@[ ~A~]"
            (elapsed-time result)
            (case (test-status result)
              (:failure "FAILURE")
              (:error "ERROR")
              (:skip "SKIP")
              (otherwise nil)))))

(defun format-test-entry (test-name &optional (total-width 78) result)
  "Format a test entry with properly aligned timing and status.
TOTAL-WIDTH specifies the desired total line width (default 78 characters)."
  (let* ((status-field (format-status-field result))
         (status-width (if status-field (length status-field) 0))
         (name-width (length (string test-name)))
         (dots-width (+ 10 (- total-width 
                              name-width 
                              status-width))))
    (format t ";;     ~A ~V,,,'.A ~A~%"
            test-name
            dots-width
            "."
            (or status-field ""))))

(defun group-tests-by-package (tests)
  "Group tests by their package, returning alist of (package-name . tests)"
  (let ((groups (reduce (lambda (m test)
                          (let ((pkg-name (package-name (symbol-package (test-symbol test)))))
                            (map:assoc m pkg-name
                                       (cons test (map:get m pkg-name)))))
                        tests
                        :initial-value map:+empty+)))
    (sort (map:seq
           (map:map groups
                    (lambda (pkg pkg-tests)
                      (declare (ignore pkg))
                      (sort pkg-tests #'string< 
                            :key (lambda (test)
                                   (symbol-name (test-symbol test)))))))
          #'string< :key #'car)))

(defun collect-tests (test-suite &key name)
  "Recursively collect all tests from test-suite and its children"
  (let ((tests '()))
    (labels ((collect (node)
               (when (typep node 'test-suite)
                 (loop for test in (map:vals (tests-of node))
                       do (cond ((or (null name)
                                     (string-equal name (test-symbol test)))
                                 (push test tests)))))
               (when (typep node 'test-node)
                 (loop for child in (map:vals (children-of node))
                       do (collect child)))))
      (collect test-suite)
      tests)))

(defun list-tests (&optional package)
  "List all tests, optionally filtered by package prefix."
  (let* ((node (if package
                   (find-test-package (pkg:parse package) *test-root*)
                   *test-root*))
         (tests (collect-tests node))
         (grouped-tests (group-tests-by-package tests)))
    
    (dolist (group grouped-tests)
      (format-package-header (car group))
      (dolist (test (cdr group))
        (format-test-entry (symbol-name (test-symbol test)))))
    
    (format t "~&~%;; Found ~D test~:P~%" (length tests))
    (length tests)))

(defun first-line (string)
  "Return the first line of a multiline string"
  (let ((newline-pos (position #\Newline string)))
    (if newline-pos
        (subseq string 0 newline-pos)
        string)))

(defun format-condition-details (result)
  "Print a message summarizing a test failure, error, or skip"
  (when (test-condition result)
    (case (test-status result)
      (:failure
       (format nil "~&~%~%~A~%~%"
               (failure-message (test-condition result))))
      (:error
       (format nil "~&~%~%~A~%~%Stack:~%~%~A~%~%"
               (original-error (test-condition result))
               (stack-trace result)))
      (:skip
       (format nil "~&;;       Skipped: ~A~%"
               (skip-message (test-condition result)))))))

(defun log-test-result (result stream)
  "Log test result information to stream"
  (format stream "~&=== TEST: ~A ===~%" (test-name result))
  (format stream "Status: ~A~%" (test-status result))
  (format stream "Time: ~,3F seconds~%" (elapsed-time result))
  
  ;; Log return value
  (when (return-value result)
    (format stream "Return Value: ~S~%" (return-value result)))
  
  ;; Log stdout output
  (when (and (stdout-output result) (not (string= (stdout-output result) "")))
    (format stream "STDOUT:~%~A~%" (stdout-output result)))
  
  ;; Log stderr output
  (when (and (stderr-output result) (not (string= (stderr-output result) "")))
    (format stream "STDERR:~%~A~%" (stderr-output result)))
  
  ;; Log condition details
  (when (test-condition result)
    (format stream "Condition: ~A~%" (test-condition result)))
  
  ;; Log assertions
  (when (test-assertions result)
    (format stream "Assertions: ~D~%" (length (test-assertions result))))
  
  (format stream "~%"))

(defclass shell-test-formatter ()
  ((failure-count :initform 0)
   (max-failures :initform 10)))


(defgeneric event (formatter event-type event-data))

(defmethod event (formatter event-type event-data)
  )

(defmethod event ((formatter shell-test-formatter) (event-type (eql :start)) event-data)
  (format t "~&Running tests:~%~%"))

(defmethod event ((formatter shell-test-formatter) (event-type (eql :start-group)) group)
  (format-package-header (first group)))

(defmethod event ((formatter shell-test-formatter) (event-type (eql :end-test)) result)
  (with-slots (failure-count max-failures) formatter
    (let ((test (test-result-test result)))
      (format-test-entry (symbol-name (test-symbol test)) 60 result)
      (when (member (test-status result) '(:failure :error))
        (when (< failure-count max-failures)
          (format t "~A" (format-condition-details result))
          (incf failure-count))))))

(defmethod event ((formatter shell-test-formatter) (event-type (eql :end)) run)
  (let ((total-failures (+ (length (test-failures run)) (length (test-errors run)))))
    (with-slots (failure-count max-failures) formatter
      (format t "~&~%Test Run Complete:~%")
      (format t ";;   Tests: ~D~%" (map:size (tests run)))
      (format t ";;   Failures: ~D~%" (length (test-failures run)))
      (format t ";;   Errors: ~D~%" (length (test-errors run)))
      (format t ";;   Skipped: ~D~%" (length (test-skips run)))
      (when (> total-failures max-failures)
        (format t ";;   (Only ~D of ~D failures/errors shown)~%" 
                (min failure-count max-failures) total-failures))
      (format t ";;   Time: ~,2F seconds~%"
              (/ (- (end-time run) (start-time run))
                 internal-time-units-per-second)))))

(defmethod event ((run test-run) type data)
  (event (test-run-formatter run) type data))

(defvar *test-formats*
  (map:make-map :shell 'shell-test-formatter
                :human 'human-test-formatter
                :junit 'junit-test-formatter))

(defun make-test-formatter (type)
  (make-instance (or (map:get *test-formats* type)
                     (error "unknonw test runner type ~s" type))))

(defun run-tests (&key package name (format :shell))
  "Run tests, optionally filtered by package.
SHOW-FAILURES controls whether to show detailed failure information.
MAX-FAILURES limits the number of failures shown (default 10).
ABORT-ON-FAILURE when non-nil, stops execution after max-failures failures."
  (let* ((node (if package
                   (find-test-package (pkg:parse package) *test-root*)
                   *test-root*))
         (formatter (make-test-formatter format))
         (tests (collect-tests node :name name))
         (grouped-tests (group-tests-by-package tests))
         (run (make-instance 'test-run
                             :formatter formatter)))    
    (event run :start run)
    (dolist (group grouped-tests)
      (event run :start-group group)
      (dolist (test (cdr group))
        (event run :start-test test)
        (let ((result (run-testable test run)))
          (event run :end-test result))))
    (setf (end-time run) (get-internal-real-time))
    (event run :end run)
    run))

(defun run-success-p (run)
  (zerop (+ (length (test-failures run))
            (length (test-errors run)))))

(define-condition test-failure (error)
  ((message :initarg :message :reader failure-message)
   (form :initarg :form :reader failure-form)))

(define-condition test-error (error)
  ((original-error :initarg :error :reader original-error)))

(define-condition test-skip (condition)
  ((message :initarg :message :reader skip-message :initform "Test skipped")))

(defmacro def-map-setter (name slot-name)
  "Define a setter function that updates a map in a slot with a new key-value pair.
   NAME is the function name to be created.
   SLOT-NAME is the slot containing the map to be modified."
  `(defun ,name (object key value)
     (with-slots (,slot-name) object
       (setf ,slot-name (map:assoc ,slot-name key value)))))

(def-map-setter set-result! tests)

(defun run-testable (test run)
  "Run a test and record results in the test-run.
Returns the test-result instance."
  (let* ((result (make-instance 'test-result
                                :test test
                                :name (test-symbol test)))
         (*test-result* result))
    (unwind-protect
         (handler-bind
             ((test-skip (lambda (c)
                           (setf (test-status result) :skip
                                 (test-condition result) c)
                           (return-from run-testable result)))
              (test-failure (lambda (c)
                              (setf (test-status result) :failure
                                    (test-condition result) c)
                              (return-from run-testable result)))
              (error (lambda (c)
                       (setf (test-status result) :error
                             (test-condition result)
                             (make-instance
                              'test-error :error c)
                             (stack-trace result)
                             (with-output-to-string (stream)
                               (sb-debug:print-backtrace :stream stream)))
                       (return-from run-testable result))))
           (run-test-body test '() result))
      (set-result! run (test-symbol test) result))
    result))

(defun get-test (suite name)
  (map:get (tests-of suite) name))

(defun set-test! (suite name test)
  (setf (tests-of suite)
        (map:assoc (tests-of suite) name test)))

(defun ensure-test (symbol)
  "Find or create a test named by SYMBOL. If test exists, reinitialize it with ARGS.
The test's package hierarchy position is derived from the symbol's package."
  (check-type symbol symbol)
  (let* ((pkg-name (package-name (symbol-package symbol)))
         (suite (ensure-test-suite pkg-name)))
    (set-test! suite symbol
               (make-instance 'test :symbol symbol)))) ; FIXME this should just be a symbol

(defmacro deftest (name &body body)
  (let ((test-name (if (consp name) (car name) name)))
    `(progn
       (defun ,test-name () ,@body)
       (ensure-test ',test-name))))

(defun record-assertion (test-fn report-fn)
  (let ((result (funcall test-fn)))
    (when *test-result*
      (push (list result report-fn) 
            (test-assertions *test-result*)))
    result))

(defun fail-assertion (form message)
  (error 'test-failure
         :message message
         :form form))

(defun skip (&optional (message "Test skipped"))
  "Skip the current test with an optional message"
  (signal 'test-skip :message message))

(defmacro is (form &optional (message nil message-p) &rest message-args)
  `(if ,form
       (when *test-result*
         (push (list t (lambda (r) r)) 
               (test-assertions *test-result*)))
       (fail-assertion ',form 
                       ,(if message-p
                            `(format nil ,message ,@message-args)
                            `(format nil "Assertion ~S failed" ',form)))))

(defmacro is-p (predicate actual expected &optional (message nil message-p) &rest message-args)
  "Test that (PREDICATE ACTUAL EXPECTED) is true."
  (let ((actual-var (gensym "ACTUAL"))
        (expected-var (gensym "EXPECTED"))
        (predicate-name (if (symbolp predicate) 
                            predicate 
                            'predicate)))
    `(let ((,actual-var ,actual)
           (,expected-var ,expected))
       (if (,predicate ,actual-var ,expected-var)
           (when *test-result*
             (push (list t (lambda (r) r)) 
                   (test-assertions *test-result*)))
           (fail-assertion '(,predicate ,actual ,expected)
                           ,(if message-p
                                `(format nil ,message ,@message-args)
                                `(format nil "~A failed: expected ~S but got ~S~@[: ~A~]"
                                         ',predicate-name ,expected-var ,actual-var
                                         ,(when message-p
                                            `(format nil ,message ,@message-args)))))))))

(defmacro is-equal (actual expected &optional (message nil message-p) &rest message-args)
  "Test that ACTUAL equals EXPECTED using EQUAL."
  `(is-p equal ,actual ,expected 
       ,@(when message-p `(,message ,@message-args))))

(defmacro is-equalp (actual expected &optional (message nil message-p) &rest message-args)
  "Test that ACTUAL equals EXPECTED using EQUALP (case-insensitive, type-coercing)."
  `(is-p equalp ,actual ,expected 
       ,@(when message-p `(,message ,@message-args))))

(defmacro is-thrown-p ((condition-class &optional regex) &body body)
  "Test that BODY throws a condition of type CONDITION-CLASS.
If REGEX is provided, the condition's printed representation must match it."
  (let ((condition-var (gensym "CONDITION"))
        (caught-var (gensym "CAUGHT")))
    `(let ((,caught-var nil))
       (handler-case
           (progn
             ,@body
             (fail-assertion '(is-thrown-p ,condition-class ,@(when regex `(,regex)) ,@body)
                             ,(if regex
                                  `(format nil "Expected ~S matching ~S but no condition was thrown" 
                                           ',condition-class ,regex)
                                  `(format nil "Expected ~S but no condition was thrown" 
                                           ',condition-class))))
         (,condition-class (,condition-var)
           (setf ,caught-var t)
           ,(if regex
                `(let ((condition-string (format nil "~A" ,condition-var)))
                   (if (re:match ,regex condition-string)
                       (when *test-result*
                         (push (list t (lambda (r) r)) 
                               (test-assertions *test-result*)))
                       (fail-assertion '(is-thrown-p ,condition-class ,regex ,@body)
                                       (format nil "Expected ~S matching ~S but got: ~A" 
                                               ',condition-class ,regex condition-string))))
                `(when *test-result*
                   (push (list t (lambda (r) r)) 
                         (test-assertions *test-result*)))))
         (condition (,condition-var)
           (when ,caught-var
             (fail-assertion '(is-thrown-p ,condition-class ,@(when regex `(,regex)) ,@body)
                             ,(if regex
                                  `(format nil "Expected ~S matching ~S but got ~S: ~A" 
                                           ',condition-class ,regex 
                                           (type-of ,condition-var) ,condition-var)
                                  `(format nil "Expected ~S but got ~S: ~A" 
                                           ',condition-class 
                                           (type-of ,condition-var) ,condition-var)))))))))

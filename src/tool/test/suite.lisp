(defpackage epsilon.tool.test.suite
  (:use
   cl
   epsilon.lib.syntax)
  (:shadow
   condition)
  (:local-nicknames
   (common epsilon.tool.common)
   (f epsilon.lib.function)
   (map epsilon.lib.map)
   (set epsilon.lib.set)
   (seq epsilon.lib.sequence)
   (str epsilon.lib.string)
   (pkg epsilon.sys.pkg)
   (build epsilon.tool.build)
   (re epsilon.lib.regex)
   (uri epsilon.lib.uri)
   (xml epsilon.lib.xml))
  (:export
   *test*
   test
   register-test
   assertions
   condition
   status
   select
   run
   pass
   fail
   skip
   errors
   failures
   skipped
   list-suites
   suite-tests))

(in-package epsilon.tool.test.suite)

(defvar *test* nil
  "Dynamically bound to current test result during execution")

(defclass test-node ()
  ((children :initform map:+empty+
             :accessor children-of
             :initarg :children
             :documentation "Map containing nested test suites")))

(defmethod collect ((suite test-node))
  "Recursively collect all tests from test-suite and its children"
  (mapcan #'collect (map:vals (children-of suite))))

(defclass test-root (test-node)
  ())

(defvar *test-root* (make-instance 'test-root))

(defclass test-suite (test-node)
  ((name :accessor name-of
         :initarg :name
         :type string
         :documentation "Single segment of hierarchical package name")
   (tests :initform set:+empty+
          :accessor tests-of
          :documentation "Set containing bound function symbols of test cases")))

(defmethod collect ((suite test-suite))
  "Recursively collect all tests from test-suite and its children"
  (append (set:seq (tests-of suite))
          (call-next-method)))

(defun register-suite (package-name)
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
         :reader test)
   (status :initform :not-run 
           :accessor status)
   (condition :initform nil 
              :accessor condition)
   (stack-trace :initform nil 
                :accessor stack-trace)
   (assertions :initform nil 
               :accessor assertions)
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
   (reporter :initarg :reporter
              :initform (error "specify test reporter")
              :reader reporter)
   (max-failures :initform 10
                 :accessor max-failures
                 :initarg :max-failures)
   (abort-on-failure :initform nil
                     :accessor abort-on-failure
                     :initarg :abort-on-failure)))

(defmethod common:event ((run test-run) type data)
  (common:event (reporter run) type data))

(defun failures (run)
  "Return list of failed test results"
  (remove-if-not (lambda (r) (eq :failure (status r)))
                 (map:vals (tests run))))

(defun errors (run)
  "Return list of errored test results"
  (remove-if-not (lambda (r) (eq :error (status r)))
                 (map:vals (tests run))))

(defun skipped (run)
  "Return list of skipped test results"
  (remove-if-not (lambda (r) (eq :skip (status r)))
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
           (let ((*test* result)
                 (*standard-output* (make-broadcast-stream *standard-output* stdout-stream))
                 (*error-output* (make-broadcast-stream *error-output* stderr-stream)))
             (setf return-value (apply test args))))
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

(defun group-by-package (tests)
  "Group tests by their package, returning alist of (package-name . tests)"
  (sort (->> (seq:seq tests)
             (seq:group-by (f:compose #'package-name #'symbol-package))
             (map:map (lambda (tests)
                        (sort (seq:realize tests) #'string< :key #'symbol-name)))
             (map:vals))
        #'string< :key (f:compose #'package-name #'symbol-package #'car)))

(defun select (&key package name)
  (collect *test-root*))

(defun run (tests reporter)
  (let ((grouped-tests (group-by-package tests))
        (run (make-instance 'test-run
                            :reporter reporter)))    
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
  (zerop (+ (length (failures run))
            (length (errors run)))))

(defun list-suites (test-run)
  "Return a sorted sequence of package names that have at least one test result"
  (sort (remove-duplicates 
         (map:vals (map:map (lambda (result)
                              (package-name (symbol-package (test result))))
                            (tests test-run)))
         :test #'string=)
        #'string<))

(defun suite-tests (test-run suite-name)
  "Return all test results for the given suite (package) name, sorted by test name"
  (sort (map:vals (map:filter (lambda (name result)
                                (string= suite-name 
                                         (package-name (symbol-package (test result)))))
                              (tests test-run)))
        #'string< :key (lambda (result) (symbol-name (test result)))))

(define-condition test-failure (error)
  ((message :initarg :message :reader failure-message)
   (form :initarg :form :reader failure-form)))

(define-condition test-error (error)
  ((original-error :initarg :error :reader original-error)))

(define-condition skip (cl:condition)
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
                                :test test))
         (*test* result))
    (unwind-protect
         (handler-bind
             ((skip (lambda (c)
                      (setf (status result) :skip
                            (condition result) c)
                      (return-from run-testable result)))
              (test-failure (lambda (c)
                              (setf (status result) :failure
                                    (condition result) c)
                              (return-from run-testable result)))
              (error (lambda (c)
                       (setf (status result) :error
                             (condition result)
                             (make-instance
                              'test-error :error c)
                             (stack-trace result)
                             (with-output-to-string (stream)
                               (sb-debug:print-backtrace :stream stream)))
                       (return-from run-testable result))))
           (run-test-body test '() result))
      (set-result! run test result))
    result))

(defun register-test (symbol)
  "Find or create a test named by SYMBOL. If test exists, reinitialize it with ARGS.
The test's package hierarchy position is derived from the symbol's package."
  (check-type symbol symbol)
  (let* ((pkg-name (package-name (symbol-package symbol))) ; FIXME this is repated all over
         (suite (register-suite pkg-name)))
    (setf (tests-of suite)
          (set:add (tests-of suite) symbol))))

(defun record-assertion (test-fn report-fn)
  (let ((result (funcall test-fn)))
    (when *test*
      (push (list result report-fn) 
            (assertions *test*)))
    result))

(defun pass ()
  (when *test*
    (push (list t (lambda (r) r)) 
          (assertions *test*))))

(defun fail (form message)
  (error 'test-failure
         :message message
         :form form))

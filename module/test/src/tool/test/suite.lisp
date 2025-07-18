(defpackage epsilon.test.suite
  (:use
   cl
   epsilon.lib.syntax
   epsilon.tool.common)
  (:shadow
   condition)
  (:local-nicknames
   (f epsilon.lib.function)
   (map epsilon.lib.map)
   (set epsilon.lib.set)
   (seq epsilon.lib.sequence)
   (str epsilon.lib.string)
   (pkg epsilon.sys.pkg)
   (build epsilon.tool.build)
   (re epsilon.lib.regex)
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
   test-count
   list-suites
   suite-tests
   ensure-test-root
   clear-tests
   list-available-packages))

(in-package epsilon.test.suite)

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

(defvar *test-root* nil
  "Global test root. Initialized on first use.")

(defun ensure-test-root ()
  "Ensure *test-root* is properly initialized"
  (unless *test-root*
    (setf *test-root* (make-instance 'test-root)))
  *test-root*)

(defun clear-tests ()
  "Clear all registered tests. Useful for testing or reloading."
  (setf *test-root* nil)
  (ensure-test-root))

(defclass test-metadata ()
  ((symbol :initarg :symbol
           :reader test-symbol
           :documentation "The test function symbol")
   (docstring :initarg :docstring
              :initform nil
              :reader test-docstring
              :documentation "The test's documentation string")))

(defclass test-suite (test-node)
  ((name :accessor name-of
         :initarg :name
         :type string
         :documentation "Single segment of hierarchical package name")
   (tests :initform map:+empty+
          :accessor tests-of
          :documentation "Map from test symbol to test-metadata")))

(defmethod collect ((suite test-suite))
  "Recursively collect all tests from test-suite and its children"
  (append (mapcar #'test-symbol (map:vals (tests-of suite)))
          (call-next-method)))

(defun register-suite (package-name)
  "Ensure package hierarchy exists, creating nodes as needed.
Returns the leaf node for package-name."
  (seq:reduce (lambda (current-node name)
                (let ((existing (map:get (children-of current-node) name)))
                  (unless existing
                    (setf existing (make-instance 'test-suite :name name))
                    (map:assoc! (children-of current-node) name existing))
                  existing))
              (pkg:parse package-name)
              :initial-value (ensure-test-root)))

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

(defmethod event ((run test-run) type data)
  (event (reporter run) type data))

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

(defun test-count (run)
  "Return the total number of tests in this run"
  (map:size (tests run)))

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
             (map:seq))
        #'string< :key #'car))

(defun select (&key package name)
  (->> (collect (ensure-test-root))
       seq:from-list
       (seq:filter (lambda (test)
                     (and (or (null package)
                              (pattern-match-p (string-downcase package)
                                               (string-downcase (package-name (symbol-package test)))))
                          (or (null name)
                              (pattern-match-p (string-downcase name)
                                               (string-downcase (symbol-name test)))))))
       seq:realize))

(defun pattern-match-p (pattern string)
  "Match pattern against string, supporting wildcards (*)"
  (if (str:contains-p pattern "*")
      (wildcard-match-p pattern string)
      (str:starts-with-p string pattern)))

(defun wildcard-match-p (pattern string)
  "Simple wildcard matching - supports * as any characters"
  (if (not (str:contains-p pattern "*"))
      (string= pattern string)
      (let ((parts (seq:realize (str:split #\* pattern))))
        (cond
          ((= (length parts) 1)
           (string= pattern string))
          ((= (length parts) 2)
           (let ((prefix (first parts))
                 (suffix (second parts)))
             (and (str:starts-with-p string prefix)
                  (str:ends-with-p string suffix)
                  (>= (length string) (+ (length prefix) (length suffix))))))
          (t
           ;; Multiple wildcards - more complex matching
           (wildcard-match-complex pattern string))))))

(defun wildcard-match-complex (pattern string)
  "Handle patterns with multiple wildcards"
  (let ((parts (seq:realize (str:split #\* pattern)))
        (pos 0))
    (loop for i from 0 below (length parts)
          for part = (nth i parts)
          do (cond
               ;; First part - must match from beginning
               ((= i 0)
                (unless (str:starts-with-p string part)
                  (return-from wildcard-match-complex nil))
                (setf pos (length part)))
               ;; Last part - must match at end
               ((= i (1- (length parts)))
                (unless (str:ends-with-p string part)
                  (return-from wildcard-match-complex nil)))
               ;; Middle part - must exist after current position
               (t
                (let ((found-pos (search part string :start2 pos)))
                  (unless found-pos
                    (return-from wildcard-match-complex nil))
                  (setf pos (+ found-pos (length part)))))))
    t))

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

(defun list-available-packages ()
  "Return a sorted list of package names that have registered tests"
  (let ((packages '()))
    (labels ((collect-packages (node)
               (when (typep node 'test-suite)
                 (when (> (map:count (tests-of node)) 0)
                   (push (name-of node) packages)))
               (map:each (lambda (name child)
                           (declare (ignore name))
                           (collect-packages child))
                         (children-of node))))
      (collect-packages (ensure-test-root))
      (sort packages #'string<))))

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
       (map:assoc! ,slot-name key value))))

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

(defun register-test (symbol &optional docstring)
  "Find or create a test named by SYMBOL. If test exists, reinitialize it with ARGS.
The test's package hierarchy position is derived from the symbol's package."
  (check-type symbol symbol)
  (let* ((pkg-name (package-name (symbol-package symbol))) ; FIXME this is repated all over
         (suite (register-suite pkg-name))
         (metadata (make-instance 'test-metadata 
                                  :symbol symbol
                                  :docstring docstring)))
    (map:assoc! (tests-of suite) symbol metadata)))

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
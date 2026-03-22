(defpackage epsilon.test.suite
  (:use :cl :epsilon.syntax)
  (:shadow condition)
  (:require (epsilon.function f)
            (epsilon.map map)
            (epsilon.set set)
            (epsilon.sequence seq)
            (epsilon.string str)
            (epsilon.sys.pkg pkg)
            (epsilon.log log)
            (epsilon.digest digest)
            (epsilon.stacktrace strace))
  (:enter t))

;;; Event Protocol
;;;
;;; The event generic function provides a callback mechanism for test reporters
;;; to receive notifications during test execution.

(defgeneric event (reporter event-type event-data)
  (:documentation "Report a test event to a reporter.
   EVENT-TYPE is a keyword like :start, :end, :start-test, :end-test, etc.
   EVENT-DATA contains relevant context for the event."))

(defmethod event (reporter event-type event-data)
  "Default method - do nothing"
  (declare (ignore reporter event-type event-data))
  nil)

;;; Hash Storage for test identification

(defvar *test-hash-map* map:+empty+
  "Map from hash prefix to full test identifier")

(defvar *test-id-map* map:+empty+
  "Map from full test identifier to hash")

(defvar *hash-length* 6
  "Default length of hash prefix to use")

;;; Hash Generation

(defun generate-test-hash (test-id)
  "Generate a hash for a test identifier.
   TEST-ID should be a string like 'module:package:test-name'"
  ;; Use MD5 from epsilon.digest, convert to lowercase hex
  (string-downcase (digest:bytes-to-hex (digest:md5 (str:string-to-octets test-id)))))

(defun find-unique-prefix (hash existing-hashes &optional (min-length *hash-length*))
  "Find the shortest unique prefix for a hash that doesn't collide with existing hashes.
   Returns the unique prefix of at least MIN-LENGTH characters."
  (loop for length from min-length to (length hash)
        for prefix = (subseq hash 0 (min length (length hash)))
        when (not (map:contains-key-p existing-hashes prefix))
        return prefix
        finally (return hash)))

(defun register-test-hash (test-symbol &optional module-name)
  "Register a test with its hash identifier.
   Returns the assigned hash prefix."
  (let* ((package-name (package-name (symbol-package test-symbol)))
         (test-name (symbol-name test-symbol))
         (test-id (if module-name
                      (format nil "~A:~A:~A" module-name package-name test-name)
                    (format nil "~A:~A" package-name test-name)))
         (full-hash (generate-test-hash test-id))
         (hash-prefix (find-unique-prefix full-hash *test-hash-map*)))

    ;; Store bidirectional mapping
    (setf *test-hash-map* (map:assoc *test-hash-map* hash-prefix test-id))
    (setf *test-id-map* (map:assoc *test-id-map* test-id hash-prefix))

    hash-prefix))

(defun hash-to-test-id (hash-prefix)
  "Convert a hash prefix to a full test identifier.
   Returns (values test-id found-p)"
  (let ((test-id (map:get *test-hash-map* hash-prefix)))
    (if test-id
        (values test-id t)
      (values nil nil))))

(defun test-id-to-hash (test-id)
  "Get the hash prefix for a test identifier."
  (map:get *test-id-map* test-id))

(defun clear-test-hashes ()
  "Clear all registered test hashes."
  (setf *test-hash-map* map:+empty+)
  (setf *test-id-map* map:+empty+))

;;; Helper functions

(defun symbol-package-name (symbol)
  "Get the package name of a symbol."
  (package-name (symbol-package symbol)))

;;; Test Categories

(defvar *test-categories* map:+empty+
  "Map from test symbol to category keyword")

(defun register-test-category (test-symbol category)
  "Register a category for TEST-SYMBOL."
  (setf *test-categories* (map:assoc *test-categories* test-symbol category)))

(defun test-category (test-symbol)
  "Return the category of TEST-SYMBOL, or NIL."
  (map:get *test-categories* test-symbol nil))

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

(defvar *test-root* nil
  "Global test root. Initialized on first use.")

(defun ensure-test-root ()
  "Ensure *test-root* is initialized"
  (unless *test-root*
    (setf *test-root* (make-instance 'test-root)))
  *test-root*)

(defun clear-tests ()
  "Clear all registered tests and unbind test function symbols for garbage collection."
  ;; First collect all test symbols before clearing the tree
  (when *test-root*
    (labels ((collect-test-symbols (node)
               (let ((symbols '()))
                 ;; Collect test symbols from test-suite nodes
                 (when (typep node 'test-suite)
                   (map:each (lambda (sym metadata)
                               (declare (ignore metadata))
                               (push sym symbols))
                             (tests-of node)))
                 ;; Recursively collect from children
                 (map:each (lambda (name child)
                             (declare (ignore name))
                             (setf symbols (append symbols (collect-test-symbols child))))
                           (children-of node))
                 symbols)))
      ;; Collect all test symbols
      (let ((test-symbols (collect-test-symbols *test-root*)))
        ;; Unbind each test function symbol to allow GC
        (dolist (sym test-symbols)
          (when (fboundp sym)
            (fmakunbound sym))))))
  ;; Clear the test tree
  (setf *test-root* nil)
  (ensure-test-root)
  ;; Clear hash mappings
  (clear-test-hashes)
  ;; Clear categories
  (setf *test-categories* map:+empty+))

;; Test metadata and suite classes moved earlier in the file

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

;;; Configurable Timeouts

(defvar *current-timeout* 10
  "Timeout for the currently running test, in seconds.
   Set to NIL to disable timeout.
   This is typically bound by the test runner based on per-test settings.")

;;; Output Capture Mode

(defvar *capture-only* t
  "When T, test stdout/stderr is captured to string streams only (no terminal output).
   When NIL, output is broadcast to both terminal and capture streams (verbose mode).
   Captured output remains available on test-result for reporters to display selectively.")

;;; Test execution

(defun redirect-console-appenders (stream)
  "Redirect all console appenders on the root logger to STREAM.
   Returns an alist of (appender . original-stream) for restoration."
  (let ((saved '()))
    (dolist (appender (log:logger-appenders log:*root-logger*))
      (when (typep appender 'log:console-appender)
        (push (cons appender (log:console-stream appender)) saved)
        (setf (log:console-stream appender) stream)))
    saved))

(defun restore-console-appenders (saved)
  "Restore console appenders to their original streams."
  (dolist (pair saved)
    (setf (log:console-stream (car pair)) (cdr pair))))

(defun run-test-body (test args result)
  (let ((metrics '(:cpu-time))  ; removed :wall-time
        (stdout-stream (make-string-output-stream))
        (stderr-stream (make-string-output-stream))
        (return-value nil)
        (timeout *current-timeout*))
    (unwind-protect
        (progn
          (setf (start-time result) (get-internal-real-time))
          (dolist (metric-type metrics)
            (start-metric metric-type result))
          (let ((*test* result)
                (*standard-output* (if *capture-only*
                                       stdout-stream
                                       (make-broadcast-stream *standard-output* stdout-stream)))
                (*error-output* (if *capture-only*
                                    stderr-stream
                                    (make-broadcast-stream *error-output* stderr-stream)))
                (saved-appenders (when *capture-only*
                                   (redirect-console-appenders stderr-stream))))
            (unwind-protect
                ;; Use configurable timeout (NIL = no timeout)
                (if timeout
                    (handler-case
                        (sb-ext:with-timeout timeout
                          (setf return-value (apply test args)))
                      (sb-ext:timeout ()
                        (error "Test timed out after ~D seconds" timeout)))
                    ;; No timeout
                    (setf return-value (apply test args)))
              (when saved-appenders
                (restore-console-appenders saved-appenders)))))
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
             (seq:group-by #'symbol-package-name)
             (map:map (lambda (tests)
                        (sort (seq:realize tests) #'string< :key #'symbol-name)))
             (map:to-alist))
        #'string< :key #'car))

(defun select (&key package name)
  (->> (collect (ensure-test-root))
       seq:seq
       (seq:filter (lambda (test)
                     (and (or (null package)
                              (pattern-match-p (string-downcase package)
                                               (string-downcase (symbol-package-name test))))
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

;;; Hook Callbacks
;;; These are set by epsilon.test to inject before/after behavior

(defvar *before-all-callback* nil
  "Function called with package-name before running package tests")

(defvar *after-all-callback* nil
  "Function called with package-name after running package tests")

(defvar *before-each-callback* nil
  "Function called with package-name before each test")

(defvar *after-each-callback* nil
  "Function called with package-name after each test")

(defvar *get-test-timeout-callback* nil
  "Function called with test-symbol to get timeout, returns seconds or NIL")

(defun run (tests reporter &key parallel (workers 1))
  "Run TESTS with REPORTER, optionally in parallel.
   PARALLEL - When T, tests marked with (:parallel t) run concurrently
   WORKERS - Number of worker threads (default: 4)"
  (let ((grouped-tests (group-by-package tests))
        (run (make-instance 'test-run
                            :reporter reporter)))
    (log:debug "Starting test run with ~D tests in ~D packages (parallel: ~A, workers: ~D)"
               (length tests) (length grouped-tests) parallel workers)
    (event run :start run)
    (dolist (group grouped-tests)
      (let ((package-name (car group))
            (group-tests (cdr group)))
        (log:debug "Running tests for package: ~A (~D tests)"
                   package-name (length group-tests))
        ;; Run before-all hook
        (when *before-all-callback*
          (funcall *before-all-callback* package-name))
        (event run :start-group group)

        ;; Run tests - either in parallel or sequentially
        (if parallel
            (run-tests-with-parallelism group-tests package-name run workers)
            (run-tests-sequentially group-tests package-name run))

        ;; Run after-all hook
        (when *after-all-callback*
          (funcall *after-all-callback* package-name))))
    (setf (end-time run) (get-internal-real-time))
    (log:debug "Test run completed - Passed: ~D, Failed: ~D, Errors: ~D, Skipped: ~D"
               (- (test-count run)
                  (length (failures run))
                  (length (errors run))
                  (length (skipped run)))
               (length (failures run))
               (length (errors run))
               (length (skipped run)))
    (event run :end run)
    run))

(defun run-tests-sequentially (tests package-name run)
  "Run TESTS sequentially with hooks."
  (dolist (test tests)
    (log:debug "Running test: ~A" (name-of test))
    ;; Run before-each hook
    (when *before-each-callback*
      (funcall *before-each-callback* package-name))
    (event run :start-test test)
    ;; Bind timeout for this test
    (let ((*current-timeout*
            (if *get-test-timeout-callback*
                (funcall *get-test-timeout-callback* test)
                *current-timeout*)))
      (let ((result (run-testable test run)))
        (log:debug "Test ~A completed with status: ~A"
                   (name-of test) (status result))
        (event run :end-test result)))
    ;; Run after-each hook
    (when *after-each-callback*
      (funcall *after-each-callback* package-name))))

(defvar *parallel-test-p-callback* nil
  "Callback function (test-symbol) -> boolean to check if test can run in parallel.
   Set by epsilon.test.parallel module when loaded.")

(defun run-tests-with-parallelism (tests package-name run workers)
  "Run TESTS with parallel execution for tests marked (:parallel t).
   Sequential tests run first, then parallel tests run concurrently."
  ;; Partition tests into parallel and sequential
  (let ((parallel-tests '())
        (sequential-tests '()))
    (dolist (test tests)
      ;; Check if test is marked for parallel execution via callback
      (if (and *parallel-test-p-callback*
               (funcall *parallel-test-p-callback* test))
          (push test parallel-tests)
          (push test sequential-tests)))
    (setf parallel-tests (nreverse parallel-tests))
    (setf sequential-tests (nreverse sequential-tests))

    (log:debug "Parallel execution: ~D parallel tests, ~D sequential tests"
               (length parallel-tests) (length sequential-tests))

    ;; Run sequential tests first (they may have side effects)
    (run-tests-sequentially sequential-tests package-name run)

    ;; Then run parallel tests concurrently
    (when parallel-tests
      (run-tests-in-parallel parallel-tests package-name run workers))))

(defun run-tests-in-parallel (tests package-name run workers)
  "Run TESTS concurrently using worker threads."
  (let* ((lock (sb-thread:make-mutex :name "parallel-test-lock"))
         (result-queue '())
         (thread-pool '()))

    ;; Create worker threads
    (let ((test-queue (copy-list tests)))
      (dotimes (i (min workers (length tests)))
        (push
         (sb-thread:make-thread
          (lambda ()
            (loop
              ;; Get next test
              (let ((test nil))
                (sb-thread:with-mutex (lock)
                  (setf test (pop test-queue)))
                (unless test (return))

                ;; Run the test
                (let ((result nil))
                  ;; Run before-each hook (thread-safe)
                  (when *before-each-callback*
                    (handler-case
                        (funcall *before-each-callback* package-name)
                      (error (e)
                        (log:warn "before-each hook error: ~A" e))))

                  ;; Run test with timeout
                  (let ((*current-timeout*
                          (if *get-test-timeout-callback*
                              (funcall *get-test-timeout-callback* test)
                              *current-timeout*)))
                    (setf result (run-testable test run)))

                  ;; Run after-each hook (thread-safe)
                  (when *after-each-callback*
                    (handler-case
                        (funcall *after-each-callback* package-name)
                      (error (e)
                        (log:warn "after-each hook error: ~A" e))))

                  ;; Queue result
                  (sb-thread:with-mutex (lock)
                    (push result result-queue))))))
          :name (format nil "test-worker-~D" i))
         thread-pool)))

    ;; Wait for all threads to complete
    (dolist (thread thread-pool)
      (sb-thread:join-thread thread))

    ;; Report results (in order they completed)
    (dolist (result (nreverse result-queue))
      (event run :start-test (test result))
      (event run :end-test result))))

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
                              (symbol-package-name (test result)))
                            (tests test-run)))
         :test #'string=)
        #'string<))

(defun suite-tests (test-run suite-name)
  "Return all test results for the given suite (package) name, sorted by test name"
  (sort (map:vals (map:filter (lambda (name result)
                                (declare (ignore name))
                                (string= suite-name
                                         (symbol-package-name (test result))))
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
                            (strace:capture-stack-trace c))
                      (return-from run-testable result))))
          (run-test-body test '() result))
      (set-result! run test result))
    result))

(defun register-test (symbol &optional docstring)
  "Find or create a test named by SYMBOL. If test exists, reinitialize it with ARGS.
The test's package hierarchy position is derived from the symbol's package."
  (check-type symbol symbol)
  (let* ((pkg-name (symbol-package-name symbol))
         (suite (register-suite pkg-name))
         (metadata (make-instance 'test-metadata
                                  :symbol symbol
                                  :docstring docstring)))
    (map:assoc! (tests-of suite) symbol metadata)
    ;; Register hash for this test
    (register-test-hash symbol)))

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

(defun fail (message &optional form)
  "Signal a test failure with MESSAGE.
   FORM is the optional expression that failed."
  (error 'test-failure
         :message message
         :form form))

;;; Hash-based test lookup

(defun find-test-by-hash (hash-prefix)
  "Find a test symbol by its hash prefix.
   Returns the test symbol or NIL if not found."
  (multiple-value-bind (test-id found-p) (hash-to-test-id hash-prefix)
    (when found-p
      ;; Parse the test-id to get package and name
      (let ((parts (seq:realize (str:split #\: test-id))))
        (when (>= (length parts) 2)
          (let* ((package-name (if (= (length parts) 3)
                                   (second parts)  ; module:package:name
                                 (first parts))) ; package:name
                 (test-name (if (= (length parts) 3)
                                (third parts)
                              (second parts)))
                 (package (find-package (string-upcase package-name))))
            (when package
              (find-symbol (string-upcase test-name) package))))))))

(defun find-test-and-module-by-hash (hash-prefix)
  "Find a test symbol and its module by hash prefix.
   Returns (values test-symbol module-name) or (values nil nil) if not found."
  (multiple-value-bind (test-id found-p) (hash-to-test-id hash-prefix)
    (when found-p
      ;; Parse the test-id to get module, package and name
      (let ((parts (seq:realize (str:split #\: test-id))))
        (when (>= (length parts) 2)
          (let* ((module-name (when (= (length parts) 3)
                                (first parts)))
                 (package-name (if (= (length parts) 3)
                                   (second parts)
                                 (first parts)))
                 (test-name (if (= (length parts) 3)
                                (third parts)
                              (second parts)))
                 (package (find-package (string-upcase package-name))))
            (if package
                (values (find-symbol (string-upcase test-name) package) module-name)
              ;; Package doesn't exist yet, return module info for loading
              (values nil (or module-name
                              ;; Try to guess module from package name
                              (guess-module-from-package package-name))))))))))

(defun guess-module-from-package (package-name)
  "Try to guess the module name from a package name.
   E.g., 'epsilon.xml.tests' -> 'epsilon.xml'"
  (let ((name-lower (string-downcase package-name)))
    (cond
     ;; Remove .tests suffix if present
     ((str:ends-with-p name-lower ".tests")
      (subseq name-lower 0 (- (length name-lower) 6)))
     ;; Remove -tests suffix if present
     ((str:ends-with-p name-lower "-tests")
      (subseq name-lower 0 (- (length name-lower) 6)))
     ;; Try removing last segment
     ((str:contains-p name-lower ".")
      (let ((dot-pos (position #\. name-lower :from-end t)))
        (when dot-pos
          (subseq name-lower 0 dot-pos))))
     ;; Default - assume it's an epsilon module
     (t (format nil "epsilon.~A" name-lower)))))

(defun list-all-tests-with-hashes ()
  "Return a list of all tests with their hash identifiers.
   Each element is (hash-prefix test-symbol test-id)."
  (let ((results '()))
    (labels ((collect-from-suite (node)
               ;; Only test-suite nodes have tests-of
               (when (typep node 'test-suite)
                 (map:each (lambda (symbol metadata)
                             (declare (ignore metadata))
                             (let* ((package-name (symbol-package-name symbol))
                                    (test-name (symbol-name symbol))
                                    (test-id (format nil "~A:~A" package-name test-name))
                                    (hash-prefix (test-id-to-hash test-id)))
                               (when hash-prefix
                                 (push (list hash-prefix symbol test-id) results))))
                           (tests-of node)))
               ;; Recursively process children
               (map:each (lambda (name child)
                           (declare (ignore name))
                           (collect-from-suite child))
                         (children-of node))))
      (collect-from-suite (ensure-test-root))
      (sort results #'string< :key #'third))))

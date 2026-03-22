;;;; This package provides a test framework with hierarchical test
;;;; organization, metrics collection, and multiple output formats.
;;;; It supports both individual test execution and batch test
;;;; running.
;;;;
;;;; To use, define tests with (deftest name ...) and run with (run)

(defpackage epsilon.test
  (:use :cl :epsilon.symbol)
  (:require (epsilon.loader loader)
            (epsilon.map map)
            (epsilon.sys.pkg pkg)
            (epsilon.file fs)
            (epsilon.test.report report)
            (epsilon.test.suite suite)
            (epsilon.test.fixture fixture)
            (epsilon.test.subtest subtest)
            (epsilon.test.snapshot snapshot)
            (epsilon.test.property property)
            (epsilon.test.mock mock)
            (epsilon.test.parallel parallel)
            (epsilon.test.isolation isolation)
            (epsilon.test.workdir workdir)
            (epsilon.annotation ann)
            (epsilon.path path)
            (epsilon.log log)
            (epsilon.sequence seq)
            (epsilon.string str))
  (:enter t))

;;; Configurable Timeouts

(defparameter *default-timeout* 10
  "Default timeout in seconds for tests. Set to NIL to disable timeouts.")

(defvar *test-noclean* nil
  "When T, retain test work directories after run for inspection.")

(defvar *test-work-root* nil
  "Bound to the run directory path during test execution.")

;;; ---------------------------------------------------------------------------
;;; Test Result Cache
;;;
;;; Dependency-based rebuild: if a module's content hash (which encodes
;;; its source and all transitive dependency hashes) hasn't changed since
;;; the last successful test run, skip re-running the tests.
;;; ---------------------------------------------------------------------------

(defvar *test-cache-enabled* t
  "When T, skip tests for modules whose content hash matches the last
   successful run.  Set to NIL or use --force to bypass.")

(defun test-cache-directory ()
  "Return the test result cache directory, or NIL if disabled.
   Respects EPSILON_TEST_CACHE env var.  Set to 'none' to disable."
  (let ((override (sb-ext:posix-getenv "EPSILON_TEST_CACHE")))
    (cond
      ((not *test-cache-enabled*) nil)
      ((and override (string-equal override "none")) nil)
      (override
       (let ((dir (format nil "~A/" override)))
         (ensure-directories-exist dir)
         dir))
      (t
       (let* ((home (or (sb-ext:posix-getenv "HOME") "/tmp"))
              (dir (format nil "~A/.epsilon/test-cache/v1/" home)))
         (ensure-directories-exist dir)
         dir)))))

(defun test-cache-path (module-name test-mode)
  "Return the cache file path for MODULE-NAME and TEST-MODE."
  (let ((cache-dir (test-cache-directory)))
    (when cache-dir
      (let ((path (format nil "~A~A/~A.passed"
                          cache-dir
                          (substitute #\/ #\. module-name)
                          (string-downcase (symbol-name test-mode)))))
        (ensure-directories-exist path)
        path))))

(defun test-cache-hit-p (module-name test-mode content-hash)
  "Return T if the test cache has a passing entry for MODULE-NAME
   with TEST-MODE that matches CONTENT-HASH."
  (when (and content-hash *test-cache-enabled*)
    (let ((path (test-cache-path module-name test-mode)))
      (when (and path (probe-file path))
        (handler-case
            (with-open-file (s path :direction :input)
              (let ((stored (read-line s nil nil)))
                (and stored (string= (string-trim '(#\Space #\Newline) stored)
                                     content-hash))))
          (error () nil))))))

(defun test-cache-store (module-name test-mode content-hash)
  "Record that MODULE-NAME passed TEST-MODE with CONTENT-HASH."
  (when (and content-hash *test-cache-enabled*)
    (let ((path (test-cache-path module-name test-mode)))
      (when path
        (handler-case
            (with-open-file (s path :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
              (write-string content-hash s))
          (error () nil))))))

(defvar *test-timeouts* map:+empty+
  "Map from test symbol to per-test timeout in seconds")

;; TODO module-file is here in order to load package-relative resources: non-source files that contain test data, metadata, configuration and so on. Ideally there should be a generic scheme for this that uses the package's load time or runtime environment.

(defun module-file (module-name relative-path)
  "Return absolute path to RELATIVE-PATH within MODULE-NAME's directory.
   Example: (module-file 'epsilon.test \"fixtures/data.txt\")"
  (let ((module (loader:get-module (loader:environment) module-name :error-p t)))
    (path:string-path-join (path::path-from-uri (loader:module-uri module)) relative-path)))

(defun fixture-path (module-name filename)
  "Return path to FILENAME in MODULE-NAME's tests/fixtures/ directory.
   Example: (fixture-path 'epsilon.json \"simple.json\")"
  (module-file module-name (format nil "tests/fixtures/~A" filename)))

(defun data-path (module-name filename)
  "Return path to FILENAME in MODULE-NAME's tests/data/ directory.
   Example: (data-path 'epsilon.compiler \"sample.s1\")"
  (module-file module-name (format nil "tests/data/~A" filename)))

(defun parse-deftest-options (body)
  "Parse options from deftest body. Returns (values options docstring real-body).
   Options are keyword forms like (:timeout 60) at the start of body."
  (let ((options map:+empty+)
        (docstring nil)
        (remaining body))
    ;; Check for docstring first
    (when (and (stringp (first remaining)) (rest remaining))
      (setf docstring (first remaining))
      (setf remaining (rest remaining)))
    ;; Parse keyword options
    (loop while (and remaining
                     (listp (first remaining))
                     (keywordp (caar remaining)))
          do (let ((opt (first remaining)))
               (setf options (map:assoc options (first opt) (second opt)))
               (setf remaining (rest remaining))))
    (values options docstring remaining)))

(defun apply-test-annotations (name annotations)
  "Apply consumed annotations to a test. Called at load time."
  (when annotations
    (ann:set-annotations name annotations)
    (let ((timeout (assoc :timeout annotations))
          (parallel (assoc :parallel annotations))
          (isolate (assoc :isolate annotations))
          (category (assoc :category annotations)))
      (when timeout
        (setf *test-timeouts* (map:assoc *test-timeouts* name (cdr timeout))))
      (when parallel
        (parallel:register-parallel-test name (cdr parallel)))
      (when isolate
        (isolation:register-isolated-test name (cdr isolate)))
      (when category
        (suite:register-test-category name (cdr category))))))

(defmacro deftest (name &body body)
  "Define a test with NAME.

   Supports optional docstring and options via #@ annotations or legacy
   in-body keyword forms:

   - (:timeout SECONDS) / #@(:timeout SECONDS) - Override default timeout
   - (:timeout nil) / #@(:timeout nil) - Disable timeout (useful for debugging)
   - (:parallel T) / #@:parallel - Mark test as safe for parallel execution
   - (:isolate :process) / #@(:isolate :process) - Run in a separate subprocess
   - #@(:category :integration) - Categorize the test

   Preferred (annotation) syntax:
   #@(:timeout 60)
   (deftest test-slow-operation
     \"Test that takes a long time\"
     (perform-expensive-computation))

   Legacy (in-body) syntax still supported:
   (deftest test-slow-operation
     \"Test that takes a long time\"
     (:timeout 60)
     (perform-expensive-computation))"
  (multiple-value-bind (options docstring real-body)
      (parse-deftest-options body)
    (declare (ignore options))
    ;; Legacy in-body option parsing (compile-time)
    (let ((timeout-opt (find-if (lambda (form)
                                  (and (listp form)
                                       (eq (first form) :timeout)))
                                body))
          (parallel-opt (find-if (lambda (form)
                                   (and (listp form)
                                        (eq (first form) :parallel)))
                                 body))
          (isolate-opt (find-if (lambda (form)
                                  (and (listp form)
                                       (eq (first form) :isolate)))
                                body)))
      `(progn
         (defun ,name ()
           ,@(when docstring (list docstring))
           ,@real-body)
         (epsilon.test.suite:register-test ',name ,docstring)
         ;; Consume #@ annotations at load time (set by with-annotations)
         (apply-test-annotations ',name (ann:consume-annotations))
         ;; Legacy in-body options (only apply if no #@ annotation overrides)
         ,@(when timeout-opt
             `((unless (map:get *test-timeouts* ',name)
                 (setf *test-timeouts*
                       (map:assoc *test-timeouts* ',name ,(second timeout-opt))))))
         ,@(when parallel-opt
             `((unless (parallel:parallel-test-p ',name)
                 (parallel:register-parallel-test ',name ,(second parallel-opt)))))
         ,@(when isolate-opt
             `((unless (isolation:isolated-test-p ',name)
                 (isolation:register-isolated-test ',name ,(second isolate-opt)))))))))

(defun get-test-timeout (test-symbol)
  "Get the timeout for a test, or the default if not specified"
  (or (map:get *test-timeouts* test-symbol)
      *default-timeout*))

(defun parse-test-spec (spec)
  "Parse a test specification into module, package, and name components.
   Returns (values module package name).
   Also supports hash format for individual test selection."
  ;; Check if spec looks like a hash (short hex string)
  (if (and (<= (length spec) 8)
           (not (find #\: spec))
           (every (lambda (c)
                    (or (digit-char-p c)
                        (member c '(#\a #\b #\c #\d #\e #\f
                                   #\A #\B #\C #\D #\E #\F))))
                  spec))
      ;; It's a hash - return it as the test name with no module/package
      (values nil nil spec)
      ;; Regular parsing
      (let* ((parts (seq:realize (str:split #\: spec)))
             (num-parts (length parts)))
        (case num-parts
          (1 ;; Just module
           (values (first parts) nil nil))
          (2 ;; module:package or :package
           (if (string= "" (first parts))
               (values nil (second parts) nil)
               (values (first parts) (second parts) nil)))
          (3 ;; module:package:name or :package:name
           (if (string= "" (first parts))
               (values nil (second parts) (third parts))
               (values (first parts) (second parts) (third parts))))
          (otherwise
           (error "Invalid test specification: ~A" spec))))))

(defun run (environment module &key package test-name (format :dot) file
                                     parallel (workers parallel:*parallel-workers*)
                                     (test-mode :unit))
  "Test an epsilon module.

  ENVIRONMENT - The loader environment with repositories configured
  MODULE - Module to test (e.g., 'epsilon', 'epsilon.http')
  PACKAGE - Specific package pattern to filter tests (optional)
  TEST-NAME - Specific test name pattern to run (optional) or hash prefix
  FORMAT - Output format: :dot, :shell, :verbose, :tap, etc. (default: :dot)
  FILE - Output file for test report (optional)
  PARALLEL - Enable parallel execution for tests marked (:parallel t)
  WORKERS - Number of worker threads for parallel execution (default: 4)
  TEST-MODE - Which test roots to load: :unit (tests/ only, default),
              :integration (integration/ only), or :all (both)

  Returns the test run result object."

  ;; Check if test-name looks like a hash and no module specified
  (when (and test-name
             (not package)
             (not module)
             (<= (length test-name) 8)
             (every (lambda (c)
                      (or (digit-char-p c)
                          (member c '(#\a #\b #\c #\d #\e #\f
                                      #\A #\B #\C #\D #\E #\F))))
                    test-name))
    ;; Hash detected - need to load all test modules to find it
    (log:debug "Hash identifier detected: ~A, loading all test modules" test-name)
    (let ((modules (loader:query-modules environment)))
      (dolist (mod modules)
        (let ((module-name (loader:module-name mod)))
          ;; Skip certain modules that are known to not have tests or cause issues
          (unless (member module-name '("epsilon.tool.repl") :test #'string=)
            (handler-case
                (handler-bind ((sb-ext:compiler-note #'muffle-warning))
                  ;; Load module if needed
                  (unless (loader:module-loaded-p mod)
                    (loader:load-module environment module-name))
                  ;; Load test resources
                  (loader:load-module-resources environment module-name :tests))
              (error (e)
                ;; Silently skip modules that fail to load
                (log:debug "Skipping module ~A: ~A" module-name e))))))))

  ;; Load test files for the module using generic resource loader
  (when module
    (when (member test-mode '(:unit :all))
      (log:debug "Loading unit tests for module: ~A" module)
      (handler-bind ((sb-ext:compiler-note #'muffle-warning))
        (loader:load-module-resources environment module :tests)))
    (when (member test-mode '(:integration :all))
      (log:debug "Loading integration tests for module: ~A" module)
      (handler-bind ((sb-ext:compiler-note #'muffle-warning))
        (loader:load-module-resources environment module :integration))))

  ;; Check if test-name looks like a hash (short hex string)
  (let* ((pattern-selected-tests
          (if (and test-name
                   (not package)
                   (not module)
                   (<= (length test-name) 8)
                   (every (lambda (c)
                            (or (digit-char-p c)
                                (member c '(#\a #\b #\c #\d #\e #\f
                                            #\A #\B #\C #\D #\E #\F))))
                          test-name))
              ;; Try to find test by hash
              (let ((test-symbol (suite:find-test-by-hash test-name)))
                (if test-symbol
                    (list test-symbol)
                    ;; Fall back to pattern matching
                    (suite:select :package package :name test-name)))
              ;; Regular pattern-based selection
              (suite:select :package package :name test-name)))
         (selected-tests pattern-selected-tests))
    (log:debug "Test selection complete: ~D tests selected"
               (length selected-tests))
    (when (or package test-name)
      (log:debug "Selected ~D tests matching package: '~A', name: '~A'"
                 (length selected-tests) package test-name))
    (when (zerop (length selected-tests))
      (log:warn "No tests found!")
      (format t "~&;;; Warning: No tests found!~%"))
    (suite:run selected-tests
               (report:make :format format
                            :file file)
               :parallel parallel
               :workers workers)))

(defun run-tests (environment specs &key verbose noclean
                                        (test-mode :unit))
  "Run tests for multiple test specifications.

  ENVIRONMENT - The loader environment with repositories configured
  SPECS - List of test specification strings (e.g., \"module:package:test\")
  VERBOSE - Use verbose output format (default: nil)
  NOCLEAN - When T, retain test work directories after run
  TEST-MODE - Which test roots to load: :unit (tests/ only, default),
              :integration (integration/ only), or :all (both)

  Modules whose content hash matches a prior successful run are skipped
  unless *test-cache-enabled* is NIL.

  Returns (VALUES SUCCESS-P FAILED-MODULES) where FAILED-MODULES is a list
  of (spec . reason) pairs.  Reason is :load-error or :test-failure."
  (log:debug "run-tests called with environment: ~A, specs: ~A, verbose: ~A"
             environment specs verbose)
  (let ((*test-noclean* noclean)
        (suite:*capture-only* (not verbose))
        (failed-modules '()))
    (dolist (spec specs)
      ;; Clear test registry to ensure isolation between modules
      (suite:clear-tests)
      (multiple-value-bind (module package-filter name-filter)
          (parse-test-spec spec)
        (log:debug "Parsed test spec ~A -> module: ~A, package: ~A, name: ~A"
                   spec module package-filter name-filter)
        ;; Load the module if specified (muffle compiler notes to reduce noise)
        (when module
          (handler-case
              (handler-bind ((sb-ext:compiler-note #'muffle-warning))
                (loader:load-module environment module))
            (error (e)
              (format *error-output* "~&  FAIL ~A  (load error: ~A)~%" spec e)
              (push (cons spec :load-error) failed-modules)
              (go :next-spec))))

        ;; Check test result cache (skip if hash unchanged since last pass)
        (let ((content-hash (when module
                              (let ((mod-info (loader:get-module environment module)))
                                (when mod-info
                                  (loader:module-content-hash mod-info))))))
          (when (and module
                     content-hash
                     (null package-filter)
                     (null name-filter)
                     (test-cache-hit-p module test-mode content-hash))
            (format t "~&  --   ~A  (up-to-date)~%" module)
            (go :next-spec))

          ;; Create work directory for this module's test run
          (let* ((run-dir (workdir:make-run-directory (or module "unknown")))
                 (tmp-dir (fs:join-paths run-dir "tmp"))
                 (*test-work-root* run-dir)
                 (fs:*work-directory* tmp-dir)
                 (path:*work-directory* tmp-dir))
            (unwind-protect
                 (progn
                   ;; Set up file logging when verbose
                   (when verbose
                     (let ((log-file (fs:join-paths run-dir "test.log")))
                       (log:add-appender (log:get-logger)
                                         (make-instance 'log:file-appender
                                                        :filename log-file))))
                   ;; Run tests with filters.  When not verbose, buffer output
                   ;; so we can emit a compact one-line status per module.
                   (let* ((format-kw (if verbose :verbose :dot))
                          (output-buffer (unless verbose
                                          (make-string-output-stream)))
                          (start-time (get-internal-real-time))
                          (result (if output-buffer
                                     (let ((*standard-output* output-buffer))
                                       (run environment module
                                            :package package-filter
                                            :test-name name-filter
                                            :format format-kw
                                            :test-mode test-mode))
                                     (run environment module
                                          :package package-filter
                                          :test-name name-filter
                                          :format format-kw
                                          :test-mode test-mode)))
                          (elapsed (/ (- (get-internal-real-time) start-time)
                                      internal-time-units-per-second))
                          (n-tests (suite:test-count result)))
                     (log:debug "EPSILON.TEST:RUN returned: ~A" result)
                     (cond
                       ((success-p result)
                        (format t "~&  ok   ~A  ~D test~:P  ~,2Fs~%"
                                spec n-tests elapsed)
                        ;; Record content hash in cache
                        (when (and module content-hash
                                   (null package-filter) (null name-filter))
                          (test-cache-store module test-mode content-hash)))
                       (t
                        (format t "~&  FAIL ~A  ~D test~:P  ~,2Fs~%"
                                spec n-tests elapsed)
                        ;; Dump buffered output so failure details are visible
                        (when output-buffer
                          (let ((captured (get-output-stream-string output-buffer)))
                            (unless (string= captured "")
                              (write-string captured))))
                        (push (cons spec :test-failure) failed-modules)))))
              ;; Cleanup unless --noclean
              (if *test-noclean*
                  (format t "~&;;; Test artifacts retained at ~A~%" run-dir)
                  (workdir:cleanup-run-directory run-dir))))))
      :next-spec)
    ;; Return accumulated results
    (values (null failed-modules) (nreverse failed-modules))))

(defun success-p (run)
  "Return T if test RUN completed successfully with no failures.
   Example: (success-p test-result) => T"
  (zerop (+ (length (suite:failures run))
            (length (suite:errors run)))))

(defun skip (&optional (message "Test skipped"))
  "Skip the current test with an optional MESSAGE.
   Example: (skip \"Feature not supported on this platform\")"
  (signal 'suite:skip :message message))

(defun fail (message &optional form)
  "Fail the current test with MESSAGE.
   FORM is the optional expression that caused the failure.
   Example: (fail \"Expected value was not returned\")"
  (suite:fail message form))

(defmacro with-label (label &body body)
  "Execute BODY with a descriptive label for grouping assertions.
   The label is recorded with test results for better reporting."
  `(let ((label-start-time (get-internal-real-time)))
     (when suite:*test*
       (push (list :label-start ,label label-start-time)
             (suite:assertions suite:*test*)))
     (unwind-protect
         (progn ,@body)
       (when suite:*test*
         (push (list :label-end ,label (get-internal-real-time))
               (suite:assertions suite:*test*))))))

;;; Improved Assertion Output

(defun format-value-for-display (value &optional (max-length 200))
  "Format a value for display in assertion output"
  (let ((str (format nil "~S" value)))
    (if (> (length str) max-length)
        (concatenate 'string (subseq str 0 (- max-length 3)) "...")
        str)))

(defun format-assertion-failure (predicate-name actual-form expected-form
                                  actual-value expected-value)
  "Format a detailed assertion failure message"
  (with-output-to-string (s)
    (format s "~&Assertion failed: ~A~%~%" predicate-name)
    (format s "  Form: (~A ~S ~S)~%~%" predicate-name actual-form expected-form)
    (format s "  Expected: ~A~%" (format-value-for-display expected-value))
    (format s "    Actual: ~A~%" (format-value-for-display actual-value))
    ;; Add diff for complex structures
    (when (and (or (listp expected-value) (hash-table-p expected-value)
                   (vectorp expected-value))
               (or (listp actual-value) (hash-table-p actual-value)
                   (vectorp actual-value)))
      (let ((diff (compute-value-diff expected-value actual-value)))
        (when diff
          (format s "~%  Differences:~%")
          (dolist (d diff)
            (format s "    ~A~%" d)))))))

(defun compute-value-diff (expected actual &optional (path nil) (max-diffs 5))
  "Compute differences between expected and actual values.
   Returns list of diff descriptions."
  (let ((diffs nil))
    (labels ((add-diff (desc)
               (when (< (length diffs) max-diffs)
                 (push desc diffs)))
             (path-str ()
               (if path
                   (format nil "~{~A~^.~}" (reverse path))
                   "root"))
             (diff-lists (exp act)
               (let ((exp-len (length exp))
                     (act-len (length act)))
                 (when (/= exp-len act-len)
                   (add-diff (format nil "~A: length differs (expected ~D, got ~D)"
                                     (path-str) exp-len act-len)))
                 (loop for i from 0 below (min exp-len act-len)
                       for e = (nth i exp)
                       for a = (nth i act)
                       unless (equal e a)
                       do (if (and (listp e) (listp a))
                              (let ((sub-diffs (compute-value-diff e a (cons (format nil "[~D]" i) path))))
                                (dolist (d sub-diffs)
                                  (add-diff d)))
                              (add-diff (format nil "~A[~D]: expected ~S, got ~S"
                                                (path-str) i e a)))))))
      (cond
        ((equal expected actual) nil)
        ((and (listp expected) (listp actual))
         (diff-lists expected actual))
        ((and (vectorp expected) (vectorp actual))
         (diff-lists (coerce expected 'list) (coerce actual 'list)))
        (t (add-diff (format nil "~A: expected ~S, got ~S"
                             (path-str) expected actual)))))
    (nreverse diffs)))

(defmacro assert-p (predicate actual expected &optional (message nil message-p) &rest message-args)
  "Test that (PREDICATE ACTUAL EXPECTED) is true.
   On failure, shows detailed comparison of actual vs expected values."
  (with-gensyms (actual-var expected-var pred-name)
    `(let ((,actual-var ,actual)
           (,expected-var ,expected)
           (,pred-name (if (functionp ,predicate)
                           (or (ignore-errors
                                 (let ((fn ,predicate))
                                   (typecase fn
                                     (compiled-function
                                      (multiple-value-bind (lambda-expression closure-p name)
                                          (function-lambda-expression fn)
                                        (declare (ignore lambda-expression closure-p))
                                        name))
                                     (t nil))))
                               ',predicate)
                           ',predicate)))
       (declare (ignorable ,pred-name))
       (if (funcall ,predicate ,actual-var ,expected-var)
           (suite:pass)
           (suite:fail '(,predicate ,actual ,expected)
                       ,(if message-p
                            `(format nil ,message ,@message-args)
                            `(format-assertion-failure
                              ,pred-name ',actual ',expected
                              ,actual-var ,expected-var)))))))

(defmacro assert-true (actual &rest optargs)
  `(assert-p #'eq (not (null ,actual)) t ,@optargs))

(defmacro assert-= (actual expected &rest optargs)
  `(assert-p #'= ,actual ,expected ,@optargs))

(defmacro assert-eq (actual expected &rest optargs)
  `(assert-p #'eq ,actual ,expected ,@optargs))

(defmacro assert-eql (actual expected &rest optargs)
  `(assert-p #'eql ,actual ,expected ,@optargs))

(defmacro assert-equal (actual expected &rest optargs)
  `(assert-p #'equal ,actual ,expected ,@optargs))

(defmacro assert-equalp (actual expected &rest optargs)
  `(assert-p #'equalp ,actual ,expected ,@optargs))

(defmacro assert-not (form &rest optargs)
  `(assert-p #'eq (not ,form) t ,@optargs))

(defmacro assert-nil (form &rest optargs)
  "Test that form evaluates to NIL"
  `(assert-p #'eq ,form nil ,@optargs))

(defmacro assert-not-null (form &rest optargs)
  "Test that form does not evaluate to NIL"
  `(assert-p (lambda (x y) (declare (ignore y)) (not (null x))) ,form t ,@optargs))

(defmacro assert-condition ((condition-class) &body body)
  "Test that BODY throws a condition of type CONDITION-CLASS."
  (with-gensyms (condition-var caught-var)
    `(let ((,caught-var nil))
       (declare (ignorable ,caught-var))
       (handler-case
           (progn
             ,@body
             (suite:fail '(assert-condition ,condition-class ,@body)
                         (format nil "Expected ~S but no condition was thrown"
                                 ',condition-class)))
         (,condition-class (,condition-var)
           (declare (ignorable ,condition-var))
           (setf ,caught-var t)
           (suite:pass))
         (condition (,condition-var)
           (when ,caught-var
             (suite:fail '(assert-condition ,condition-class ,@body)
                         (format nil "Expected ~S but got ~S: ~A"
                                 ',condition-class
                                 (type-of ,condition-var) ,condition-var))))))))

;; Re-export fixture functionality

(defmacro fixture (&rest args)
  `(fixture:fixture ,@args))

(defmacro with-fixture (&rest args)
  `(fixture:with-fixture ,@args))

;; Re-export subtest functionality

(defmacro subtest (name &body body)
  "Execute BODY as a named subtest within the current test.
   Subtests can be nested to create hierarchical test structures.

   Example:
   (deftest test-user-operations
     (subtest \"create\"
       (subtest \"with valid data\"
         (assert-true (create-user :name \"Alice\")))
       (subtest \"with invalid data\"
         (assert-condition (validation-error) (create-user)))))"
  `(subtest:subtest ,name ,@body))

;;; Table-Driven Tests

(defun keyword-to-symbol (kw)
  "Convert a keyword to a symbol in the current package.
   :input -> INPUT"
  (if (keywordp kw)
      (intern (symbol-name kw))
      kw))

(defmacro deftest-table (name docstring columns &body rows-and-body)
  "Define a table-driven test where each row becomes a subtest.

   COLUMNS is a list of keywords naming the columns.
   ROWS-AND-BODY contains data rows followed by the test body.
   The last form in ROWS-AND-BODY is the test body; all preceding
   forms are data rows.

   Example:
   (deftest-table test-parse-integer
     \"Parse integer handles various inputs correctly\"
     (:input :expected)
     (\"42\" 42)
     (\"-17\" -17)
     (\"0\" 0)
     (assert-= (parse-integer input) expected))

   Each row runs as a subtest named by the first column value."
  (let* ((rows (butlast rows-and-body))
         (body (car (last rows-and-body)))
         (row-var (gensym "ROW"))
         ;; Convert keywords to symbols for binding
         (var-names (mapcar #'keyword-to-symbol columns))
         (first-var (first var-names)))
    `(deftest ,name
       ,docstring
       (dolist (,row-var ',rows)
         (destructuring-bind ,var-names ,row-var
           (subtest (format nil "~A" ,first-var)
             ,body))))))

(defmacro deftest-cases (name docstring cases &body body)
  "Define a test with named cases, each becoming a subtest.

   Example:
   (deftest-cases test-parse-integer
     \"Parse integer handles various inputs correctly\"
     ((\"positive\"   :input \"42\"    :expected 42)
      (\"negative\"   :input \"-17\"   :expected -17))
     (assert-= (parse-integer input) expected))"
  (let ((expanded-subtests
         (loop for case-spec in cases
               for case-name = (first case-spec)
               for bindings = (rest case-spec)
               collect `(subtest ,case-name
                          (let ,(loop for (key val) on bindings by #'cddr
                                      ;; Keyword symbol-name is just the name without colon
                                      ;; e.g., (symbol-name :input) => "INPUT"
                                      collect `(,(intern (symbol-name key) *package*)
                                                ,val))
                            ,@body)))))
    `(deftest ,name
       ,docstring
       ,@expanded-subtests)))

;;; Before/After Hooks

(defvar *before-all-hooks* map:+empty+
  "Map from package name to list of before-all hook functions")

(defvar *after-all-hooks* map:+empty+
  "Map from package name to list of after-all hook functions")

(defvar *before-each-hooks* map:+empty+
  "Map from package name to list of before-each hook functions")

(defvar *after-each-hooks* map:+empty+
  "Map from package name to list of after-each hook functions")

(defmacro before-all ((&key package) &body body)
  "Register a hook to run once before all tests in PACKAGE.

   Example:
   (before-all (:package :epsilon.database-tests)
     (start-test-database))"
  (let ((pkg-name (or package '*package*)))
    `(setf *before-all-hooks*
           (map:assoc *before-all-hooks* ,pkg-name
                      (cons (lambda () ,@body)
                            (or (map:get *before-all-hooks* ,pkg-name) nil))))))

(defmacro after-all ((&key package) &body body)
  "Register a hook to run once after all tests in PACKAGE.

   Example:
   (after-all (:package :epsilon.database-tests)
     (stop-test-database))"
  (let ((pkg-name (or package '*package*)))
    `(setf *after-all-hooks*
           (map:assoc *after-all-hooks* ,pkg-name
                      (cons (lambda () ,@body)
                            (or (map:get *after-all-hooks* ,pkg-name) nil))))))

(defmacro before-each ((&key package) &body body)
  "Register a hook to run before each test in PACKAGE.

   Example:
   (before-each (:package :epsilon.database-tests)
     (begin-transaction))"
  (let ((pkg-name (or package '*package*)))
    `(setf *before-each-hooks*
           (map:assoc *before-each-hooks* ,pkg-name
                      (cons (lambda () ,@body)
                            (or (map:get *before-each-hooks* ,pkg-name) nil))))))

(defmacro after-each ((&key package) &body body)
  "Register a hook to run after each test in PACKAGE.

   Example:
   (after-each (:package :epsilon.database-tests)
     (rollback-transaction))"
  (let ((pkg-name (or package '*package*)))
    `(setf *after-each-hooks*
           (map:assoc *after-each-hooks* ,pkg-name
                      (cons (lambda () ,@body)
                            (or (map:get *after-each-hooks* ,pkg-name) nil))))))

(defun run-before-all-hooks (package-name)
  "Run all before-all hooks for a package"
  (dolist (hook (reverse (map:get *before-all-hooks* package-name)))
    (funcall hook)))

(defun run-after-all-hooks (package-name)
  "Run all after-all hooks for a package"
  (dolist (hook (reverse (map:get *after-all-hooks* package-name)))
    (funcall hook)))

(defun run-before-each-hooks (package-name)
  "Run all before-each hooks for a package"
  (dolist (hook (reverse (map:get *before-each-hooks* package-name)))
    (funcall hook)))

(defun run-after-each-hooks (package-name)
  "Run all after-each hooks for a package"
  (dolist (hook (reverse (map:get *after-each-hooks* package-name)))
    (funcall hook)))

;;; Initialize Suite Callbacks

(defun setup-suite-callbacks ()
  "Set up callbacks in epsilon.test.suite for hooks and timeouts"
  ;; Set timeout callback
  (setf suite:*get-test-timeout-callback* #'get-test-timeout)
  ;; Set hook callbacks
  (setf suite:*before-all-callback* #'run-before-all-hooks)
  (setf suite:*after-all-callback* #'run-after-all-hooks)
  (setf suite:*before-each-callback* #'run-before-each-hooks)
  (setf suite:*after-each-callback* #'run-after-each-hooks))

;; Initialize callbacks when module loads
(setup-suite-callbacks)

;;; Thread-local Test State Isolation

(defun call-with-fresh-test-state (fn)
  "Call FN with all mutable test globals rebound to fresh values.
   Used by parallel module runners so each worker thread gets isolated state."
  (let (;; Suite-level test registry
        (suite:*test-root* nil)
        (suite:*test-hash-map* map:+empty+)
        (suite:*test-id-map* map:+empty+)
        (suite:*test-categories* map:+empty+)
        (suite:*test* nil)
        ;; Test-level state
        (*test-timeouts* map:+empty+)
        (*before-all-hooks* map:+empty+)
        (*after-all-hooks* map:+empty+)
        (*before-each-hooks* map:+empty+)
        (*after-each-hooks* map:+empty+)
        ;; Keep suite callbacks pointing at the right functions
        (suite:*before-all-callback* #'run-before-all-hooks)
        (suite:*after-all-callback* #'run-after-all-hooks)
        (suite:*before-each-callback* #'run-before-each-hooks)
        (suite:*after-each-callback* #'run-after-each-hooks)
        (suite:*get-test-timeout-callback* #'get-test-timeout))
    (funcall fn)))

;;; Snapshot Testing

(defvar *update-snapshots* nil
  "When T, update snapshot files instead of comparing.
   Set via --update-snapshots command line flag.")

(defun get-current-module-name ()
  "Get the module name for the current test's package."
  (let* ((pkg *package*)
         (pkg-name (package-name pkg)))
    ;; Try to derive module name from package name
    ;; epsilon.json-tests -> epsilon.json
    ;; epsilon.test.improvements-tests -> epsilon.test
    (cond
      ((str:ends-with-p (string-downcase pkg-name) "-tests")
       (subseq pkg-name 0 (- (length pkg-name) 6)))
      ((str:ends-with-p (string-downcase pkg-name) ".tests")
       (subseq pkg-name 0 (- (length pkg-name) 6)))
      (t pkg-name))))

(defmacro snapshot-matches (snapshot-name actual &key (format :lisp) redact)
  "Assert that ACTUAL matches the stored snapshot.

   SNAPSHOT-NAME - String identifying the snapshot (can include subdirs like \"parser/ast\")
   ACTUAL - The value to compare against the snapshot
   FORMAT - :lisp (default), :text, or :json
   REDACT - If T, apply redaction patterns before comparison

   Workflow:
   1. First run: creates snapshot file, test passes with warning
   2. Subsequent runs: compares against snapshot, fails on mismatch
   3. With --update-snapshots: regenerates snapshot file

   Example:
   (deftest test-parser-output
     (let ((ast (parse \"(+ 1 2)\")))
       (snapshot-matches \"parser/simple-add\" ast)))

   (deftest test-json-output
     (snapshot-matches \"api/response\"
                       (json:encode response)
                       :format :json))"
  (let ((module-name-var (gensym "MODULE"))
        (test-name-var (gensym "TEST")))
    `(let ((,module-name-var (get-current-module-name))
           (,test-name-var (format nil "~A:~A"
                                   (package-name *package*)
                                   ,(if (boundp 'suite:*test*)
                                        '(when suite:*test*
                                           (symbol-name (suite:test suite:*test*)))
                                        "unknown"))))
       ;; Sync update flag
       (setf snapshot:*update-snapshots* *update-snapshots*)
       (snapshot:snapshot-matches-impl ,module-name-var
                                       ,snapshot-name
                                       ,actual
                                       :format ,format
                                       :redact ,redact
                                       :test-name ,test-name-var))))

(defun set-update-snapshots (value)
  "Set whether to update snapshots instead of comparing.
   Typically called based on --update-snapshots flag."
  (setf *update-snapshots* value)
  (setf snapshot:*update-snapshots* value))

(defmacro with-redactions (redactions &body body)
  "Execute BODY with snapshot redaction patterns applied.

   REDACTIONS is a list of (pattern replacement) pairs.

   Example:
   (with-redactions ((\"[0-9a-f-]{36}\" \"<UUID>\")
                     (\"\\\\d{4}-\\\\d{2}-\\\\d{2}\" \"<DATE>\"))
     (snapshot-matches \"output\" (generate-report)))"
  `(progn
     (snapshot:clear-redactions)
     ,@(mapcar (lambda (r)
                 `(snapshot:add-redaction ,(first r) ,(second r)))
               redactions)
     (unwind-protect
         (progn ,@body)
       (snapshot:clear-redactions))))

;;; Property-Based Testing

(defmacro deftest-property (name docstring (&key generators (num-tests 100) seed) &body body)
  "Define a property-based test.

   NAME - Test name symbol
   DOCSTRING - Test documentation
   GENERATORS - List of (var generator) bindings
   NUM-TESTS - Number of random tests to run (default 100)
   SEED - Random seed for reproducibility (optional)
   BODY - Test body that should return T for success

   Example:
   (deftest-property test-reverse-involutive
     \"Reversing a list twice yields the original list\"
     (:generators ((xs (gen-list (gen-integer :min 0 :max 100)))))
     (equal (reverse (reverse xs)) xs))

   (deftest-property test-sort-idempotent
     \"Sorting a sorted list yields the same list\"
     (:generators ((xs (gen-list (gen-integer))))
      :num-tests 200)
     (equal (sort (copy-list (sort (copy-list xs) #'<)) #'<)
            (sort (copy-list xs) #'<)))"
  (let* ((gen-vars (mapcar #'first generators))
         (gen-exprs (mapcar #'second generators))
         (property-fn (gensym "PROPERTY"))
         (result-var (gensym "RESULT")))
    `(deftest ,name
       ,docstring
       (let ((,property-fn (lambda ,gen-vars ,@body))
             (gens (list ,@gen-exprs)))
         (let ((,result-var (property:run-property ,property-fn gens
                                                   :num-tests ,num-tests
                                                   :seed ,seed)))
           (unless (property:property-success-p ,result-var)
             (error "~A" (property:format-property-failure ,result-var))))))))

;; Re-export commonly used generators for convenience
(defun gen-integer (&key (min most-negative-fixnum) (max most-positive-fixnum))
  "Generate random integers in range [MIN, MAX]."
  (property:gen-integer :min min :max max))

(defun gen-natural ()
  "Generate non-negative integers."
  (property:gen-natural))

(defun gen-positive ()
  "Generate positive integers (>= 1)."
  (property:gen-positive))

(defun gen-boolean ()
  "Generate random booleans."
  (property:gen-boolean))

(defun gen-char (&key (min 0) (max 127))
  "Generate random characters in code range [MIN, MAX]."
  (property:gen-char :min min :max max))

(defun gen-ascii-char ()
  "Generate printable ASCII characters."
  (property:gen-ascii-char))

(defun gen-string (&key (max-length 100))
  "Generate random strings up to MAX-LENGTH."
  (property:gen-string :max-length max-length))

(defun gen-ascii-string (&key (max-length 100))
  "Generate ASCII strings up to MAX-LENGTH."
  (property:gen-ascii-string :max-length max-length))

(defun gen-list (element-gen &key (max-length 50))
  "Generate lists of elements from ELEMENT-GEN up to MAX-LENGTH."
  (property:gen-list element-gen :max-length max-length))

(defun gen-vector (element-gen &key (max-length 50))
  "Generate vectors of elements from ELEMENT-GEN up to MAX-LENGTH."
  (property:gen-vector element-gen :max-length max-length))

(defun gen-one-of (&rest generators)
  "Generate a value from one of the given generators."
  (apply #'property:gen-one-of generators))

(defun gen-element (elements)
  "Generate one of the given ELEMENTS."
  (property:gen-element elements))

(defun gen-such-that (predicate gen &key (max-tries 100))
  "Generate values from GEN that satisfy PREDICATE."
  (property:gen-such-that predicate gen :max-tries max-tries))

(defun gen-tuple (&rest generators)
  "Generate a list of values, one from each generator."
  (apply #'property:gen-tuple generators))

(defun gen-constant (value)
  "Always generate the same VALUE."
  (property:gen-constant value))

;;; Mocking and Stubbing

(defmacro with-stub ((fn-spec replacement) &body body)
  "Execute BODY with FN-SPEC stubbed to REPLACEMENT.

   Example:
   (with-stub (http:get (returns '(:status 200 :body \"{}\")))
     (let ((result (fetch-user-data \"alice\")))
       (assert-true result)))"
  `(mock:with-stub (,fn-spec ,replacement) ,@body))

(defmacro with-stubs (stubs &body body)
  "Execute BODY with multiple stubs.

   Example:
   (with-stubs ((http:get (returns '(:status 200)))
                (db:query (returns '())))
     (process-request))"
  `(mock:with-stubs ,stubs ,@body))

(defmacro with-mock ((fn-spec &key returns times with-args) &body body)
  "Execute BODY with FN-SPEC mocked and verify expectations.

   Example:
   (with-mock (send-email :times 1
                          :with-args (\"alice@test.com\" :any :any)
                          :returns t)
     (register-user \"alice\" \"alice@test.com\"))"
  `(mock:with-mock (,fn-spec :returns ,returns
                             :times ,times
                             :with-args ,with-args)
     ,@body))

(defmacro with-spy ((fn-spec calls-var) &body body)
  "Execute BODY with FN-SPEC spied on, binding calls to CALLS-VAR.

   Example:
   (with-spy (log:info calls)
     (process-request)
     (assert-= 2 (length calls)))"
  `(mock:with-spy (,fn-spec ,calls-var) ,@body))

;; Re-export mock utilities
(defun returns (value)
  "Create a stub that always returns VALUE."
  (mock:returns value))

(defun returns-sequence (values)
  "Create a stub that returns VALUES in sequence."
  (mock:returns-sequence values))

(defun raises (condition &rest args)
  "Create a stub that raises CONDITION."
  (apply #'mock:raises condition args))

(defun verify-called (fn-name)
  "Verify FN-NAME was called at least once."
  (mock:verify-called fn-name))

(defun verify-not-called (fn-name)
  "Verify FN-NAME was never called."
  (mock:verify-not-called fn-name))

(defun verify-call-count (fn-name expected)
  "Verify FN-NAME was called EXPECTED times."
  (mock:verify-call-count fn-name expected))

(defun verify-called-with (fn-name &rest expected-args)
  "Verify FN-NAME was called with EXPECTED-ARGS."
  (apply #'mock:verify-called-with fn-name expected-args))

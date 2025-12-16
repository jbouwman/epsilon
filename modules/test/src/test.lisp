;;;; This package provides a test framework with hierarchical test
;;;; organization, metrics collection, and multiple output formats.
;;;; It supports both individual test execution and batch test
;;;; running.
;;;;
;;;; Te use, define tests with (deftest name ...) and run with (run)

(defpackage epsilon.test
  (:use
   cl
   epsilon.symbol)
  (:local-nicknames
   (loader epsilon.loader)
   (map epsilon.map)
   (pkg epsilon.sys.pkg)
   (re epsilon.regex)
   (report epsilon.test.report)
   (suite epsilon.test.suite)
   (fixture epsilon.test.fixture)
   (path epsilon.path)
   (log epsilon.log)
   (seq epsilon.sequence)
   (str epsilon.string))
  (:export

   ;; defining tests
   deftest
   with-label
   module-file
   fixture-path
   data-path

   ;; fixtures
   fixture
   with-fixture

   ;; executing tests
   skip
   run
   run-tests
   parse-test-spec
   success-p
   ;; main removed - test command handled directly by main.lisp

   ;; assertions
   is
   is-p
   is-=
   is-eq
   is-eql
   is-equal
   is-equalp
   is-not
   is-not-null
   is-true
   is-thrown))

(in-package epsilon.test)

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

(defmacro deftest (name &body body)
  (let ((docstring (when (and (stringp (first body))
                              (rest body))
                     (first body)))
        (real-body (if (and (stringp (first body))
                            (rest body))
                       (rest body)
                     body)))
    `(progn
       (defun ,name () 
         ,@(when docstring (list docstring))
         ,@real-body)
       (suite:register-test ',name ,docstring))))

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

(defun run (environment module &key package test-name (format :shell) file)
  "Test an epsilon module.
  
  ENVIRONMENT - The loader environment with repositories configured
  MODULE - Module to test (e.g., 'epsilon.core', 'epsilon.http')
  PACKAGE - Specific package pattern to filter tests (optional)
  TEST-NAME - Specific test name pattern to run (optional) or hash prefix
  FORMAT - Output format: :shell, :verbose, :junit, :tap, etc. (default: :shell)
  FILE - Output file for test report (optional)
  
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
          (unless (member module-name '("epsilon.release" "epsilon.tool.repl") :test #'string=)
            (handler-case
                (progn
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
    (log:debug "Loading tests for module: ~A" module)
    (loader:load-module-resources environment module :tests))
  
  ;; Check if test-name looks like a hash (short hex string)
  (let ((selected-tests 
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
           (suite:select :package package :name test-name))))
    (log:debug "Test selection complete: ~D tests selected" (length selected-tests))
    (when (or package test-name)
      (log:debug "Selected ~D tests matching package: '~A', name: '~A'" 
                 (length selected-tests) package test-name))
    (when (zerop (length selected-tests))
      (log:warn "No tests found!")
      (format t "~&;;; Warning: No tests found!~%"))
    (suite:run selected-tests
               (report:make :format format
                            :file file))))

(defun run-tests (environment specs &key verbose)
  "Run tests for multiple test specifications.
  
  ENVIRONMENT - The loader environment with repositories configured
  SPECS - List of test specification strings (e.g., \"module:package:test\")
  VERBOSE - Use verbose output format (default: nil)
  
  Returns T if all tests pass, raises error if any fail."
  (log:debug "run-tests called with environment: ~A, specs: ~A, verbose: ~A" 
             environment specs verbose)
  (dolist (spec specs)
    (multiple-value-bind (module package-filter name-filter)
        (parse-test-spec spec)
      (log:debug "Parsed test spec ~A -> module: ~A, package: ~A, name: ~A"
                 spec module package-filter name-filter)
      ;; Load the module if specified
      (when module
        (loader:load-module environment module))
      ;; Run tests with filters
      (let* ((format (if verbose :verbose :shell))
             (result (run environment module 
                         :package package-filter
                         :test-name name-filter
                         :format format)))
        (log:debug "EPSILON.TEST:RUN returned: ~A" result)
        ;; Check if tests passed
        (log:debug "Calling SUCCESS-P with result: ~A" result)
        (unless (success-p result)
          (error "Tests failed for ~A" spec)))
      (format t "Successfully tested ~A~%" spec))))

(defun success-p (run)
  "Return T if test RUN completed successfully with no failures.
   Example: (success-p test-result) => T"
  (zerop (+ (length (suite:failures run))
            (length (suite:errors run)))))

(defun skip (&optional (message "Test skipped"))
  "Skip the current test with an optional MESSAGE.
   Example: (skip \"Feature not supported on this platform\")"
  (signal 'suite:skip :message message))

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

(defmacro is-p (predicate actual expected &optional (message nil message-p) &rest message-args)
  "Test that (PREDICATE ACTUAL EXPECTED) is true."
  (with-gensyms (actual-var expected-var)
		`(let ((,actual-var ,actual)
		       (,expected-var ,expected))
		   (if (funcall ,predicate ,actual-var ,expected-var)
		       (suite:pass)
		     (suite:fail '(,predicate ,actual ,expected)
				 ,(if message-p
				      `(format nil ,message ,@message-args)
				    `(format nil "~A failed: expected ~S but got ~S~@[: ~A~]"
					     ,predicate ,expected-var ,actual-var
					     ,(when message-p
						`(format nil ,message ,@message-args)))))))))

(defmacro is (actual &rest optargs)
  `(is-p #'eq (not (null ,actual)) t ,@optargs))

(defmacro is-= (actual expected &rest optargs)
  `(is-p #'= ,actual ,expected ,@optargs))

(defmacro is-eq (actual expected &rest optargs)
  `(is-p #'eq ,actual ,expected ,@optargs))

(defmacro is-eql (actual expected &rest optargs)
  `(is-p #'eql ,actual ,expected ,@optargs))

(defmacro is-equal (actual expected &rest optargs)
  `(is-p #'equal ,actual ,expected ,@optargs))

(defmacro is-equalp (actual expected &rest optargs)
  `(is-p #'equalp ,actual ,expected ,@optargs))

(defmacro is-not (form &rest optargs)
  `(is-p #'eq (not ,form) t ,@optargs))

(defmacro is-true (form &rest optargs)
  "Test that form evaluates to exactly T (not just truthy)"
  `(is-p #'eq ,form t ,@optargs))

(defmacro is-not-null (form &rest optargs)
  "Test that form does not evaluate to NIL"
  `(is-p (lambda (x y) (declare (ignore y)) (not (null x))) ,form t ,@optargs))

(defmacro is-thrown ((condition-class &optional regex) &body body)
  "Test that BODY throws a condition of type CONDITION-CLASS.
If REGEX is provided, the condition's printed representation must match it."
  (with-gensyms (condition-var caught-var)
		`(let ((,caught-var nil))
		   (declare (ignorable ,caught-var))
		   (handler-case
		       (progn
			 ,@body
			 (suite:fail '(is-thrown ,condition-class ,@(when regex `(,regex)) ,@body)
				     ,(if regex
					  `(format nil "Expected ~S matching ~S but no condition was thrown"
						   ',condition-class ,regex)
					`(format nil "Expected ~S but no condition was thrown"
						 ',condition-class))))
		     (,condition-class (,condition-var)
				       (declare (ignorable ,condition-var))
				       (setf ,caught-var t)
				       ,(if regex
					    `(let ((condition-string (format nil "~A" ,condition-var)))
					       (if (re:match ,regex condition-string)
						   (suite:pass)
						 (suite:fail '(is-thrown ,condition-class ,regex ,@body)
							     (format nil "Expected ~S matching ~S but got: ~A"
								     ',condition-class ,regex condition-string))))
					  `(suite:pass)))
		     (condition (,condition-var)
				(when ,caught-var
				  (suite:fail '(is-thrown ,condition-class ,@(when regex `(,regex)) ,@body)
					      ,(if regex
						   `(format nil "Expected ~S matching ~S but got ~S: ~A"
							    ',condition-class ,regex
							    (type-of ,condition-var) ,condition-var)
						 `(format nil "Expected ~S but got ~S: ~A"
							  ',condition-class
							  (type-of ,condition-var) ,condition-var)))))))))

;; Re-export fixture functionality

(defmacro fixture (&rest args)
  `(fixture:fixture ,@args))

(defmacro with-fixture (&rest args)
  `(fixture:with-fixture ,@args))

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
   (log epsilon.log))
  (:export

   ;; defining tests
   deftest
   with-label
   project-file
   
   ;; fixtures
   fixture
   with-fixture

   ;; executing tests
   skip
   run
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

;; TODO project-file is here in order to load package-relative resources: non-source files that contain test data, metadata, configuration and so on. Ideally there should be a generic scheme for this that uses the package's load time or runtime environment.

;; FIXME rename to module-file

(defun project-file (project-name relative-path)
  (let ((module (loader:get-module (loader:environment) project-name :error-p t)))
    (path:string-path-join (path::path-from-uri (loader::package-uri module)) relative-path)))

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

(defun run (environment module &key test-name (format :shell) file)
  "Test an epsilon module.
  
  ENVIRONMENT - The loader environment with repositories configured
  MODULE - Module to test (e.g., 'epsilon.core', 'epsilon.http')
  TEST-NAME - Specific test name pattern to run (optional)
  FORMAT - Output format: :repl, :shell, or :junit (default: :shell)
  FILE - Output file for test report (optional)
  
  Returns the test run result object."
  
  ;; Load test files for the module using generic resource loader
  (when module
    (log:debug "Loading tests for module: ~A" module)
    (loader:load-module-resources environment module :tests))
  
  ;; Select tests based on test-name pattern if provided
  (let ((selected-tests (if test-name
                            (suite:select :name test-name)
                            (suite:select))))
    (log:debug "Test selection complete: ~D tests selected" (length selected-tests))
    (when test-name
      (log:debug "Selected ~D tests matching name '~A'" 
                (length selected-tests) test-name))
    (when (zerop (length selected-tests))
      (log:warn "No tests found!")
      (format t "~&;;; Warning: No tests found!~%"))
    (suite:run selected-tests
               (report:make :format format
                            :file file))))

(defun success-p (run)
  (zerop (+ (length (suite:failures run))
            (length (suite:errors run)))))

(defun skip (&optional (message "Test skipped"))
  "Skip the current test with an optional message"
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
  `(is-p (lambda (x) (not (null x))) ,form t ,@optargs))

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

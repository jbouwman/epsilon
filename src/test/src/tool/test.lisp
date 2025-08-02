;;;; This module provides a test framework with hierarchical test
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
   (build epsilon.tool.build)
   (map epsilon.map)
   (pkg epsilon.sys.pkg)
   (re epsilon.regex)
   (report epsilon.test.report)
   (suite epsilon.test.suite)
   (fixture epsilon.test.fixture)
   (path epsilon.path))
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
   run-all
   success-p
   main  ; Added for dev.lisp dispatcher

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
   is-thrown))

(in-package epsilon.test)

(defun project-file (project-name relative-path)
  (let ((package (build:get-package project-name :error-p t)))
    (path:string-path-join (path::path-from-uri (build::package-uri package)) relative-path)))

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

;;; TODO rmeove 'module' argument -- 'package' here is 'epsilon
;;; package'. Rename the 'package' argument to 'name' and use it to
;;; apply a regexp.

(defun run (&key package name module (format :shell) (file nil))
  "Run tests, optionally filtered by name, package, or module.

MODULE - Module name to test (e.g., 'epsilon.core', 'epsilon.http'). 
         Runs tests for packages belonging to that module.
PACKAGE - Specific package name to test.
NAME - Specific test name to run.
FORMAT may be one of repl, shell, junit.
If FILE is provided, write the report to the named file."
  ;; If a module is specified, ensure it and its tests are loaded
  (when module
    (format t "~&;;; Building module: ~A~%" module)
    (build:build module)  ; Build the module itself (without tests)
    (format t "~&;;; Loading module: ~A~%" module)
    (build:load-package module)  ; Load the package and its dependencies
    (format t "~&;;; Loading tests for module: ~A~%" module)
    (build:build-tests module))  ; Build and load just the tests
  
  ;; Only apply selectors if explicitly provided
  (let ((selected-tests (if (or package name)
                            ;; Explicit selectors provided - filter tests
                            (suite:select :package package :name name)
                            ;; No selectors - run all loaded tests
                            (suite:select))))
    (if (or package name)
        (format t "~&;;; Selected ~D tests~@[ for package pattern '~A'~]~@[ matching name '~A'~]~%" 
                (length selected-tests) package name)
        (format t "~&;;; Running all ~D loaded tests~%" (length selected-tests)))
    (when (zerop (length selected-tests))
      (format t "~&;;; Warning: No tests found!~%")
      (let ((available (suite:list-available-packages)))
        (when available
          (format t "~&;;; Available test packages: ~{~A~^, ~}~%" available))))
    (suite:run selected-tests
               (report:make :format format
                            :file file))))

(defun success-p (run)
  (zerop (+ (length (suite:failures run))
            (length (suite:errors run)))))

(defun run-all (&key (format :shell) (file nil) include-platform)
  "Run tests for all registered modules.
  
  FORMAT - Output format: :shell, :junit, :repl
  FILE - If provided, write the report to the named file
  INCLUDE-PLATFORM - Also test platform-specific modules (default NIL)"
  
  ;; Get list of all modules to test
  (let* ((all-modules (build:list-available-packages :include-platform include-platform))
         (total-failures 0)
         (total-errors 0)
         (total-tests 0)
         (module-results '()))
    
    (format t "~&;;; Running tests for all modules (~D total)~%" (length all-modules))
    
    ;; Test each module
    (dolist (module all-modules)
      (format t "~&~%;;; ======================================~%")
      (format t ";;; Testing module: ~A~%" module)
      (format t ";;; ======================================~%")
      
      (handler-case
          (let ((result (run :module module :format format :file file)))
            (push (cons module result) module-results)
            (when result
              (incf total-failures (length (suite:failures result)))
              (incf total-errors (length (suite:errors result)))
              (incf total-tests (suite:test-count result))))
        (error (e)
          (format t "~&;;; ERROR testing ~A: ~A~%" module e)
          (push (cons module :error) module-results))))
    
    ;; Summary
    (format t "~&~%;;; ======================================~%")
    (format t ";;; Test Summary~%")
    (format t ";;; ======================================~%")
    (format t ";;; Total modules tested: ~D~%" (length all-modules))
    (format t ";;; Total tests run: ~D~%" total-tests)
    (format t ";;; Total failures: ~D~%" total-failures)
    (format t ";;; Total errors: ~D~%" total-errors)
    
    ;; Show module-by-module results
    (format t "~&~%;;; Module Results:~%")
    (dolist (result (nreverse module-results))
      (let ((module (car result))
            (run-result (cdr result)))
        (cond
          ((eq run-result :error)
           (format t ";;;   ~A: ERROR~%" module))
          ((null run-result)
           (format t ";;;   ~A: NO TESTS~%" module))
          (t
           (let ((failures (length (suite:failures run-result)))
                 (errors (length (suite:errors run-result)))
                 (tests (suite:test-count run-result)))
             (format t ";;;   ~A: ~D tests, ~D failures, ~D errors~%" 
                     module tests failures errors))))))
    
    (format t ";;; ======================================~%")
    
    ;; Return success status
    (and (zerop total-failures) (zerop total-errors))))

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

(defun main (parsed-args)
  "Main entry point for test command from dev.lisp dispatcher"
  (let* ((args (funcall (read-from-string "epsilon.tool.dev::parsed-args-arguments") parsed-args))
         (options (funcall (read-from-string "epsilon.tool.dev::parsed-args-options") parsed-args))
         ;; Parse options - prioritize --package over --module and positional args
         (target-package (or (map:get options "package") 
                             (map:get options "module") 
                             (first args)))
         (name (map:get options "name"))
         (format-str (or (map:get options "format") "shell"))
         (format (cond
                   ((string= format-str "junit") :junit)
                   ((string= format-str "repl") :repl)
                   (t :shell)))
         (file (map:get options "file"))
         (include-platform (map:get options "include-platform")))
    
    ;; Check if 'all' is specified
    (cond
      (target-package
       ;; Run tests for specific package
       (format t "~&;;; Test main called with package: ~A~%" target-package)
       (run :module target-package
            :package nil  ; Don't filter by Lisp package when testing epsilon package
            :name name
            :format format
            :file file))
      (t
       ;; Default to epsilon.core if no package specified
       (run :module "epsilon.core"
            :package nil  ; Don't filter by Lisp package by default
            :name name
            :format format
            :file file)))))

;; Re-export fixture functionality
;; Note: directly exporting the symbols from the fixture package
(defmacro fixture (&rest args)
  `(fixture:fixture ,@args))

(defmacro with-fixture (&rest args)
  `(fixture:with-fixture ,@args))

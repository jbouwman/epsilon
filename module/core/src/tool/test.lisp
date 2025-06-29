(defpackage epsilon.tool.test
  (:use
   cl
   epsilon.lib.symbol)
  (:local-nicknames
   (build epsilon.tool.build)
   (map epsilon.lib.map)
   (pkg epsilon.sys.pkg)
   (re epsilon.lib.regex)
   (report epsilon.tool.test.report)
   (suite epsilon.tool.test.suite)
   (uri epsilon.lib.uri))
  (:export
   deftest
   skip
   run
   project-file
   success-p

   ;; assertions
   is
   is-p
   is-=
   is-eq
   is-eql
   is-equal
   is-equalp
   is-not-null
   is-thrown))

(in-package epsilon.tool.test)

(defun project-file (project-name relative-path)
  (uri:path (uri:merge (or (map:get build::*modules* project-name)
                           (error "unknown project ~s" project-name))
                       relative-path)))

(defmacro deftest (name &body body)
  `(progn
     (defun ,name () ,@body)
     (suite:register-test ',name)))

(defun run (&key package name module (format :shell) (file nil))
  "Run tests, optionally filtered by name, package, or module.

MODULE - Module name to test (e.g., 'epsilon.core', 'epsilon.http'). 
         Runs tests for packages belonging to that module.
PACKAGE - Specific package name to test.
NAME - Specific test name to run.
FORMAT may be one of repl, shell, junit.
If FILE is provided, write the report to the named file."
  (let ((target-package (or package
                            (when module
                              ;; Convert module name to package pattern
                              ;; e.g., "epsilon.core" -> filter packages starting with "epsilon."
                              (format nil "~A.*" module)))))
    (suite:run (suite:select :package target-package
                             :name name)
               (report:make :format format
                            :file file))))

(defun success-p (run)
  (zerop (+ (length (suite:failures run))
            (length (suite:errors run)))))

(defun skip (&optional (message "Test skipped"))
  "Skip the current test with an optional message"
  (signal 'suite:skip :message message))

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

(defmacro is-thrown ((condition-class &optional regex) &body body)
  "Test that BODY throws a condition of type CONDITION-CLASS.
If REGEX is provided, the condition's printed representation must match it."
  (with-gensyms (condition-var caught-var)
    `(let ((,caught-var nil))
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

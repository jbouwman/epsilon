(defpackage epsilon.tool.test-cmd
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (argparse epsilon.argparse)
   (build epsilon.tool.build)
   (build-env epsilon.tool.build-environment)
   (proto epsilon.build.protocol)
   (main epsilon.main)
   (cmd epsilon.tool.package-command)))

(in-package epsilon.tool.test-cmd)

(defclass test (cmd:package-command)
  ())

(defun run-tests (module &key test-name)
  "Run tests for the given module, optionally a specific test"
  ;; Ensure epsilon.test is built and loaded
  (unless (find-package "EPSILON.TEST")
    (handler-case
        (progn
          (build:build "epsilon.test")
          (build:load-package "epsilon.test"))
      (error (e)
        (error "Failed to load epsilon.test package: ~A~%~
                The test framework requires epsilon.regex and epsilon.xml packages ~
                which are not part of the base epsilon runtime." e))))
  
  ;; Call epsilon.test:run with the module
  (let ((test-pkg (find-package "EPSILON.TEST")))
    (if test-pkg
        (let ((run-fn (find-symbol "RUN" test-pkg)))
          (if run-fn
              (if test-name
                  (funcall run-fn :name test-name :module module)
                  (funcall run-fn :module module))
              (error "EPSILON.TEST:RUN not found")))
        (error "epsilon.test package not found after loading"))))

(defmethod main:run-command ((command test) parsed-args)
  (handler-case
      (let* ((env (build:make-build-environment))
             (options (argparse:parsed-options parsed-args))
             (repos (map:get options "package_repo"))
             (parallel (map:get options "parallel"))
             (test-name (map:get options "test")))
        ;; Configure environment
        (dolist (repo repos)
          (build-env:add-package-repo env repo))
        (when parallel
          (setf (build-env:option-parallel (build-env:environment-options env)) t))
        ;; Use this environment for package resolution and build
        (let* ((build:*current-environment* env)
               (package (cmd:resolve-package command parsed-args)))
          ;; Build the package first
          (build:build-with-environment env package)
          
          ;; Now let the test system handle test files
          ;; The test system will compile and load test files as needed
          (run-tests package :test-name test-name)))
    (simple-error (e)
      (format *error-output* "Error: ~A~%" e)
      (sb-ext:quit :unix-status 1))))

(defmethod main:argument-parser ((command test))
  (let ((parser (argparse:make-parser 
                 :command "test"
                 :description "Run test suites")))
    (cmd:add-package-arguments command parser)
    (argparse:add-argument parser "--test"
                          :help "Run a specific test by name"
                          :metavar "TEST-NAME")
    (argparse:add-argument parser "--parallel"
                          :action 'store-true
                          :help "Use parallel compilation (experimental)")
    parser))

(main:register-command 'test)

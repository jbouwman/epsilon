;;;; Main Command Line Interface
;;;;
;;;; This module serves as the main registration and extension point for
;;;; development tools. It provides a lightweight command dispatcher that
;;;; demand-loads tool modules as needed, keeping the core bootstrap minimal.

(defpackage epsilon.main
  (:use cl)
  (:shadow #:package)  ; Shadow CL:PACKAGE to define our macro
  (:local-nicknames
   (map epsilon.map)
   (str epsilon.string)
   (seq epsilon.sequence)
   (loader epsilon.loader)
   (path epsilon.path)
   (argparse epsilon.argparse)
   (log epsilon.log)
   (env epsilon.sys.env)
   (table epsilon.table)
   (common epsilon.tool.common))
  (:export
   #:cli-run
   #:package))

(in-package epsilon.main)

;;; Global variables

(defvar *user-directory* nil
  "Original user directory before epsilon changes to its home directory")

(defun initialize-parser ()
  "Initialize the main argument parser"
  (let ((parser (argparse:make-parser 
                 :command "epsilon"
                 :description "Epsilon development environment")))
    ;; Add global options
    (argparse:add-argument parser "--help"
                           :action 'store-true
                           :help "Show this help message")
    (argparse:add-argument parser "--version"
                           :action 'store-true
                           :help "Show version information")
    (argparse:add-argument parser "--version-json"
                           :action 'store-true
                           :help "Show version information as JSON")
    (argparse:add-argument parser "--debug"
                           :action 'store-true
                           :help "Enable debugger (don't use --disable-debugger)")
    (argparse:add-argument parser "--log"
                           :help "Configure logging (e.g., debug, trace:epsilon.*)")
    ;; Add eval option
    (argparse:add-argument parser "--eval"
                           :nargs '?
                           :default :not-provided
                           :help "Evaluate expression (reads from stdin if no expression given)")
    ;; Add module loading option
    (argparse:add-argument parser "--module"
                           :action 'append
                           :metavar "MODULE"
                           :help "Load specified module before evaluation")
    ;; Add path option for direct module path
    (argparse:add-argument parser "--path"
                           :metavar "PATH"
                           :help "Path to module directory (alternative to --repository)")
    ;; Add module-dir option to scan directories for modules
    (argparse:add-argument parser "--module-dir"
                           :action 'append
                           :metavar "DIRECTORY"
                           :help "Scan directory for modules (can be used multiple times)")
    ;; Add exec option
    (argparse:add-argument parser "--exec"
                           :metavar "PACKAGE:FUNCTION"
                           :help "Execute a package function with remaining args")
    ;; Add modules option  
    (argparse:add-argument parser "--modules"
                           :action 'store-true
                           :help "List available modules")
    ;; Add load option
    (argparse:add-argument parser "--load"
                           :action 'append
                           :metavar "FILE"
                           :help "Load a Lisp file (can be used multiple times)")
    ;; Add run option for scripts with package declarations
    (argparse:add-argument parser "--run"
                           :metavar "FILE"
                           :help "Run a script file (auto-loads dependencies from package declaration)")
    ;; Add test option
    (argparse:add-argument parser "--test"
                           :action 'append
                           :metavar "MODULE[:PACKAGE[:NAME]]"
                           :help "Test specified module, package, or individual test (can be used multiple times)")
    ;; Add list-tests option
    (argparse:add-argument parser "--list-tests"
                           :action 'store-true
                           :help "List all tests with their hash identifiers")
    ;; Add verbose option
    (argparse:add-argument parser "--verbose"
                           :action 'store-true
                           :help "Enable verbose test output")
    ;; Add quiet option
    (argparse:add-argument parser "--quiet"
                           :action 'store-true
                           :help "Suppress warnings and minimize output")
    ;; Add library management options
    (argparse:add-argument parser "--check-libraries"
                           :action 'store-true
                           :help "Check status of all registered libraries")
    (argparse:add-argument parser "--library-info"
                           :metavar "NAME"
                           :help "Show information about a specific library")
    (argparse:add-argument parser "--available-libraries"
                           :action 'store-true
                           :help "List all available libraries")
    (argparse:add-argument parser "--missing-libraries"
                           :action 'store-true
                           :help "List missing critical libraries")
    ;; Development tools
    (argparse:add-argument parser "--init"
                           :action 'store-true
                           :help "Initialize a new project in current directory")
    (argparse:add-argument parser "--new"
                           :metavar "NAME"
                           :help "Create a new module with the given name")
    (argparse:add-argument parser "--doctor"
                           :action 'store-true
                           :help "Run environment diagnostics")
    ;; Module installation options
    (argparse:add-argument parser "--install"
                           :metavar "SPEC"
                           :help "Install module from GitHub (github:owner/repo[@version])")
    (argparse:add-argument parser "--install-manifest"
                           :nargs '?
                           :default :not-provided
                           :help "Install from manifest file (default: epsilon.manifest)")
    (argparse:add-argument parser "--install-checksum"
                           :metavar "CHECKSUM"
                           :help "SHA-256 checksum for --install (sha256:hexdigest)")
    (argparse:add-argument parser "--install-name"
                           :metavar "NAME"
                           :help "Module name for --install (required for URL installs)")
    parser))

;;; Pure Data Transformation Functions

(defun parse-exec-spec (spec-string)
  "Parse a package:function specification into components.
   Returns (values package-name function-name) or (values nil function-name) if no package specified."
  (let ((colon-pos (position #\: spec-string)))
    (if colon-pos
        (values (subseq spec-string 0 colon-pos)
                (subseq spec-string (1+ colon-pos)))
        (values nil spec-string))))

(defun split-args-at-delimiter (args &optional (delimiter "--"))
  "Split argument list at delimiter. Returns (values before-args after-args)."
  (let ((delimiter-pos (position delimiter args :test #'string=)))
    (if delimiter-pos
        (values (subseq args 0 delimiter-pos)
                (subseq args (1+ delimiter-pos)))
        (values args nil))))

(defun get-version-info ()
  "Get version information as a map."
  (map:make-map
   "version" (env:version)
   "sbcl_version" (lisp-implementation-version)
   "os" (string-downcase (symbol-name (env:platform)))
   "architecture" (machine-type)
   "epsilon_home" (or (sb-ext:posix-getenv "EPSILON_HOME") "")
   "lisp_implementation" (lisp-implementation-type)))

(defun format-version-table ()
  "Format version information as a structured table string."
  (let ((info (get-version-info)))
    (format nil "╭────────────────────────────────────────╮~%~
                 │                EPSILON                 │~%~
                 ├────────────────────────────────────────┤~%~
                 │ Version:      ~24A │~%~
                 │ SBCL:         ~24A │~%~
                 │ OS:           ~24A │~%~
                 │ Architecture: ~24A │~%~
                 ╰────────────────────────────────────────╯"
            (map:get info "version")
            (map:get info "sbcl_version")
            (map:get info "os")
            (map:get info "architecture"))))

(defun format-version-json ()
  "Format version information as JSON string."
  (let ((info (get-version-info)))
    ;; Simple JSON formatting without requiring epsilon.json module
    (format nil "{\"version\":\"~A\",\"sbcl_version\":\"~A\",\"os\":\"~A\",\"architecture\":\"~A\",\"epsilon_home\":\"~A\",\"lisp_implementation\":\"~A\"}"
            (map:get info "version")
            (map:get info "sbcl_version")
            (map:get info "os")
            (map:get info "architecture")
            (map:get info "epsilon_home")
            (map:get info "lisp_implementation"))))

(defun parse-main-spec (main-spec)
  "Parse a main specification, handling both string and list forms.
   Returns (values package-name function-name)."
  (cond
    ;; String like 'epsilon.tool.repl:main'
    ((stringp main-spec)
     (parse-exec-spec main-spec))
    ;; List like (epsilon.tool.repl:main \"arg1\" \"arg2\")
    ((and (listp main-spec) (stringp (first main-spec)))
     (parse-exec-spec (first main-spec)))
    ;; List like (epsilon.tool.repl main \"arg1\" \"arg2\")
    ((and (listp main-spec) 
          (stringp (first main-spec))
          (stringp (second main-spec)))
     (values (first main-spec) (second main-spec)))
    ;; Invalid format
    (t (error "Invalid main spec format: ~S" main-spec))))

;;; Environment Setup Functions

(defun setup-build-environment ()
  "Create and configure a build environment.
   Only adds auto-detected repositories (user dir + epsilon src).
   --path arguments are handled separately."
  (let ((env (loader:environment))
        (epsilon-user (sb-ext:posix-getenv "EPSILON_USER")))
    (loader:register-module env epsilon-user)
    env))

(defun process-repository-options (environment parsed-options)
  "Process --repository, --path, and --module-dir options from parsed arguments.
   Updates the environment and returns any errors."
  (let ((module-path (map:get parsed-options "path"))
        (module-dirs (map:get parsed-options "module_dir"))
        (errors nil))
    
    ;; Handle --path option - register single module directory
    (when module-path
      ;; module-path is a string from the command line parser
      ;; Resolve relative paths relative to user's original working directory
      (let* ((is-absolute (or (char= (char module-path 0) #\/)
                              #+win32 (and (>= (length module-path) 3)
                                           (char= (char module-path 1) #\:))))
             (user-dir (sb-ext:posix-getenv "EPSILON_USER"))
             (base-dir (if user-dir (truename user-dir) (truename ".")))
             (resolved-path (if is-absolute
                                (pathname module-path)
                                (merge-pathnames (pathname module-path) base-dir)))
             ;; Normalize the resolved path to handle .. components
             (normalized-path (handler-case 
                                  (truename resolved-path)
                                (error ()
                                  ;; If truename fails, try parsing the path manually
                                  (pathname (namestring resolved-path))))))
        (unless (probe-file normalized-path)
          (push (format nil "Path does not exist: ~A" normalized-path) errors))
        (unless errors
          (loader:register-module environment (namestring normalized-path)))))
    
    ;; Handle --module-dir option - scan directories for modules
    (when module-dirs
      (dolist (dir module-dirs)
        (let* ((is-absolute (or (char= (char dir 0) #\/)
                                #+win32 (and (>= (length dir) 3)
                                             (char= (char dir 1) #\:))))
               (user-dir (sb-ext:posix-getenv "EPSILON_USER"))
               (base-dir (if user-dir (truename user-dir) (truename ".")))
               (resolved-dir (if is-absolute
                                 (pathname dir)
                                 (merge-pathnames (pathname dir) base-dir)))
               ;; Normalize the resolved path to handle .. components
               (normalized-dir (handler-case 
                                   (truename resolved-dir)
                                 (error ()
                                   ;; If truename fails, try parsing the path manually
                                   (pathname (namestring resolved-dir))))))
          (unless (probe-file normalized-dir)
            (push (format nil "Module directory does not exist: ~A" normalized-dir) errors))
          (unless errors
            (loader:scan-module-directory environment (namestring normalized-dir))))))
    
    errors))

(defun load-modules (environment modules)
  "Load multiple modules with error handling."
  (dolist (module modules)
    (loader:load-module environment module)))

(defun load-files (files)
  "Load multiple Lisp files."
  (dolist (file files)
    (handler-case
        (progn
          (format *error-output* "Loading file ~A...~%" file)
          (load file)
          (format *error-output* "Successfully loaded ~A~%" file))
      (error (e)
        (format *error-output* "Error loading file ~A: ~A~%" file e)
        (sb-ext:exit :code 1)))))

(defun test-modules (environment specs &key verbose)
  "Test multiple modules with result collection by delegating to epsilon.test."
  (log:debug "test-modules called with environment: ~A, specs: ~A, verbose: ~A" 
             environment specs verbose)
  (loader:load-module environment "epsilon.test")
  (let ((run-tests-fn (find-symbol "RUN-TESTS" (find-package "EPSILON.TEST"))))
    (unless run-tests-fn
      (error "EPSILON.TEST:RUN-TESTS not found"))
    (funcall run-tests-fn environment specs :verbose verbose)))

;;; Command Execution Functions

(defun execute-evaluation (environment expression &key modules)
  "Execute a Lisp expression with proper module loading."
  (when modules
    (load-modules environment modules))
  
  ;; Read and evaluate the expression
  (let ((form (if (stringp expression)
                  (read-from-string expression)
                  expression)))
    (eval form)))

(defun execute-package-function (package-spec function-spec args)
  "Execute a specific function from a package with given arguments."
  (let* ((package (if package-spec
                      (find-package (string-upcase package-spec))
                      *package*))
         (symbol (when package
                   (find-symbol (string-upcase function-spec) package))))
    
    (unless package
      (error "Package ~A not found.~%Make sure to load it with --module or that it's exported by a loaded module." 
             package-spec))
    
    (unless symbol
      (error "Function ~A not found in package ~A.~%Available exports: ~{~A~^, ~}" 
             function-spec (package-name package)
             (let ((exports '()))
               (do-external-symbols (s package exports)
                 (push (symbol-name s) exports)))))
    
    (unless (fboundp symbol)
      (error "Symbol ~A is not a function" symbol))
    
    ;; Call the function with args
    (log:info "Executing ~A:~A with args: ~S" 
              (or package-spec (package-name *package*)) 
              function-spec args)
    (apply symbol args)))

(defun remove-exec-args (args)
  "Remove --exec and its associated arguments from the args list."
  (let ((filtered-args '())
        (i 0))
    (loop while (< i (length args))
          for arg = (nth i args)
          do (cond
               ((string= arg "--exec")
                ;; Skip --exec and its spec
                (when (< (1+ i) (length args))
                  (incf i 2)) ; Skip --exec and spec
                ;; Skip following --key value pairs
                (loop while (and (< i (length args))
                                 (>= (length (nth i args)) 2)
                                 (string= "--" (subseq (nth i args) 0 2))
                                 (not (member (nth i args) 
                                              '("--load" "--eval" "--exec" "--module" "--test")
                                              :test #'string=)))
                      do (incf i 2))) ; Skip --key value pairs
               (t 
                (push arg filtered-args)
                (incf i))))
    (reverse filtered-args)))

(defun collect-ordered-actions (args)
  "Collect --load, --eval, and --exec actions in the order they appear."
  (let ((actions '())
        (i 0))
    (loop while (< i (length args))
          for arg = (nth i args)
          do (cond
               ((string= arg "--load")
                (when (< (1+ i) (length args))
                  (push (list :load (nth (1+ i) args)) actions)
                  (incf i 2)))
               ((string= arg "--eval")
                (when (< (1+ i) (length args))
                  (push (list :eval (nth (1+ i) args)) actions)
                  (incf i 2)))
               ((string= arg "--exec")
                (when (< (1+ i) (length args))
                  ;; Collect exec spec and following keyword arguments
                  (let ((exec-spec (nth (1+ i) args))
                        (keyword-args '())
                        (j (+ i 2)))
                    ;; Collect --key value pairs after exec spec
                    (loop while (and (< j (length args))
                                     (>= (length (nth j args)) 2)
                                     (string= "--" (subseq (nth j args) 0 2))
                                     ;; Stop at other known flags
                                     (not (member (nth j args) 
                                                  '("--load" "--eval" "--exec" "--module" "--test")
                                                  :test #'string=)))
                          do (when (< (1+ j) (length args))
                               (let ((key (subseq (nth j args) 2))
                                     (value (nth (1+ j) args)))
                                 (push (intern (string-upcase key) :keyword) keyword-args)
                                 (push value keyword-args))
                               (incf j 2)))
                    (push (list :exec exec-spec (reverse keyword-args)) actions)
                    (setf i j))))
               (t (incf i))))
    (reverse actions)))

(defun process-ordered-actions (environment actions passthrough-args)
  "Process --load, --eval, and --exec actions in order."
  (dolist (action actions)
    (case (first action)
      (:load
       (load (second action)))
      (:eval
       (let ((result (execute-evaluation environment (second action) :modules nil)))
         (unless (eq result (values))
           (format t "~A~%" result))))
      (:exec
       (let ((exec-spec (second action))
             (keyword-args (third action)))
         ;; Extract package name and load if needed
         (multiple-value-bind (package-spec function-spec)
             (parse-exec-spec exec-spec)
           (when (and package-spec
                      (>= (length package-spec) 8)
                      (string= "epsilon." package-spec :end2 8))
             (loader:load-module environment package-spec))
           ;; Combine keyword args with passthrough args
           (let ((all-args (append keyword-args passthrough-args)))
             (execute-package-function package-spec function-spec all-args))))))))

(defun get-command-parser (command-name)
  "Get the parser for a command (stub for future use)."
  ;; This is a stub - in the future, commands could be loaded dynamically
  ;; For now, we don't have any registered commands
  (declare (ignore command-name))
  nil)

(defun get-subcommand-parser (command-name subcommand-name)
  "Get the parser for a subcommand (stub for future use)."
  (declare (ignore command-name subcommand-name))
  nil)

(defun show-help-and-exit (parser &optional (exit-code 0))
  "Display enhanced help with sections and examples."
  (declare (ignore parser))
  (format t "~%Epsilon - A Modern Lisp Environment~%")
  (format t "====================================~%~%")
  (format t "Usage: epsilon [OPTIONS] [COMMAND]~%~%")

  (format t "Getting Started:~%")
  (format t "  epsilon                    Start interactive REPL~%")
  (format t "  epsilon --init             Initialize new project~%")
  (format t "  epsilon --doctor           Check environment setup~%")
  (format t "~%")

  (format t "Evaluation:~%")
  (format t "  --eval EXPR                Evaluate expression and exit~%")
  (format t "  --load FILE                Load Lisp file (repeatable)~%")
  (format t "  --run FILE                 Run script (auto-loads package dependencies)~%")
  (format t "  --exec PKG:FN              Execute function with remaining args~%")
  (format t "~%")

  (format t "Modules:~%")
  (format t "  --module MODULE            Load module before execution (repeatable)~%")
  (format t "  --modules                  List all available modules~%")
  (format t "  --path PATH                Set module search path~%")
  (format t "  --module-dir DIR           Add module directory (repeatable)~%")
  (format t "~%")

  (format t "Testing:~%")
  (format t "  --test MODULE              Run tests for module (repeatable)~%")
  (format t "  --list-tests               List all tests with identifiers~%")
  (format t "  --verbose                  Enable verbose test output~%")
  (format t "~%")

  (format t "Development:~%")
  (format t "  --init                     Initialize new project in current directory~%")
  (format t "  --new NAME                 Create new module from template~%")
  (format t "  --doctor                   Run environment diagnostics~%")
  (format t "~%")

  (format t "Self-Update:~%")
  (format t "  --exec epsilon.update:check        Check for updates~%")
  (format t "  --exec epsilon.update:update       Update to latest version~%")
  (format t "~%")

  (format t "Options:~%")
  (format t "  --version                  Show version information~%")
  (format t "  --version-json             Show version as JSON~%")
  (format t "  --debug                    Enable debugger~%")
  (format t "  --quiet                    Suppress warnings~%")
  (format t "  --log SPEC                 Configure logging (e.g., debug, trace:*)~%")
  (format t "  --help                     Show this help message~%")
  (format t "~%")

  (format t "Examples:~%")
  (format t "  epsilon                              Start REPL~%")
  (format t "  epsilon --eval \"(+ 1 2 3)\"           Evaluate expression~%")
  (format t "  epsilon --module epsilon.json        Load JSON module~%")
  (format t "  epsilon --test epsilon.core          Run core tests~%")
  (format t "  epsilon --load script.lisp           Load and run script~%")
  (format t "  epsilon --run examples/mtls.lisp     Run script with auto-deps~%")
  (format t "  epsilon --exec myapp:main -- args    Run app with arguments~%")
  (format t "~%")

  (format t "Project Setup:~%")
  (format t "  epsilon --init                       Initialize in current dir~%")
  (format t "  epsilon --new myutil                 Create new module~%")
  (format t "  epsilon --new mytool --template cli  Create CLI tool~%")
  (format t "~%")

  (format t "Documentation: https://github.com/jbouwman/epsilon~%")
  (format t "Version: ~A~%" (env:version))
  (sb-ext:exit :code exit-code))

(defun display-modules (environment)
  "Display all modules with their virtual capabilities mixed in."
  (let* ((modules (loader:query-modules environment))
         (module-data '()))
    
    (dolist (module modules)
      (let* ((name (loader:module-name module))
             (metadata (loader:module-metadata module))
             (version (or (getf metadata :version) "?"))
             (platform (getf metadata :platform))
             (status (if (loader:module-loaded-p module) "LOADED" "AVAILABLE"))
             (location (path:path-string (loader:module-location module))))
        
        ;; Add to module list (skip platform-specific unless on that platform)
        (when (or (not platform)
                  (string-equal platform (string-downcase (symbol-name (env:platform)))))
          (push (list name version status location) module-data))))
    
    
    ;; Sort modules by name
    (setf module-data (sort module-data #'string< :key #'first))
    
    ;; Display modules
    (format t "~%Modules~%")
    (format t "=======~%~%")
    (if module-data
        (let ((tbl (table:simple-table
                    (list "NAME" "VERSION" "STATUS" "LOCATION")
                    module-data)))
          (table:print-table tbl))
        (format t "No modules found.~%"))
    
    ;; Summary
    (format t "~%Found ~D module~:P~%" (length module-data))))

(defun list-all-tests (environment)
  "List all available tests with their hash identifiers"
  ;; Load test module
  (handler-case
      (loader:load-module environment "epsilon.test")
    (error (e)
      (format *error-output* "Error loading test module: ~A~%" e)
      (return-from list-all-tests)))
  
  ;; Load all test modules to populate the registry
  (let ((modules (loader:query-modules environment)))
    (dolist (module modules)
      (handler-case
          (let ((module-name (loader:module-name module)))
            ;; Only load test resources for modules that are already loaded
            (when (loader:module-loaded-p module)
              (loader:load-module-resources environment module-name :tests)))
        (error (e)
          ;; Ignore modules without tests or with errors
          (log:debug "Skipping tests for module: ~A" e)
          nil))))
  
  ;; Get the list of tests with hashes
  (let ((list-fn (find-symbol "LIST-ALL-TESTS-WITH-HASHES" 
                              (find-package "EPSILON.TEST.SUITE"))))
    (when list-fn
      (let ((tests (funcall list-fn)))
        (if tests
            (progn
              (format t "~&Test ID   Package                              Test Name~%")
              (format t "--------  -----------------------------------  --------------------------~%")
              (dolist (entry tests)
                (destructuring-bind (hash symbol test-id) entry
                  (declare (ignore test-id))
                  (let* ((package-name (package-name (symbol-package symbol)))
                         (test-name (symbol-name symbol)))
                    (format t "~8A  ~35A  ~A~%" 
                            hash package-name test-name)))))
            (format t "~&No tests registered.~%"))))))

(defun start-interactive-repl ()
  "Start the epsilon interactive REPL."
  (format t "Epsilon REPL~%")
  (format t "Type (quit) or Ctrl+D to exit~%~%")
  (sb-impl::toplevel-init))

(defun process-test-option (environment parsed-args)
  "Process --test and --list-tests options from parsed arguments.
   Returns T if tests were run and should exit."
  ;; Handle --list-tests
  (when (map:get (argparse:parsed-options parsed-args) "list_tests")
    (handler-case
        (progn
          (list-all-tests environment)
          (return-from process-test-option t))
      (error (e)
        (format *error-output* "Error listing tests: ~A~%" e)
        (return-from process-test-option t))))
  
  ;; Handle --test
  (let ((modules-to-test (map:get (argparse:parsed-options parsed-args) "test"))
        (verbose (map:get (argparse:parsed-options parsed-args) "verbose")))
    (when modules-to-test
      (test-modules environment modules-to-test :verbose verbose)
      ;; Check if we should exit
      (let ((has-other-actions 
             (or (and (map:get (argparse:parsed-options parsed-args) "eval")
                      (not (eq (map:get (argparse:parsed-options parsed-args) "eval") :not-provided)))
                 (map:get (argparse:parsed-options parsed-args) "exec")
                 (map:get (argparse:parsed-options parsed-args) "load")
                 (argparse:parsed-command parsed-args)
                 (argparse:parsed-positionals parsed-args))))
        (not has-other-actions)))))

(defun evaluate-expression (environment expression-string modules)
  "Evaluate a Lisp expression with optional module loading"
  ;; Load requested modules
  (dolist (module modules)
    (handler-case
        (progn
          (format *error-output* "Loading module ~A...~%" module)
          (loader:load-module environment module))
      (error (e)
        (format *error-output* "Error loading module ~A: ~A~%" module e)
        (sb-ext:exit :code 1))))
  
  ;; Evaluate expression(s)
  (handler-case
      (with-input-from-string (stream expression-string)
        (loop for form = (read stream nil :eof)
              until (eq form :eof)
              do (let* ((result (eval form))
                        (*print-pretty* t))
                   (unless (eq result (values))
                     (format t "~A~%" result)))))
    (end-of-file ()
      (format *error-output* "Error: Incomplete expression~%")
      (sb-ext:exit :code 1))
    (reader-error (e)
      (format *error-output* "Read error: ~A~%" e)
      (sb-ext:exit :code 1))
    (error (e)
      (format *error-output* "Evaluation error: ~A~%" e)
      (sb-ext:exit :code 1))))

;; Module discovery helper
;; Note: This could potentially use epsilon.tool.common if loaded,
;; but main.lisp needs to work without additional dependencies

(defun find-module-directory (module-name)
  "Try to find a module directory by searching common locations"
  (let ((possible-paths (list
                           ;; Current directory
                         (merge-pathnames module-name (sb-posix:getcwd))
                         ;; Parent directory
                         (merge-pathnames (format nil "../~A" module-name) (sb-posix:getcwd))
                         ;; Direct path if module-name is actually a path
                         (when (or (search "/" module-name) (search "\\" module-name))
                           (pathname module-name)))))
    (loop for path in possible-paths
          when (and path 
                    (probe-file path)
                    (probe-file (merge-pathnames "module.lisp" path)))
            return (namestring path))))

;;; Script Running (--run)

(defun read-package-form (filepath)
  "Read the first (package ...) form from a script file.
   Returns the form or NIL if not found."
  (with-open-file (stream filepath :if-does-not-exist :error)
    (let ((*read-eval* nil)) ; Safety: don't evaluate during read
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            when (and (consp form)
                      (symbolp (first form))
                      (string-equal "PACKAGE" (symbol-name (first form))))
              return form))))

(defun extract-package-imports (package-form)
  "Extract import specs from a (package name (import ...)) form.
   Returns a list of (package-name nickname) pairs."
  (let ((imports-clause (find-if (lambda (clause)
                                   (and (consp clause)
                                        (symbolp (first clause))
                                        (string-equal "IMPORT" (symbol-name (first clause)))))
                                 (cddr package-form))))
    (when imports-clause
      (mapcar (lambda (spec)
                (if (consp spec)
                    (list (string (first spec)) (string (second spec)))
                    (list (string spec) nil)))
              (cdr imports-clause)))))

(defun package-name-to-module-name (package-name)
  "Derive module name from a package name.
   For epsilon.http.simple -> epsilon.http
   For epsilon.crypto.certificates -> epsilon.crypto
   For epsilon.path -> epsilon.path (resolved later to epsilon.core if needed)"
  (let* ((name-lower (string-downcase package-name))
         (parts (seq:realize (str:split #\. name-lower)))
         (num-parts (length parts)))
    (cond
      ;; Not an epsilon package - can't auto-resolve
      ((or (< num-parts 2)
           (not (string-equal "epsilon" (first parts))))
       nil)
      ;; epsilon.X -> epsilon.X
      ((= num-parts 2)
       name-lower)
      ;; epsilon.X.Y[.Z...] -> epsilon.X
      (t
       (format nil "~A.~A" (first parts) (second parts))))))

(defun resolve-script-dependencies (environment imports)
  "Given import specs, determine which modules need to be loaded.
   Returns a list of module names to load."
  (let ((modules-to-load '()))
    (dolist (import-spec imports)
      (let* ((package-name (first import-spec))
             (module-name (package-name-to-module-name package-name)))
        (when module-name
          ;; Try to find a module with that name
          (let ((found-module (loader:get-module environment module-name)))
            (cond
              ;; Module exists with derived name
              (found-module
               (pushnew module-name modules-to-load :test #'string=))
              ;; For 2-part names (epsilon.X), try epsilon.core as fallback
              ;; (epsilon.path, epsilon.string, etc. are from epsilon.core)
              ((and (= 2 (length (seq:realize (str:split #\. module-name))))
                    (loader:get-module environment "epsilon.core"))
               (pushnew "epsilon.core" modules-to-load :test #'string=))
              ;; No module found - might be an error, but we'll let loading fail naturally
              (t nil))))))
    (nreverse modules-to-load)))

(defun transform-package-form-to-defpackage (package-form)
  "Transform a (package name (import ...)) form into defpackage + in-package forms."
  (let* ((name (second package-form))
         (imports (extract-package-imports package-form))
         (local-nicknames (mapcar (lambda (spec)
                                    (let ((pkg-name (first spec))
                                          (nickname (second spec)))
                                      (list (intern (string-upcase nickname))
                                            (intern (string-upcase pkg-name) :keyword))))
                                  (remove-if-not #'second imports))))
    `(progn
       (defpackage ,name
         (:use cl)
         ,@(when local-nicknames
             `((:local-nicknames ,@local-nicknames))))
       (in-package ,name))))

;;; Package Macro for Dependency-Driven Compilation
;;;
;;; This macro enables modules outside epsilon.core to use a simpler package
;;; declaration form that automatically loads dependencies:
;;;
;;;   (package my-module
;;;     (import (epsilon.http.simple http)
;;;             (epsilon.crypto certs)))
;;;
;;; At compile/load time, this loads the required modules and expands to
;;; a standard defpackage with local-nicknames.

(defmacro package (name &body clauses)
  "Define a package with automatic dependency loading.

   Usage:
     (package my-package
       (import (epsilon.http.simple http)
               (epsilon.json json)))

   Each import specifies a package and an optional local nickname.
   The system automatically loads the modules that provide these packages
   before defining the package.

   This expands to a standard DEFPACKAGE with :local-nicknames."
  (let* ((form (list* 'package name clauses))
         (imports (extract-package-imports form))
         (module-loads (mapcar (lambda (spec)
                                 (let ((module-name (package-name-to-module-name (first spec))))
                                   (when module-name
                                     `(when (and (boundp 'loader:*environment*)
                                                 loader:*environment*)
                                        (let ((env loader:*environment*))
                                          (unless (loader:module-loaded-p
                                                   (or (loader:get-module env ,module-name)
                                                       (loader:get-module env "epsilon.core")))
                                            (loader:load-module env
                                              (if (loader:get-module env ,module-name)
                                                  ,module-name
                                                  "epsilon.core"))))))))
                               imports))
         (local-nicknames (mapcar (lambda (spec)
                                    (let ((pkg-name (first spec))
                                          (nickname (second spec)))
                                      (when nickname
                                        (list (intern (string-upcase nickname))
                                              (intern (string-upcase pkg-name) :keyword)))))
                                  imports)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,@(remove nil module-loads))
       (defpackage ,name
         (:use cl)
         ,@(when (remove nil local-nicknames)
             `((:local-nicknames ,@(remove nil local-nicknames)))))
       (in-package ,name))))

(defun run-script (environment filepath passthrough-args)
  "Run a script file with automatic dependency loading.
   Parses the (package ...) form, loads required modules, then evaluates the file."
  (let* ((user-dir (sb-ext:posix-getenv "EPSILON_USER"))
         (resolved-path (if (and (> (length filepath) 0)
                                 (char= (char filepath 0) #\/))
                            filepath
                            (if user-dir
                                ;; Ensure user-dir ends with / for proper merge
                                (let ((dir (if (char= (char user-dir (1- (length user-dir))) #\/)
                                               user-dir
                                               (concatenate 'string user-dir "/"))))
                                  (namestring (merge-pathnames filepath dir)))
                                filepath))))
    (unless (probe-file resolved-path)
      (format *error-output* "Error: Script file not found: ~A~%" resolved-path)
      (sb-ext:exit :code 1))

    ;; Read the package form
    (let ((package-form (read-package-form resolved-path)))
      (unless package-form
        (format *error-output* "Error: No (package ...) form found in ~A~%" resolved-path)
        (sb-ext:exit :code 1))

      ;; Extract imports and resolve dependencies
      (let* ((imports (extract-package-imports package-form))
             (modules-to-load (resolve-script-dependencies environment imports)))

        ;; Load required modules
        (dolist (module-name modules-to-load)
          (handler-case
              (progn
                (log:info "Loading module: ~A" module-name)
                (loader:load-module environment module-name))
            (error (e)
              (format *error-output* "Error loading module ~A: ~A~%" module-name e)
              (sb-ext:exit :code 1))))

        ;; Transform and evaluate the package form
        (let ((defpackage-form (transform-package-form-to-defpackage package-form)))
          (eval defpackage-form))

        ;; Load the rest of the file (skip the package form we already processed)
        (with-open-file (stream resolved-path)
          (let ((*read-eval* t))
            ;; Skip forms until we're past the package form
            (loop for form = (read stream nil :eof)
                  until (eq form :eof)
                  unless (and (consp form)
                              (symbolp (first form))
                              (string-equal "PACKAGE" (symbol-name (first form))))
                    do (eval form))))

        ;; Call main function if it exists
        (let* ((package-name (second package-form))
               (package (find-package (string-upcase (string package-name)))))
          (when package
            (let ((main-fn (find-symbol "MAIN" package)))
              (when (and main-fn (fboundp main-fn))
                (log:info "Calling ~A:MAIN" package-name)
                ;; Call with passthrough args if any, otherwise with no args
                (if passthrough-args
                    (apply main-fn passthrough-args)
                    (funcall main-fn))))))))))

(defun run (args)
  "Dispatch to the appropriate command handler"
  (let* ((arg-parser (initialize-parser))
         (environment (setup-build-environment)))
    ;; Set global REPL environment for backward compatibility
    (setf loader:*environment* environment)
    ;; Split args at "--" to separate epsilon args from passthrough args
    (multiple-value-bind (epsilon-args passthrough-args)
        (split-args-at-delimiter args)
      ;; Extract exec actions first to avoid argparse conflicts with keyword args
      (let ((ordered-actions (collect-ordered-actions epsilon-args))
            (exec-args '()))
        ;; Remove exec-related args from epsilon-args for clean argparse parsing
        (dolist (action ordered-actions)
          (when (eq (first action) :exec)
            (setf exec-args (append exec-args (list action)))))
        ;; Filter out exec and related args for argparse
        (let* ((filtered-args (remove-exec-args epsilon-args))
               (parsed-args (argparse:parse-args arg-parser filtered-args)))
        ;; Check global options
        (when (map:get (argparse:parsed-options parsed-args) "quiet")
          ;; Set minimal logging and suppress warnings
          (map:assoc! (loader:environment-config environment) :warning-behavior :muffle)
          (log:configure-from-string "warn"))
        
        (when (map:get (argparse:parsed-options parsed-args) "log")
          (log:configure-from-string (map:get (argparse:parsed-options parsed-args) "log")))
            
        ;; Handle --help
        (when (map:get (argparse:parsed-options parsed-args) "help")
          (show-help-and-exit arg-parser))
            
        ;; Handle --version and --version-json
        (when (map:get (argparse:parsed-options parsed-args) "version_json")
          (format t "~A~%" (format-version-json))
          (sb-ext:exit :code 0))
        (when (map:get (argparse:parsed-options parsed-args) "version")
          (format t "~A~%" (format-version-table))
          (sb-ext:exit :code 0))
            
        ;; Process --path and --repository flags to set up environment
        (let ((errors (process-repository-options environment 
                                                  (argparse:parsed-options parsed-args))))
          (when errors
            (dolist (error errors)
              (format *error-output* "Error: ~A~%" error))
            (sb-ext:exit :code 1)))
            
        ;; Process --module flags to load specific modules
        (let ((modules-to-load (map:get (argparse:parsed-options parsed-args) "module")))
          (when modules-to-load
            (load-modules environment modules-to-load)))
            
        ;; STEP 3: Process --test flag separately (not part of action sequence)
        (when (process-test-option environment parsed-args)
          (sb-ext:exit :code 0))
            
        ;; Handle --modules (can be processed anytime after environment setup)
        (when (map:get (argparse:parsed-options parsed-args) "modules")
          (display-modules environment)
          (sb-ext:exit :code 0))
        
        ;; Handle library diagnostic commands
        (when (map:get (argparse:parsed-options parsed-args) "check_libraries")
          (handler-case
              (progn
                (loader:load-module environment "epsilon.library")
                (let ((check-fn (find-symbol "CHECK-LIBRARIES" (find-package "EPSILON.LIBRARY"))))
                  (when check-fn
                    (let ((results (funcall check-fn)))
                      (format t "~&Library Status~%")
                      (format t "==============~%~%")
                      (loop for (name . status) in (epsilon.map:seq results)
                            do (format t "~20A: ~A~%" name (first status)))))))
            (error (e)
              (format *error-output* "Error checking libraries: ~A~%" e)))
          (sb-ext:exit :code 0))
        
        (when (map:get (argparse:parsed-options parsed-args) "library_info")
          (handler-case
              (progn
                (loader:load-module environment "epsilon.library")
                (let* ((lib-name (map:get (argparse:parsed-options parsed-args) "library_info"))
                       (info-fn (find-symbol "LIBRARY-INFO" (find-package "EPSILON.LIBRARY"))))
                  (when info-fn
                    (let ((info (funcall info-fn (intern (string-upcase lib-name) :keyword))))
                      (if info
                          (progn
                            (format t "~&Library: ~A~%" (getf info :name))
                            (format t "Base name: ~A~%" (getf info :base-name))
                            (format t "Version: ~A~%" (or (getf info :version) "unspecified"))
                            (format t "Bundled: ~A~%" (if (getf info :bundled-p) "yes" "no"))
                            (format t "Critical: ~A~%" (if (getf info :critical-p) "yes" "no"))
                            (format t "Platforms: ~A~%" (getf info :platforms))
                            (format t "Description: ~A~%" (getf info :description))
                            (format t "Path: ~A~%" (or (getf info :path) "not found")))
                          (format t "Library ~A not registered~%" lib-name))))))
            (error (e)
              (format *error-output* "Error getting library info: ~A~%" e)))
          (sb-ext:exit :code 0))
        
        (when (map:get (argparse:parsed-options parsed-args) "available_libraries")
          (handler-case
              (progn
                (loader:load-module environment "epsilon.library")
                (let ((avail-fn (find-symbol "AVAILABLE-LIBRARIES" (find-package "EPSILON.LIBRARY"))))
                  (when avail-fn
                    (let ((libs (funcall avail-fn)))
                      (format t "~&Available Libraries~%")
                      (format t "==================~%")
                      (dolist (lib libs)
                        (format t "  ~A~%" lib))))))
            (error (e)
              (format *error-output* "Error listing available libraries: ~A~%" e)))
          (sb-ext:exit :code 0))
        
        (when (map:get (argparse:parsed-options parsed-args) "missing_libraries")
          (handler-case
              (progn
                (loader:load-module environment "epsilon.library")
                (let ((missing-fn (find-symbol "MISSING-LIBRARIES" (find-package "EPSILON.LIBRARY"))))
                  (when missing-fn
                    (let ((libs (funcall missing-fn)))
                      (if libs
                          (progn
                            (format t "~&Missing Critical Libraries~%")
                            (format t "=========================~%")
                            (dolist (lib libs)
                              (format t "  ~A~%" lib)))
                          (format t "~&All critical libraries are available~%"))))))
            (error (e)
              (format *error-output* "Error listing missing libraries: ~A~%" e)))
          (sb-ext:exit :code 0))

        ;; Handle --init (project initialization)
        (when (map:get (argparse:parsed-options parsed-args) "init")
          (handler-case
              (progn
                (loader:load-module environment "epsilon.cli")
                (let ((init-fn (find-symbol "INIT" (find-package "EPSILON.CLI"))))
                  (when init-fn
                    (funcall init-fn))))
            (error (e)
              (format *error-output* "Error initializing project: ~A~%" e)
              (sb-ext:exit :code 1)))
          (sb-ext:exit :code 0))

        ;; Handle --new (module scaffolding)
        (when (map:get (argparse:parsed-options parsed-args) "new")
          (handler-case
              (progn
                (loader:load-module environment "epsilon.cli")
                (let ((new-fn (find-symbol "NEW-MODULE" (find-package "EPSILON.CLI")))
                      (module-name (map:get (argparse:parsed-options parsed-args) "new")))
                  (when new-fn
                    (funcall new-fn :name module-name))))
            (error (e)
              (format *error-output* "Error creating module: ~A~%" e)
              (sb-ext:exit :code 1)))
          (sb-ext:exit :code 0))

        ;; Handle --doctor (diagnostics)
        (when (map:get (argparse:parsed-options parsed-args) "doctor")
          (handler-case
              (progn
                (loader:load-module environment "epsilon.cli")
                (let ((doctor-fn (find-symbol "DOCTOR" (find-package "EPSILON.CLI")))
                      (verbose (map:get (argparse:parsed-options parsed-args) "verbose")))
                  (when doctor-fn
                    (let ((success (funcall doctor-fn :verbose verbose)))
                      (sb-ext:exit :code (if success 0 1))))))
            (error (e)
              (format *error-output* "Error running diagnostics: ~A~%" e)
              (sb-ext:exit :code 1)))
          (sb-ext:exit :code 0))

        ;; Handle --install (ad-hoc module installation)
        (when (map:get (argparse:parsed-options parsed-args) "install")
          (handler-case
              (progn
                (loader:load-module environment "epsilon.install")
                (let ((install-fn (find-symbol "INSTALL" (find-package "EPSILON.INSTALL")))
                      (spec (map:get (argparse:parsed-options parsed-args) "install"))
                      (checksum (map:get (argparse:parsed-options parsed-args) "install_checksum"))
                      (name (map:get (argparse:parsed-options parsed-args) "install_name")))
                  (when install-fn
                    ;; Determine if it's a GitHub spec or URL
                    (cond
                      ((or (str:starts-with-p "github:" spec)
                           (and (position #\/ spec)
                                (not (str:starts-with-p "http" spec))))
                       ;; GitHub spec (github:owner/repo or owner/repo)
                       (let ((github-spec (if (str:starts-with-p "github:" spec)
                                              (subseq spec 7)
                                              spec)))
                         (funcall install-fn :github github-spec :checksum checksum :name name)))
                      ((or (str:starts-with-p "https://" spec)
                           (str:starts-with-p "http://" spec))
                       ;; Direct URL
                       (funcall install-fn :url spec :checksum checksum :name name))
                      (t
                       (format *error-output* "Invalid install spec: ~A~%" spec)
                       (format *error-output* "Use github:owner/repo or a direct URL~%")
                       (sb-ext:exit :code 1))))))
            (error (e)
              (format *error-output* "Error installing module: ~A~%" e)
              (sb-ext:exit :code 1)))
          (sb-ext:exit :code 0))

        ;; Handle --install-manifest (manifest-based installation)
        (let ((manifest-arg (map:get (argparse:parsed-options parsed-args) "install_manifest")))
          (when (and manifest-arg (not (eq manifest-arg :not-provided)))
            (handler-case
                (progn
                  (loader:load-module environment "epsilon.install")
                  (let ((install-fn (find-symbol "INSTALL-MANIFEST" (find-package "EPSILON.INSTALL")))
                        (manifest-path (if (stringp manifest-arg) manifest-arg "epsilon.manifest")))
                    (when install-fn
                      (funcall install-fn :path manifest-path))))
              (error (e)
                (format *error-output* "Error installing from manifest: ~A~%" e)
                (sb-ext:exit :code 1)))
            (sb-ext:exit :code 0)))

        ;; Handle --run (script with package declaration)
        (when (map:get (argparse:parsed-options parsed-args) "run")
          (handler-case
              (run-script environment
                          (map:get (argparse:parsed-options parsed-args) "run")
                          passthrough-args)
            (error (e)
              (format *error-output* "Error running script: ~A~%" e)
              (sb-ext:exit :code 1)))
          (sb-ext:exit :code 0))

        ;; STEP 4: Process --load, --eval, and --exec in order of appearance
        (when ordered-actions
          (process-ordered-actions environment ordered-actions passthrough-args)
          (sb-ext:exit :code 0))
            
        ;; No command, no positionals, no eval - start REPL
        (start-interactive-repl))))))

(defun cli-run (&optional args)
  "Main entry point from from epsilon script"
  (declare (ignore args))
  (let ((posix-args (rest sb-ext:*posix-argv*)))
    (when (string-equal "--" (car posix-args))
      (pop posix-args))
    (run posix-args)))

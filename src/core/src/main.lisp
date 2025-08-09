;;;; Main Command Line Interface
;;;;
;;;; This module serves as the main registration and extension point for
;;;; development tools. It provides a lightweight command dispatcher that
;;;; demand-loads tool modules as needed, keeping the core bootstrap minimal.

(defpackage epsilon.main
  (:use cl)
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
   #:cli-run))

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
    ;; Add package loading option
    (argparse:add-argument parser "--package"
                           :action 'append
                           :metavar "PACKAGE"
                           :help "Load specified package before evaluation")
    ;; Add path option for direct package path
    (argparse:add-argument parser "--path"
                           :metavar "PATH"
                           :help "Path to package directory (alternative to --repository)")
    ;; Add exec option
    (argparse:add-argument parser "--exec"
                           :metavar "PACKAGE:FUNCTION"
                           :help "Execute a package function with remaining args")
    ;; Add packages option  
    (argparse:add-argument parser "--packages"
                           :action 'store-true
                           :help "List available packages")
    ;; Add load option
    (argparse:add-argument parser "--load"
                           :action 'append
                           :metavar "FILE"
                           :help "Load a Lisp file (can be used multiple times)")
    ;; Add test option
    (argparse:add-argument parser "--test"
                           :action 'append
                           :metavar "PACKAGE"
                           :help "Build and test specified package (can be used multiple times)")
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

(defun format-version-table ()
  "Format version information as a structured table string."
  (format nil "╭────────────────────────────────────────╮~%~
               │                EPSILON                 │~%~
               ├────────────────────────────────────────┤~%~
               │ Version:      ~24A │~%~
               │ SBCL:         ~24A │~%~
               │ OS:           ~24A │~%~
               │ Architecture: ~24A │~%~
               ╰────────────────────────────────────────╯"
          (env:version)
          (lisp-implementation-version)
          (string-downcase (symbol-name (env:platform)))
          (machine-type)))

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
        (epsilon-user (sb-ext:posix-getenv "EPSILON_USER"))
        (epsilon-home (sb-ext:posix-getenv "EPSILON_HOME")))
    
    ;; Add user's directory if it contains a package.lisp
    (loader:register-package-directory env epsilon-user)
    
    ;; Add epsilon's src directory for built-in packages
    (let ((repo (path:path-string (path:path-join epsilon-home "src"))))
      (loader:scan-directory-for-packages env (path:ensure-path repo)))
    env))

(defun process-repository-options (environment parsed-options)
  "Process --repository and --path options from parsed arguments.
   Updates the environment and returns any errors."
  (let ((package-path (map:get parsed-options "path"))
        (errors nil))
    
    ;; Handle --path option - register single package directory
    (when package-path
      ;; package-path is a string from the command line parser
      ;; Resolve relative paths relative to user's original working directory
      (let* ((is-absolute (or (char= (char package-path 0) #\/)
                              #+win32 (and (>= (length package-path) 3)
                                           (char= (char package-path 1) #\:))))
             (user-dir (sb-ext:posix-getenv "EPSILON_USER"))
             (base-dir (if user-dir (pathname user-dir) (truename ".")))
             (resolved-path (if is-absolute
                                (pathname package-path)
                                (merge-pathnames (pathname package-path) base-dir))))
        (unless (probe-file resolved-path)
          (push (format nil "Path does not exist: ~A" resolved-path) errors))
        (unless errors
          (loader:register-package-directory environment (namestring resolved-path)))))
    
    errors))

(defun load-packages (environment packages)
  "Load multiple packages with error handling."
  (dolist (pkg packages)
    (loader:load-package environment pkg)))

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

(defun test-packages (environment packages)
  "Test multiple packages with result collection."
  (log:debug "test-packages called with environment: ~A, packages: ~A" environment packages)
  (loader:load-package environment "epsilon.test")
  (let ((run-fn (find-symbol "RUN" (find-package "EPSILON.TEST"))))
    (dolist (pkg packages)
      (loader:load-package environment pkg)
      (let ((result (funcall run-fn environment pkg)))
        (log:debug "EPSILON.TEST:RUN returned: ~A" result)
        ;; Check if tests passed
        (let ((success-p-fn (find-symbol "SUCCESS-P" (find-package "EPSILON.TEST"))))
          (unless success-p-fn
            (error "EPSILON.TEST:SUCCESS-P not found"))
          (log:debug "Calling SUCCESS-P with result: ~A" result)
          (unless (funcall success-p-fn result)
            (error "Tests failed for ~A" pkg))))
      (format t "Successfully tested ~A~%" pkg))))

;;; Command Execution Functions

(defun execute-evaluation (environment expression &key packages)
  "Execute a Lisp expression with proper package loading."
  (when packages
    (load-packages environment packages))
  
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
      (error "Package ~A not found.~%Make sure to load it with --package or that it's exported by a loaded package." 
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
                  (push (list :exec (nth (1+ i) args)) actions)
                  (incf i 2)))
               (t (incf i))))
    (reverse actions)))

(defun process-ordered-actions (environment actions passthrough-args)
  "Process --load, --eval, and --exec actions in order."
  (dolist (action actions)
    (case (first action)
      (:load
       (load (second action)))
      (:eval
       (let ((result (execute-evaluation environment (second action) :packages nil)))
         (unless (eq result (values))
           (format t "~A~%" result))))
      (:exec
       (let ((exec-spec (second action)))
         ;; Extract package name and load if needed
         (multiple-value-bind (package-spec function-spec)
             (parse-exec-spec exec-spec)
           (when (and package-spec
                      (>= (length package-spec) 8)
                      (string= "epsilon." package-spec :end2 8))
             (loader:load-package environment package-spec))
           (execute-package-function package-spec function-spec passthrough-args)))))))

(defun make-package-table-data (packages local-package)
  "Create table data for package listing."
  (let ((package-data '()))
    ;; Add local package if present
    (when local-package
      (push (list (getf local-package :name)
                  (or (getf local-package :version) "unknown")
                  "LOCAL"
                  (getf local-package :path))
            package-data))
    
    ;; Add all other packages
    (dolist (pkg-name packages)
      ;; Skip if this is the same as our local package
      (unless (and local-package 
                   (string= pkg-name (getf local-package :name)))
        (push (list pkg-name "?" "AVAILABLE" "unknown") 
              package-data)))
    
    (nreverse package-data)))

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
  "Display help for the given parser and exit."
  (argparse:print-help parser)
  (sb-ext:exit :code exit-code))

(defun display-package-list (packages local-package)
  "Display a formatted table of available packages."
  (let ((package-data (make-package-table-data packages local-package)))
    ;; Sort by name
    (setf package-data (sort package-data #'string< :key #'first))
    
    (cond
      ;; If we have packages to show
      (package-data
       (let ((tbl (table:simple-table
                   (list "NAME" "VERSION" "STATUS" "LOCATION")
                   package-data)))
         (table:print-table tbl))
       (terpri))
      ;; No packages found
      (t
       (format t "No packages found.~%")))
    
    (format t "~%Found ~D package~:P~%" (length package-data))))

(defun display-packages (environment)
  "Display all packages with their virtual capabilities mixed in."
  (let* ((all-packages (loader:query-packages environment))
         (package-data '())
         (virtual-capabilities (map:make-map)))
    
    ;; Collect packages and track their virtual capabilities
    (dolist (pkg-info all-packages)
      (let* ((name (loader:package-info-name pkg-info))
             (metadata (loader:package-metadata pkg-info))
             (provides (or (getf metadata :provides) (list name)))
             (version (or (getf metadata :version) "?"))
             (platform (getf metadata :platform))
             (status (if (loader:package-loaded-p pkg-info) "LOADED" "AVAILABLE"))
             (location (let ((loc (loader:package-location pkg-info)))
                         (if (pathnamep loc)
                             (namestring loc)
                             (format nil "~A" loc)))))
        
        ;; Add to package list (skip platform-specific unless on that platform)
        (when (or (not platform)
                  (string-equal platform (string-downcase (symbol-name (env:platform)))))
          (let ((capabilities (if (> (length provides) 1)
                                  (format nil "~A (provides: ~{~A~^, ~})" name
                                          (remove name provides :test #'string=))
                                  name)))
            (push (list capabilities version status location) package-data)))
        
        ;; Track virtual capabilities separately from concrete packages
        (dolist (capability provides)
          (unless (string= capability name)
            (map:update! virtual-capabilities capability
                         (lambda (providers)
                           (cons name providers)))))))
    
    
    ;; Sort packages by name
    (setf package-data (sort package-data #'string< :key #'first))
    
    ;; Display packages
    (format t "~%PACKAGES:~%")
    (format t "=========~%~%")
    (if package-data
        (let ((tbl (table:simple-table
                    (list "NAME" "VERSION" "STATUS" "LOCATION")
                    package-data)))
          (table:print-table tbl))
        (format t "No packages found.~%"))
    
    ;; Display virtual capabilities summary if any exist
    (let ((virtual-count (map:size virtual-capabilities)))
      (when (> virtual-count 0)
        (format t "~%~%Note: ~D virtual capabilit~:@P provided by packages above~%"
                virtual-count)))
    
    ;; Summary
    (format t "~%Found ~D package~:P~%" (length package-data))))

(defun start-interactive-repl ()
  "Start the epsilon interactive REPL."
  (format t "Starting Epsilon REPL...~%")
  (format t "Type (quit) or Ctrl+D to exit~%~%")
  (sb-impl::toplevel-init))

(defun process-test-option (environment parsed-args)
  "Process --test option from parsed arguments.
   Returns T if tests were run and should exit."
  (let ((packages-to-test (map:get (argparse:parsed-options parsed-args) "test")))
    (when packages-to-test
      (test-packages environment packages-to-test)
      ;; Check if we should exit
      (let ((has-other-actions 
             (or (and (map:get (argparse:parsed-options parsed-args) "eval")
                      (not (eq (map:get (argparse:parsed-options parsed-args) "eval") :not-provided)))
                 (map:get (argparse:parsed-options parsed-args) "exec")
                 (map:get (argparse:parsed-options parsed-args) "load")
                 (argparse:parsed-command parsed-args)
                 (argparse:parsed-positionals parsed-args))))
        (not has-other-actions)))))

(defun evaluate-expression (environment expression-string packages)
  "Evaluate a Lisp expression with optional package loading"
  ;; Load requested packages
  (dolist (pkg packages)
    (handler-case
        (progn
          (format *error-output* "Loading package ~A...~%" pkg)
          (loader:load-package environment pkg))
      (error (e)
        (format *error-output* "Error loading package ~A: ~A~%" pkg e)
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

(defun find-package-directory (package-name)
  "Try to find a package directory by searching common locations"
  (let ((possible-paths (list
                           ;; Current directory
                         (merge-pathnames package-name (sb-posix:getcwd))
                         ;; Parent directory
                         (merge-pathnames (format nil "../~A" package-name) (sb-posix:getcwd))
                         ;; Direct path if package-name is actually a path
                         (when (or (search "/" package-name) (search "\\" package-name))
                           (pathname package-name)))))
    (loop for path in possible-paths
          when (and path 
                    (probe-file path)
                    (probe-file (merge-pathnames "package.lisp" path)))
            return (namestring path))))

(defun run (args)
  "Dispatch to the appropriate command handler"
  (let* ((arg-parser (initialize-parser))
         (environment (setup-build-environment)))
    ;; Set global REPL environment for backward compatibility
    (setf loader:*environment* environment)
    ;; Split args at "--" to separate epsilon args from passthrough args
    (multiple-value-bind (epsilon-args passthrough-args)
        (split-args-at-delimiter args)
      (let ((parsed-args (argparse:parse-args arg-parser epsilon-args)))
        ;; Check global options
        (when (map:get (argparse:parsed-options parsed-args) "log")
          (log:configure-from-string (map:get (argparse:parsed-options parsed-args) "log")))
            
        ;; Handle --help
        (when (map:get (argparse:parsed-options parsed-args) "help")
          (show-help-and-exit arg-parser))
            
        ;; Handle --version
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
            
        ;; Process --package flags to load specific packages
        (let ((packages-to-load (map:get (argparse:parsed-options parsed-args) "package")))
          (when packages-to-load
            (load-packages environment packages-to-load)))
            
        ;; STEP 3: Process --test flag separately (not part of action sequence)
        (when (process-test-option environment parsed-args)
          (sb-ext:exit :code 0))
            
        ;; Handle --packages (can be processed anytime after environment setup)
        (when (map:get (argparse:parsed-options parsed-args) "packages")
          (display-packages environment)
          (sb-ext:exit :code 0))
            
        ;; STEP 4: Process --load, --eval, and --exec in order of appearance
        (let ((ordered-actions (collect-ordered-actions epsilon-args)))
          (when ordered-actions
            (process-ordered-actions environment ordered-actions passthrough-args)
            (sb-ext:exit :code 0)))
            
        ;; No command, no positionals, no eval - start REPL
        (start-interactive-repl)))))

(defun cli-run (&optional args)
  "Main entry point from from epsilon script"
  (declare (ignore args))
  (let ((posix-args (rest sb-ext:*posix-argv*)))
    (when (string-equal "--" (car posix-args))
      (pop posix-args))
    (run posix-args)))

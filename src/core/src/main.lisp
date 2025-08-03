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
   (build epsilon.tool.build)
   (path epsilon.path)
   (package epsilon.tool.package)
   (proto epsilon.build.protocol)
   (argparse epsilon.argparse)
   (log epsilon.log)
   (build-env epsilon.tool.build-environment)
   (env epsilon.sys.env))
  (:export
   #:cli-run
   #:register-command
   #:command
   #:run-command
   #:argument-parser))

(in-package epsilon.main)

;;; Global variables

(defvar *user-directory* nil
  "Original user directory before epsilon changes to its home directory")

;;; Command Base Class and Protocol

(defclass command ()
  ()
  (:documentation "Base class for all epsilon commands"))

(defgeneric run-command (command parsed-args)
  (:documentation "Execute the command with parsed arguments"))

(defgeneric argument-parser (command)
  (:documentation "Return an argparse parser for this command"))

;;; Command Registry

(defparameter *command-registry* (map:make-map)
  "Registry of available commands")

(defparameter *main-parser* nil
  "Main argument parser")

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
    ;; Add repository option
    (argparse:add-argument parser "--repository"
                          :action 'append
                          :metavar "PATH"
                          :help "Add package repository path")
    ;; Add exec option
    (argparse:add-argument parser "--exec"
                          :metavar "PACKAGE:FUNCTION"
                          :help "Execute a package function with remaining args")
    parser))

(defun register-command (command-class)
  "Register a command class"
  (unless *main-parser*
    (setf *main-parser* (initialize-parser)))
  
  (let* ((instance (make-instance command-class))
         (parser (argument-parser instance))
         (name (argparse:parser-command parser))
         (description (argparse:parser-description parser))
         (subparser (argparse:add-command *main-parser* name :description description)))
    ;; Copy arguments from command parser to subparser
    (dolist (arg (slot-value parser 'argparse::arguments))
      (push arg (slot-value subparser 'argparse::arguments)))
    ;; Copy commands from command parser to subparser
    (map:each (lambda (cmd-name cmd-parser)
                (setf (argparse:parser-commands subparser)
                      (map:assoc (argparse:parser-commands subparser) cmd-name cmd-parser))
                ;; Set the parent of the command's subcommands to point to the registered subparser
                (setf (slot-value cmd-parser 'argparse::parent) subparser))
              (argparse:parser-commands parser))
    ;; Also set parent of the original parser to main parser for consistency
    (setf (slot-value parser 'argparse::parent) *main-parser*)
    ;; Store the instance
    (map:assoc! *command-registry* name instance)
    name))

(defun list-commands ()
  "List all registered commands"
  (map:keys *command-registry*))



(defun evaluate-expression (expression-string packages)
  "Evaluate a Lisp expression with optional package loading"
  ;; Load requested packages
  (dolist (pkg packages)
    (handler-case
        (progn
          (format *error-output* "Loading package ~A...~%" pkg)
          (build:build pkg)
          (build:load-package pkg))
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

(defun run-package-main (package-name args)
  "Run a package's main function"
  (let* ((env (build:make-build-environment))
         (build:*current-environment* env)
         (package-source (build-env:ensure-package-source env))
         (metadata (proto:load-package-metadata package-source package-name))
         (main-spec (getf metadata :main)))
    
    ;; Validate package has main entry point
    (unless main-spec
      (error "Package ~A missing :main entry point in package.lisp" package-name))
    
    ;; Build the package
    (log:info "Building package: ~A" package-name)
    (handler-case
        (build:build-with-environment env package-name)
      (error (e)
        (error "Failed to build package ~A: ~A" package-name e)))
    
    ;; Parse and resolve main function
    (let* ((spec-string (string main-spec))
           (colon-pos (position #\: spec-string))
           (package-spec (when colon-pos (subseq spec-string 0 colon-pos)))
           (function-spec (if colon-pos 
                             (subseq spec-string (1+ colon-pos))
                             spec-string))
           (package (if package-spec
                       (find-package (string-upcase package-spec))
                       *package*))
           (symbol (when package
                     (find-symbol (string-upcase function-spec) package))))
      
      (unless package
        (error "Package ~A not found for main function ~A" 
               package-spec main-spec))
      
      (unless symbol
        (error "Function ~A not found in package ~A" 
               function-spec (package-name package)))
      
      (unless (fboundp symbol)
        (error "Symbol ~A is not a function" symbol))
      
      ;; Call main function
      (log:info "Starting ~A" package-name)
      (handler-case
          (apply symbol args)
        (error (e)
          (format *error-output* "~%Error in main function: ~A~%" e)
          (sb-ext:exit :code 1))))))

(defun exec-package-function (exec-spec packages repos args)
  "Execute a package function with the given arguments.
   EXEC-SPEC should be in the format 'package:function' or just 'function'.
   PACKAGES is a list of packages to load before execution.
   REPOS is a list of repository paths to add.
   ARGS are the arguments to pass to the function."
  (let* ((env (build:make-build-environment))
         (build:*current-environment* env))
    
    ;; Add repositories
    (dolist (repo repos)
      (build-env:add-package-repo env repo))
    
    ;; Load packages
    (dolist (pkg packages)
      (handler-case
          (progn
            (log:info "Loading package ~A..." pkg)
            (build:build-with-environment env pkg)
            (build:load-package pkg))
        (error (e)
          (format *error-output* "Error loading package ~A: ~A~%" pkg e)
          (sb-ext:exit :code 1))))
    
    ;; Parse exec spec
    (let* ((colon-pos (position #\: exec-spec))
           (package-spec (when colon-pos (subseq exec-spec 0 colon-pos)))
           (function-spec (if colon-pos 
                             (subseq exec-spec (1+ colon-pos))
                             exec-spec))
           (package (if package-spec
                       (find-package (string-upcase package-spec))
                       *package*))
           (symbol (when package
                     (find-symbol (string-upcase function-spec) package))))
      
      (unless package
        (error "Package ~A not found" package-spec))
      
      (unless symbol
        (error "Function ~A not found in package ~A" 
               function-spec (package-name package)))
      
      (unless (fboundp symbol)
        (error "Symbol ~A is not a function" symbol))
      
      ;; Call the function with args
      (log:info "Executing ~A with args: ~S" exec-spec args)
      (handler-case
          (apply symbol args)
        (error (e)
          (format *error-output* "~%Error executing ~A: ~A~%" exec-spec e)
          (sb-ext:exit :code 1))))))

(defun run (args)
  "Dispatch to the appropriate command handler"
  (unless *main-parser*
    (setf *main-parser* (initialize-parser)))
  
  ;; Split args at "--" to separate epsilon args from passthrough args
  (let* ((double-dash-pos (position "--" args :test #'string=))
         (epsilon-args (if double-dash-pos
                          (subseq args 0 double-dash-pos)
                          args))
         (passthrough-args (when double-dash-pos
                            (subseq args (1+ double-dash-pos)))))
    
    ;; Check if help is requested for a command or subcommand
  (let ((help-pos (position "--help" args :test #'string=)))
    (when help-pos
      (cond
        ;; Subcommand help: epsilon COMMAND SUBCOMMAND --help
        ((and (>= help-pos 2) (>= (length args) 3))
         (let* ((command-name (first args))
                (subcommand-name (second args))
                (instance (map:get *command-registry* command-name)))
           (when instance
             (let* ((parser (argument-parser instance))
                    (subparser (map:get (argparse:parser-commands parser) subcommand-name)))
               (if subparser
                   (progn
                     (argparse:print-help subparser)
                     (sb-ext:exit :code 0))
                   ;; Subcommand not found, show main command help
                   (progn
                     (argparse:print-help parser)
                     (sb-ext:exit :code 0)))))))
        ;; Top-level command help: epsilon COMMAND --help
        ((and (>= help-pos 1) (>= (length args) 2))
         (let* ((command-name (first args))
                (instance (map:get *command-registry* command-name)))
           (when instance
             (let ((parser (argument-parser instance)))
               (argparse:print-help parser)
               (sb-ext:exit :code 0))))))))
    
    (handler-case
        (let ((parsed-args (argparse:parse-args *main-parser* epsilon-args)))
        ;; Check global options
        (when (map:get (argparse:parsed-options parsed-args) "log")
          (log:configure-from-string (map:get (argparse:parsed-options parsed-args) "log")))
        
        ;; Handle --help
        (when (map:get (argparse:parsed-options parsed-args) "help")
          (argparse:print-help *main-parser*)
          (sb-ext:exit :code 0))
        
        ;; Handle --version
        (when (map:get (argparse:parsed-options parsed-args) "version")
          (format t "╭────────────────────────────────────────╮~%")
          (format t "│                EPSILON                 │~%")
          (format t "├────────────────────────────────────────┤~%")
          (format t "│ Version:      ~24A │~%" (env:version))
          (format t "│ SBCL:         ~24A │~%" (lisp-implementation-version))
          (format t "│ OS:           ~24A │~%" (string-downcase (symbol-name (env:platform))))
          (format t "│ Architecture: ~24A │~%" (machine-type))
          (format t "╰────────────────────────────────────────╯~%")
          (sb-ext:exit :code 0))
        
        ;; Handle --eval
        (let ((eval-option (map:get (argparse:parsed-options parsed-args) "eval"))
              (packages (map:get (argparse:parsed-options parsed-args) "package")))
          ;; Check if --eval was provided (will be non-nil and not :not-provided if flag was present)
          (when (and eval-option (not (eq eval-option :not-provided)))
            ;; Evaluation mode
            (let ((expression-string
                   (if (equal eval-option "")
                       ;; --eval with no value: read from stdin
                       (with-output-to-string (str)
                         (loop for line = (read-line *standard-input* nil nil)
                               while line
                               do (write-line line str)))
                       ;; --eval with value: use provided expression
                       eval-option)))
              
              ;; Check if we have any input
              (when (zerop (length (str:trim expression-string)))
                (format *error-output* "Error: No expression provided~%")
                (sb-ext:exit :code 1))
              
              (evaluate-expression expression-string (or packages nil))
              (sb-ext:exit :code 0))))
        
        ;; Handle --exec
        (let ((exec-spec (map:get (argparse:parsed-options parsed-args) "exec"))
              (repositories (map:get (argparse:parsed-options parsed-args) "repository"))
              (packages (map:get (argparse:parsed-options parsed-args) "package")))
          (when exec-spec
            (exec-package-function exec-spec (or packages nil) (or repositories nil) 
                                  (or passthrough-args nil))
            (sb-ext:exit :code 0)))
        
        ;; Set up build environment with repositories if specified
        (let ((repositories (map:get (argparse:parsed-options parsed-args) "repository")))
          (when repositories
            (let ((env (build:current-environment)))
              (dolist (repo repositories)
                (build-env:add-package-repo env repo)))))
        
        ;; Check for command
        (let ((command-name (argparse:parsed-command parsed-args))
              (positionals (argparse:parsed-positionals parsed-args)))
          (cond
            ;; Explicit command specified
            (command-name
             (let ((instance (map:get *command-registry* command-name)))
               (if instance
                   (run-command instance (or (argparse:parsed-subresult parsed-args) parsed-args))
                   (error "Unknown command: ~A" command-name))))
            ;; No command but have positionals - treat first as package to run
            (positionals
             (run-package-main (first positionals) (rest positionals))
             (sb-ext:exit :code 0))
            ;; No command, no positionals, no eval - start REPL
            (t
             (format t "Starting Epsilon REPL...~%")
             (format t "Type (quit) or Ctrl+D to exit~%~%")
             (sb-impl::toplevel-init)))))
    (argparse:argument-error (e)
      (format *error-output* "Error: ~A~%~%" (argparse:error-message e))
      ;; Try to show help for the specific parser that caused the error
      (let ((error-parser (argparse:error-parser e)))
        (if error-parser
            (argparse:print-help error-parser)
            (argparse:print-help *main-parser*)))
      (sb-ext:exit :code 1)))))

(defun cli-run (&optional args)
  "Main entry point from from epsilon script"
  (declare (ignore args))
  (let ((posix-args (rest sb-ext:*posix-argv*)))
    (when (string-equal "--" (car posix-args))
      (pop posix-args))
    (run posix-args)))


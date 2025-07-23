;;;; Development Tool Dispatcher
;;;;
;;;; This module serves as the main registration and extension point for
;;;; development tools. It provides a lightweight command dispatcher that
;;;; demand-loads tool modules as needed, keeping the core bootstrap minimal.

(defpackage #:epsilon.tool.dev
  (:use #:cl)
  (:local-nicknames
   (#:map #:epsilon.map)
   (#:str #:epsilon.string)
   (#:build #:epsilon.tool.build)
   (#:fs #:epsilon.sys.fs)
   (#:path #:epsilon.path)
   (#:package #:epsilon.tool.package))
  (:export
   #:main
   #:register-tool
   #:list-tools
   #:initialize-tool-registry
   #:handle-build-help
   #:handle-build-all-help
   #:handle-bench-help
   #:parsed-args
   #:parsed-args-command
   #:parsed-args-arguments
   #:parsed-args-options))

(in-package #:epsilon.tool.dev)

;;; Tool Registry

(defparameter *tool-registry* (map:make-map)
  "Registry of available development tools and their implementing modules")

(defparameter *loaded-tools* (map:make-map)
  "Cache of already loaded tool modules")

(defstruct tool-info
  "Information about a registered tool"
  (name nil :type string)
  (module nil :type (or null string))
  (function nil :type symbol)
  (help-function nil :type (or null symbol))
  (description nil :type string)
  (available-p t :type boolean))

;;; Default tool registrations

(defun initialize-tool-registry ()
  "Initialize the registry with core tools"
  (setf *tool-registry* 
        (map:from-pairs
         `(("build" . ,(make-tool-info
                        :name "build"
                        :module nil  ; build is already loaded in core
                        :function 'handle-build
                        :help-function 'handle-build-help
                        :description "Build epsilon modules"))
           ("test" . ,(make-tool-info
                       :name "test"
                       :module "epsilon.test"  ; test is now separate module
                       :function 'test-main  ; Placeholder - will be resolved dynamically
                       :description "Run test suites"))
           ("clean" . ,(make-tool-info
                        :name "clean"
                        :module nil  ; clean uses core functionality
                        :function 'handle-clean
                        :help-function 'handle-clean-help
                        :description "Remove build artifacts and cached files"))
           ("release" . ,(make-tool-info
                          :name "release"
                          :module "epsilon.release"  ; demand-load release module
                          :function 'release-main  ; Placeholder - will be resolved dynamically
                          :help-function 'handle-release-help
                          :description "Build complete Epsilon release with EPK packages and distributions"))
           ("package" . ,(make-tool-info
                          :name "package"
                          :module nil  ; package is in core
                          :function 'handle-package
                          :help-function 'handle-package-help
                          :description "Manage EPK packages"))
           ;; These will be uncommented when the modules exist
           #|("lsp" . ,(make-tool-info
                      :name "lsp"
                      :module "epsilon.lsp"
                      :function (read-from-string "epsilon.lsp:main")
                      :description "Start Language Server Protocol server"))
           ("hot-reload" . ,(make-tool-info
                             :name "hot-reload"
                             :module "epsilon.hot-reload"
                             :function (read-from-string "epsilon.tool.hot-reload:main")
                             :description "Interactive development with hot code reloading"))
           ("build-all" . ,(make-tool-info
                            :name "build-all"
                            :module nil  ; build is already loaded in core
                            :function 'handle-build-all
                            :help-function 'handle-build-all-help
                            :description "Build all epsilon modules"))
           ("test-all" . ,(make-tool-info
                           :name "test-all"
                           :module "epsilon.test"
                           :function 'test-all-main
                           :description "Run all test suites"))
           ("bench" . ,(make-tool-info
                        :name "bench"
                        :module nil  ; benchmark is in core
                        :function 'handle-bench
                        :help-function 'handle-bench-help
                        :description "Run benchmarks"))
           ("repl" . ,(make-tool-info
                       :name "repl"
                       :module "epsilon.repl"
                       :function (read-from-string "epsilon.repl:main")
                       :description "Start an enhanced REPL"))|#))))

;;; Tool Registration API

(defun register-tool (name &key module function help-function description)
  "Register a new development tool"
  (setf *tool-registry*
        (map:assoc *tool-registry* name
                   (make-tool-info
                    :name name
                    :module module
                    :function function
                    :help-function help-function
                    :description description))))

(defun list-tools ()
  "List all registered tools"
  (map:keys *tool-registry*))

;;; Command Line Parsing

(defstruct parsed-args
  command
  arguments
  options)

(defun parse-command-line (args)
  "Parse command line arguments into command, args, and options"
  (let ((command nil)
        (arguments '())
        (options (map:make-map)))
    
    ;; Extract global options first (before command)
    (loop while (and args (str:starts-with-p (first args) "--"))
          do (let* ((arg (pop args))
                    (equals-pos (position #\= arg))
                    (key (if equals-pos
                             (subseq arg 2 equals-pos)
                             (subseq arg 2)))
                    (value (if equals-pos
                               (subseq arg (1+ equals-pos))
                               (if (and args (not (str:starts-with-p (first args) "-")))
                                   (pop args)
                                   t))))
               (setf options (map:assoc options key value))))
    
    ;; Get command
    (when args
      (setf command (pop args)))
    
    ;; Process remaining arguments - separate options from positional args
    (loop while args
          do (let ((arg (first args)))
               (cond
                 ;; Option
                 ((str:starts-with-p arg "--")
                  (pop args)
                  (let* ((equals-pos (position #\= arg))
                         (key (if equals-pos
                                  (subseq arg 2 equals-pos)
                                  (subseq arg 2)))
                         (value (if equals-pos
                                    (subseq arg (1+ equals-pos))
                                    (if (and args (not (str:starts-with-p (first args) "-")))
                                        (pop args)
                                        t))))
                    (setf options (map:assoc options key value))))
                 ;; Positional argument
                 (t
                  (push (pop args) arguments)))))
    
    (make-parsed-args :command command
                      :arguments (nreverse arguments)
                      :options options)))

;;; Tool Loading and Dispatch

(defun load-tool-module (tool-info)
  "Load a tool module if not already loaded"
  (let ((module-name (tool-info-module tool-info)))
    (when (and module-name
               (not (map:get *loaded-tools* module-name)))
      (handler-case
          (progn
            (format t ";;; Loading tool module: ~A~%" module-name)
            (build:build module-name)
            (setf *loaded-tools* (map:assoc *loaded-tools* module-name t))
            t)
        (error (e)
          (format t ";;; Warning: Failed to load ~A: ~A~%" module-name e)
          (setf (tool-info-available-p tool-info) nil)
          nil)))))

(defun dispatch-tool (command args)
  "Dispatch to the appropriate tool handler"
  (let ((tool-info (map:get *tool-registry* command)))
    (cond
      ((null tool-info)
       (format t "Unknown command: ~A~%" command)
       (print-usage)
       nil)
      
      ((not (tool-info-available-p tool-info))
       (format t "Tool '~A' is not available (module failed to load)~%" command)
       nil)
      
      (t
       ;; Check for help request
       (when (map:get (parsed-args-options args) "help")
         (if (tool-info-help-function tool-info)
             (progn
               ;; Load module if needed for help function
               (when (tool-info-module tool-info)
                 (unless (load-tool-module tool-info)
                   (return-from dispatch-tool nil)))
               ;; Call the tool's help function
               (funcall (tool-info-help-function tool-info) args)
               (return-from dispatch-tool t))
             ;; No help function available, show basic info
             (progn
               (format t "~A: ~A~%" command (tool-info-description tool-info))
               (format t "~%No detailed help available for this tool.~%")
               (return-from dispatch-tool t))))
       
       ;; Load module if needed
       (when (tool-info-module tool-info)
         (unless (load-tool-module tool-info)
           (return-from dispatch-tool nil)))
       
       ;; Call the tool's function
       (funcall (tool-info-function tool-info) args)))))

;;; Built-in Commands

(defun test-main (parsed-args)
  "Dynamically dispatch to epsilon.test:main after loading the module"
  (funcall (find-symbol "MAIN" "EPSILON.TEST") parsed-args))

(defun handle-build (parsed-args)
  "Handle build command - this is always available"
  (let* ((options (parsed-args-options parsed-args))
         (modules (or (parsed-args-arguments parsed-args)
                      (list "epsilon.core")))
         (force (map:get options "force"))
         (include-tests (map:get options "include-tests")))
    
    ;; Build specific modules only - removed 'all' special case
    (dolist (module modules)
      (format t ";;; Building module: ~A~%" module)
      (build:build module 
                   :force force
                   :include-tests include-tests))))

#++
(defun handle-build-all (parsed-args)
  "Handle build-all command"
  (let* ((options (parsed-args-options parsed-args))
         (force (map:get options "force"))
         (include-tests (map:get options "include-tests"))
         (include-platform (map:get options "include-platform")))
    (build:build-all :force force
                     :include-tests include-tests
                     :include-platform include-platform)))

(defun test-all-main (parsed-args)
  "Dynamically dispatch to epsilon.test:test-all after loading the module"
  (funcall (find-symbol "TEST-ALL" "EPSILON.TEST") parsed-args))

(defun release-main (parsed-args)
  "Dynamically dispatch to epsilon.tool.release:main after loading the module"
  (funcall (find-symbol "MAIN" "EPSILON.TOOL.RELEASE") parsed-args))

(defun handle-build-help (parsed-args)
  "Show help for build command"
  (declare (ignore parsed-args))
  (format t "build - Build specific epsilon modules~%~%")
  (format t "Usage: epsilon build [options] [modules...]~%~%")
  (format t "Arguments:~%")
  (format t "  modules              One or more module names to build (default: epsilon.core)~%~%")
  (format t "Options:~%")
  (format t "  --force              Force compilation of all files regardless of timestamps~%")
  (format t "  --include-tests      Also build test files~%")
  (format t "  --help               Show this help message~%~%")
  (format t "Examples:~%")
  (format t "  epsilon build                         # Build epsilon.core~%")
  (format t "  epsilon build epsilon.core            # Build epsilon.core explicitly~%")
  (format t "  epsilon build epsilon.http            # Build epsilon.http module~%")
  (format t "  epsilon build --force                 # Force rebuild of epsilon.core~%"))

(defun handle-build-all-help (parsed-args)
  "Show help for build-all command"
  (declare (ignore parsed-args))
  (format t "build-all - Build all epsilon modules~%~%")
  (format t "Usage: epsilon build-all [options]~%~%")
  (format t "Options:~%")
  (format t "  --force              Force compilation of all files regardless of timestamps~%")
  (format t "  --include-tests      Also build test files~%")
  (format t "  --include-platform   Include platform-specific modules~%")
  (format t "  --help               Show this help message~%~%")
  (format t "Examples:~%")
  (format t "  epsilon build-all                     # Build all non-platform modules~%")
  (format t "  epsilon build-all --include-platform  # Build all modules including platform-specific~%")
  (format t "  epsilon build-all --force             # Force rebuild of all modules~%"))

(defun handle-bench (parsed-args)
  "Handle bench command - dynamically dispatch to benchmark:main"
  (funcall (find-symbol "MAIN" "EPSILON.TOOL.BENCHMARK") parsed-args))

(defun handle-bench-help (parsed-args)
  "Show help for bench command"
  (declare (ignore parsed-args))
  (format t "bench - Run benchmarks~%~%")
  (format t "Usage: epsilon bench [options] [benchmarks...]~%~%")
  (format t "Arguments:~%")
  (format t "  benchmarks      Specific benchmarks to run~%~%")
  (format t "Options:~%")
  (format t "  --suite SUITE   Run specific benchmark suite~%")
  (format t "  --help          Show this help message~%~%")
  (format t "Examples:~%")
  (format t "  epsilon bench                    # List available benchmarks~%")
  (format t "  epsilon bench --suite msgpack    # Run MessagePack benchmarks~%")
  (format t "  epsilon bench --suite all        # Run all benchmark suites~%"))


(defun handle-clean (parsed-args)
  "Handle clean command - remove build artifacts and cached files"
  (let* ((options (parsed-args-options parsed-args))
         (dry-run (map:get options "dry-run"))
         (verbose (map:get options "verbose"))
         (files-removed 0)
         (dirs-removed 0))
    
    (format t "~%Cleaning build artifacts...~%")
    
    ;; Clean main target directory
    (when (fs:exists-p "target")
      (when (or verbose dry-run)
        (format t "~:[Would remove~;Removing~] directory: target/~%" (not dry-run)))
      (unless dry-run
        (fs:delete-directory "target"))
      (incf dirs-removed))
    
    ;; Clean module target directories
    (when (fs:exists-p "module")
      (dolist (module-name (fs:list-dir "module"))
        (let ((module-target (path:string-path-join "module" module-name "target")))
          (when (fs:exists-p module-target)
            (when (or verbose dry-run)
              (format t "~:[Would remove~;Removing~] directory: ~A~%" (not dry-run) module-target))
            (unless dry-run
              (fs:delete-directory module-target))
            (incf dirs-removed)))))
    
    ;; Clean FASL files
    (let ((fasl-files (fs:list-files "." ".fasl")))
      (dolist (fasl-file fasl-files)
        (when (or verbose dry-run)
          (format t "~:[Would remove~;Removing~] file: ~A~%" (not dry-run) fasl-file))
        (unless dry-run
          (fs:delete-file* fasl-file))
        (incf files-removed)))
    
    ;; Clean log files
    (let ((log-files (fs:list-files "." ".log")))
      (dolist (log-file log-files)
        (when (or verbose dry-run)
          (format t "~:[Would remove~;Removing~] file: ~A~%" (not dry-run) log-file))
        (unless dry-run
          (fs:delete-file* log-file))
        (incf files-removed)))
    
    (if dry-run
        (format t "~%Dry run complete: Would remove ~D files and ~D directories~%" 
                files-removed dirs-removed)
        (format t "~%Clean complete: Removed ~D files and ~D directories~%" 
                files-removed dirs-removed))))

(defun handle-clean-help (parsed-args)
  "Show help for clean command"
  (declare (ignore parsed-args))
  (format t "clean - Remove build artifacts and cached files~%~%")
  (format t "Usage: epsilon [global-options] clean [options]~%~%")
  (format t "Global Options:~%")
  (format t "  --log SPEC           Configure logging (e.g., 'debug:epsilon.sys.fs')~%")
  (format t "  --verbose            Enable debug logging~%")
  (format t "  --quiet              Only show warnings and errors~%~%")
  (format t "Options:~%")
  (format t "  --dry-run            Show what would be removed without actually removing~%")
  (format t "  --verbose            Show each file/directory being removed~%")
  (format t "  --help               Show this help message~%~%")
  (format t "Removes:~%")
  (format t "  - target/ directories (build outputs)~%")
  (format t "  - *.fasl files (compiled Lisp)~%")
  (format t "  - *.log files (log files)~%")
  (format t "  - Temporary files (*~~, *.bak)~%~%")
  (format t "Examples:~%")
  (format t "  epsilon clean                    # Remove all build artifacts~%")
  (format t "  epsilon clean --dry-run          # Show what would be removed~%")
  (format t "  epsilon clean --verbose          # Show each file being removed~%"))


(defun handle-release-help (parsed-args)
  "Show help for release command"
  (declare (ignore parsed-args))
  (format t "release - Build complete Epsilon release with EPK packages and distributions~%~%")
  (format t "Usage: epsilon [global-options] release --version VERSION [options]~%~%")
  (format t "Global Options:~%")
  (format t "  --log SPEC           Configure logging~%")
  (format t "  --verbose            Enable debug logging~%")
  (format t "  --quiet              Only show warnings and errors~%~%")
  (format t "Options:~%")
  (format t "  --version VERSION    Release version (required)~%")
  (format t "  --modules MODULES    Comma-separated list of modules to include~%")
  (format t "  --platforms SPEC     Comma-separated platform specs (e.g. linux-x86-64,darwin-arm64)~%")
  (format t "  --output DIR         Output directory (default: target/release)~%")
  (format t "  --help               Show this help message~%~%")
  (format t "Generates:~%")
  (format t "  - EPK files for all modules~%")
  (format t "  - Platform-specific distributions (tar.gz)~%")
  (format t "  - Release index with package metadata~%")
  (format t "  - SHA256 checksums for all artifacts~%~%")
  (format t "Examples:~%")
  (format t "  epsilon release --version 1.0.0~%")
  (format t "  epsilon release --version 1.0.0 --modules epsilon.core,epsilon.net~%")
  (format t "  epsilon release --version 1.0.0 --platforms linux-x86-64~%"))

(defun handle-package (parsed-args)
  "Handle package command"
  (let* ((args (parsed-args-arguments parsed-args))
         (subcommand (first args))
         (rest-args (rest args)))
    
    (cond
      ((string= subcommand "list")
       (let ((packages (package:list-packages)))
         (dolist (pkg packages)
           (if (consp pkg)
               (format t "~A (~A)~%" (car pkg) (cdr pkg))
               (format t "~A~%" pkg)))))
      
      ((string= subcommand "info")
       (let ((pkg-name (first rest-args)))
         (if pkg-name
             (let ((info (package:package-info pkg-name)))
               (if info
                   (format t "Package: ~A~%Version: ~A~%Description: ~A~%"
                           (getf info :name)
                           (getf info :version "unknown")
                           (getf info :description "No description"))
                   (format t "Package ~A not found~%" pkg-name)))
             (format t "Error: Package name required~%"))))
      
      ((string= subcommand "install")
       (let ((pkg-name (first rest-args)))
         (if pkg-name
             (package:install-package pkg-name :verbose t)
             (format t "Error: Package name required~%"))))
      
      ((string= subcommand "search")
       (let ((query (first rest-args)))
         (if query
             (let ((results (package:search-packages query)))
               (dolist (result results)
                 (format t "~A - ~A~%" 
                         (getf result :name)
                         (getf result :description "No description"))))
             (format t "Error: Search query required~%"))))
      
      (t
       (handle-package-help parsed-args)))))

(defun handle-package-help (parsed-args)
  "Show help for package command"
  (declare (ignore parsed-args))
  (format t "package - Manage EPK packages~%~%")
  (format t "Usage: epsilon [global-options] package <subcommand> [options]~%~%")
  (format t "Subcommands:~%")
  (format t "  list                 List all packages~%")
  (format t "  info <package>       Show package information~%")
  (format t "  install <package>    Install a package~%")
  (format t "  search <query>       Search for packages~%")
  (format t "  help                 Show this help message~%~%")
  (format t "Examples:~%")
  (format t "  epsilon package list                     # List all packages~%")
  (format t "  epsilon package info epsilon.parsing     # Show package info~%")
  (format t "  epsilon package install epsilon.yaml     # Install a package~%")
  (format t "  epsilon package search http              # Search for packages~%"))

(defun handle-help (parsed-args)
  "Show help for a specific command or general help"
  (declare (ignore parsed-args))
  (print-usage))

(defun print-usage ()
  "Print usage information"
  (format t "Usage: epsilon [global-options] <command> [arguments]~%~%")
  (format t "Available commands:~%")
  (map:each (lambda (name tool-info)
              (format t "  ~12A ~A~@[ (module: ~A)~]~%"
                      name
                      (tool-info-description tool-info)
                      (when (and (tool-info-module tool-info)
                                 (not (tool-info-available-p tool-info)))
                        "not available")))
            *tool-registry*)
  (format t "~%Global options:~%")
  (format t "  --help              Show this help message~%")
  (format t "  --version           Show version information~%")
  (format t "  --verbose           Enable verbose output~%"))

;;; Main Entry Point

(defun main ()
  "Main entry point for development tools"
  ;; Initialize registry
  (initialize-tool-registry)
  
  ;; Parse command line
  (let* ((args (rest sb-ext:*posix-argv*))
         (parsed (parse-command-line args)))
    
    ;; Handle special cases
    (cond
      ;; No command - show help
      ((null (parsed-args-command parsed))
       (print-usage))
      
      ;; Help command
      ((string= (parsed-args-command parsed) "help")
       (handle-help parsed))
      
      ;; Version (only if no command specified)
      ((and (null (parsed-args-command parsed))
            (map:get (parsed-args-options parsed) "version"))
       (format t "Epsilon development tools v2.0.0~%"))
      
      ;; Dispatch to tool
      (t
       (dispatch-tool (parsed-args-command parsed) parsed)))))

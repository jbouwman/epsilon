;;;; Development Tool Dispatcher
;;;;
;;;; This module serves as the main registration and extension point for
;;;; development tools. It provides a lightweight command dispatcher that
;;;; demand-loads tool modules as needed, keeping the core bootstrap minimal.

(defpackage #:epsilon.tool.dev
  (:use #:cl)
  (:local-nicknames
   (#:map #:epsilon.lib.map)
   (#:str #:epsilon.lib.string)
   (#:build #:epsilon.tool.build))
  (:export
   #:main
   #:register-tool
   #:list-tools
   #:initialize-tool-registry
   #:handle-build-help
   #:handle-bench-help
   #:parsed-args))

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
           ("bench" . ,(make-tool-info
                        :name "bench"
                        :module nil  ; benchmark is in core
                        :function (read-from-string "epsilon.tool.benchmark:main")
                        :help-function 'handle-bench-help
                        :description "Run benchmarks"))
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
                      (list "epsilon.core"))))
    (dolist (module modules)
      (format t ";;; Building module: ~A~%" module)
      (build:build module))))

(defun handle-build-help (parsed-args)
  "Show help for build command"
  (declare (ignore parsed-args))
  (format t "build - Build epsilon modules~%~%")
  (format t "Usage: epsilon build [modules...]~%~%")
  (format t "Arguments:~%")
  (format t "  modules         One or more module names to build (default: epsilon.core)~%~%")
  (format t "Options:~%")
  (format t "  --force         Force compilation of all files regardless of timestamps~%")
  (format t "  --help          Show this help message~%~%")
  (format t "Examples:~%")
  (format t "  epsilon build                    # Build epsilon.core~%")
  (format t "  epsilon build epsilon.core      # Build epsilon.core explicitly~%")
  (format t "  epsilon build epsilon.http      # Build epsilon.http module~%")
  (format t "  epsilon build --force            # Force rebuild of epsilon.core~%"))

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
  (let* ((args (or #+sbcl (rest sb-ext:*posix-argv*)
                   '()))
         (parsed (parse-command-line args)))
    
    ;; Handle special cases
    (cond
      ;; No command - show help
      ((null (parsed-args-command parsed))
       (print-usage))
      
      ;; Help command
      ((string= (parsed-args-command parsed) "help")
       (handle-help parsed))
      
      ;; Version
      ((map:get (parsed-args-options parsed) "version")
       (format t "Epsilon development tools v2.0.0~%"))
      
      ;; Dispatch to tool
      (t
       (dispatch-tool (parsed-args-command parsed) parsed)))))
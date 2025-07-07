(defpackage #:epsilon.tool.dev
  (:use
   #:cl)
  (:local-nicknames
   (#:map #:epsilon.lib.map)
   (#:seq #:epsilon.lib.sequence)
   (#:str #:epsilon.lib.string)
   (#:pkg #:epsilon.sys.pkg)
   (#:build #:epsilon.tool.build)
   (#:test #:epsilon.tool.test)
   (#:bench #:epsilon.tool.benchmark)
   (#:re #:epsilon.lib.regex)
   (#:uri #:epsilon.lib.uri))
  ;; Try to use logging, but don't fail if not available
  #+nil (:local-nicknames (#:log #:epsilon.lib.log))
  (:export
   :main))

(in-package #:epsilon.tool.dev)

;;; Argument Parsing

(defclass parsed-args ()
  ((global-options :initarg :global-options :accessor global-options :initform (map:make-map))
   (command :initarg :command :accessor command)
   (command-options :initarg :command-options :accessor command-options :initform (map:make-map))))

(defun parse-arguments (argv)
  "Parse command line arguments into global options, command, and command options"
  (let ((global-options (map:make-map))
        (command nil)
        (command-options (map:make-map))
        (parsing-global t)
        (i 0))
    
    (loop while (< i (length argv))
          for arg = (nth i argv)
          do (cond
               ;; Global option
               ((and parsing-global (str:starts-with-p arg "--"))
                (let ((option-name (subseq arg 2))
                      (option-value (when (< (1+ i) (length argv))
                                      (nth (1+ i) argv))))
                  (if (and option-value (not (str:starts-with-p option-value "--")))
                      (progn
                        (map:assoc! global-options option-name option-value)
                        (incf i 2))
                      (progn
                        (map:assoc! global-options option-name t)
                        (incf i)))))
               
               ;; Command found
               ((and parsing-global (not (str:starts-with-p arg "--")))
                (setf command arg)
                (setf parsing-global nil)
                (incf i))
               
               ;; Command option
               ((str:starts-with-p arg "--")
                (let ((option-name (subseq arg 2))
                      (option-value (when (< (1+ i) (length argv))
                                      (nth (1+ i) argv))))
                  (if (and option-value (not (str:starts-with-p option-value "--")))
                      (progn
                        (map:assoc! command-options option-name option-value)
                        (incf i 2))
                      (progn
                        (map:assoc! command-options option-name t)
                        (incf i)))))
               
               ;; Positional argument for command
               (t
                (map:assoc! command-options "module" arg)
                (incf i))))
    
    (make-instance 'parsed-args
                   :global-options global-options
                   :command command
                   :command-options command-options)))

(defun split-comma-list (value)
  "Split comma-separated values and trim whitespace"
  (when value
    (mapcar #'str:trim (str:split #\, value))))

(defun try-log (level format-string &rest args)
  "Try to log, fall back to format if logging not available"
  (handler-case
      (let ((logger (find-symbol "GET-LOGGER" :epsilon.lib.log)))
        (when logger
          (let ((log-fn (find-symbol (string-upcase level) :epsilon.lib.log)))
            (when log-fn
              (apply log-fn format-string args)
              (return-from try-log t)))))
    (error ()))
  ;; Fallback to regular format
  (apply #'format t format-string args)
  (terpri))

(defun apply-global-options (global-options)
  "Apply global options like logging configuration"
  (let ((log-config (map:get global-options "log")))
    (when log-config
      (handler-case
          (let ((config-fn (find-symbol "CONFIGURE-FROM-STRING" :epsilon.lib.log)))
            (when config-fn
              (funcall config-fn log-config)
              (try-log :info "Logging configured: ~A" log-config)))
        (error ()
          (format t ";;; Log configuration requested but logging not available: ~A~%" log-config)))))
  
  (let ((verbose (map:get global-options "verbose")))
    (when verbose
      (handler-case
          (let ((set-level-fn (find-symbol "SET-LEVEL" :epsilon.lib.log)))
            (when set-level-fn
              (funcall set-level-fn "root" :debug)
              (try-log :debug "Verbose mode enabled")))
        (error ()
          (format t ";;; Verbose mode requested but logging not available~%")))))
  
  (let ((quiet (map:get global-options "quiet")))
    (when quiet
      (handler-case
          (let ((set-level-fn (find-symbol "SET-LEVEL" :epsilon.lib.log)))
            (when set-level-fn
              (funcall set-level-fn "root" :warn)
              (try-log :warn "Quiet mode enabled")))
        (error ()
          (format t ";;; Quiet mode requested but logging not available~%"))))))

;;; Commands

(defun run-build (parsed-args)
  "Handle build command"
  (let* ((options (command-options parsed-args))
         (module (or (map:get options "module")
                     (error "Module name required. Usage: build --module <module-name>"))))
    (format t ";;; Building module: ~A~%" module)
    (build:build module)))

(defun run-test (parsed-args)
  "Handle test command with support for multiple modules and name patterns"
  (let* ((options (command-options parsed-args))
         (modules (split-comma-list (map:get options "module")))
         (test-pattern (map:get options "test"))
         (package-pattern (map:get options "package"))
         (format (intern (string-upcase (or (map:get options "format") "shell")) :keyword))
         (file (map:get options "file")))
    
    (cond
      (modules
       ;; Module-specific testing
       (format t ";;; Running tests for modules: ~{~A~^, ~}~%" modules)
       (when test-pattern
         (format t ";;; Filtering tests matching: ~A~%" test-pattern))
       
       ;; Build all modules first
       (dolist (module modules)
         (format t ";;; Building module: ~A~%" module)
         (build:build module))
       
       ;; Run tests for each module
       (let ((all-success t))
         (dolist (module modules)
           (format t ";;; Testing module: ~A~%" module)
           (let ((result (test:run :module module 
                                  :name test-pattern
                                  :package package-pattern
                                  :format format
                                  :file file)))
             (unless (test:success-p result)
               (setf all-success nil))))
         
         (unless all-success
           (sb-posix:exit 1))))
      
      (t
       ;; No module specified - run all available tests
       (format t ";;; Running all available tests~%")
       (when test-pattern
         (format t ";;; Filtering tests matching: ~A~%" test-pattern))
       (when package-pattern
         (format t ";;; Filtering packages matching: ~A~%" package-pattern))
       
       (let ((result (test:run :module nil
                              :name test-pattern
                              :package package-pattern
                              :format format
                              :file file)))
         (unless (test:success-p result)
           (sb-posix:exit 1)))))))

(defun run-benchmark (parsed-args)
  "Handle benchmark command"
  (let* ((options (command-options parsed-args))
         (suite (map:get options "suite"))
         (benchmarks (split-comma-list (map:get options "benchmarks"))))
    
    (cond
      ;; Run benchmark suite
      (suite
       (format t ";;; Running benchmark suite: ~A~%" suite)
       (cond
         ((string= suite "msgpack")
          (load "module/core/tests/lib/msgpack-binary-benchmark.lisp")
          (funcall (find-symbol "RUN-MSGPACK-BENCHMARKS" "EPSILON.LIB.MSGPACK.BINARY.BENCHMARK")))
         ((string= suite "all")
          (load "module/core/tests/lib/msgpack-binary-benchmark.lisp")
          (funcall (find-symbol "RUN-COMPLETE-BENCHMARK-SUITE" "EPSILON.LIB.MSGPACK.BINARY.BENCHMARK")))
         (t
          (error "Unknown benchmark suite: ~A. Available: msgpack, all" suite))))
      
      ;; Run specific benchmarks
      (benchmarks
       (format t ";;; Running benchmarks: ~{~A~^, ~}~%" benchmarks)
       (let* ((benchmark-names (mapcar (lambda (name) (intern (string-upcase name) :keyword))
                                      benchmarks))
              (results (mapcar (lambda (name)
                                (let ((fn (bench:get-benchmark name)))
                                  (if fn
                                      (funcall fn)
                                      (progn
                                        (format t ";;; Warning: Benchmark '~A' not found, skipping~%" name)
                                        nil))))
                              benchmark-names))
              (valid-results (remove nil results)))
         
         (if valid-results
             (progn
               (dolist (result valid-results)
                 (bench:format-benchmark-result result))
               (format t "~%")
               (bench:format-comparison (apply #'bench:compare-benchmarks valid-results)))
             (format t ";;; Warning: No valid benchmarks found~%"))))
      
      ;; List available benchmarks
      (t
       (let ((available (bench:list-benchmarks)))
         (if available
             (progn
               (format t "Available benchmarks:~%")
               (dolist (name available)
                 (format t "  ~A~%" name)))
             (format t "No benchmarks registered.~%")))))))

(defvar *commands*
  (map:make-map "build" #'run-build
                "test" #'run-test
                "benchmark" #'run-benchmark
                "bench" #'run-benchmark))

(defun print-usage ()
  "Print usage information"
  (format t "Usage: epsilon [global-options] <command> [command-options]~%~%")
  (format t "Global Options:~%")
  (format t "  --log SPEC     Configure logging (e.g., 'debug:epsilon.*,trace:epsilon.lib.yaml')~%")
  (format t "  --verbose      Enable debug logging~%")
  (format t "  --quiet        Enable only warning+ logging~%~%")
  (format t "Commands:~%")
  (format t "  build --module MODULE                     Build the specified module~%")
  (format t "  test [options]                            Run tests~%")
  (format t "    --module MODULE1[,MODULE2,...]  Build and test specific modules~%")
  (format t "    --test PATTERN                  Filter tests by name pattern (supports wildcards)~%")
  (format t "    --package PATTERN               Filter tests by package pattern~%")
  (format t "    --format FORMAT                 Output format (shell, repl, junit)~%")
  (format t "    --file FILE                     Write results to file~%")
  (format t "  benchmark [options]                       Run benchmarks~%")
  (format t "    --suite SUITE      Run benchmark suite (msgpack, all)~%")
  (format t "    --benchmarks LIST  Run specific benchmarks (comma-separated)~%~%")
  (format t "Examples:~%")
  (format t "  epsilon test                              # Run all available tests~%")
  (format t "  epsilon test --test 'parse-*'             # Run all tests matching pattern~%")
  (format t "  epsilon --log 'debug:epsilon.lib.*' test --module epsilon.core --test 'parse-*'~%")
  (format t "  epsilon test --module 'epsilon.core,lsp' --format junit --file results.xml~%")
  (format t "  epsilon benchmark --suite all~%"))

(defun main ()
  "Main entry point with sophisticated argument parsing"
  (let ((argv (cdr sb-ext:*posix-argv*)))
    (handler-case
        (if (or (null argv) 
                (member (first argv) '("help" "--help" "-h") :test #'string=))
            (print-usage)
            (let ((parsed (parse-arguments argv)))
              (unless (command parsed)
                (error "No command specified"))
              
              (apply-global-options (global-options parsed))
              
              (let ((command-fn (map:get *commands* (command parsed))))
                (unless command-fn
                  (error "Unknown command: ~A" (command parsed)))
                
                (format t ";;; Executing command: ~A~%" (command parsed))
                (funcall command-fn parsed))))
      (error (e)
        (format t "Error: ~A~%" e)
        (sb-posix:exit 1)))))
      

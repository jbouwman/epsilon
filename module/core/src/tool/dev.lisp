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
  (:export
   :main))

(in-package #:epsilon.tool.dev)

(defun run-build (&rest args)
  "Handle build command with required module name"
  (unless (and args (not (str:starts-with-p (first args) "--")))
    (error "Module name required. Usage: build <module-name> [options]"))
  (let ((module (first args))
        (build-args (cdr args)))
    (apply #'build:build module (as-keys build-args))))

(defun run-test (&rest args)
  "Handle test command with required module name"
  (unless (and args (not (str:starts-with-p (first args) "--")))
    (error "Module name required. Usage: test <module-name> [options]"))
  (let ((module (first args))
        (test-args (cdr args)))
    ;; Build first
    (build:build module)
    ;; Then run tests
    (let ((result (apply #'test:run test-args)))
      (unless (test:success-p result)
        (sb-posix:exit 1)))))

(defun run-benchmark (&rest args)
  "Handle benchmark command"
  (let ((benchmark-args args))
    (cond
      ;; No arguments - list available benchmarks
      ((null benchmark-args)
       (let ((benchmarks (bench:list-benchmarks)))
         (if benchmarks
             (progn
               (format t "Available benchmarks:~%")
               (dolist (name benchmarks)
                 (format t "  ~A~%" name)))
             (format t "No benchmarks registered.~%"))))
      
      ;; :SUITE keyword - run a predefined benchmark suite
      ((member :suite benchmark-args)
       (let* ((suite-position (position :suite benchmark-args))
              (suite-name (when (and suite-position 
                                    (< (1+ suite-position) (length benchmark-args)))
                           (nth (1+ suite-position) benchmark-args))))
         (cond
           ((null suite-name)
            (format t "No suite name provided. Available suites: msgpack, all~%"))
           ((string= suite-name "msgpack")
            (format t "Running MessagePack benchmark suite...~%")
            (load "module/core/tests/lib/msgpack-binary-benchmark.lisp")
            (funcall (find-symbol "RUN-MSGPACK-BENCHMARKS" "EPSILON.LIB.MSGPACK.BINARY.BENCHMARK")))
           ((string= suite-name "all")
            (format t "Running all benchmark suites...~%")
            (load "module/core/tests/lib/msgpack-binary-benchmark.lisp")
            (funcall (find-symbol "RUN-COMPLETE-BENCHMARK-SUITE" "EPSILON.LIB.MSGPACK.BINARY.BENCHMARK")))
           (t
            (format t "Unknown benchmark suite: ~A~%" suite-name)
            (format t "Available suites: msgpack, all~%")))))
      
      ;; Single benchmark name
      ((= (length benchmark-args) 1)
       (let* ((benchmark-name (intern (string-upcase (first benchmark-args)) :keyword))
              (benchmark-fn (bench:get-benchmark benchmark-name)))
         (if benchmark-fn
             (let ((result (funcall benchmark-fn)))
               (bench:format-benchmark-result result))
             (format t "Benchmark '~A' not found.~%" (first benchmark-args)))))
      
      ;; Multiple benchmark names for comparison
      (t
       (let* ((benchmark-names (mapcar (lambda (name) (intern (string-upcase name) :keyword))
                                      benchmark-args))
              (results (mapcar (lambda (name)
                                (let ((fn (bench:get-benchmark name)))
                                  (if fn
                                      (funcall fn)
                                      (progn
                                        (format t "Warning: Benchmark '~A' not found, skipping.~%" name)
                                        nil))))
                              benchmark-names))
              (valid-results (remove nil results)))
         
         (if valid-results
             (progn
               (dolist (result valid-results)
                 (bench:format-benchmark-result result))
               (format t "~%")
               (bench:format-comparison (apply #'bench:compare-benchmarks valid-results)))
             (format t "No valid benchmarks found.~%")))))))

(defun run-check-updates (&rest args)
  "Check for available updates"
  (declare (ignore args))
  (format t "Checking for epsilon updates...~%")
  ;; Simplified version check using system curl command
  (handler-case
      (let* ((curl-cmd (format nil "curl -s 'https://api.github.com/repos/jbouwman/epsilon/releases/latest'"))
             (current-version "0.1.0"))  ; Current version
        ;; Try to run curl command and capture output
        (with-open-stream (stream (sb-ext:process-output 
                                   (sb-ext:run-program "/bin/sh" 
                                                       (list "-c" curl-cmd)
                                                       :output :stream
                                                       :wait nil)))
          (let ((response (with-output-to-string (s)
                            (loop for line = (read-line stream nil nil)
                                  while line do (write-line line s)))))
            (let ((tag-start (search "\"tag_name\":" response)))
              (if tag-start
                  (let* ((value-start (+ tag-start 12))
                         (quote-start (position #\" response :start value-start))
                         (quote-end (position #\" response :start (1+ quote-start)))
                         (latest-version (subseq response (1+ quote-start) quote-end)))
                    (setf latest-version (if (str:starts-with-p latest-version "v")
                                            (subseq latest-version 1)
                                            latest-version))
                    (format t "Current version: ~A~%" current-version)
                    (format t "Latest version: ~A~%" latest-version)
                    (if (string= current-version latest-version)
                        (progn
                          (format t "Epsilon is up to date~%")
                          (sb-posix:exit 0))
                        (progn
                          (format t "Update available: ~A -> ~A~%" current-version latest-version)
                          (sb-posix:exit 1))))
                  (progn
                    (format t "Failed to parse update information~%")
                    (sb-posix:exit 1)))))))
    (error (e)
      (format t "Error checking for updates: ~A~%" e)
      (format t "Please check your internet connection and try again.~%")
      (sb-posix:exit 1))))

(defun run-version-info (&rest args)
  "Show version and installation information"
  (declare (ignore args))
  (format t "Current version: 0.1.0~%")
  (format t "Installation path: ~A~%" (first sb-ext:*posix-argv*))
  (format t "Update channel: stable~%")
  (format t "Auto-check updates: false~%"))

(defun run-update (&rest args)
  "Update epsilon to latest version"
  (declare (ignore args))
  (format t "Epsilon self-update is not yet implemented.~%")
  (format t "Please visit https://github.com/jbouwman/epsilon/releases for manual updates.~%")
  (sb-posix:exit 1))

(defvar *commands*
  (map:make-map "build" #'run-build
                "test" #'run-test
                "benchmark" #'run-benchmark
                "bench" #'run-benchmark
                "check-updates" #'run-check-updates
                "version-info" #'run-version-info
                "update" #'run-update))

(defun as-keys (list)
  (mapcar (lambda (element)
            (cond ((keywordp element)
                   element)  ; Already a keyword, pass through
                  ((stringp element)
                   (if (str:starts-with-p element "--")
                       (intern (string-upcase (subseq element 2)) :keyword)
                       element))
                  (t
                   element)))
          list))

(defun show-help ()
  "Show available commands and usage"
  (format t "epsilon - Common Lisp utility library and development tools~%~%")
  (format t "Usage: epsilon <command> [options]~%~%")
  (format t "Available commands:~%")
  (format t "  build <module>      Build the specified module~%")
  (format t "  test <module>       Run tests for the specified module~%")
  (format t "  benchmark           List available benchmarks~%")
  (format t "  benchmark <name>    Run specific benchmark~%")
  (format t "  bench               Alias for benchmark~%")
  (format t "  check-updates       Check for available updates~%")
  (format t "  version-info        Show version and installation information~%")
  (format t "  update              Update epsilon to latest version~%")
  (format t "~%"))

(defun main ()
  (let ((argv (cdr sb-ext:*posix-argv*)))
    (cond
      ;; No arguments or help requested
      ((or (null argv) 
           (member (first argv) '("help" "--help" "-h") :test #'string=))
       (show-help))
      ;; Check for version flags
      ((member (first argv) '("--version" "-v") :test #'string=)
       (run-version-info))
      ;; Check for update flags  
      ((member (first argv) '("--check-updates") :test #'string=)
       (run-check-updates))
      ;; Check for update command
      ((member (first argv) '("--update") :test #'string=)
       (run-update))
      ;; Regular command processing
      (t
       (let ((command (map:get *commands* (car argv))))
         (unless command
           (format t "Unknown command: ~A~%~%" (car argv))
           (show-help)
           (sb-posix:exit 1))
         (apply command (as-keys (cdr argv))))))))
      

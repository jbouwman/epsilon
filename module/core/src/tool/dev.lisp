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
        #+win32
        (sb-ext:exit :code 1)
        #-win32
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

(defvar *commands*
  (map:make-map "build" #'run-build
                "test" #'run-test
                "benchmark" #'run-benchmark
                "bench" #'run-benchmark))

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

(defun main ()
  (let ((argv (cdr sb-ext:*posix-argv*)))
    (unless argv
      (error "No argument provided, must be one of ~A" (map:keys *commands*)))
    (let ((command (map:get *commands* (car argv))))
      (unless command
        (error "Unknown command ~A, must be one of ~A" command (map:keys *commands*)))
      (apply command (as-keys (cdr argv))))))
      

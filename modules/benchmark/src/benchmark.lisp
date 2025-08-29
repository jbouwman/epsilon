;;;; Performance Benchmarking Framework
;;;;
;;;; This module provides a benchmarking framework for measuring and
;;;; comparing performance of code segments.

(defpackage epsilon.tool.benchmark
  (:use cl)
  (:local-nicknames
   (time epsilon.time)
   (map epsilon.map)
   (argparse epsilon.argparse)
   (main epsilon.main)
   (fmt epsilon.format))
  (:export
   ;; Main API
   main  ; Entry point for dev.lisp dispatcher
   benchmark
   run-benchmark
   compare-benchmarks
   
   ;; Result inspection
   benchmark-result
   benchmark-result-p
   benchmark-result-name
   benchmark-result-time
   benchmark-result-iterations
   benchmark-result-ops-per-sec
   benchmark-result-memory
   benchmark-result-notes
   benchmark-result-samples
   benchmark-result-mean
   benchmark-result-median
   benchmark-result-std-dev
   benchmark-result-percentiles
   benchmark-result-confidence-interval
   
   ;; Configuration
   *default-min-time*
   *default-max-iterations*
   *collect-memory-stats*
   *warmup-time*
   *confidence-level*
   *bootstrap-iterations*
   
   ;; Convenience macros
   defbenchmark
   register-benchmark
   get-benchmark
   list-benchmarks
   run-benchmark-suite
   
   ;; Baseline management
   save-baseline
   load-baseline
   compare-with-baseline
   
   ;; Formatting
   format-benchmark-result
   format-comparison
   format-as-json
   format-as-csv
   
   ;; Algorithm comparison
   compare-implementations
   
   ;; Timing utilities
   with-benchmark-timing))

(in-package :epsilon.tool.benchmark)

(defclass benchmark ()
  ())

;;;; A benchmarking tool for performance measurement and comparison

;;; Configuration

(defparameter *default-min-time* 1.0
  "Minimum time to run each benchmark in seconds")

(defparameter *default-max-iterations* 10000000
  "Maximum number of iterations for any benchmark")

(defparameter *collect-memory-stats* nil
  "Whether to collect memory allocation statistics")

(defparameter *warmup-time* 0.1
  "Time to spend warming up before measurement")

(defparameter *confidence-level* 0.95
  "Confidence level for statistical intervals (0.95 = 95%)")

(defparameter *bootstrap-iterations* 1000
  "Number of bootstrap iterations for confidence intervals")

;;; Data structures

(defstruct benchmark-result
  "Results from running a benchmark"
  name           ; benchmark name
  time           ; total time taken in seconds
  iterations     ; number of iterations performed
  ops-per-sec    ; operations per second
  memory         ; memory allocation info (bytes allocated)
  notes          ; additional notes or metadata
  samples        ; list of individual sample times
  mean           ; mean time per operation
  median         ; median time per operation
  std-dev        ; standard deviation
  percentiles    ; alist of percentiles (50, 90, 95, 99)
  confidence-interval) ; confidence interval for mean

;;; Statistical utilities

(defun calculate-percentile (sorted-data percentile)
  "Calculate the percentile value from sorted data"
  (let* ((n (length sorted-data))
         (k (* percentile (1- n)))
         (f (floor k))
         (c (ceiling k)))
    (if (= f c)
        (elt sorted-data f)
        (/ (+ (elt sorted-data f) (elt sorted-data c)) 2))))

(defun calculate-statistics (samples)
  "Calculate statistical measures from samples"
  (when (null samples)
    (return-from calculate-statistics nil))
  
  (let* ((n (length samples))
         (sorted (sort (copy-list samples) #'<))
         (sum (reduce #'+ samples))
         (mean (/ sum n))
         (median (calculate-percentile sorted 0.5))
         (variance (/ (reduce #'+ (mapcar (lambda (x) (expt (- x mean) 2)) samples)) n))
         (std-dev (sqrt variance))
         (percentiles (list (cons 50 median)
                           (cons 90 (calculate-percentile sorted 0.90))
                           (cons 95 (calculate-percentile sorted 0.95))
                           (cons 99 (calculate-percentile sorted 0.99)))))
    (list :mean mean
          :median median
          :std-dev std-dev
          :percentiles percentiles)))

(defun bootstrap-confidence-interval (samples &key (confidence *confidence-level*) 
                                                   (iterations *bootstrap-iterations*))
  "Calculate bootstrap confidence interval for the mean"
  (when (null samples)
    (return-from bootstrap-confidence-interval nil))
  
  (let* ((n (length samples))
         (bootstrap-means
          (loop repeat iterations
                collect (let ((resample (loop repeat n
                                             collect (elt samples (random n)))))
                         (/ (reduce #'+ resample) n))))
         (sorted-means (sort bootstrap-means #'<))
         (alpha (- 1 confidence))
         (lower-idx (floor (* (/ alpha 2) iterations)))
         (upper-idx (floor (* (- 1 (/ alpha 2)) iterations))))
    (cons (elt sorted-means lower-idx)
          (elt sorted-means upper-idx))))

;;; Core benchmarking

(defmacro benchmark (name &body body)
  "Define a benchmark with NAME that executes BODY"
  `(lambda ()
     (run-benchmark-internal ,name (lambda () ,@body))))

(defun run-benchmark (benchmark-fn &key 
                                     (name "anonymous")
                                     (min-time *default-min-time*)
                                     (max-iterations *default-max-iterations*)
                                     (collect-memory *collect-memory-stats*)
                                     (warmup-time *warmup-time*))
  "Run a benchmark function and return performance results with statistical analysis"
  ;; Warmup phase
  (let ((warmup-end (+ (get-internal-real-time)
                      (* warmup-time internal-time-units-per-second))))
    (loop while (< (get-internal-real-time) warmup-end)
          do (funcall benchmark-fn)))
  
  ;; Determine appropriate batch size
  (let* ((test-start (get-internal-real-time))
         (_ (funcall benchmark-fn))
         (test-end (get-internal-real-time))
         (single-time (/ (- test-end test-start) internal-time-units-per-second))
         (batch-size (max 1 (min 1000 (floor (/ 0.001 (max single-time 1e-9))))))
         (samples nil)
         (total-iterations 0)
         (start-memory (when collect-memory (sb-ext:get-bytes-consed))))
    
    (declare (ignore _))
    
    ;; Collect samples
    (let ((benchmark-end (+ (get-internal-real-time)
                           (* min-time internal-time-units-per-second))))
      (loop while (and (< (get-internal-real-time) benchmark-end)
                       (< total-iterations max-iterations)
                       (< (length samples) 1000)) ; Limit number of samples
            do (let ((sample-start (get-internal-real-time)))
                 (dotimes (i batch-size)
                   (funcall benchmark-fn))
                 (let* ((sample-end (get-internal-real-time))
                        (sample-time (/ (- sample-end sample-start)
                                      internal-time-units-per-second
                                      batch-size)))
                   (push sample-time samples)
                   (incf total-iterations batch-size)))))
    
    ;; Calculate statistics
    (let* ((stats (calculate-statistics samples))
           (total-time (reduce #'+ samples))
           (mean-time (getf stats :mean))
           (ops-per-sec (if (and mean-time (> mean-time 0))
                           (/ 1 mean-time)
                           most-positive-fixnum))
           (end-memory (when collect-memory (sb-ext:get-bytes-consed)))
           (memory-used (when (and start-memory end-memory)
                         (- end-memory start-memory)))
           (confidence (bootstrap-confidence-interval samples)))
      
      (make-benchmark-result
       :name name
       :time total-time
       :iterations total-iterations
       :ops-per-sec ops-per-sec
       :memory memory-used
       :samples samples
       :mean mean-time
       :median (getf stats :median)
       :std-dev (getf stats :std-dev)
       :percentiles (getf stats :percentiles)
       :confidence-interval confidence))))

(defun run-benchmark-internal (name fn)
  "Internal function to run a benchmark"
  (run-benchmark fn :name name))

(defun compare-benchmarks (&rest benchmark-results)
  "Compare multiple benchmark results measuring the SAME operation"
  (when benchmark-results
    ;; Group results by name to ensure we're comparing like with like
    (let ((grouped (make-hash-table :test #'equal)))
      (dolist (result benchmark-results)
        (push result (gethash (benchmark-result-name result) grouped nil)))
      
      ;; If all results have the same name, do the comparison
      (if (= (hash-table-count grouped) 1)
          (let* ((fastest (reduce (lambda (a b) 
                                   (if (> (benchmark-result-ops-per-sec a)
                                         (benchmark-result-ops-per-sec b))
                                       a b))
                                 benchmark-results))
                 (fastest-ops (benchmark-result-ops-per-sec fastest))
                 (comparisons (mapcar (lambda (result)
                                       (let ((ratio (if (> (benchmark-result-ops-per-sec result) 0)
                                                       (/ fastest-ops 
                                                         (benchmark-result-ops-per-sec result))
                                                       most-positive-fixnum)))
                                         (list :name (benchmark-result-name result)
                                               :ops-per-sec (benchmark-result-ops-per-sec result)
                                               :relative-speed ratio)))
                                     benchmark-results)))
            
            (list :fastest (benchmark-result-name fastest)
                  :comparisons comparisons))
          
          ;; If results have different names, return comparison by group
          (let ((group-comparisons nil))
            (maphash (lambda (name results)
                      (when (> (length results) 1)
                        (push (cons name (apply #'compare-benchmarks results))
                              group-comparisons)))
                    grouped)
            (if group-comparisons
                (list :grouped-comparisons group-comparisons)
                (list :error "Cannot compare benchmarks with different names" 
                      :names (loop for result in benchmark-results
                                  collect (benchmark-result-name result)))))))))

;;; Utility functions

(defun format-benchmark-result (result &optional (stream t))
  "Format a benchmark result for display with statistical information"
  (format stream "~&Benchmark: ~A~%" (benchmark-result-name result))
  (format stream "  Iterations: ~:D~%" (benchmark-result-iterations result))
  (format stream "  Total time: ~A~%" (fmt:format-duration (benchmark-result-time result)))
  
  ;; Statistical measures
  (when (benchmark-result-mean result)
    (format stream "  Mean: ~A/op~%" (fmt:format-duration (benchmark-result-mean result)))
    (format stream "  Median: ~A/op~%" (fmt:format-duration (benchmark-result-median result)))
    (format stream "  Std Dev: ~A~%" (fmt:format-duration (benchmark-result-std-dev result))))
  
  ;; Confidence interval
  (when (benchmark-result-confidence-interval result)
    (let ((ci (benchmark-result-confidence-interval result)))
      (format stream "  95% CI: [~A, ~A]/op~%" 
              (fmt:format-duration (car ci)) 
              (fmt:format-duration (cdr ci)))))
  
  ;; Percentiles
  (when (benchmark-result-percentiles result)
    (format stream "  Percentiles:~%")
    (dolist (p (benchmark-result-percentiles result))
      (format stream "    P~D: ~A/op~%" (car p) (fmt:format-duration (cdr p)))))
  
  (format stream "  Throughput: ~A~%" (fmt:format-throughput (benchmark-result-ops-per-sec result)))
  
  (when (benchmark-result-memory result)
    (format stream "  Memory: ~A allocated~%" (fmt:format-bytes (benchmark-result-memory result)))
    (when (> (benchmark-result-iterations result) 0)
      (let ((mem-per-op (/ (benchmark-result-memory result) 
                          (benchmark-result-iterations result))))
        (format stream "  Memory/op: ~A~%" (fmt:format-bytes mem-per-op)))))
  
  (when (benchmark-result-notes result)
    (format stream "  Notes: ~A~%" (benchmark-result-notes result))))

(defun format-comparison (comparison &optional (stream t))
  "Format a benchmark comparison for display"
  (cond
    ;; Handle error case
    ((getf comparison :error)
     (format stream "~&Comparison Error: ~A~%" (getf comparison :error))
     (when (getf comparison :names)
       (format stream "Attempted to compare: ~{~A~^, ~}~%" (getf comparison :names))))
    
    ;; Handle grouped comparisons
    ((getf comparison :grouped-comparisons)
     (format stream "~&Benchmark Comparisons (grouped by operation):~%")
     (dolist (group (getf comparison :grouped-comparisons))
       (format stream "~%Operation: ~A~%" (car group))
       (format-comparison (cdr group) stream)))
    
    ;; Handle single comparison
    (t
     (let ((fastest (getf comparison :fastest))
           (comparisons (getf comparison :comparisons)))
       
       (format stream "~&Benchmark Comparison:~%")
       (format stream "Fastest: ~A~%~%" fastest)
       
       (dolist (comp comparisons)
         (let ((name (getf comp :name))
               (ops (getf comp :ops-per-sec))
               (ratio (getf comp :relative-speed)))
           (format stream "~A: ~:D ops/sec" name (round ops))
           (if (< ratio 1.01)
               (format stream " (fastest)~%")
               (format stream " (~,1Fx slower)~%" ratio))))))))

;;; Algorithm comparison

(defun compare-implementations (implementations &key 
                                                 (min-time *default-min-time*)
                                                 (warmup-time *warmup-time*)
                                                 (output-format :text))
  "Compare different implementations of the same algorithm.
   IMPLEMENTATIONS should be an alist of (name . function) pairs."
  (let ((results nil))
    (dolist (impl implementations)
      (let* ((name (car impl))
             (fn (cdr impl))
             (result (run-benchmark fn 
                                   :name (format nil "~A" name)
                                   :min-time min-time
                                   :warmup-time warmup-time)))
        (push result results)))
    
    (setf results (nreverse results))
    
    ;; Sort by performance
    (setf results (sort results #'> :key #'benchmark-result-ops-per-sec))
    
    ;; Format output
    (case output-format
      (:json (format-as-json results))
      (:csv (format-as-csv results))
      (otherwise
       (format t "~&Algorithm Implementation Comparison~%")
       (format t "====================================~%~%")
       
       ;; Show individual results
       (dolist (result results)
         (format t "~A:~%" (benchmark-result-name result))
         (format t "  Mean: ~A/op~%" (fmt:format-duration (benchmark-result-mean result)))
         (format t "  Throughput: ~A~%" (fmt:format-throughput (benchmark-result-ops-per-sec result)))
         (when (benchmark-result-percentiles result)
           (format t "  P95: ~A/op~%" 
                   (fmt:format-duration (cdr (assoc 95 (benchmark-result-percentiles result))))))
         (terpri))
       
       ;; Show relative performance
       (when (> (length results) 1)
         (let ((fastest (first results)))
           (format t "Relative Performance:~%")
           (dolist (result results)
             (let ((ratio (/ (benchmark-result-mean result)
                           (benchmark-result-mean fastest))))
               (format t "  ~A: " (benchmark-result-name result))
               (if (eq result fastest)
                   (format t "1.00x (baseline)~%")
                   (format t "~,2Fx slower~%" ratio))))))))
    
    results))

;;; Benchmark suite support

(defvar *benchmark-registry* (map:make-map)
  "Registry of named benchmarks")

(defun register-benchmark (name benchmark-fn)
  "Register a named benchmark function"
  (setf *benchmark-registry* 
        (map:assoc *benchmark-registry* name benchmark-fn)))

(defun get-benchmark (name)
  "Retrieve a registered benchmark by name"
  (map:get *benchmark-registry* name))

(defun list-benchmarks ()
  "List all registered benchmark names"
  (map:keys *benchmark-registry*))

(defun run-benchmark-suite (&optional (benchmark-names :default))
  "Run a suite of benchmarks and return comparison results"
  (let* ((names (if (eq benchmark-names :default)
                    (list-benchmarks)
                    benchmark-names))
         (results (mapcar (lambda (name)
                           (let ((fn (get-benchmark name)))
                             (if fn
                                 (run-benchmark fn :name (string name))
                                 (error "Unknown benchmark: ~A" name))))
                         names)))
    
    (values results (if results
                        (apply #'compare-benchmarks results)
                        nil))))

;;; Macros for convenient benchmark definition

(defmacro defbenchmark (name (&key (min-time '*default-min-time*)
                                   (max-iterations '*default-max-iterations*))
                        &body body)
  "Define and register a named benchmark"
  (declare (ignore min-time max-iterations))
  `(register-benchmark ',name (lambda () ,@body)))

(defmacro with-benchmark-timing (&body body)
  "Execute BODY and return timing information"
  (let ((start-time (gensym "START"))
        (end-time (gensym "END")))
    `(let ((,start-time (get-internal-real-time)))
       (multiple-value-prog1
           (progn ,@body)
         (let ((,end-time (get-internal-real-time)))
           (format t "~&Execution time: ~,3F seconds~%"
                   (/ (- ,end-time ,start-time) internal-time-units-per-second)))))))

#| ;; Commented out - main:run-command doesn't exist
(defmethod main:run-command ((command benchmark) parsed-args)|#
(defun run-command-benchmark (command parsed-args)
  "Main entry point for benchmark command"
  (let* ((args (if (typep parsed-args 'argparse:parsed-arguments)
                   ;; New argparse style
                   (argparse:parsed-positionals parsed-args)
                   ;; Old dev.lisp style
                   (funcall (read-from-string "epsilon.tool.dev::parsed-args-arguments") parsed-args)))
         (options (if (typep parsed-args 'argparse:parsed-arguments)
                      (argparse:parsed-options parsed-args)
                      (funcall (read-from-string "epsilon.tool.dev::parsed-args-options") parsed-args)))
         (suite (when options (map:get options "suite")))
         (iterations (when options (or (map:get options "iterations") 1000))))
    (declare (ignore iterations)) ; TODO: Use iterations parameter when implemented
    (cond
      ;; Run specific suite
      (suite
       (format t "Running benchmark suite: ~A~%" suite)
       ;; TODO: Implement suite running
       (format t "Suite benchmarks not yet implemented~%"))
      
      ;; No arguments - list available benchmarks
      ((null args)
       (let ((available (list-benchmarks)))
         (if available
             (format t "Available benchmarks: ~{~A~^, ~}~%" available)
             (format t "No benchmarks registered.~%"))))
      
      ;; Run specific benchmarks
      (t
       (let ((benchmark-names (mapcar #'intern args)))
         (handler-case
             (multiple-value-bind (results comparison)
                 (run-benchmark-suite benchmark-names)
               ;; Print individual results
               (dolist (result results)
                 (format-benchmark-result result))
               ;; Print comparison if multiple benchmarks
               (when (> (length results) 1)
                 (format t "~%")
                 (format-comparison comparison)))
           (error (e)
             (format t "Error running benchmarks: ~A~%" e))))))))

;;; Command registration for new modular system

#| ;; Commented out - main:argument-parser doesn't exist
(defmethod main:argument-parser ((command benchmark))|#
(defun make-benchmark-parser ()
  (let ((parser (argparse:make-parser 
                 :command "benchmark"
                 :description "Run performance benchmarks")))
    ;; Add arguments
    (argparse:add-argument parser "benchmarks"
                          :nargs '*
                          :help "Specific benchmarks to run (optional)")
    (argparse:add-argument parser "--suite"
                          :help "Run specific benchmark suite")
    (argparse:add-argument parser "--iterations"
                          :type 'integer
                          :default 1000
                          :help "Number of iterations per benchmark")
    (argparse:add-argument parser "--format"
                          :choices '("text" "json" "csv")
                          :default "text"
                          :help "Output format")
    parser))

;;; Baseline management

(defvar *baseline-directory* #p"benchmarks/baselines/"
  "Directory for storing baseline benchmark results")

(defun ensure-baseline-directory ()
  "Ensure the baseline directory exists"
  (ensure-directories-exist *baseline-directory*))

(defun baseline-filename (name)
  "Generate filename for a baseline"
  (merge-pathnames (format nil "~A.baseline" name) *baseline-directory*))

(defun save-baseline (result &optional name)
  "Save benchmark result as a baseline"
  (ensure-baseline-directory)
  (let* ((baseline-name (or name (benchmark-result-name result)))
         (filename (baseline-filename baseline-name))
         (data (list :name (benchmark-result-name result)
                     :mean (benchmark-result-mean result)
                     :median (benchmark-result-median result)
                     :std-dev (benchmark-result-std-dev result)
                     :ops-per-sec (benchmark-result-ops-per-sec result)
                     :percentiles (benchmark-result-percentiles result)
                     :confidence-interval (benchmark-result-confidence-interval result)
                     :timestamp (get-universal-time))))
    (with-open-file (stream filename 
                           :direction :output
                           :if-exists :supersede)
      (write data :stream stream :readably t))
    (format t "Baseline saved: ~A~%" filename)
    data))

(defun load-baseline (name)
  "Load a baseline benchmark result"
  (let ((filename (baseline-filename name)))
    (when (probe-file filename)
      (with-open-file (stream filename :direction :input)
        (read stream)))))

(defun compare-with-baseline (result &optional baseline-name)
  "Compare benchmark result with baseline"
  (let* ((name (or baseline-name (benchmark-result-name result)))
         (baseline (load-baseline name)))
    (when baseline
      (let* ((current-mean (benchmark-result-mean result))
             (baseline-mean (getf baseline :mean))
             (ratio (/ current-mean baseline-mean))
             (percent-change (* 100 (- ratio 1)))
             (regression-threshold 1.1)) ; 10% slower is regression
        (list :baseline baseline
              :current result
              :ratio ratio
              :percent-change percent-change
              :regression (> ratio regression-threshold)
              :improvement (< ratio 0.9))))))

;;; Output formats

(defun format-as-json (result &optional (stream t))
  "Format benchmark result as JSON"
  (format stream "{~%")
  (format stream "  \"name\": ~S,~%" (benchmark-result-name result))
  (format stream "  \"iterations\": ~D,~%" (benchmark-result-iterations result))
  (format stream "  \"totalTime\": ~F,~%" (benchmark-result-time result))
  (format stream "  \"mean\": ~F,~%" (or (benchmark-result-mean result) 0))
  (format stream "  \"median\": ~F,~%" (or (benchmark-result-median result) 0))
  (format stream "  \"stdDev\": ~F,~%" (or (benchmark-result-std-dev result) 0))
  (format stream "  \"opsPerSec\": ~F,~%" (benchmark-result-ops-per-sec result))
  
  (when (benchmark-result-percentiles result)
    (format stream "  \"percentiles\": {~%")
    (let ((percentiles (benchmark-result-percentiles result)))
      (loop for (p . val) in percentiles
            for i from 0
            do (format stream "    \"p~D\": ~F~:[,~;~]~%" 
                      p val (= i (1- (length percentiles))))))
    (format stream "  },~%"))
  
  (when (benchmark-result-confidence-interval result)
    (let ((ci (benchmark-result-confidence-interval result)))
      (format stream "  \"confidenceInterval\": {~%")
      (format stream "    \"lower\": ~F,~%" (car ci))
      (format stream "    \"upper\": ~F~%" (cdr ci))
      (format stream "  },~%")))
  
  (when (benchmark-result-memory result)
    (format stream "  \"memoryBytes\": ~D,~%" (benchmark-result-memory result)))
  
  (format stream "  \"timestamp\": ~D~%" (get-universal-time))
  (format stream "}~%"))

(defun format-as-csv (results &optional (stream t))
  "Format benchmark results as CSV"
  (format stream "Name,Iterations,Time,Mean,Median,StdDev,OpsPerSec,Memory~%")
  (dolist (result (if (listp results) results (list results)))
    (format stream "~A,~D,~F,~F,~F,~F,~F,~D~%"
            (benchmark-result-name result)
            (benchmark-result-iterations result)
            (benchmark-result-time result)
            (or (benchmark-result-mean result) 0)
            (or (benchmark-result-median result) 0)
            (or (benchmark-result-std-dev result) 0)
            (benchmark-result-ops-per-sec result)
            (or (benchmark-result-memory result) 0))))

;; (main:register-command 'benchmark) ; Commented out - function doesn't exist

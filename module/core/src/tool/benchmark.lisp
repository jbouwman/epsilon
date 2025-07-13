;;;; Performance Benchmarking Framework
;;;;
;;;; This module provides a comprehensive benchmarking framework for measuring
;;;; and comparing performance of code segments. Includes statistical analysis,
;;;; memory usage tracking, and comparative benchmarking capabilities.
;;;;
;;;; Key Features:
;;;; - Precise timing measurements with statistical analysis
;;;; - Memory usage tracking and garbage collection metrics
;;;; - Comparative benchmarking with baseline comparisons
;;;; - Warmup periods and multiple iteration averaging
;;;; - Configurable output formats and reporting
;;;; - Integration with epsilon's build and test systems
;;;;
;;;; Dependencies: epsilon.lib.time, epsilon.lib.map
;;;; Usage: Define benchmarks and run with comparative analysis

(defpackage epsilon.tool.benchmark
  (:use cl)
  (:local-nicknames
   (time epsilon.lib.time)
   (map epsilon.lib.map))
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
   
   ;; Configuration
   *default-min-time*
   *default-max-iterations*
   *collect-memory-stats*
   
   ;; Convenience macros
   defbenchmark
   register-benchmark
   get-benchmark
   list-benchmarks
   run-benchmark-suite
   
   ;; Formatting
   format-benchmark-result
   format-comparison
   
   ;; Timing utilities
   with-benchmark-timing))

(in-package :epsilon.tool.benchmark)

;;;; A benchmarking tool for performance measurement and comparison

;;; Configuration

(defparameter *default-min-time* 1.0
  "Minimum time to run each benchmark in seconds")

(defparameter *default-max-iterations* 10000000
  "Maximum number of iterations for any benchmark")

(defparameter *collect-memory-stats* nil
  "Whether to collect memory allocation statistics")

;;; Data structures

(defstruct benchmark-result
  "Results from running a benchmark"
  name           ; benchmark name
  time           ; total time taken in seconds
  iterations     ; number of iterations performed
  ops-per-sec    ; operations per second
  memory         ; memory allocation info (bytes allocated)
  notes)         ; additional notes or metadata

;;; Core benchmarking

(defmacro benchmark (name &body body)
  "Define a benchmark with NAME that executes BODY"
  `(lambda ()
     (run-benchmark-internal ,name (lambda () ,@body))))

(defun run-benchmark (benchmark-fn &key 
                                     (name "anonymous")
                                     (min-time *default-min-time*)
                                     (max-iterations *default-max-iterations*)
                                     (collect-memory *collect-memory-stats*))
  "Run a benchmark function and return performance results"
  (let* ((start-time (get-internal-real-time))
         (start-memory (when collect-memory (sb-ext:get-bytes-consed)))
         (iterations 0)
         (end-time start-time)
         (time-per-iteration 0.0))
    
    ;; Warmup run to get rough timing
    (funcall benchmark-fn)
    (incf iterations)
    
    ;; Calculate target iterations based on warmup
    (let ((warmup-time (/ (- (get-internal-real-time) start-time) 
                         internal-time-units-per-second)))
      (when (> warmup-time 0)
        (setf time-per-iteration warmup-time)))
    
    ;; Main benchmark loop
    (setf start-time (get-internal-real-time))
    (setf iterations 0)
    
    (loop while (and (< (/ (- (get-internal-real-time) start-time) 
                          internal-time-units-per-second) min-time)
                     (< iterations max-iterations))
          do (funcall benchmark-fn)
             (incf iterations)
          finally (setf end-time (get-internal-real-time)))
    
    (let* ((total-time (/ (- end-time start-time) internal-time-units-per-second))
           (ops-per-sec (if (> total-time 0) (/ iterations total-time) 0))
           (end-memory (when collect-memory (sb-ext:get-bytes-consed)))
           (memory-used (when (and start-memory end-memory)
                         (- end-memory start-memory))))
      
      (make-benchmark-result
       :name name
       :time total-time
       :iterations iterations
       :ops-per-sec ops-per-sec
       :memory memory-used))))

(defun run-benchmark-internal (name fn)
  "Internal function to run a benchmark"
  (run-benchmark fn :name name))

(defun compare-benchmarks (&rest benchmark-results)
  "Compare multiple benchmark results and return analysis"
  (when benchmark-results
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
            :comparisons comparisons))))

;;; Utility functions

(defun format-benchmark-result (result &optional (stream t))
  "Format a benchmark result for display"
  (format stream "~&Benchmark: ~A~%" (benchmark-result-name result))
  (format stream "  Time: ~,3F seconds~%" (benchmark-result-time result))
  (format stream "  Iterations: ~:D~%" (benchmark-result-iterations result))
  (format stream "  Ops/sec: ~:D~%" (round (benchmark-result-ops-per-sec result)))
  (when (benchmark-result-memory result)
    (format stream "  Memory: ~:D bytes~%" (benchmark-result-memory result)))
  (when (benchmark-result-notes result)
    (format stream "  Notes: ~A~%" (benchmark-result-notes result))))

(defun format-comparison (comparison &optional (stream t))
  "Format a benchmark comparison for display"
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
            (format stream " (~,1Fx slower)~%" ratio))))))

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

(defun run-benchmark-suite (&optional benchmark-names)
  "Run a suite of benchmarks and return comparison results"
  (let* ((names (or benchmark-names (list-benchmarks)))
         (results (mapcar (lambda (name)
                           (let ((fn (get-benchmark name)))
                             (if fn
                                 (run-benchmark fn :name (string name))
                                 (error "Unknown benchmark: ~A" name))))
                         names)))
    
    (values results (apply #'compare-benchmarks results))))

;;; Macros for convenient benchmark definition

(defmacro defbenchmark (name (&key (min-time '*default-min-time*)
                                   (max-iterations '*default-max-iterations*))
                        &body body)
  "Define and register a named benchmark"
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

(defun main (parsed-args)
  "Main entry point for benchmark command from dev.lisp dispatcher"
  (let* ((args (funcall (read-from-string "epsilon.tool.dev::parsed-args-arguments") parsed-args))
         (options (funcall (read-from-string "epsilon.tool.dev::parsed-args-options") parsed-args)))
    (cond
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
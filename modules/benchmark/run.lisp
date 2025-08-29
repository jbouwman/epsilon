;;;; Benchmark Runner
;;;;
;;;; Simple entry point for running benchmarks

(in-package :epsilon.tool.benchmark.suites)

(defun print-help ()
  "Print usage information"
  (format t "Epsilon Benchmark Runner~%")
  (format t "~%")
  (format t "Usage: epsilon --module epsilon.benchmark --eval \"(epsilon.tool.benchmark.suites:main \\\"COMMAND\\\")\"~%")
  (format t "~%")
  (format t "Commands:~%")
  (format t "  help      - Show this help message~%")
  (format t "  run       - Run default benchmark suites (HAMT-focused)~%")
  (format t "  all       - Run all benchmark suites~%")
  (format t "  quick     - Run quick benchmarks (for CI)~%")
  (format t "  critical  - Run performance-critical benchmarks~%")
  (format t "  baseline  - Save current results as baseline~%")
  (format t "  compare   - Compare with saved baseline~%")
  (format t "  list      - List available suites~%")
  (format t "  <suite>   - Run specific suite by name~%")
  (format t "~%")
  (format t "Examples:~%")
  (format t "  ./scripts/benchmark.sh run         # Run default suites (core + functional data)~%")
  (format t "  ./scripts/benchmark.sh critical    # Run performance-critical benchmarks~%")
  (format t "  ./scripts/benchmark.sh quick       # Quick CI smoke tests~%")
  (format t "  ./scripts/benchmark.sh baseline    # Save baselines for regression detection~%")
  (format t "  ./scripts/benchmark.sh compare     # Check for performance regressions~%"))

(defun main (&optional (command "help"))
  "Main entry point for benchmark runner"
  (handler-case
      (cond
        ((string= command "help")
         (print-help))
        
        ((string= command "run")
         (run-default-benchmarks))
        
        ((string= command "all")
         (run-all-benchmarks))
        
        ((string= command "quick")
         (run-ci-benchmarks))
        
        ((string= command "critical")
         (run-performance-critical-benchmarks))
        
        ((string= command "baseline")
         (save-baselines))
        
        ((string= command "compare")
         (compare-baselines))
        
        ((string= command "list")
         (register-all-suites)
         (format t "Available benchmark suites:~%")
         (dolist (suite (list-suites))
           (let ((s (get-suite suite)))
             (format t "  ~A~@[ - ~A~]~%" 
                     suite 
                     (when s (benchmark-suite-description s))))))
        
        (t
         (let ((suite (intern (string-upcase command) :keyword)))
           (register-all-suites)
           (if (get-suite suite)
               (run-suite suite)
               (format t "Unknown command or suite: ~A~%~%Try 'help' for usage.~%" command)))))
    
    (error (e)
      (format t "Error: ~A~%" e)
      (sb-ext:exit :code 1))))
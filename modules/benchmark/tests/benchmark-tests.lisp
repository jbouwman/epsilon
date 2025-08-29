;;;; Comprehensive tests for epsilon.tool.benchmark

(defpackage epsilon.tool.benchmark.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (benchmark epsilon.tool.benchmark)
   (map epsilon.map)))

(in-package epsilon.tool.benchmark.tests)

;;; Basic Benchmark Execution Tests

(deftest benchmark-result-structure-test
  "Test that benchmark results have correct structure"
  (let ((result (benchmark:run-benchmark (lambda () (+ 1 2 3))
                                         :name "simple-addition"
                                         :min-time 0.1
                                         :max-iterations 1000)))
    (is (benchmark:benchmark-result-p result))
    (is-equal (benchmark:benchmark-result-name result) "simple-addition")
    (is (>= (benchmark:benchmark-result-time result) 0))  ; Time can be 0 for very fast operations
    (is (> (benchmark:benchmark-result-iterations result) 0))
    (is (> (benchmark:benchmark-result-ops-per-sec result) 0))))

(deftest benchmark-memory-tracking-test
  "Test memory allocation tracking"
  (let ((benchmark:*collect-memory-stats* t))
    (let ((result (benchmark:run-benchmark 
                   (lambda () (make-list 100 :initial-element 42))
                   :name "memory-allocation"
                   :min-time 0.1
                   :max-iterations 100
                   :collect-memory t)))
      (is (benchmark:benchmark-result-p result))
      (is (numberp (benchmark:benchmark-result-memory result)))
      (is (> (benchmark:benchmark-result-memory result) 0)))))

(deftest benchmark-min-time-constraint-test
  "Test that benchmarks run for at least min-time"
  (let* ((start-time (get-internal-real-time))
         (result (benchmark:run-benchmark (lambda () (sleep 0.001))
                                          :name "sleep-test"
                                          :min-time 0.2
                                          :max-iterations 1000000))
         (elapsed (/ (- (get-internal-real-time) start-time)
                     internal-time-units-per-second)))
    (is (>= elapsed 0.15))  ; Allow some tolerance
    (is (>= (benchmark:benchmark-result-time result) 0.15))))

(deftest benchmark-max-iterations-constraint-test
  "Test that benchmarks respect max iterations limit"
  (let ((result (benchmark:run-benchmark (lambda () (+ 1 1))
                                         :name "max-iterations-test"
                                         :min-time 10.0  ; Very long time
                                         :max-iterations 50)))  ; But limited iterations
    (is (<= (benchmark:benchmark-result-iterations result) 50))))

;;; Benchmark Macro Tests

(deftest benchmark-macro-test
  "Test benchmark macro creates proper function"
  (let ((bench-fn (benchmark:benchmark "macro-test" (+ 2 3))))
    (is (functionp bench-fn))
    (let ((result (funcall bench-fn)))
      (is (benchmark:benchmark-result-p result))
      (is-equal (benchmark:benchmark-result-name result) "macro-test"))))

;;; Benchmark Registration Tests

(deftest defbenchmark-registration-test
  "Test defbenchmark macro registers benchmarks"
  (benchmark:defbenchmark test-fibonacci ()
    (labels ((fib (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))))
      (fib 10)))
  
  (is (member 'test-fibonacci (benchmark:list-benchmarks)))
  (is (functionp (benchmark:get-benchmark 'test-fibonacci))))

(deftest register-benchmark-function-test
  "Test manual benchmark registration"
  (benchmark:register-benchmark 'manual-test (lambda () (* 6 7)))
  (is (member 'manual-test (benchmark:list-benchmarks)))
  (is (functionp (benchmark:get-benchmark 'manual-test))))

(deftest benchmark-registry-isolation-test
  "Test that benchmark registry is properly isolated"
  (let ((initial-benchmarks (benchmark:list-benchmarks)))
    (benchmark:register-benchmark 'temp-benchmark (lambda () nil))
    (is (member 'temp-benchmark (benchmark:list-benchmarks)))
    
    ;; Clean up by resetting registry (if such function exists)
    ;; For now, just verify the benchmark was added
    (is (> (length (benchmark:list-benchmarks)) (length initial-benchmarks)))))

;;; Benchmark Suite Tests

(deftest benchmark-suite-execution-test
  "Test running a suite of benchmarks"
  ;; Register test benchmarks
  (benchmark:register-benchmark 'suite-test-1 (lambda () (+ 1 2)))
  (benchmark:register-benchmark 'suite-test-2 (lambda () (* 3 4)))
  
  (multiple-value-bind (results comparison)
      (benchmark:run-benchmark-suite '(suite-test-1 suite-test-2))
    
    (is (= (length results) 2))
    (is (every #'benchmark:benchmark-result-p results))
    
    ;; Check comparison structure
    (is (listp comparison))
    (is (getf comparison :fastest))
    (is (listp (getf comparison :comparisons)))))

(deftest benchmark-suite-empty-test
  "Test running empty benchmark suite"
  (multiple-value-bind (results comparison)
      (benchmark:run-benchmark-suite '())
    (is (null results))
    (is (null comparison))))

(deftest benchmark-suite-unknown-benchmark-test
  "Test error handling for unknown benchmarks"
  (is-thrown (error)
    (benchmark:run-benchmark-suite '(non-existent-benchmark))))

;;; Comparison Tests

(deftest benchmark-comparison-test
  "Test benchmark comparison functionality"
  (let ((fast-result (benchmark:run-benchmark (lambda () (+ 1 1))
                                              :name "fast"
                                              :min-time 0.1))
        (slow-result (benchmark:run-benchmark (lambda () (sleep 0.001))
                                              :name "slow" 
                                              :min-time 0.1)))
    
    (let ((comparison (benchmark:compare-benchmarks fast-result slow-result)))
      (is (listp comparison))
      (is (getf comparison :fastest))
      (is (listp (getf comparison :comparisons)))
      
      ;; The fast benchmark should be fastest
      (is-equal (getf comparison :fastest) "fast")
      
      ;; Check comparison entries
      (let ((comparisons (getf comparison :comparisons)))
        (is (= (length comparisons) 2))
        (is (every (lambda (comp) (getf comp :name)) comparisons))
        (is (every (lambda (comp) (getf comp :ops-per-sec)) comparisons))
        (is (every (lambda (comp) (getf comp :relative-speed)) comparisons))))))

(deftest benchmark-comparison-single-test
  "Test comparison with single benchmark"
  (let ((result (benchmark:run-benchmark (lambda () nil) :name "single")))
    (let ((comparison (benchmark:compare-benchmarks result)))
      (is (listp comparison))
      (is-equal (getf comparison :fastest) "single"))))

;;; Formatting Tests

(deftest format-benchmark-result-test
  "Test benchmark result formatting"
  (let ((result (benchmark:run-benchmark (lambda () (+ 1 2))
                                         :name "format-test"
                                         :min-time 0.1)))
    ;; Test that formatting doesn't error
    (with-output-to-string (stream)
      (benchmark:format-benchmark-result result stream))
    
    ;; Test that it produces some output
    (let ((output (with-output-to-string (stream)
                    (benchmark:format-benchmark-result result stream))))
      (is (> (length output) 0))
      (is (search "format-test" output))
      (is (search "Time:" output))
      (is (search "Iterations:" output))
      (is (search "Ops/sec:" output)))))

(deftest format-comparison-test
  "Test benchmark comparison formatting"
  (let ((result1 (benchmark:run-benchmark (lambda () (+ 1 1)) :name "test1"))
        (result2 (benchmark:run-benchmark (lambda () (* 2 2)) :name "test2")))
    
    (let ((comparison (benchmark:compare-benchmarks result1 result2)))
      ;; Test that formatting doesn't error
      (with-output-to-string (stream)
        (benchmark:format-comparison comparison stream))
      
      ;; Test that it produces some output
      (let ((output (with-output-to-string (stream)
                      (benchmark:format-comparison comparison stream))))
        (is (> (length output) 0))
        (is (search "Benchmark Comparison:" output))
        (is (search "Fastest:" output))))))

;;; Timing Utility Tests

(deftest with-benchmark-timing-test
  "Test with-benchmark-timing macro"
  (let ((output (with-output-to-string (*standard-output*)
                  (benchmark:with-benchmark-timing
                      (sleep 0.01)
                    (+ 1 2 3)))))
    (is (search "Execution time:" output))
    (is (search "seconds" output))))

;;; Configuration Tests

(deftest default-configuration-test
  "Test default configuration values"
  (is (numberp benchmark:*default-min-time*))
  (is (> benchmark:*default-min-time* 0))
  (is (numberp benchmark:*default-max-iterations*))
  (is (> benchmark:*default-max-iterations* 0))
  (is (member benchmark:*collect-memory-stats* '(t nil))))

(deftest configuration-override-test
  "Test configuration override in run-benchmark"
  (let ((result (benchmark:run-benchmark (lambda () nil)
                                         :name "config-test"
                                         :min-time 0.05
                                         :max-iterations 10)))
    ;; Should respect the overridden values
    (is (<= (benchmark:benchmark-result-iterations result) 10))))

;;; Error Handling Tests

(deftest benchmark-error-handling-test
  "Test error handling in benchmark execution"
  ;; This should not crash the benchmark system
  (is-thrown (error)
    (benchmark:run-benchmark (lambda () (error "Intentional error"))
                             :name "error-test"
                             :min-time 0.1)))

(deftest benchmark-zero-iterations-test
  "Test handling of benchmarks that can't complete any iterations"
  (let ((result (benchmark:run-benchmark (lambda () (sleep 1))
                                         :name "slow-test"
                                         :min-time 0.1
                                         :max-iterations 1)))
    ;; Should complete at least one iteration
    (is (>= (benchmark:benchmark-result-iterations result) 1))))

;;; Statistical Tests

(deftest benchmark-statistical-measures-test
  "Test that statistical measures are calculated correctly"
  (let ((result (benchmark:run-benchmark (lambda () (* 2 3))
                                         :name "stats-test"
                                         :min-time 0.2)))
    ;; Check all statistical fields are present
    (is (benchmark:benchmark-result-mean result))
    (is (benchmark:benchmark-result-median result))
    (is (benchmark:benchmark-result-std-dev result))
    (is (benchmark:benchmark-result-percentiles result))
    (is (benchmark:benchmark-result-confidence-interval result))
    (is (benchmark:benchmark-result-samples result))
    
    ;; Statistical sanity checks
    (is (> (benchmark:benchmark-result-mean result) 0))
    (is (>= (benchmark:benchmark-result-std-dev result) 0))
    (is (listp (benchmark:benchmark-result-percentiles result)))
    (is (consp (benchmark:benchmark-result-confidence-interval result)))
    
    ;; Confidence interval should contain the mean
    (let ((ci (benchmark:benchmark-result-confidence-interval result))
          (mean (benchmark:benchmark-result-mean result)))
      (is (<= (car ci) mean))
      (is (>= (cdr ci) mean)))))

(deftest benchmark-percentiles-test
  "Test percentile calculations"
  (let ((result (benchmark:run-benchmark (lambda () (sleep 0.0001))
                                         :name "percentile-test"
                                         :min-time 0.3)))
    (let ((percentiles (benchmark:benchmark-result-percentiles result)))
      ;; Should have standard percentiles
      (is (assoc 50 percentiles))
      (is (assoc 90 percentiles))
      (is (assoc 95 percentiles))
      (is (assoc 99 percentiles))
      
      ;; Percentiles should be in order
      (is (<= (cdr (assoc 50 percentiles))
              (cdr (assoc 90 percentiles))))
      (is (<= (cdr (assoc 90 percentiles))
              (cdr (assoc 95 percentiles))))
      (is (<= (cdr (assoc 95 percentiles))
              (cdr (assoc 99 percentiles)))))))

(deftest benchmark-warmup-test
  "Test that warmup phase works"
  (let ((cold-result (benchmark:run-benchmark (lambda () (mapcar #'1+ '(1 2 3 4 5)))
                                              :name "cold-test"
                                              :min-time 0.1
                                              :warmup-time 0))
        (warm-result (benchmark:run-benchmark (lambda () (mapcar #'1+ '(1 2 3 4 5)))
                                              :name "warm-test"
                                              :min-time 0.1
                                              :warmup-time 0.05)))
    ;; Both should produce valid results
    (is (benchmark:benchmark-result-p cold-result))
    (is (benchmark:benchmark-result-p warm-result))))

;;; Baseline Tests

(deftest benchmark-baseline-save-load-test
  "Test saving and loading baseline results"
  (let* ((result (benchmark:run-benchmark (lambda () (+ 1 2 3))
                                          :name "baseline-test"
                                          :min-time 0.1))
         (saved (benchmark:save-baseline result "test-baseline"))
         (loaded (benchmark:load-baseline "test-baseline")))
    
    (is saved)
    (is loaded)
    (is (equal (getf loaded :name) "baseline-test"))
    (is (numberp (getf loaded :mean)))
    (is (numberp (getf loaded :timestamp)))))

(deftest benchmark-baseline-comparison-test
  "Test baseline comparison functionality"
  (let* ((baseline-result (benchmark:run-benchmark (lambda () (+ 1 2))
                                                   :name "comparison-test"
                                                   :min-time 0.1))
         (_ (benchmark:save-baseline baseline-result "comparison-test"))
         (current-result (benchmark:run-benchmark (lambda () (+ 1 2))
                                                  :name "comparison-test"
                                                  :min-time 0.1))
         (comparison (benchmark:compare-with-baseline current-result)))
    
    (declare (ignore _))
    (is comparison)
    (is (getf comparison :baseline))
    (is (getf comparison :current))
    (is (numberp (getf comparison :ratio)))
    (is (numberp (getf comparison :percent-change)))
    (is (member (getf comparison :regression) '(t nil)))
    (is (member (getf comparison :improvement) '(t nil)))))

;;; Output Format Tests

(deftest benchmark-json-format-test
  "Test JSON output formatting"
  (let ((result (benchmark:run-benchmark (lambda () nil)
                                         :name "json-test"
                                         :min-time 0.1)))
    (let ((json-output (with-output-to-string (stream)
                        (benchmark:format-as-json result stream))))
      (is (> (length json-output) 0))
      (is (search "\"name\"" json-output))
      (is (search "\"mean\"" json-output))
      (is (search "\"median\"" json-output))
      (is (search "\"percentiles\"" json-output))
      (is (search "\"confidenceInterval\"" json-output)))))

(deftest benchmark-csv-format-test
  "Test CSV output formatting"
  (let ((results (list
                  (benchmark:run-benchmark (lambda () (+ 1 1)) 
                                          :name "csv-test-1" 
                                          :min-time 0.05)
                  (benchmark:run-benchmark (lambda () (* 2 2)) 
                                          :name "csv-test-2" 
                                          :min-time 0.05))))
    (let ((csv-output (with-output-to-string (stream)
                       (benchmark:format-as-csv results stream))))
      (is (> (length csv-output) 0))
      (is (search "Name,Iterations,Time" csv-output))
      (is (search "csv-test-1" csv-output))
      (is (search "csv-test-2" csv-output)))))

;;; Performance Regression Tests

(deftest benchmark-consistency-test
  "Test that benchmarks produce consistent results"
  (let ((results (loop repeat 3
                       collect (benchmark:run-benchmark (lambda () (+ 1 1))
                                                        :name "consistency-test"
                                                        :min-time 0.1))))
    ;; All results should be benchmark-result objects
    (is (every #'benchmark:benchmark-result-p results))
    
    ;; Operations per second should be in similar range (within 50% of each other)
    (let ((ops-rates (mapcar #'benchmark:benchmark-result-ops-per-sec results)))
      (let ((min-rate (apply #'min ops-rates))
            (max-rate (apply #'max ops-rates)))
        (is (< (/ max-rate min-rate) 2.0))))))  ; Less than 2x difference

;;; Integration Tests

(deftest benchmark-registry-persistence-test
  "Test that registered benchmarks persist across calls"
  (benchmark:register-benchmark 'persistence-test (lambda () (* 7 8)))
  
  ;; Should be available in subsequent calls
  (is (member 'persistence-test (benchmark:list-benchmarks)))
  (is (functionp (benchmark:get-benchmark 'persistence-test)))
  
  ;; Should be runnable via suite
  (multiple-value-bind (results comparison)
      (benchmark:run-benchmark-suite '(persistence-test))
    (is (= (length results) 1))
    (is-equal (benchmark:benchmark-result-name (first results)) "PERSISTENCE-TEST")))

(deftest benchmark-memory-efficiency-test
  "Test that benchmark execution doesn't leak memory excessively"
  ;; This is a smoke test - just ensure we can run many benchmarks
  ;; without obvious memory issues
  (dotimes (i 10)
    (benchmark:run-benchmark (lambda () (make-list 10))
                             :name (format nil "memory-test-~D" i)
                             :min-time 0.01
                             :max-iterations 100))
  ;; If we get here without error, memory handling is probably OK
  (is t))

;;; Command Line Interface Tests (if available)

(deftest benchmark-command-class-test
  "Test benchmark command class exists"
  (is (find-class 'benchmark:benchmark nil)))

;;; Utility Function Tests

(deftest benchmark-result-accessors-test
  "Test all benchmark result accessors work correctly"
  (let ((result (benchmark:run-benchmark (lambda () (list 1 2 3))
                                         :name "accessor-test"
                                         :min-time 0.1)))
    
    ;; Test all accessors
    (is (stringp (benchmark:benchmark-result-name result)))
    (is (numberp (benchmark:benchmark-result-time result)))
    (is (integerp (benchmark:benchmark-result-iterations result)))
    (is (numberp (benchmark:benchmark-result-ops-per-sec result)))
    
    ;; Memory may be nil if not tracked
    (let ((memory (benchmark:benchmark-result-memory result)))
      (is (or (null memory) (numberp memory))))
    
    ;; Notes may be nil
    (let ((notes (benchmark:benchmark-result-notes result)))
      (is (or (null notes) (stringp notes))))))

;;; Edge Case Tests

(deftest benchmark-very-fast-operation-test
  "Test benchmarking very fast operations"
  (let ((result (benchmark:run-benchmark (lambda () nil)
                                         :name "very-fast"
                                         :min-time 0.1)))
    ;; Should still produce valid results
    (is (benchmark:benchmark-result-p result))
    (is (> (benchmark:benchmark-result-iterations result) 1000))))  ; Should do many iterations

(deftest benchmark-different-return-types-test
  "Test benchmarking functions with different return types"
  (let ((results (list
                  (benchmark:run-benchmark (lambda () 42) :name "number")
                  (benchmark:run-benchmark (lambda () "string") :name "string")
                  (benchmark:run-benchmark (lambda () '(1 2 3)) :name "list")
                  (benchmark:run-benchmark (lambda () nil) :name "nil"))))
    
    ;; All should produce valid benchmark results regardless of return type
    (is (every #'benchmark:benchmark-result-p results))
    (is (every (lambda (r) (> (benchmark:benchmark-result-iterations r) 0)) results))))

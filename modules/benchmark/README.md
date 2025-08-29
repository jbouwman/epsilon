# Epsilon Benchmark Framework

A comprehensive performance benchmarking framework for measuring and comparing execution times, throughput, and memory usage of code segments.

## Overview

The Epsilon benchmark framework provides:

- **Statistical rigor** with bootstrap confidence intervals and percentile analysis
- **Automatic warmup and calibration** for stable measurements
- **Memory allocation tracking** via SBCL integration with per-operation metrics
- **Baseline management** for regression detection and trend analysis
- **Multiple output formats** (text, JSON, CSV) for CI/CD integration
- **Organized benchmark suites** for systematic performance testing
- **Make targets** for easy integration into build workflows

## Quick Start

### Using Make Targets

```bash
# Run all benchmark suites
make benchmark

# Quick smoke test benchmarks
make benchmark-quick

# Save current performance as baseline
make benchmark-baseline

# Compare with baseline (detect regressions)
make benchmark-compare
```

### Basic Benchmarking

```lisp
(use-package :epsilon.tool.benchmark)

;; Simple benchmark with statistical analysis
(defparameter result
  (run-benchmark (lambda () (+ 1 2 3))
                 :name "simple-addition"
                 :warmup-time 0.1))

(format-benchmark-result result)
;; Output:
;; Benchmark: simple-addition
;;   Iterations: 2,043,891
;;   Total time: 125.00 ms
;;   Mean: 61.00 ns/op
;;   Median: 59.00 ns/op
;;   Std Dev: 3.00 ns
;;   95% CI: [58.00 ns, 64.00 ns]/op
;;   Percentiles:
;;     P50: 59.00 ns/op
;;     P90: 65.00 ns/op
;;     P95: 68.00 ns/op
;;     P99: 75.00 ns/op
;;   Throughput: 16.35 M ops/sec
```

### Benchmark Definition

```lisp
;; Define reusable benchmarks
(defbenchmark string-concatenation ()
  (concatenate 'string "hello" " " "world"))

(defbenchmark list-operations ()
  (let ((lst '(1 2 3 4 5)))
    (reverse (sort (copy-list lst) #'<))))

;; Run benchmark suite
(multiple-value-bind (results comparison)
    (run-benchmark-suite '(string-concatenation list-operations))
  (format-comparison comparison))
```

### Memory Tracking

```lisp
(let ((*collect-memory-stats* t))
  (defparameter memory-result
    (run-benchmark (lambda () 
                     (make-list 1000 :initial-element 42))
                   :name "list-allocation")))

(format-benchmark-result memory-result)
;; Output includes memory allocation info
```

## Core API

### Benchmark Execution

**`run-benchmark`** `(function &key name min-time max-iterations collect-memory)`

Executes a benchmark function with automatic iteration calibration:

- **`function`** - Lambda or function to benchmark
- **`name`** - Descriptive name for the benchmark
- **`min-time`** - Minimum time to run (default 1.0 seconds)
- **`max-iterations`** - Maximum iterations (default 10,000,000)
- **`collect-memory`** - Track memory allocation (default nil)

```lisp
(run-benchmark #'expensive-computation
               :name "fibonacci-calculation"
               :min-time 2.0
               :max-iterations 1000
               :collect-memory t)
```

**`benchmark`** `(name &body body)`

Macro for inline benchmark definition:

```lisp
(funcall (benchmark "hash-table-lookup"
           (gethash :key *large-hash-table*)))
```

### Benchmark Registration

**`defbenchmark`** `(name (&key min-time max-iterations) &body body)`

Define and register a named benchmark:

```lisp
(defbenchmark matrix-multiplication ()
  (let ((a (make-array '(100 100) :initial-element 1.0))
        (b (make-array '(100 100) :initial-element 2.0)))
    (matrix-multiply a b)))
```

**`register-benchmark`** `(name function)`

Manually register a benchmark function:

```lisp
(register-benchmark 'custom-sort
                   (lambda () (sort (random-list 1000) #'<)))
```

### Benchmark Management

**`list-benchmarks`** `()`

List all registered benchmark names:

```lisp
(list-benchmarks)  ; => (string-concatenation list-operations matrix-multiplication)
```

**`get-benchmark`** `(name)`

Retrieve a registered benchmark:

```lisp
(get-benchmark 'string-concatenation)  ; => #<FUNCTION>
```

### Suite Execution

**`run-benchmark-suite`** `(&optional benchmark-names)`

Run multiple benchmarks and compare results:

```lisp
;; Run all registered benchmarks
(multiple-value-bind (results comparison)
    (run-benchmark-suite)
  (dolist (result results)
    (format-benchmark-result result))
  (format-comparison comparison))

;; Run specific benchmarks
(run-benchmark-suite '(string-ops list-ops hash-ops))
```

### Result Analysis

**`compare-implementations`** `(implementations &key min-time warmup-time output-format)`

Compare different implementations of the same algorithm:

```lisp
;; Define different sorting implementations
(defun bubble-sort (list) ...)
(defun quick-sort (list) ...)
(defun merge-sort (list) ...)

;; Compare them fairly
(compare-implementations 
  '((bubble-sort . ,(lambda () (bubble-sort test-data)))
    (quick-sort  . ,(lambda () (quick-sort test-data)))
    (merge-sort  . ,(lambda () (merge-sort test-data)))))

;; Output:
;; Algorithm Implementation Comparison
;; ====================================
;;
;; quick-sort:
;;   Mean: 0.000012 seconds/op
;;   Ops/sec: 83,333
;;   P95: 0.000015 seconds/op
;;
;; merge-sort:
;;   Mean: 0.000018 seconds/op
;;   Ops/sec: 55,556
;;   P95: 0.000022 seconds/op
;;
;; bubble-sort:
;;   Mean: 0.000145 seconds/op
;;   Ops/sec: 6,897
;;   P95: 0.000162 seconds/op
;;
;; Relative Performance:
;;   quick-sort: 1.00x (baseline)
;;   merge-sort: 1.50x slower
;;   bubble-sort: 12.08x slower
```

**`compare-benchmarks`** `(&rest benchmark-results)`

Compare multiple runs of the SAME benchmark (for consistency checking):

```lisp
;; Run the same benchmark multiple times
(defparameter runs
  (loop repeat 3 
        collect (run-benchmark #'my-function 
                              :name "my-function")))

;; Compare runs for consistency
(format-comparison (apply #'compare-benchmarks runs))
```

## Data Structures

### `benchmark-result`

Structure containing benchmark execution results:

```lisp
(defstruct benchmark-result
  name           ; Benchmark name
  time           ; Total execution time (seconds)
  iterations     ; Number of iterations performed
  ops-per-sec    ; Operations per second
  memory         ; Bytes allocated (if tracked)
  notes)         ; Additional metadata
```

**Accessors:**
- `benchmark-result-name`
- `benchmark-result-time`
- `benchmark-result-iterations`
- `benchmark-result-ops-per-sec`
- `benchmark-result-memory`

## Configuration

### Global Parameters

**`*default-min-time*`** - Default minimum execution time (1.0 seconds)
**`*default-max-iterations*`** - Default maximum iterations (10,000,000)
**`*collect-memory-stats*`** - Global memory tracking flag (nil)

```lisp
;; Configure default timing
(setf *default-min-time* 2.0)
(setf *default-max-iterations* 1000000)

;; Enable memory tracking globally
(setf *collect-memory-stats* t)
```

## Command Line Interface

### Basic Usage

```bash
# List available benchmarks
./epsilon benchmark

# Run specific benchmarks
./epsilon benchmark string-ops list-ops

# Run benchmark suite
./epsilon benchmark --suite core-operations

# Configure iterations
./epsilon benchmark matrix-ops --iterations 5000

# Change output format
./epsilon benchmark --format json hash-operations
```

### Output Formats

**Text Format (default):**
```
Benchmark: string-concatenation
  Time: 0.125 seconds
  Iterations: 2,043,891
  Ops/sec: 16,351,128
```

**JSON Format:**
```json
{
  "name": "string-concatenation",
  "time": 0.125,
  "iterations": 2043891,
  "ops_per_sec": 16351128,
  "memory": null
}
```

**CSV Format:**
```csv
name,time,iterations,ops_per_sec,memory
string-concatenation,0.125,2043891,16351128,
```

## Advanced Features

### Statistical Analysis

The framework provides comprehensive statistical analysis:

```lisp
;; Bootstrap confidence intervals
(let ((result (run-benchmark #'my-function 
                            :confidence-level 0.99)))  ; 99% CI
  (format t "99% confident the mean is between ~F and ~F~%"
          (car (benchmark-result-confidence-interval result))
          (cdr (benchmark-result-confidence-interval result))))

;; Access detailed statistics
(format t "P95 latency: ~F seconds~%"
        (cdr (assoc 95 (benchmark-result-percentiles result))))
```

### Baseline Management

Track performance over time and detect regressions:

```lisp
;; Save current performance as baseline
(let ((result (run-benchmark #'critical-function 
                            :name "critical-op")))
  (save-baseline result))

;; Later, compare with baseline
(let* ((current (run-benchmark #'critical-function 
                              :name "critical-op"))
       (comparison (compare-with-baseline current)))
  (when (getf comparison :regression)
    (error "Performance regression detected: ~,1f% slower"
           (getf comparison :percent-change))))
```

### Benchmark Suites

Organize related benchmarks for systematic testing:

```lisp
(use-package :epsilon.tool.benchmark.suites)

;; Define a custom suite
(define-suite my-algorithms
    (:description "Algorithm performance comparison")
  my-sort-algorithm
  my-search-algorithm
  my-hash-algorithm)

;; Run suite with different output formats
(run-suite 'my-algorithms :output-format :json)  ; For CI/CD
(run-suite 'my-algorithms :output-format :csv)   ; For spreadsheets
```

### Custom Timing

**`with-benchmark-timing`** - Execute code with timing output:

```lisp
(with-benchmark-timing
  (expensive-operation)
  (another-operation))
;; Output: Execution time: 1.234 seconds
```

### Memory Profiling

Track memory allocation patterns:

```lisp
(let ((*collect-memory-stats* t))
  (defparameter mem-intensive
    (run-benchmark (lambda ()
                     (let ((data (make-hash-table)))
                       (loop for i from 1 to 1000
                             do (setf (gethash i data) (make-list i)))
                       data))
                   :name "memory-intensive")))

(format t "Allocated ~:D bytes per iteration~%"
        (/ (benchmark-result-memory mem-intensive)
           (benchmark-result-iterations mem-intensive)))
```

## Performance Characteristics

### Timing Accuracy

- **Warmup run** eliminates cold-start effects
- **Automatic calibration** determines appropriate iteration count
- **High-resolution timing** using `get-internal-real-time`
- **Statistical stability** through sufficient sampling

### Memory Tracking

- **SBCL integration** via `sb-ext:get-bytes-consed`
- **Per-iteration accuracy** for allocation-intensive code
- **Minimal overhead** when disabled
- **Garbage collection aware** measurements

### Overhead Analysis

The framework itself introduces minimal overhead:
- Function call overhead: ~10 nanoseconds
- Timing overhead: ~100 nanoseconds per measurement
- Memory tracking overhead: ~50 nanoseconds when enabled

## Best Practices

### Benchmark Design

```lisp
;; Good: Isolate the operation being measured
(defbenchmark efficient-search ()
  (binary-search *sorted-data* target))

;; Avoid: Including setup in the benchmark
(defbenchmark inefficient-search ()
  (let ((data (sort (copy-list *unsorted-data*) #'<)))  ; Setup overhead
    (binary-search data target)))
```

### Result Interpretation

```lisp
;; Compare relative performance, not absolute numbers
(multiple-value-bind (results comparison)
    (run-benchmark-suite '(algorithm-v1 algorithm-v2))
  ;; Focus on the ratio between implementations
  (format-comparison comparison))
```

### Statistical Validity

- Run benchmarks multiple times for consistency
- Use appropriate minimum time for stable results
- Account for system load and background processes
- Compare algorithms with similar input sizes

## Integration

### CI/CD Integration

```bash
# Generate machine-readable benchmark reports
./epsilon benchmark --format json > benchmark-results.json

# Compare against baseline performance
./epsilon benchmark core-suite --baseline previous-results.json
```

### Development Workflow

```lisp
;; Quick performance check during development
(with-benchmark-timing
  (new-algorithm input-data))

;; Formal benchmark for performance regression testing
(run-benchmark-suite '(critical-operations))
```

The Epsilon benchmark framework provides accurate, reliable performance measurement with minimal setup overhead, making it ideal for both ad-hoc performance analysis and systematic performance regression testing.
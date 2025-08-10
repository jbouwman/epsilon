# Epsilon Benchmark Framework

A comprehensive performance benchmarking framework for measuring and comparing execution times, throughput, and memory usage of code segments.

## Overview

The Epsilon benchmark framework provides:

- **Accurate timing measurement** with automatic iteration calibration
- **Memory allocation tracking** via SBCL integration
- **Statistical analysis** with operations-per-second calculation
- **Benchmark comparison** and relative performance analysis
- **Multiple output formats** (text, JSON, CSV)
- **Benchmark suites** for organized performance testing
- **Command-line interface** for development workflow integration

## Quick Start

### Basic Benchmarking

```lisp
(use-package :epsilon.tool.benchmark)

;; Simple benchmark
(defparameter result
  (run-benchmark (lambda () (+ 1 2 3))
                 :name "simple-addition"))

(format-benchmark-result result)
;; Output:
;; Benchmark: simple-addition
;;   Time: 0.125 seconds
;;   Iterations: 2,043,891
;;   Ops/sec: 16,351,128
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

**`compare-benchmarks`** `(&rest benchmark-results)`

Compare multiple benchmark results:

```lisp
(defparameter results
  (list (run-benchmark #'function-a :name "Algorithm A")
        (run-benchmark #'function-b :name "Algorithm B")
        (run-benchmark #'function-c :name "Algorithm C")))

(format-comparison (apply #'compare-benchmarks results))
;; Output:
;; Benchmark Comparison:
;; Fastest: Algorithm B
;;
;; Algorithm B: 1,234,567 ops/sec (fastest)
;; Algorithm A: 987,654 ops/sec (1.3x slower)
;; Algorithm C: 456,789 ops/sec (2.7x slower)
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

### Custom Timing

**`with-benchmark-timing`** - Execute code with timing output:

```lisp
(with-benchmark-timing
  (expensive-operation)
  (another-operation))
;; Output: Execution time: 1.234 seconds
```

### Benchmark Suites

Organize related benchmarks into logical groups:

```lisp
;; Define suite benchmarks
(defbenchmark sort-bubble-sort () (bubble-sort *test-data*))
(defbenchmark sort-quick-sort () (quick-sort *test-data*))
(defbenchmark sort-merge-sort () (merge-sort *test-data*))

;; Run sorting algorithm comparison
(run-benchmark-suite '(sort-bubble-sort sort-quick-sort sort-merge-sort))
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
# epsilon.test

Comprehensive testing framework for Epsilon with property-based testing, snapshots, benchmarks, and parallel execution.

## Installation

```lisp
;; In your module definition
(package my-tests
  (import (epsilon.test test)))
```

## Features

- **Unit Testing** - Define and run tests with rich assertions
- **Property-Based Testing** - QuickCheck-style generative testing
- **Snapshot Testing** - Jest/Insta-style golden file testing
- **Benchmarking** - Statistical analysis of performance
- **Parallel Execution** - Run tests concurrently
- **Process Isolation** - Safe testing of crash-prone code
- **Mocking** - Spies, stubs, and mocks

## Quick Start

```lisp
(package my-tests
  (import (epsilon.test test)))

(test:deftest addition-works
  "Basic arithmetic should work"
  (test:is (= 4 (+ 2 2)))
  (test:is (= 0 (+ -1 1))))

;; Run tests
;; ./epsilon --test my-tests
```

## Writing Tests

### Basic Tests

```lisp
(test:deftest test-name
  "Optional documentation string"
  (test:is (= expected actual))
  (test:ok (predicate value)))
```

### Assertions

```lisp
;; Equality assertions
(test:is (= expected actual))            ; Numeric equality
(test:is (equal expected actual))        ; General equality
(test:is (string= expected actual))      ; String equality
(test:is (equalp expected actual))       ; Case-insensitive

;; Boolean assertions
(test:ok expression)                     ; Expression is truthy
(test:ok (not expression))               ; Expression is falsy

;; Approximate equality (for floats)
(test:is (test:approx= 3.14 pi 0.01))   ; Within tolerance

;; Type assertions
(test:is (typep value 'string))
(test:is (listp value))

;; Collection assertions
(test:is (member item list))
(test:is (= (length list) 3))
```

### Error Assertions

```lisp
;; Assert that code signals a condition
(test:signals error
  (error "This should be caught"))

;; Assert specific condition type
(test:signals my-error-type
  (signal-my-error))

;; Assert no error is signaled
(test:no-error
  (safe-operation))
```

### Test Setup and Teardown

```lisp
;; Per-test setup
(test:deftest with-setup
  (test:with-fixture
    :setup (lambda () (create-temp-file))
    :teardown (lambda (file) (delete-file file))
    (lambda (file)
      (test:ok (probe-file file)))))

;; Shared setup for multiple tests
(test:deffixture database-connection ()
  :setup (lambda () (connect-database))
  :teardown (lambda (conn) (disconnect conn)))

(test:deftest uses-database
  (test:with-fixtures (database-connection conn)
    (test:ok (query conn "SELECT 1"))))
```

## Property-Based Testing

Generate random inputs to find edge cases automatically.

### Basic Properties

```lisp
(test:defproperty reverse-reverse
  "Reversing a list twice yields the original"
  ((xs (test:gen-list (test:gen-integer))))
  (test:is (equal xs (reverse (reverse xs)))))

(test:defproperty sort-is-sorted
  "Sorted list is always sorted"
  ((xs (test:gen-list (test:gen-integer))))
  (test:ok (sorted-p (sort (copy-list xs) #'<))))
```

### Built-in Generators

```lisp
(test:gen-integer)                      ; Random integer
(test:gen-integer :min 0 :max 100)      ; Bounded integer
(test:gen-float)                        ; Random float
(test:gen-string)                       ; Random string
(test:gen-string :length 10)            ; Fixed length
(test:gen-symbol)                       ; Random symbol
(test:gen-boolean)                      ; T or NIL

(test:gen-list gen)                     ; List of gen values
(test:gen-list gen :min-length 1 :max-length 10)
(test:gen-vector gen)                   ; Vector of gen values

(test:gen-one-of '(:a :b :c))          ; One of the values
(test:gen-frequency                     ; Weighted choice
  '((80 . (test:gen-integer))
    (20 . (test:gen-nil))))

(test:gen-json-value)                   ; Valid JSON value
```

### Custom Generators

```lisp
(defun gen-user ()
  "Generate a random user"
  (test:gen-let ((name (test:gen-string :min-length 1))
                 (age (test:gen-integer :min 0 :max 120))
                 (email (gen-email)))
    (make-user :name name :age age :email email)))

(test:defproperty user-serialization
  ((user (gen-user)))
  (test:is (equal user (deserialize (serialize user)))))
```

### Shrinking

When a property fails, the framework automatically shrinks the failing input to find the minimal failing case:

```lisp
;; If this fails with xs = (1 2 3 4 5), shrinking might find
;; that xs = (3) is the minimal failing case
(test:defproperty all-small
  ((xs (test:gen-list (test:gen-integer :min 0 :max 10))))
  (test:ok (every (lambda (x) (< x 3)) xs)))
```

## Snapshot Testing

Compare output against saved "golden" files.

```lisp
(test:deftest json-output-format
  (let ((data '(:users ((:name "Alice") (:name "Bob")))))
    (test:assert-snapshot "users.json" (json:encode data :pretty t))))

;; First run: creates snapshots/users.json
;; Subsequent runs: compares against saved snapshot
```

### Updating Snapshots

```bash
# Update all snapshots
./epsilon --test my-tests --update-snapshots

# Review changes in version control
git diff snapshots/
```

## Benchmarking

Measure and track performance.

### Basic Benchmarks

```lisp
(test:defbenchmark json-parsing
  "Benchmark JSON parsing speed"
  :setup (let ((json (make-large-json)))
           (lambda () json))
  :body (lambda (json)
          (json:parse json)))

;; Run benchmarks
;; ./epsilon --benchmark my-tests
```

### Benchmark Options

```lisp
(test:defbenchmark my-benchmark
  :warmup 100          ; Warmup iterations
  :iterations 1000     ; Measurement iterations
  :setup (lambda () (prepare-data))
  :teardown (lambda (data) (cleanup data))
  :body (lambda (data) (process data)))
```

### Output

```
json-parsing
  Mean:    1.234ms
  Std Dev: 0.056ms
  Min:     1.123ms
  Max:     1.456ms
  Median:  1.230ms
  Samples: 1000
```

### Regression Detection

```bash
# Save baseline
./epsilon --benchmark my-tests --save-baseline

# Check for regressions (fails if >10% slower)
./epsilon --benchmark my-tests --check-regression
```

## Parallel Execution

Run tests concurrently for faster feedback.

```lisp
;; Tests run in parallel by default
./epsilon --test my-tests

;; Force sequential execution
./epsilon --test my-tests --sequential

;; Specify worker count
./epsilon --test my-tests --workers 4
```

### Isolated Tests

For tests that can't run in parallel:

```lisp
(test:deftest modifies-global-state
  :isolated t  ; Runs in separate process
  (setf *global* 42)
  (test:is (= *global* 42)))
```

## Mocking

### Spies

Track function calls without changing behavior:

```lisp
(test:with-spy (http:get spy)
  (my-function-that-fetches)
  (test:is (= 1 (test:spy-call-count spy)))
  (test:is (equal "https://api.example.com"
                  (first (test:spy-calls spy)))))
```

### Stubs

Replace function behavior:

```lisp
(test:with-stub (http:get (constantly mock-response))
  (let ((result (my-function-that-fetches)))
    (test:is (equal expected result))))
```

### Mocks

Combine spy and stub:

```lisp
(test:with-mock (http:get mock)
  :returns mock-response
  :when (lambda (url) (search "api.example.com" url))

  (my-function-that-fetches)

  (test:is (= 1 (test:mock-call-count mock))))
```

## Running Tests

### Command Line

```bash
# Run all tests in a module
./epsilon --test epsilon.json

# Run specific test
./epsilon --test epsilon.json --filter "parse-*"

# Verbose output
./epsilon --test epsilon.json --verbose

# Stop on first failure
./epsilon --test epsilon.json --fail-fast

# Generate JUnit XML report
./epsilon --test epsilon.json --output junit.xml
```

### Programmatic

```lisp
;; Run and get results
(let ((results (test:run-tests 'my-tests)))
  (test:results-passed results)    ; Number passed
  (test:results-failed results)    ; Number failed
  (test:results-errors results))   ; Number with errors

;; Run with options
(test:run-tests 'my-tests
  :verbose t
  :fail-fast t
  :filter "parse-*")
```

## Test Organization

### Recommended Structure

```
my-module/
  src/
    my-module.lisp
  tests/
    my-module-tests.lisp    ; Unit tests
    my-module-props.lisp    ; Property tests
    snapshots/              ; Snapshot files
    fixtures/               ; Test data files
```

### Test File Template

```lisp
;;;; my-module-tests.lisp - Tests for my-module

(package my-module-tests
  (import (epsilon.test test)
          (my-module m)))

;;; Unit Tests

(test:deftest basic-operation
  (test:is (= 42 (m:compute 6 7))))

;;; Property Tests

(test:defproperty roundtrip
  ((x (test:gen-integer)))
  (test:is (= x (m:decode (m:encode x)))))
```

## See Also

- [CONTRIBUTING.md](../../CONTRIBUTING.md) - Testing requirements for contributions
- [epsilon](../core/) - Core module with test utilities

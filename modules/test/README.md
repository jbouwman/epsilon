# Epsilon Test Framework

A comprehensive testing framework for Epsilon with hierarchical test organization, fixtures, multiple output formats, and integration with the build system.

## Overview

The Epsilon test framework provides:

- **Hierarchical test organization** with suites and packages
- **Rich assertion macros** for various comparison types
- **Test fixtures** for setup/teardown operations
- **Multiple output formats** (shell, JUnit XML, REPL)
- **Build system integration** for module-based testing
- **Test selection** by name, package, or module
- **Metrics collection** with detailed reporting

## Core Components

### Test Definition

Use `deftest` to define individual tests:

```lisp
(deftest basic-arithmetic-test
  "Test basic arithmetic operations"
  (is-= (+ 2 3) 5)
  (is-= (* 4 5) 20)
  (is-not (= 10 11)))

(deftest string-operations-test
  "Test string manipulation"
  (is-equal (string-upcase "hello") "HELLO")
  (is (string-contains-p "hello world" "world")))
```

### Assertion Macros

**Basic assertions:**
- `(is form)` - Test that form is truthy
- `(is-not form)` - Test that form is falsy

**Equality assertions:**
- `(is-= actual expected)` - Numeric equality (`=`)
- `(is-eq actual expected)` - Identity (`eq`)
- `(is-eql actual expected)` - Same object (`eql`)
- `(is-equal actual expected)` - Structural equality (`equal`)
- `(is-equalp actual expected)` - Case-insensitive equality (`equalp`)

**Exception testing:**
```lisp
(is-thrown (error) (error "This will throw"))
(is-thrown (type-error "Invalid.*type") (length 42))
```

### Test Fixtures

Set up and tear down test environments:

```lisp
(fixture database-fixture
  :setup (lambda () (connect-to-test-db))
  :teardown (lambda () (disconnect-from-test-db)))

(deftest database-test
  "Test database operations"
  (with-fixture database-fixture
    (is-equal (count-records :users) 0)
    (create-user "Alice")
    (is-= (count-records :users) 1)))
```

### Test Labels

Group related assertions within a test:

```lisp
(deftest comprehensive-test
  "Test multiple aspects of a function"
  (with-label "Input validation"
    (is-thrown (error) (my-function nil))
    (is-thrown (error) (my-function "")))
  
  (with-label "Normal operation"
    (is-equal (my-function "hello") "HELLO")
    (is-equal (my-function "world") "WORLD")))
```

## Running Tests

### Single Module Testing

```bash
# Test a specific module
./epsilon test --module epsilon.core

# Test with specific format
./epsilon test --module epsilon.http --format junit

# Save results to file
./epsilon test --module epsilon.json --output results.xml
```

### Programmatic Testing

```lisp
;; Run all tests in a module
(epsilon.test:run :module "epsilon.core")

;; Run specific package tests
(epsilon.test:run :package "epsilon.map")

;; Run specific test
(epsilon.test:run :name "map-creation-test")

;; Run with different output format
(epsilon.test:run :module "epsilon.string" :format :junit :file "results.xml")
```

### Run All Tests

```lisp
;; Test all modules
(epsilon.test:run-all)

;; Include platform-specific modules
(epsilon.test:run-all :include-platform t)

;; Generate JUnit report
(epsilon.test:run-all :format :junit :file "full-test-report.xml")
```

## Output Formats

### Shell Format (Default)
Human-readable console output with colors and progress indicators:
```
;;; Running 15 tests...
✓ basic-arithmetic-test (0.001s)
✓ string-operations-test (0.002s)
✗ failing-test (0.001s)
  Expected 5 but got 4

Tests: 15, Passed: 14, Failed: 1, Errors: 0
```

### JUnit XML Format
Compatible with CI/CD systems and test reporting tools:
```xml
<testsuite name="epsilon.core" tests="15" failures="1" errors="0" time="0.125">
  <testcase name="basic-arithmetic-test" time="0.001"/>
  <testcase name="failing-test" time="0.001">
    <failure message="Expected 5 but got 4"/>
  </testcase>
</testsuite>
```

### REPL Format
Minimal output suitable for interactive development:
```
15 tests: 14 passed, 1 failed
```

## Advanced Features

### Test Selection

Filter tests by various criteria:

```lisp
;; Run tests matching name pattern
(epsilon.test:run :name ".*arithmetic.*")

;; Run tests from specific packages
(epsilon.test:run :package "epsilon.map epsilon.set")

;; Combine filters
(epsilon.test:run :module "epsilon.core" :name ".*creation.*")
```

### Skip Tests

Conditionally skip tests:

```lisp
(deftest platform-specific-test
  "Test platform-specific functionality"
  (when (not (platform-supported-p))
    (skip "Platform not supported"))
  (is-equal (platform-function) expected-result))
```

### Build System Integration

The test framework integrates with Epsilon's build system:

- **Automatic compilation** - Tests are compiled with their modules
- **Dependency resolution** - Test dependencies are loaded automatically
- **Incremental testing** - Only recompile tests when source changes
- **Module isolation** - Each module's tests run in clean environment

## Test Organization

### Module Structure
```
src/test/
├── module.lisp          # Module definition
├── src/tool/
│   ├── test.lisp         # Main test API
│   └── test/
│       ├── suite.lisp    # Test suite management
│       ├── report.lisp   # Output formatting
│       └── fixture.lisp  # Fixture system
└── tests/
    └── test-framework-tests.lisp  # Self-tests
```

### Test Discovery

Tests are automatically discovered based on:
- **Package naming** - Packages ending in `-tests` or `.tests`
- **Function naming** - Functions defined with `deftest`
- **Module structure** - Tests in `tests/` directories

## Best Practices

### Test Naming
```lisp
;; Good: Descriptive, specific names
(deftest map-assoc-creates-new-map)
(deftest string-split-handles-empty-string)
(deftest http-client-handles-timeout)

;; Avoid: Generic names
(deftest test1)
(deftest basic-test)
```

### Test Structure
```lisp
(deftest well-structured-test
  "Clear documentation of what is being tested"
  ;; Arrange
  (let ((input "test data")
        (expected "expected result"))
    
    ;; Act
    (let ((actual (function-under-test input)))
      
      ;; Assert
      (is-equal actual expected))))
```

### Error Messages
```lisp
;; Provide context in assertion messages
(is-= (length result) 3 "Result should contain exactly 3 items")
(is result "Function should return non-nil value")
```

## Performance

- **Fast execution** - Minimal overhead for assertion checking
- **Parallel testing** - Can run multiple test modules concurrently
- **Incremental compilation** - Only recompile changed tests
- **Memory efficient** - Tests run in isolated environments

## Dependencies

- **epsilon.core** - Core utilities and data structures
- **SBCL** - Steel Bank Common Lisp
- **Build system** - Epsilon's module management

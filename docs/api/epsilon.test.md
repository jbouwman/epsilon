# epsilon.test

Testing framework for Epsilon modules.

## Overview

The `epsilon.test` package provides test definition, execution, and reporting. Supports fixtures, multiple assertion types, and output formats including shell, JUnit XML, and TAP.

## Quick Start

```lisp
(defpackage :my-app-tests
  (:use :cl :epsilon.test))

(in-package :my-app-tests)

;; Define a test
(deftest test-arithmetic
  "Test basic arithmetic operations"
  (is-= (+ 2 2) 4)
  (is-= (* 3 3) 9)
  (is-not (= 1 2)))

;; Run tests
(run (epsilon.loader:environment) 'my-app)
```

## Test Definition

### deftest
`(deftest name &body body)`

Define a test function and register it with the test suite.

```lisp
(deftest test-string-operations
  "Test string manipulation functions"
  (is-equal (string-upcase "hello") "HELLO")
  (is (string= "abc" "abc"))
  (is-true (stringp "test")))
```

### with-label
`(with-label label &body body)`

Group assertions under a descriptive label.

```lisp
(deftest test-user-validation
  (with-label "Valid users"
    (is-true (valid-user-p (make-user :name "Alice")))
    (is-true (valid-user-p (make-user :name "Bob"))))
  
  (with-label "Invalid users"
    (is-not (valid-user-p (make-user :name "")))
    (is-not (valid-user-p nil))))
```

## Test Execution

### run
`(run environment module &key package test-name format file) => test-result`

Run tests for a module with optional filtering.

```lisp
;; Run all tests for a module
(run env 'epsilon.core)

;; Run specific package tests
(run env 'epsilon.core :package "epsilon.string")

;; Run specific test by name pattern
(run env 'epsilon.core :test-name "string.*")

;; Output to file in JUnit format
(run env 'epsilon.core :format :junit :file "results.xml")
```

**Parameters:**
- `environment` - The loader environment
- `module` - Module to test (e.g., 'epsilon.core')
- `package` - Package pattern to filter tests (optional)
- `test-name` - Test name pattern to run (optional)
- `format` - Output format: :shell, :verbose, :junit, :tap (default: :shell)
- `file` - Output file for test report (optional)

### success-p
`(success-p run) => boolean`

Return T if test run completed successfully with no failures.

```lisp
(let ((result (run env 'my-module)))
  (if (success-p result)
      (format t "All tests passed!~%")
      (format t "Some tests failed~%")))
```

### skip
`(skip &optional message)`

Skip the current test with an optional message.

```lisp
(deftest test-platform-specific
  (when (not (featurep :unix))
    (skip "Unix-only test"))
  (is-equal (get-home-dir) "/home/user"))
```

## Assertions

### Basic Assertions

#### is
`(is form &optional description)`

Assert that form evaluates to true.

```lisp
(is (> 10 5))
(is (listp '(1 2 3)) "Should be a list")
```

#### is-true
`(is-true form &optional description)`

Assert that form evaluates to true (explicit).

```lisp
(is-true (evenp 4))
(is-true (null '()))
```

#### is-not
`(is-not form &optional description)`

Assert that form evaluates to false.

```lisp
(is-not (= 1 2))
(is-not (member 'x '(a b c)))
```

#### is-not-null
`(is-not-null form &optional description)`

Assert that form does not evaluate to NIL.

```lisp
(is-not-null (find 'a '(a b c)))
(is-not-null (gethash :key *table*))
```

### Equality Assertions

#### is-=
`(is-= actual expected &optional description)`

Assert numeric equality using =.

```lisp
(is-= (+ 2 2) 4)
(is-= (* 3.0 2.0) 6.0)
```

#### is-eq
`(is-eq actual expected &optional description)`

Assert object identity using EQ.

```lisp
(is-eq 'foo 'foo)
(is-eq (intern "TEST") 'test)
```

#### is-eql
`(is-eql actual expected &optional description)`

Assert equality using EQL.

```lisp
(is-eql 42 42)
(is-eql #\a #\a)
```

#### is-equal
`(is-equal actual expected &optional description)`

Assert structural equality using EQUAL.

```lisp
(is-equal '(1 2 3) '(1 2 3))
(is-equal "hello" "hello")
```

#### is-equalp
`(is-equalp actual expected &optional description)`

Assert equality using EQUALP (case-insensitive strings, etc).

```lisp
(is-equalp "Hello" "HELLO")
(is-equalp #(1 2 3) '(1 2 3))
```

### Exception Assertions

#### is-thrown
`(is-thrown condition-type form &optional description)`

Assert that form signals a specific condition.

```lisp
(is-thrown division-by-zero (/ 1 0))
(is-thrown type-error (+ "string" 1))
```

### Predicate Assertion

#### is-p
`(is-p predicate value &optional description)`

Assert that (predicate value) returns true.

```lisp
(is-p #'stringp "hello")
(is-p #'evenp 4)
(is-p #'listp '(a b c))
```

## Fixtures

### fixture
`(fixture name &key setup teardown)`

Define a test fixture with setup and teardown.

```lisp
(fixture database-fixture
  :setup (lambda () 
           (setf *db* (connect-db))
           (init-test-data))
  :teardown (lambda ()
              (cleanup-test-data)
              (disconnect-db *db*)))
```

### with-fixture
`(with-fixture fixture-name &body body)`

Run body with fixture setup/teardown.

```lisp
(deftest test-database-operations
  (with-fixture database-fixture
    (is-equal (count-users) 0)
    (add-user "Alice")
    (is-equal (count-users) 1)))
```

## Utilities

### module-file
`(module-file module-name relative-path) => absolute-path`

Return absolute path to file within module's directory.

```lisp
(module-file 'epsilon.test "fixtures/data.txt")
; => "/path/to/epsilon/modules/test/fixtures/data.txt"
```

## Output Formats

### Shell Format (Default)
Concise output with colored status indicators:
- Green dots for passed tests
- Red F for failures
- Yellow S for skipped

### Verbose Format
Detailed output showing each test and assertion:
```
Running test-arithmetic...
  ✓ (+ 2 2) = 4
  ✓ (* 3 3) = 9
  PASSED
```

### JUnit XML Format
Standard JUnit XML for CI integration:
```xml
<testsuite name="epsilon.core" tests="30" failures="0">
  <testcase name="test-string-split" time="0.001"/>
  ...
</testsuite>
```

### TAP Format
Test Anything Protocol output:
```
1..30
ok 1 - test-string-split
ok 2 - test-string-join
...
```

## Test Organization

### Test Discovery
Tests are automatically discovered when loading module test files:
- Test files should be in `modules/<name>/tests/`
- Files should match pattern `*-tests.lisp`
- Tests are registered when `deftest` is evaluated

### Test Selection
Use patterns to run specific tests:
```lisp
;; Run tests matching package pattern
(run env 'epsilon.core :package "epsilon.string.*")

;; Run tests matching name pattern
(run env 'epsilon.core :test-name "test-.*-operation")
```

## Examples

### Basic Test Suite

```lisp
(defpackage :my-lib-tests
  (:use :cl :epsilon.test :my-lib))

(in-package :my-lib-tests)

(deftest test-parse-integer-valid
  "Test parsing valid integers"
  (is-= (parse-int "42") 42)
  (is-= (parse-int "-10") -10)
  (is-= (parse-int "0") 0))

(deftest test-parse-integer-invalid
  "Test parsing invalid integers"
  (is-thrown parse-error (parse-int "abc"))
  (is-thrown parse-error (parse-int "12.34"))
  (is-null (parse-int "" :default nil)))
```

### Testing with Fixtures

```lisp
(fixture temp-file-fixture
  :setup (lambda ()
           (setf *temp-file* (make-temp-file)))
  :teardown (lambda ()
              (when (probe-file *temp-file*)
                (delete-file *temp-file*))))

(deftest test-file-operations
  (with-fixture temp-file-fixture
    (write-to-file *temp-file* "test data")
    (is-equal (read-from-file *temp-file*) "test data")
    (is-true (file-exists-p *temp-file*))))
```

### Hierarchical Test Organization

```lisp
(deftest test-user-management
  (with-label "User creation"
    (let ((user (create-user :name "Alice")))
      (is-not-null user)
      (is-equal (user-name user) "Alice")))
  
  (with-label "User validation"
    (is-true (valid-user-p (make-user :name "Bob" :age 25)))
    (is-not (valid-user-p (make-user :name "" :age 25)))
    (is-not (valid-user-p (make-user :name "Eve" :age -1))))
  
  (with-label "User persistence"
    (with-fixture database-fixture
      (let ((user (create-user :name "Charlie")))
        (save-user user)
        (is-equal (find-user "Charlie") user)))))
```

## Testing Guidelines

1. Use `test-` prefix for test names
2. Test single behaviors per test
3. Select appropriate equality assertion
4. Use fixtures for resource cleanup
5. Group assertions with `with-label`
6. Include edge cases and error conditions
7. Mock external dependencies for speed

## Integration with CI/CD

```bash
# Run tests with JUnit output for CI
./epsilon --test epsilon.core --format junit --output test-results.xml

# Run specific test suite
./epsilon --test my-app:my-package

# Run with verbose output for debugging
./epsilon --test my-app --format verbose
```

## See Also

- [epsilon.test.fixture](epsilon.test.fixture.md) - Fixture details
- [epsilon.test.report](epsilon.test.report.md) - Report formats
- [Development Guide](../development/testing.md) - Testing best practices
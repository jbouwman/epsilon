# Testing Framework Enhancement Roadmap

## Current Status

Epsilon has a functional testing framework with test definition, execution, and reporting capabilities. The framework needs completion and additional features for development workflows.

## Phase 1: Framework Completion (1-2 weeks)

### Low Difficulty Items
- **Complete missing definitions**: Fix `test` class, `timing-metric` class, and `sort-into-suites` function
- **Fix export inconsistencies**: Resolve `equal`/`equal-p` vs `is-equal`/`is-equalp` mismatches  
- **Complete formatter factory**: Implement consistent `formatter:make` interface

### Medium Difficulty Items
- **TAP formatter**: Output test results in Test Anything Protocol format for integration with TAP consumers
- **JUnit XML formatter**: Generate JUnit-compatible XML reports for CI/CD integration and IDE test runners

## Phase 2: Architecture Refactoring (2-3 weeks)

### Component Extraction
- **Extract metrics system**: Move to `src/tool/test/metrics.lisp` with expanded capabilities
  - Memory usage, GC statistics, assertion count metrics
  - Metric aggregation and reporting utilities
  - Performance regression detection
- **Extract results management**: Move to `src/tool/test/results.lisp`
  - Result filtering, sorting, analysis functions
  - Separate execution from presentation logic
  - Historical result comparison
- **Create test registry**: Extract to `src/tool/test/registry.lisp`
  - Test metadata (tags, descriptions, dependencies)
  - Suite dependency tracking
  - Test discovery and organization

## Phase 3: Additional Testing Features (3-4 weeks)

### Additional Test Capabilities
- **Parameterized tests**: Data-driven testing with multiple input sets
- **Test fixtures**: Setup and teardown with scope management ✅ **(COMPLETED)**
- **Mocking framework**: Test doubles for isolation and verification
- **Property-based testing**: Generative testing with shrinking
- **Snapshot testing**: Golden file comparison for complex outputs

### Integration Features
- **IDE integration**: LSP support for test discovery and execution
- **Build system integration**: Automatic test execution on file changes
- **Coverage reporting**: Code coverage analysis and visualization
- **Parallel execution**: Multi-threaded test running for large suites

## Technical Architecture

### Core Components
1. **Test Registry**: Centralized test discovery and metadata management
2. **Execution Engine**: Parallel test runner with isolation
3. **Metrics Collection**: Performance and resource usage tracking
4. **Results Management**: Storage, analysis, and historical comparison
5. **Formatter System**: Pluggable output formats for different consumers

### Integration Points
- **epsilon.tool.build**: Automatic test discovery and dependency tracking
- **epsilon.lsp**: IDE integration for test running and results
- **epsilon.map**: Efficient test metadata and result storage
- **epsilon.sys.thread**: Parallel test execution
- **epsilon.json**: JUnit XML and JSON result export

## Success Metrics

### Phase 1 Success Criteria
- All tests run without framework errors
- Consistent API with exports
- TAP and JUnit output compatibility

### Phase 2 Success Criteria
- Clean separation of concerns in framework architecture
- Extensible plugin system for new formatters
- Performance metrics collection and reporting

### Phase 3 Success Criteria
- Testing capabilities matching other frameworks
- IDE integration for interactive development
- Parallel execution for large test suites

## Test Fixtures System

### Overview

The epsilon test framework provides a  fixture system for managing test setup and teardown, similar to frameworks like JUnit or pytest.

### Defining Fixtures

Use `deffixture` to define reusable test setup/teardown:

```lisp
(deffixture test-database ()
  (:setup
   (let ((db (create-test-database)))
     (populate-test-data db)
     db))
  (:teardown
   (destroy-test-database db)))
```

### Using Fixtures

Use `with-fixture` for single fixtures:

```lisp
(deftest test-database-operations ()
  (with-fixture (db test-database)
    (is-equal 10 (count-users db))
    (add-user db "test@example.com")
    (is-equal 11 (count-users db))))
```

Use `with-fixtures` for multiple fixtures:

```lisp
(deftest test-api-integration ()
  (with-fixtures ((server test-server)
                  (db test-database)
                  (client test-client))
    (let ((response (client-get client "/api/users")))
      (is-equal 200 (response-status response)))))
```

### Built-in Fixtures

#### Temporary Files and Directories

```lisp
(deftest test-file-processing ()
  (with-temp-file (file :content "test data" :suffix ".txt")
    (is (file-exists-p file))
    (is-equal "test data" (read-file file))))

(deftest test-directory-operations ()
  (with-temp-directory (dir :prefix "test-")
    (let ((file (path-join dir "test.txt")))
      (write-file file "content")
      (is (file-exists-p file)))))
```

#### Test Data and Environment

```lisp
(deftest test-data-processing ()
  (with-test-data (users (list (make-user :name "Alice")
                               (make-user :name "Bob")))
    (is-equal 2 (length users))))

(deftest test-clean-environment ()
  (with-clean-environment ("HOME" "USER" "PATH")
    ;; Test runs with these env vars temporarily cleared
    (is-null (getenv "HOME"))))
```

### Fixture Parameters

Fixtures can accept parameters for flexible configuration:

```lisp
(deffixture test-server-on-port (port)
  (:setup
   (start-server :port port))
  (:teardown
   (stop-server)))

(deftest test-specific-port ()
  (with-fixture (server test-server-on-port 8080)
    (is-equal 8080 (server-port server))))
```

### Best Practices

1. **Keep fixtures focused**: Each fixture should handle one concern
2. **Always provide teardown**: Ensure resources are cleaned up
3. **Make fixtures reusable**: Design for use across multiple tests
4. **Use built-in fixtures**: Prefer built-in fixtures when applicable
5. **Handle errors gracefully**: Teardown should run even if setup fails

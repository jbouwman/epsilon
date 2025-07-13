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
- **Test fixtures**: Setup and teardown with scope management
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
- **epsilon.lib.map**: Efficient test metadata and result storage
- **epsilon.sys.thread**: Parallel test execution
- **epsilon.lib.json**: JUnit XML and JSON result export

## Success Metrics

### Phase 1 Success Criteria
- All tests run without framework errors
- Consistent API with proper exports
- TAP and JUnit output compatibility

### Phase 2 Success Criteria
- Clean separation of concerns in framework architecture
- Extensible plugin system for new formatters
- Performance metrics collection and reporting

### Phase 3 Success Criteria
- Testing capabilities matching other frameworks
- IDE integration for interactive development
- Parallel execution for large test suites
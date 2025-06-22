# Epsilon Wishlist

Future capabilities and enhancements organized by difficulty and priority.

## Testing Framework

### Medium Difficulty
- **TAP formatter**: Output test results in Test Anything Protocol format for integration with TAP consumers
- **JUnit XML formatter**: Generate JUnit-compatible XML reports for CI/CD integration and IDE test runners

## Module System

### Low Difficulty
- **Simplified package definition macro**: Replace verbose `defpackage` with cleaner `module` syntax
  - Support `(import package...)` for regular imports
  - Support `(import (package alias)...)` for aliased imports  
  - Support `(from package symbols...)` for selective imports
  - Support `(export symbols...)` and `(shadow symbols...)`
  - Automatic `in-package` after definition
  - Good pretty-printing with parentheses around all clauses

## Test Framework Completion

### Low Difficulty  
- **Complete missing definitions**: Fix `test` class, `timing-metric` class, and `sort-into-suites` function
- **Fix export inconsistencies**: Resolve `equal`/`equal-p` vs `is-equal`/`is-equalp` mismatches
- **Complete formatter factory**: Implement consistent `formatter:make` interface

### Medium Difficulty
- **Extract metrics system**: Move to `src/tool/test/metrics.lisp` with expanded capabilities
  - Memory usage, GC statistics, assertion count metrics
  - Metric aggregation and reporting utilities
- **Extract results management**: Move to `src/tool/test/results.lisp`
  - Result filtering, sorting, analysis functions
  - Separate execution from presentation logic
- **Create test registry**: Extract to `src/tool/test/registry.lisp`
  - Test metadata (tags, descriptions, dependencies)
  - Suite dependency tracking

## Performance & Benchmarking

### Medium Difficulty
- **Data structure benchmarks**: Performance testing suite for map, set, and sequence implementations
  - Add/remove operations across different sizes (10, 100, 1K, 10K, 100K elements)
  - Lookup performance comparison with native hash tables and lists
  - Memory usage profiling for different data structure sizes
  - Iteration and traversal performance measurements

## Low Priority Items

*Additional items to be added as requirements are identified*
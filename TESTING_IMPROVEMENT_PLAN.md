# Testing Improvement Plan

## Executive Summary

This document outlines a comprehensive plan to improve testing across the epsilon project, with specific focus on achieving better test coverage, reliability, and automation after merging the compute branch.

## Current State

### Main Branch Testing
- **Status**: Well-established test infrastructure
- **Recent improvements**: Linux async networking stack testing (#145)
- **Benchmark framework**: Recently enhanced (#143)
- **Test organization**: Consistent across modules

### Compute Branch Testing
- **Total tests**: 173
- **Passing**: 120 (69.4%)
- **Skipped**: 53 (30.6%)
- **Test files**: 21 files covering all major features
- **Issues**: Syntax errors, placeholder implementations, incomplete features

### Gap Analysis

**Strengths**:
- Good test coverage of implemented features
- Well-organized test files by functionality
- Comprehensive test suites (autodiff, broadcasting, egraph, etc.)
- Test helper utilities in place

**Weaknesses**:
- 30.6% tests skipped (53 tests)
- Some tests have syntax errors
- Missing integration tests between modules
- No property-based testing
- Limited numerical accuracy tests
- No automated performance regression testing
- Missing cross-platform validation

## Testing Improvement Phases

## Phase 1: Fix Existing Tests (Week 1-2)

### Goal: Achieve >80% pass rate, 0 errors in compute module

### 1.1 Fix Syntax Errors

**Tasks**:
- [ ] Review all test files for syntax errors
- [ ] Fix `is-thrown` macro usage (needs parentheses around condition type)
- [ ] Remove undefined macros (e.g., `c:with-memoization`)
- [ ] Fix tape node count expectations
- [ ] Ensure all test helper functions are defined

**Files to review**:
- `modules/compute/tests/auto-eval-tests.lisp`
- `modules/compute/tests/autodiff-tests.lisp`
- `modules/compute/tests/egraph-tests.lisp`
- All other test files with errors

**Acceptance Criteria**:
- All tests have valid syntax
- No undefined functions/macros
- Tests run without errors

---

### 1.2 Categorize Skipped Tests

**Tasks**:
- [ ] Review all 53 skipped tests
- [ ] Categorize by reason:
  - Not implemented (feature missing)
  - Integration test (requires other modules)
  - Known bug (documented issue)
  - Performance test (requires optimization)
  - Platform-specific (Linux/Darwin only)
- [ ] Add clear skip markers with reasons
- [ ] Document in test status report

**Skip marker format**:
```lisp
(skip-test "Feature not yet implemented: reverse-mode autodiff gradient accumulation"
  (test-gradient-accumulation ...))
```

**Acceptance Criteria**:
- All skips have documented reasons
- Categorization complete
- Test status report updated

---

### 1.3 Implement Test Mocking/Stubbing

**Tasks**:
- [ ] Create mock implementations for unimplemented features
- [ ] Add stubs for BLAS operations (use native fallback in tests)
- [ ] Mock GPU operations (CPU fallback)
- [ ] Create test fixtures for common scenarios

**Example**:
```lisp
;; Test with mocked BLAS
(with-mocked-blas (:backend :native)
  (test-matrix-multiplication ...))
```

**Acceptance Criteria**:
- Mocking framework in place
- Tests can run without external dependencies
- Test isolation improved

---

## Phase 2: Add Missing Test Categories (Week 3-4)

### 2.1 Integration Tests

**Tasks**:
- [ ] Compute + Foreign module integration
  - Test BLAS integration with foreign FFI
  - Test marshalling of tensor data
- [ ] Compute + Core module integration
  - Test with epsilon.map data structures
  - Test with core utilities
- [ ] Cross-module data flow tests
- [ ] End-to-end example tests

**New test files**:
- `modules/compute/tests/integration-tests.lisp`
- `modules/compute/tests/ffi-integration-tests.lisp`
- `modules/compute/tests/examples-tests.lisp`

**Acceptance Criteria**:
- Integration test suite created
- All integration tests pass
- Examples validated as tests

---

### 2.2 Numerical Accuracy Tests

**Tasks**:
- [ ] Test autodiff against known derivatives
- [ ] Test matrix operations against NumPy/MATLAB results
- [ ] Test broadcasting against NumPy behavior
- [ ] Test special functions (sin, cos, exp, log) accuracy
- [ ] Validate gradient checking (finite differences)
- [ ] Test numerical stability edge cases

**Test categories**:
```lisp
;; Test autodiff accuracy
(deftest test-gradient-accuracy ()
  (let* ((x (c:var 'x))
         (f (c:sin x))
         (df (c:diff f x))
         (numerical-grad (finite-difference #'sin 1.0)))
    (assert-close (c:evaluate df '((x . 1.0)))
                  numerical-grad
                  :tolerance 1e-10)))
```

**Acceptance Criteria**:
- Numerical accuracy test suite created
- All operations tested against reference implementations
- Tolerance levels documented

---

### 2.3 Property-Based Testing

**Tasks**:
- [ ] Install/integrate property-based testing framework
- [ ] Define properties for symbolic operations
  - Commutativity: `a + b = b + a`
  - Associativity: `(a + b) + c = a + (b + c)`
  - Distributivity: `a * (b + c) = a*b + a*c`
- [ ] Define properties for autodiff
  - Linearity: `d/dx(af + bg) = a*d/dx(f) + b*d/dx(g)`
  - Chain rule validation
- [ ] Define properties for broadcasting
  - Shape consistency
  - Commutativity with broadcasting
- [ ] Define properties for E-graph
  - Equivalence preservation
  - Optimization correctness

**Framework options**:
- cl-quickcheck (Common Lisp port of QuickCheck)
- Custom property testing utilities

**Acceptance Criteria**:
- Property-based test framework integrated
- Properties defined for major features
- Tests generate random inputs

---

### 2.4 Edge Case Testing

**Tasks**:
- [ ] Test with empty tensors
- [ ] Test with scalar operations
- [ ] Test with very large tensors (memory limits)
- [ ] Test with very small numbers (underflow)
- [ ] Test with very large numbers (overflow)
- [ ] Test with NaN and Inf
- [ ] Test with negative zero
- [ ] Test with zero-dimensional arrays

**Acceptance Criteria**:
- Edge cases identified and documented
- Tests created for all edge cases
- Behavior documented

---

## Phase 3: Performance & Regression Testing (Week 5-6)

### 3.1 Benchmark Suite for Compute Module

**Tasks**:
- [ ] Create benchmark suite using epsilon.benchmark module
- [ ] Benchmark key operations:
  - Matrix multiplication (various sizes)
  - Autodiff backward pass
  - Broadcasting operations
  - E-graph saturation
  - Symbolic simplification
- [ ] Establish baseline performance
- [ ] Document performance characteristics
- [ ] Set performance budgets

**Benchmark file**:
`modules/compute/benchmarks/compute-benchmarks.lisp`

**Example**:
```lisp
(defbenchmark matrix-multiply-small ()
  (let ((a (random-matrix 10 10))
        (b (random-matrix 10 10)))
    (c:matmul a b)))

(defbenchmark matrix-multiply-medium ()
  (let ((a (random-matrix 100 100))
        (b (random-matrix 100 100)))
    (c:matmul a b)))

(defbenchmark autodiff-backward ()
  (let* ((x (c:var 'x))
         (y (c:var 'y))
         (f (c:+ (c:^ x 2) (c:* 3 y))))
    (c:backward f)))
```

**Acceptance Criteria**:
- Benchmark suite created
- Baselines established
- Performance characteristics documented

---

### 3.2 Performance Regression Detection

**Tasks**:
- [ ] Integrate benchmark suite into CI/CD
- [ ] Save benchmark baselines
- [ ] Compare against baselines on each commit
- [ ] Alert on regressions >10%
- [ ] Generate performance reports

**Implementation**:
```bash
# Run benchmarks and compare
make benchmark-compute
make benchmark-compare
```

**Acceptance Criteria**:
- Automated performance testing
- Regression detection working
- Performance reports generated

---

### 3.3 Memory Leak Testing

**Tasks**:
- [ ] Test for memory leaks in long-running operations
- [ ] Test E-graph memory growth
- [ ] Test autodiff tape cleanup
- [ ] Monitor memory usage during tests
- [ ] Add memory profiling tests

**Acceptance Criteria**:
- Memory leak tests created
- No memory leaks detected
- Memory usage documented

---

## Phase 4: Cross-Platform Testing (Week 7)

### 4.1 Platform-Specific Test Suites

**Tasks**:
- [ ] Identify platform-specific code
- [ ] Create Linux-specific test suite
- [ ] Create Darwin-specific test suite
- [ ] Create Windows-specific test suite (if applicable)
- [ ] Test FFI on each platform
- [ ] Test file I/O on each platform

**Acceptance Criteria**:
- Platform-specific tests identified
- Tests pass on all supported platforms

---

### 4.2 Continuous Integration Setup

**Tasks**:
- [ ] Set up CI for Linux testing
- [ ] Set up CI for macOS testing
- [ ] Set up CI for Windows testing (if needed)
- [ ] Run full test suite on each platform
- [ ] Run benchmarks on standardized hardware
- [ ] Generate test reports

**CI Platforms**:
- GitHub Actions
- GitLab CI
- Travis CI
- CircleCI

**Acceptance Criteria**:
- CI running on all platforms
- Test results reported
- Failures block merges

---

## Phase 5: Test Infrastructure Improvements (Week 8)

### 5.1 Test Organization

**Tasks**:
- [ ] Categorize tests:
  - **Unit tests**: Test individual functions
  - **Integration tests**: Test module interactions
  - **System tests**: Test end-to-end workflows
  - **Performance tests**: Benchmark critical operations
  - **Regression tests**: Prevent known bugs
- [ ] Separate fast and slow tests
- [ ] Create test suite configurations
- [ ] Document test organization

**Test suite examples**:
```bash
make test-unit       # Fast unit tests only
make test-all        # All tests
make test-integration # Integration tests
make test-performance # Benchmarks
```

**Acceptance Criteria**:
- Tests organized by category
- Fast/slow test separation
- Make targets for each category

---

### 5.2 Test Documentation

**Tasks**:
- [ ] Document test conventions
- [ ] Document test helper functions
- [ ] Create test writing guide
- [ ] Document assertion macros
- [ ] Add examples of good tests

**Documentation file**: `modules/compute/tests/README.md`

**Acceptance Criteria**:
- Test documentation complete
- Test writing guide available
- Examples provided

---

### 5.3 Test Reporting & Coverage

**Tasks**:
- [ ] Integrate code coverage tools
- [ ] Generate coverage reports
- [ ] Set coverage targets (>90% for core features)
- [ ] Visualize coverage
- [ ] Report coverage in CI

**Tools**:
- sb-cover (SBCL code coverage)
- Coverage reporters

**Acceptance Criteria**:
- Code coverage measured
- Coverage reports generated
- Coverage targets set

---

## Phase 6: Specialized Testing (Week 9-10)

### 6.1 Fuzzing

**Tasks**:
- [ ] Set up fuzzing framework
- [ ] Fuzz symbolic expression parser
- [ ] Fuzz pattern matcher
- [ ] Fuzz autodiff engine
- [ ] Fuzz broadcasting logic
- [ ] Document found issues

**Acceptance Criteria**:
- Fuzzing infrastructure in place
- Critical paths fuzzed
- Found issues fixed

---

### 6.2 Stress Testing

**Tasks**:
- [ ] Test with very deep expression trees
- [ ] Test with very large tensors
- [ ] Test with many variables
- [ ] Test E-graph saturation limits
- [ ] Test memory limits
- [ ] Document resource limits

**Acceptance Criteria**:
- Stress tests created
- Resource limits documented
- Graceful degradation verified

---

### 6.3 Differential Testing

**Tasks**:
- [ ] Compare compute module results with:
  - NumPy (broadcasting, matmul)
  - JAX (autodiff)
  - SymPy (symbolic computation)
  - Mathematica/Maxima (symbolic integration)
- [ ] Generate random test cases
- [ ] Validate equivalence
- [ ] Document differences

**Acceptance Criteria**:
- Differential tests created
- Results validated against reference implementations
- Differences documented and justified

---

## Test Quality Metrics

### Coverage Goals

| Module | Current | Target (3 months) | Target (6 months) |
|--------|---------|-------------------|-------------------|
| Core symbolic | 75% | 85% | 95% |
| Broadcasting | 80% | 90% | 95% |
| Autodiff | 60% | 80% | 90% |
| E-graph | 65% | 80% | 90% |
| Integration | 0% | 50% | 80% |
| **Overall** | **69%** | **82%** | **92%** |

### Test Pass Rate Goals

| Phase | Pass Rate | Errors | Skipped |
|-------|-----------|--------|---------|
| Current | 69.4% | Unknown | 30.6% |
| Phase 1 (Week 2) | >80% | 0 | <20% |
| Phase 2 (Week 4) | >85% | 0 | <15% |
| Phase 5 (Week 8) | >90% | 0 | <10% |
| Phase 6 (Week 10) | >95% | 0 | <5% |

### Performance Goals

| Operation | Current | Target | Stretch Goal |
|-----------|---------|--------|--------------|
| MatMul (100x100) | TBD | <1ms | <0.5ms |
| Autodiff backward | TBD | <10ms | <5ms |
| E-graph saturation | TBD | <100ms | <50ms |
| Broadcasting | TBD | <1ms | <0.5ms |

## Automation & CI/CD

### Automated Test Execution

```yaml
# .github/workflows/test.yml
name: Tests

on: [push, pull_request]

jobs:
  test:
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest]
        lisp: [sbcl]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v2
      - name: Setup Lisp
        uses: 40ants/setup-lisp@v1
        with:
          implementation: ${{ matrix.lisp }}
      - name: Run unit tests
        run: make test-unit
      - name: Run integration tests
        run: make test-integration
      - name: Generate coverage
        run: make coverage
      - name: Upload coverage
        uses: codecov/codecov-action@v2
```

### Automated Performance Testing

```yaml
# .github/workflows/benchmark.yml
name: Performance

on: [push, pull_request]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Run benchmarks
        run: make benchmark-compute
      - name: Compare with baseline
        run: make benchmark-compare
      - name: Upload results
        uses: actions/upload-artifact@v2
        with:
          name: benchmark-results
          path: benchmark-results.json
```

## Test Maintenance

### Regular Activities

**Weekly**:
- [ ] Review test failures
- [ ] Update skipped tests if features implemented
- [ ] Review new test additions

**Monthly**:
- [ ] Review test coverage reports
- [ ] Update performance baselines
- [ ] Review and update test documentation
- [ ] Identify flaky tests

**Quarterly**:
- [ ] Full test suite review
- [ ] Evaluate test infrastructure
- [ ] Update testing strategy
- [ ] Review test metrics vs goals

## Resources & Tools

### Required Tools

1. **Test Framework**: Built-in epsilon test framework
2. **Coverage**: sb-cover (SBCL)
3. **Benchmarking**: epsilon.benchmark module
4. **Property Testing**: cl-quickcheck
5. **CI/CD**: GitHub Actions
6. **Fuzzing**: Custom fuzzing utilities

### Documentation

1. Test writing guide
2. Test conventions document
3. Coverage reports
4. Performance baselines
5. Platform-specific notes

## Success Criteria

### Phase 1 Success (Week 2)
- [ ] >80% test pass rate
- [ ] 0 test errors
- [ ] All skipped tests categorized
- [ ] Test status report published

### Phase 2 Success (Week 4)
- [ ] Integration tests created
- [ ] Numerical accuracy tests passing
- [ ] Property-based tests implemented
- [ ] Edge cases covered

### Phase 3 Success (Week 6)
- [ ] Benchmark suite complete
- [ ] Performance baselines established
- [ ] No memory leaks
- [ ] Regression detection working

### Phase 4 Success (Week 7)
- [ ] Cross-platform testing working
- [ ] CI/CD pipeline operational
- [ ] Test reports automated

### Phase 5 Success (Week 8)
- [ ] Tests organized and documented
- [ ] Coverage >82%
- [ ] Test documentation complete

### Phase 6 Success (Week 10)
- [ ] Fuzzing operational
- [ ] Stress tests passing
- [ ] Differential tests validating
- [ ] >90% coverage
- [ ] >95% pass rate

## Timeline Summary

| Week | Phase | Key Deliverables |
|------|-------|------------------|
| 1-2 | Phase 1 | Fix existing tests, >80% pass rate |
| 3-4 | Phase 2 | Integration, numerical, property-based tests |
| 5-6 | Phase 3 | Benchmarks, performance regression detection |
| 7 | Phase 4 | Cross-platform CI/CD |
| 8 | Phase 5 | Test organization, coverage >82% |
| 9-10 | Phase 6 | Fuzzing, stress testing, >90% coverage |

**Total Duration**: 10 weeks
**Estimated Effort**: 40-50 person-days
**Recommended**: 1-2 developers focused on testing

---

*Generated: 2025-11-10*
*Status: Draft*

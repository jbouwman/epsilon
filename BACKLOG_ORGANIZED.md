# Organized Backlog - Compute Module & Merge

This document organizes the work needed to successfully merge the compute branch and continue development.

## Phase 1: Merge Preparation (Immediate)

### Issue #1: Investigate and Restore Removed Modules
**Priority**: P0 - Blocker
**Effort**: Medium (1-2 days)
**Assignee**: TBD

**Description**:
The compute branch removes several modules that may still be active on main. Need to investigate and restore as needed.

**Tasks**:
- [ ] Verify if `modules/benchmark/` should be restored (appears active on main PR #143)
- [ ] Check `modules/http2/` deprecation status
- [ ] Check `modules/web/` deprecation status (may be intentionally removed)
- [ ] Check `modules/websocket/` status (commit shows "websocket restored #134")
- [ ] Restore non-deprecated modules from main branch
- [ ] Document decision for each module

**Acceptance Criteria**:
- Clear documentation of which modules were restored and why
- No active modules accidentally removed
- All restored modules' tests pass

---

### Issue #2: Rebase Compute Branch on Main
**Priority**: P0 - Blocker
**Effort**: Medium (1-2 days)
**Assignee**: TBD

**Description**:
Compute branch is ~34 commits behind main. Needs to be rebased to incorporate recent improvements.

**Tasks**:
- [ ] Create backup branch of current compute state
- [ ] Rebase compute on latest main
- [ ] Resolve conflicts incrementally
- [ ] Test after each major conflict resolution
- [ ] Validate compute module tests still work

**Acceptance Criteria**:
- Compute branch successfully rebased on main
- No regressions in compute module functionality
- Test pass rate maintained or improved

---

### Issue #3: Establish Test Baseline
**Priority**: P0 - Blocker
**Effort**: Small (0.5 days)
**Assignee**: TBD

**Description**:
Document current test status on both branches before merge.

**Tasks**:
- [ ] Run full test suite on main branch
- [ ] Document all test results (pass/fail/skip counts)
- [ ] Run full test suite on compute branch
- [ ] Document compute module test status
- [ ] Identify any existing test failures
- [ ] Create test status report

**Acceptance Criteria**:
- Complete test status documented for both branches
- Known failures identified and documented
- Baseline established for regression detection

---

## Phase 2: Merge Execution

### Issue #4: Merge Compute Module (Low Risk)
**Priority**: P1 - High
**Effort**: Small (0.5 days)
**Assignee**: TBD

**Description**:
Add the compute module to main branch. This should be low-conflict since it's mostly new code.

**Tasks**:
- [ ] Create merge branch from main
- [ ] Merge compute module files
- [ ] Verify no conflicts with existing modules
- [ ] Run compute module tests
- [ ] Run full test suite to check for regressions

**Acceptance Criteria**:
- Compute module successfully added to main
- No regressions in existing tests
- Compute module tests maintain pass rate

---

### Issue #5: Merge Foreign Module Enhancements
**Priority**: P1 - High
**Effort**: Medium (1-2 days)
**Assignee**: TBD

**Description**:
Compute branch has significant improvements to the foreign module (smart-ffi, marshalling, etc.).

**Tasks**:
- [ ] Review foreign module changes on compute branch
- [ ] Check for conflicts with main's FFI cleanup (PR #140)
- [ ] Merge smart-ffi.lisp and marshalling.lisp
- [ ] Merge enhanced test coverage
- [ ] Run foreign module tests
- [ ] Validate existing FFI usage not broken

**Acceptance Criteria**:
- Foreign module enhancements merged
- All foreign module tests pass
- No regressions in existing FFI functionality
- Migration guide available if API changed

---

### Issue #6: Resolve Networking Module Conflicts
**Priority**: P1 - High
**Effort**: Large (2-3 days)
**Assignee**: TBD

**Description**:
Both branches modified darwin and linux networking modules. Main has recent async improvements, compute has structural changes.

**Tasks**:
- [ ] Compare darwin module changes between branches
- [ ] Compare linux module changes between branches
- [ ] Identify conflicting changes
- [ ] Decide merge strategy (favor main, port compute improvements)
- [ ] Manual merge of networking changes
- [ ] Test on Linux platform
- [ ] Test on Darwin platform
- [ ] Run networking integration tests

**Acceptance Criteria**:
- Networking modules successfully merged
- All networking tests pass on both platforms
- No regressions in async functionality
- Recent main improvements preserved

---

### Issue #7: Handle Core Module Reorganization
**Priority**: P2 - Medium
**Effort**: Large (2-3 days) OR Small (0.5 days if reverting)
**Assignee**: TBD

**Description**:
Compute branch moves all core/src/*.lisp to core/src/lib/*.lisp. Need to decide whether to keep this reorganization.

**Tasks**:
- [ ] Evaluate benefits of reorganization
- [ ] Assess migration cost (import path changes across codebase)
- [ ] **Decision**: Keep reorganization or revert to main structure
- [ ] If keeping: Update all imports across modules
- [ ] If keeping: Create migration guide
- [ ] If reverting: Restore main's structure
- [ ] Run full test suite
- [ ] Validate all modules load correctly

**Acceptance Criteria**:
- Decision documented with justification
- All modules build successfully
- All tests pass
- Migration guide created if needed

---

## Phase 3: Testing & Quality

### Issue #8: Fix Compute Module Test Failures
**Priority**: P1 - High
**Effort**: Large (3-5 days)
**Assignee**: TBD

**Description**:
Compute module has 53 skipped tests (30.6%). Need to improve test pass rate to >80%.

**Tasks**:
- [ ] Review all skipped tests
- [ ] Categorize by reason (not implemented, syntax errors, etc.)
- [ ] Fix syntax errors in tests
- [ ] Add skip markers with clear reasons for incomplete features
- [ ] Implement mocking/stubbing for unimplemented features
- [ ] Target: reduce skips to <20%
- [ ] Target: 0 test errors
- [ ] Target: >80% pass rate

**Related**: See compute/IMPLEMENTATION-STATUS.md for details

**Acceptance Criteria**:
- Test pass rate >80%
- Zero test errors
- All skips have documented reasons
- No test syntax errors

---

### Issue #9: Create Integration Tests
**Priority**: P2 - Medium
**Effort**: Medium (2-3 days)
**Assignee**: TBD

**Description**:
Create tests that verify compute module integrates with other modules.

**Tasks**:
- [ ] Compute + Foreign (BLAS integration stub)
- [ ] Compute + Core (data structure usage)
- [ ] Cross-platform tests (Linux/Darwin)
- [ ] Compute module examples work end-to-end
- [ ] Document integration patterns

**Acceptance Criteria**:
- Integration test suite created
- All integration tests pass
- Examples documented and working

---

### Issue #10: Performance Testing & Benchmarking
**Priority**: P2 - Medium
**Effort**: Medium (1-2 days)
**Assignee**: TBD

**Description**:
Ensure no performance regressions and benchmark compute module.

**Tasks**:
- [ ] Run benchmark suite on merged code
- [ ] Compare with baseline from main
- [ ] Identify any regressions
- [ ] Create compute module benchmarks
- [ ] Benchmark key operations (matmul, autodiff, etc.)
- [ ] Document performance characteristics

**Acceptance Criteria**:
- No performance regressions vs main
- Compute module benchmarks created
- Performance characteristics documented

---

## Phase 4: Documentation

### Issue #11: Update Root Documentation
**Priority**: P1 - High
**Effort**: Small (1 day)
**Assignee**: TBD

**Description**:
Update main repository documentation to include compute module.

**Tasks**:
- [ ] Add compute module to main README
- [ ] Document capabilities and use cases
- [ ] Add getting started guide
- [ ] Update module dependency graph
- [ ] Document known limitations (53 skipped tests)
- [ ] Link to compute module docs

**Acceptance Criteria**:
- README updated with compute module
- Getting started guide available
- Known limitations documented

---

### Issue #12: Create Migration Guide (If Needed)
**Priority**: P2 - Medium
**Effort**: Small (0.5-1 day)
**Assignee**: TBD

**Description**:
If core reorganization or other breaking changes are kept, provide migration guide.

**Tasks**:
- [ ] Document all breaking changes
- [ ] Provide before/after examples
- [ ] Update import statements
- [ ] Document deprecated APIs
- [ ] Provide migration script if needed

**Acceptance Criteria**:
- Migration guide complete if needed
- All breaking changes documented
- Examples updated

---

## Phase 5: Compute Module Feature Development

### Critical Features (P0)

#### Issue #13: Complete Reverse-Mode Autodiff
**Priority**: P0 - Critical
**Effort**: Large (5-7 days)
**Assignee**: TBD

**Description**:
Reverse-mode autodiff is partially implemented but not complete. This is critical for ML applications.

**Tasks**:
- [ ] Review current tape-based implementation
- [ ] Complete gradient computation for all operations
- [ ] Implement gradient accumulation
- [ ] Add vector-Jacobian products (VJP)
- [ ] Handle broadcasting in gradients
- [ ] Add comprehensive tests
- [ ] Benchmark performance

**Related**:
- compute/docs/REVERSE_AUTODIFF_PLAN.md
- compute/src/reverse-autodiff.lisp (partial implementation)

**Acceptance Criteria**:
- Full reverse-mode autodiff working
- All gradient tests pass
- Performance acceptable for medium-sized models
- Documentation with examples

---

#### Issue #14: Implement Hardware Acceleration (BLAS Integration)
**Priority**: P0 - Critical
**Effort**: Large (5-7 days)
**Assignee**: TBD

**Description**:
Complete FFI bindings to OpenBLAS/MKL for 10-100x performance improvement.

**Tasks**:
- [ ] Complete FFI bindings to OpenBLAS/MKL
- [ ] Matrix multiplication (DGEMM)
- [ ] Matrix-vector operations (DGEMV)
- [ ] Vector operations (DDOT, DAXPY)
- [ ] Linear system solving (DGESV)
- [ ] Eigenvalue computation (DGEEV)
- [ ] SVD decomposition (DGESVD)
- [ ] Automatic fallback to native implementation
- [ ] Performance benchmarks

**Related**:
- compute/src/blas-stub.lisp (stub implementation)
- compute/wip/blas-ffi.lisp (work in progress)
- compute/BACKLOG.md §1.1

**Acceptance Criteria**:
- BLAS bindings working
- 10-100x speedup for large matrices
- Tests pass with both BLAS and native backends
- Automatic backend selection

---

### High Priority Features (P1)

#### Issue #15: Complete Forward-Mode Autodiff
**Priority**: P1 - High
**Effort**: Medium (3-5 days)
**Assignee**: TBD

**Description**:
Forward-mode autodiff for Jacobian computation and directional derivatives.

**Tasks**:
- [ ] Dual number implementation
- [ ] Jacobian computation (full)
- [ ] Directional derivatives
- [ ] Efficient implementation for low-input-dimension problems
- [ ] Tests and benchmarks

**Related**: compute/src/autodiff.lisp (placeholder exists)

**Acceptance Criteria**:
- Forward-mode autodiff working
- Jacobian tests pass
- Performance acceptable

---

#### Issue #16: Complete Einstein Summation
**Priority**: P1 - High
**Effort**: Medium (2-3 days)
**Assignee**: TBD

**Description**:
Complete einsum implementation with optimization.

**Tasks**:
- [ ] Full index notation parsing
- [ ] Optimization of contraction order
- [ ] Support for named axes
- [ ] Efficient contraction algorithms
- [ ] Automatic optimization of contraction paths
- [ ] Comprehensive tests

**Related**: compute/tests/einsum-tests.lisp

**Acceptance Criteria**:
- Full einsum support
- Optimal contraction order
- Tests pass

---

#### Issue #17: Fix E-graph Associativity Handling
**Priority**: P1 - High
**Effort**: Large (4-6 days)
**Assignee**: TBD

**Description**:
E-graph currently has issues with associative expressions not achieving equivalence.

**Tasks**:
- [ ] Improve pattern matching for nested expressions
- [ ] Implement AC-matching algorithm
- [ ] Better canonicalization
- [ ] Incremental rebuilding
- [ ] Performance improvements
- [ ] Fix skipped associativity tests

**Related**:
- compute/docs/EGRAPH_IMPLEMENTATION.md
- compute/src/egraph.lisp
- compute/tests/egraph-tests.lisp (associativity tests skipped)

**Acceptance Criteria**:
- Associativity tests pass
- E-graph handles nested expressions correctly
- Performance acceptable (no exponential growth)

---

### Medium Priority Features (P2)

#### Issue #18: Symbolic Integration
**Priority**: P2 - Medium
**Effort**: Large (5-7 days)
**Assignee**: TBD

**Tasks**:
- [ ] Polynomial integration
- [ ] Trigonometric integration
- [ ] Integration by parts
- [ ] Substitution methods
- [ ] Tests and examples

**Related**: compute/BACKLOG.md §2.1

---

#### Issue #19: Equation Solving
**Priority**: P2 - Medium
**Effort**: Large (5-7 days)
**Assignee**: TBD

**Tasks**:
- [ ] Linear equation systems
- [ ] Polynomial root finding
- [ ] Nonlinear equation solving (Newton-Raphson)
- [ ] Tests and examples

**Related**: compute/BACKLOG.md §2.1

---

#### Issue #20: Higher-Order Derivatives
**Priority**: P2 - Medium
**Effort**: Medium (3-5 days)
**Assignee**: TBD

**Tasks**:
- [ ] Hessian computation
- [ ] Mixed partial derivatives
- [ ] Taylor series coefficients
- [ ] Optimal Hessian computation (forward-over-reverse)

**Related**: compute/BACKLOG.md §1.2

---

#### Issue #21: Native Code Compilation
**Priority**: P2 - Medium
**Effort**: Large (7-10 days)
**Assignee**: TBD

**Tasks**:
- [ ] LLVM backend
- [ ] C code generation
- [ ] JIT compilation
- [ ] Expression compilation
- [ ] Hot path detection

**Related**: compute/BACKLOG.md §4.1

---

### Infrastructure & Quality (P2-P3)

#### Issue #22: Improve Error Messages
**Priority**: P2 - Medium
**Effort**: Small (1-2 days)
**Assignee**: TBD

**Tasks**:
- [ ] More specific error messages
- [ ] Include context in errors
- [ ] Provide recovery suggestions
- [ ] User-friendly error output

**Related**: compute/docs/QUALITY_ANALYSIS.md

---

#### Issue #23: Remove Production Warnings
**Priority**: P2 - Medium
**Effort**: Small (0.5 days)
**Assignee**: TBD

**Tasks**:
- [ ] Remove warning statements from production code
- [ ] Use proper logging framework
- [ ] Separate debug and production code

**Related**: compute/docs/QUALITY_ANALYSIS.md §Critical Issues

---

#### Issue #24: Add Property-Based Testing
**Priority**: P3 - Low
**Effort**: Medium (2-3 days)
**Assignee**: TBD

**Tasks**:
- [ ] Property-based tests for symbolic operations
- [ ] Property-based tests for autodiff
- [ ] Property-based tests for broadcasting
- [ ] Numerical accuracy tests

---

#### Issue #25: Debugging & Profiling Tools
**Priority**: P3 - Low
**Effort**: Medium (3-4 days)
**Assignee**: TBD

**Tasks**:
- [ ] Computation graph visualization (GraphViz export)
- [ ] Interactive visualization
- [ ] Gradient flow visualization
- [ ] Operation timing
- [ ] Memory profiling
- [ ] Bottleneck identification

**Related**: compute/BACKLOG.md §4.2

---

### Specialized Features (P3-P4 - Future)

#### Issue #26: GPU Acceleration
**Priority**: P4 - Future
**Effort**: Very Large (15-20 days)
**Assignee**: TBD

**Tasks**:
- [ ] CUDA backend for NVIDIA GPUs
- [ ] OpenCL backend for cross-platform
- [ ] Automatic CPU/GPU dispatch
- [ ] GPU memory management

**Related**: compute/BACKLOG.md §1.1

---

#### Issue #27: Quantum Computing Features
**Priority**: P4 - Future
**Effort**: Large (7-10 days)
**Assignee**: TBD

**Tasks**:
- [ ] Quantum gate operations
- [ ] State vector simulation
- [ ] Quantum circuit optimization

**Related**:
- compute/tests/quantum-tests.lisp
- compute/BACKLOG.md §3.1

---

#### Issue #28: Statistical Computing
**Priority**: P4 - Future
**Effort**: Large (7-10 days)
**Assignee**: TBD

**Tasks**:
- [ ] Probability distributions
- [ ] Statistical tests
- [ ] Random number generation

**Related**: compute/BACKLOG.md §3.2

---

#### Issue #29: Signal Processing
**Priority**: P4 - Future
**Effort**: Large (7-10 days)
**Assignee**: TBD

**Tasks**:
- [ ] Fourier transforms (FFT)
- [ ] Wavelet transforms
- [ ] Digital filters

**Related**: compute/BACKLOG.md §3.3

---

## Summary

### Immediate Priorities (Next 2 Weeks)
1. Issues #1-7: Merge preparation and execution
2. Issue #8: Fix compute module tests
3. Issue #11: Update documentation

### Short-term (1 Month)
1. Issue #13: Complete reverse-mode autodiff (CRITICAL)
2. Issue #14: BLAS integration (CRITICAL)
3. Issue #15: Forward-mode autodiff
4. Issue #17: Fix E-graph issues

### Medium-term (2-3 Months)
1. Issue #16: Complete einsum
2. Issues #18-21: Advanced features
3. Issues #22-25: Infrastructure improvements

### Long-term (Future)
1. Issues #26-29: Specialized features
2. Community feedback and iteration
3. Production hardening

---

**Total Issues Created**: 29
**Estimated Total Effort**: 90-120 person-days
**Recommended Team Size**: 2-3 developers
**Estimated Timeline**: 3-6 months for full implementation

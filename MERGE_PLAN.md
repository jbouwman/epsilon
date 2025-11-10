# Compute Branch Merge Plan

## Executive Summary

The `compute` branch contains significant work on a new machine learning/scientific computing module but has diverged substantially from `main`. This document outlines the strategy for merging compute into main while preserving valuable work from both branches.

## Branch Status

### Main Branch (Current)
- **Last commit**: `9bd7ad0` - "Improve testing of Linux async networking stack (#145)"
- **Recent work**:
  - Networking improvements (Linux/Darwin async stack)
  - Benchmark framework enhancements (#143)
  - File organization and naming consistency (#141)
  - Module structure improvements (#142)
  - FFI package cleanup (#140)
- **Status**: Clean working directory

### Compute Branch
- **Last commit**: `0d888c0` - "compute module backlog"
- **Diverged from main**: ~34 commits behind
- **Major changes**:
  - **Adds**: Complete `modules/compute/` module (~10k LOC)
  - **Removes**: `benchmark/`, `http2/`, `web/`, `websocket/` modules
  - **Reorganizes**: `core/` module (files moved to `lib/` subdirectory)
  - **Modifies**: `darwin/`, `linux/`, `foreign/`, `crypto/`, `digest/` modules

## Analysis

### 1. Compute Module (NEW)

**Status**: 69.4% test passing rate (120/173 tests)

**Capabilities**:
- Symbolic computation and expression manipulation
- Tensor broadcasting (NumPy-compatible)
- Forward-mode automatic differentiation
- Reverse-mode autodiff (partial implementation)
- E-graph equality saturation for symbolic optimization
- Einstein summation notation
- Matrix operations
- Pattern matching and simplification rules

**Documentation Quality**: Excellent
- `BACKLOG.md` - Comprehensive feature roadmap
- `IMPLEMENTATION-STATUS.md` - Current implementation state
- `MODULE_STATUS.md` - Test status and component overview
- `QUALITY_ANALYSIS.md` - Code quality assessment
- Domain-specific docs (egraph, autodiff plans)

**Test Coverage**:
- Total: 173 tests
- Passing: 120 (69.4%)
- Skipped: 53 (30.6%)
- 21 test files covering all major features

### 2. Removed Modules - CRITICAL ISSUE

The compute branch removes 4 active modules that exist on main:

#### `modules/benchmark/` ⚠️ ACTIVE ON MAIN
- Recently updated on main (PR #143)
- Well-documented framework with statistical analysis
- Used for performance regression testing
- **Recommendation**: MUST PRESERVE - restore from main

#### `modules/http2/` ⚠️ POTENTIALLY ACTIVE
- Has src, tests, examples on main
- May be superseded by http module improvements
- **Recommendation**: Investigate if deprecated; if not, restore

#### `modules/web/` ⚠️ POTENTIALLY DEPRECATED
- Found commit "remove web module" in history
- May have been intentionally removed
- **Recommendation**: Verify deprecation status

#### `modules/websocket/` ⚠️ POTENTIALLY ACTIVE
- Has README and tests on main
- Commit history shows "websocket restored (#134)"
- **Recommendation**: Likely needed - investigate restoration

### 3. Core Module Reorganization

Compute branch moves all `modules/core/src/*.lisp` → `modules/core/src/lib/*.lisp`

**Impact**:
- Breaking change for module structure
- Affects import paths
- Needs validation across all modules

**Recommendation**: Evaluate if this reorganization provides value vs. migration cost

### 4. Foreign Module Enhancements

Compute branch adds significant FFI improvements:
- `smart-ffi.lisp` - Enhanced FFI interface
- `marshalling.lisp` - Data marshalling utilities
- `migration-guide.md` - FFI migration documentation
- Enhanced test coverage

**Status**: Appears to be valuable improvements
**Recommendation**: Preserve these enhancements

### 5. Darwin/Linux Networking Changes

Both branches modified networking modules:
- Main: Improved testing, async stack refinements
- Compute: Structural changes, new async implementation

**Risk**: HIGH - Potential conflicts in critical system modules
**Recommendation**: Careful manual merge required

## Merge Strategy

### Phase 1: Preparation (Before Merge)

1. **Restore Removed Modules**
   - Cherry-pick benchmark module from main
   - Investigate http2, websocket status
   - Restore any non-deprecated modules

2. **Sync Main Changes**
   - Rebase compute branch on latest main
   - Resolve conflicts incrementally
   - Test after each major conflict resolution

3. **Test Baseline**
   - Run full test suite on main (establish baseline)
   - Run full test suite on compute
   - Document expected test status

### Phase 2: Merge Execution

**Approach**: Incremental merge with testing at each step

```bash
# 1. Create merge branch from main
git checkout -b merge/compute-to-main origin/main

# 2. Identify non-conflicting changes first
#    - Compute module addition (new files)
#    - Documentation additions
#    - Foreign module enhancements (if non-conflicting)

# 3. Merge in stages:
#    a. Add compute module (should be clean)
#    b. Merge foreign module changes
#    c. Merge networking changes (high conflict risk)
#    d. Handle core reorganization (if keeping)

# 4. Restore removed modules
git checkout main -- modules/benchmark
# (restore others as needed)

# 5. Run tests after each stage
make test

# 6. Fix any integration issues
```

### Phase 3: Testing & Validation

1. **Full Test Suite**
   - All existing tests must still pass
   - Compute module tests: target 80%+ pass rate
   - No regressions in existing functionality

2. **Integration Testing**
   - Verify compute module works with other modules
   - Test FFI enhancements don't break existing usage
   - Validate networking changes on both Linux and Darwin

3. **Performance Testing**
   - Run benchmark suite (restored from main)
   - Ensure no performance regressions
   - Benchmark compute module operations

### Phase 4: Documentation

1. **Update Root Documentation**
   - Add compute module to main README
   - Document new capabilities
   - Update module dependency graph

2. **Migration Guide**
   - If core reorganization is kept, provide migration guide
   - Document any API changes
   - Update examples

3. **Known Issues**
   - Document compute module's 53 skipped tests
   - List features still in development
   - Provide roadmap from BACKLOG.md

## Conflict Resolution Strategy

### High-Risk Conflicts

1. **Networking Modules (darwin/linux)**
   - Main has recent improvements
   - Compute has structural changes
   - **Strategy**: Favor main's implementation, port compute improvements carefully

2. **Core Module Reorganization**
   - **Decision needed**: Keep reorganization or revert?
   - **Impact analysis**: Review all imports across codebase
   - **Recommendation**: Unless strong justification, revert to main's structure

3. **Foreign Module**
   - Both branches modified
   - **Strategy**: Merge both sets of improvements, prefer compute's enhancements

### Medium-Risk Conflicts

1. **Test Organization**
   - Some test files moved to `lib/` subdirectories
   - **Strategy**: Maintain main's organization unless compute's provides clear benefit

2. **Module Metadata**
   - module.lisp files modified in several modules
   - **Strategy**: Merge dependencies, keep main's structure

## Backlog Organization

Convert compute module's BACKLOG.md into GitHub issues:

### Priority 1: Critical (Create Issues)
- Complete FFI bindings to OpenBLAS/MKL
- Implement forward-mode autodiff
- Complete reverse-mode autodiff
- Fix E-graph associativity handling
- Complete Einstein summation

### Priority 2: Advanced Features (Create Issues)
- Symbolic integration
- Equation solving
- E-graph performance improvements
- Custom rewrite rules API

### Priority 3: Infrastructure (Create Issues)
- Native code generation / JIT
- Debugging & profiling tools
- Comprehensive documentation
- Property-based testing

### Priority 4: Specialized (Backlog for Later)
- Quantum computing features
- Statistical computing
- Signal processing
- GPU acceleration

## Testing Improvement Plan

### Immediate Actions

1. **Fix Compute Module Tests**
   - Address syntax errors in tests
   - Implement mocking for unimplemented features
   - Add skip markers with reasons for incomplete features
   - Target: 80%+ pass rate, 0 errors

2. **Add Integration Tests**
   - Compute + Foreign (BLAS integration)
   - Compute + Core (data structures)
   - Cross-platform tests (Linux/Darwin)

3. **Restore Benchmark Tests**
   - Ensure benchmark module tests pass
   - Add compute module benchmarks
   - Performance regression suite

### Long-term Improvements

1. **Test Coverage**
   - Add property-based tests for compute module
   - Numerical accuracy tests
   - Edge case testing
   - Increase coverage to >90%

2. **CI/CD Enhancements**
   - Automated testing on merge
   - Performance regression detection
   - Cross-platform testing
   - Test result reporting

3. **Test Organization**
   - Categorize tests (unit, integration, performance)
   - Separate fast/slow tests
   - Better test documentation

## Risk Assessment

### High Risk
- ⚠️ **Networking module conflicts**: Could break production systems
- ⚠️ **Missing modules**: Removed modules may be dependencies
- ⚠️ **Core reorganization**: Wide-reaching import path changes

### Medium Risk
- ⚠️ **Test failures**: 53 skipped tests in compute module
- ⚠️ **FFI changes**: Could affect existing FFI usage
- ⚠️ **Integration issues**: Compute module may not integrate smoothly

### Low Risk
- ✓ **Documentation**: Well-documented changes
- ✓ **Compute module**: Mostly new code, minimal conflicts
- ✓ **Foreign enhancements**: Appears well-tested

## Success Criteria

### Must Have (Before Merge to Main)
- [ ] All main branch tests continue to pass
- [ ] Benchmark module restored and working
- [ ] No critical modules accidentally removed
- [ ] Compute module tests: >75% passing, 0 errors
- [ ] Networking functionality validated on Linux and Darwin
- [ ] Documentation updated

### Should Have
- [ ] Compute module tests: >80% passing
- [ ] Integration tests created
- [ ] Performance benchmarks run
- [ ] Migration guide created (if needed)
- [ ] GitHub issues created from backlog

### Nice to Have
- [ ] Compute module tests: >90% passing
- [ ] Full property-based test suite
- [ ] GPU acceleration plan
- [ ] Tutorial documentation

## Timeline Estimate

- **Phase 1 (Preparation)**: 1-2 days
- **Phase 2 (Merge Execution)**: 2-3 days
- **Phase 3 (Testing)**: 2-3 days
- **Phase 4 (Documentation)**: 1 day
- **Total**: 6-9 days

## Recommendations

### Immediate Next Steps

1. **Investigate removed modules** - Determine which must be restored
2. **Create backup branch** - Preserve current state
3. **Rebase compute on main** - Get up to date with recent changes
4. **Test current state** - Establish baseline
5. **Begin incremental merge** - Start with low-risk changes

### Long-term Strategy

1. **Establish compute module as experimental** - Mark as beta/preview
2. **Incremental feature completion** - Work through backlog systematically
3. **Focus on high-value features** - Prioritize reverse-mode autodiff
4. **Build example applications** - Demonstrate compute module value
5. **Community feedback** - Gather usage feedback before 1.0

## Appendix: File Change Statistics

```
260 files changed
+19,569 insertions
-23,259 deletions
Net: -3,690 lines
```

**Major additions**:
- modules/compute/: ~10,000 lines (new module)
- Foreign module enhancements: ~1,500 lines
- Documentation: ~1,000 lines

**Major deletions**:
- Removed modules: ~15,000 lines
- Benchmark module: ~3,000 lines
- HTTP2/Web/Websocket: ~12,000 lines

---

*Generated: 2025-11-10*
*Branch: claude/review-compute-branch-011CUySpVBnvqGDDXJvYuHRm*

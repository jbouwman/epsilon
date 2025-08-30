# Compute Module Backlog

This document outlines planned features, improvements, and known issues for the epsilon.compute module.

## Priority 1: Critical Performance & Functionality

### 1.1 Hardware Acceleration
**Status**: Stub implementation only  
**Impact**: High - 10-100x performance improvement for matrix operations

#### Tasks:
- [ ] Complete FFI bindings to OpenBLAS/MKL
  - [ ] Matrix multiplication (DGEMM)
  - [ ] Matrix-vector operations (DGEMV)
  - [ ] Vector operations (DDOT, DAXPY)
  - [ ] Linear system solving (DGESV)
  - [ ] Eigenvalue computation (DGEEV)
  - [ ] SVD decomposition (DGESVD)
- [ ] GPU acceleration support
  - [ ] CUDA backend for NVIDIA GPUs
  - [ ] OpenCL backend for cross-platform GPU support
  - [ ] Automatic CPU/GPU dispatch based on problem size
- [ ] SIMD optimizations for CPU operations
  - [ ] Vectorized elementary operations
  - [ ] Aligned memory allocation
  - [ ] Compiler intrinsics integration

#### Implementation Notes:
- Stub exists in `blas-stub.lisp` with fallback implementations
- Library detection code present but not connected
- Consider using foreign function interface from epsilon.foreign module

### 1.2 Advanced Automatic Differentiation
**Status**: Reverse-mode implemented, forward-mode missing  
**Impact**: High - Required for many ML/scientific applications

#### Tasks:
- [ ] Forward-mode automatic differentiation
  - [ ] Dual number implementation
  - [ ] Jacobian computation
  - [ ] Directional derivatives
- [ ] Higher-order derivatives
  - [ ] Hessian computation
  - [ ] Mixed partial derivatives
  - [ ] Taylor series coefficients
- [ ] Performance optimizations
  - [ ] Gradient checkpointing for memory efficiency
  - [ ] Tape compression
  - [ ] Operation fusion
  - [ ] Sparse gradient support
- [ ] Mixed-mode AD
  - [ ] Automatic mode selection
  - [ ] Optimal Hessian computation (forward-over-reverse)

#### Implementation Notes:
- Current reverse-mode in `reverse-autodiff.lisp` is functional
- Memory tracking implemented but could be optimized
- Need better handling of broadcasting in gradients

### 1.3 Tensor Operations
**Status**: Partial implementation  
**Impact**: High - Core functionality for scientific computing

#### Tasks:
- [ ] Complete Einstein summation
  - [ ] Full index notation parsing
  - [ ] Optimization of contraction order
  - [ ] Support for named axes
- [ ] Tensor contractions
  - [ ] Efficient contraction algorithms
  - [ ] Automatic optimization of contraction paths
- [ ] Sparse tensor support
  - [ ] COO format
  - [ ] CSR/CSC formats
  - [ ] Sparse operations (SpMV, SpMM)
- [ ] Strided operations
  - [ ] Views without copying
  - [ ] Efficient slicing
  - [ ] Broadcasting optimizations

## Priority 2: Advanced Symbolic Computation

### 2.1 Symbolic Mathematics
**Status**: Basic symbolic operations implemented  
**Impact**: Medium - Enables computer algebra capabilities

#### Tasks:
- [ ] Symbolic integration
  - [ ] Polynomial integration
  - [ ] Trigonometric integration
  - [ ] Integration by parts
  - [ ] Substitution methods
- [ ] Symbolic differentiation improvements
  - [ ] Implicit differentiation
  - [ ] Parametric derivatives
- [ ] Equation solving
  - [ ] Linear equation systems
  - [ ] Polynomial root finding
  - [ ] Nonlinear equation solving (Newton-Raphson)
- [ ] Series expansion
  - [ ] Taylor series
  - [ ] Laurent series
  - [ ] Fourier series
  - [ ] Asymptotic expansions

### 2.2 E-graph Improvements
**Status**: Basic implementation with some optimization issues  
**Impact**: Medium - Better optimization capabilities

#### Tasks:
- [ ] Fix associativity/commutativity handling
  - [ ] Improve pattern matching for nested expressions
  - [ ] AC-matching algorithm implementation
  - [ ] Better canonicalization
- [ ] Advanced extraction strategies
  - [ ] Multi-objective extraction
  - [ ] Learned cost functions
  - [ ] Domain-specific heuristics
- [ ] Performance improvements
  - [ ] Incremental rebuilding
  - [ ] Better indexing structures
  - [ ] Parallel rule application
- [ ] User-facing features
  - [ ] Custom rewrite rules API
  - [ ] Rule debugging interface
  - [ ] Visualization of e-graph structure

#### Known Issues:
- Associativity tests currently skipped due to convergence issues
- May generate excessive facts during saturation

## Priority 3: Specialized Domains

### 3.1 Quantum Computing
**Status**: Stub only  
**Impact**: Low-Medium - Specialized use cases

#### Tasks:
- [ ] Quantum gate operations
  - [ ] Basic gates (Pauli, Hadamard, CNOT)
  - [ ] Parametric gates (rotation gates)
  - [ ] Multi-qubit gates
- [ ] State vector simulation
  - [ ] Efficient state representation
  - [ ] Measurement simulation
  - [ ] Entanglement detection
- [ ] Quantum circuit optimization
  - [ ] Gate fusion
  - [ ] Circuit simplification
  - [ ] Transpilation to hardware constraints

### 3.2 Statistical Computing
**Status**: Not implemented  
**Impact**: Medium - Useful for data science applications

#### Tasks:
- [ ] Probability distributions
  - [ ] Common distributions (Normal, Poisson, etc.)
  - [ ] Sampling methods
  - [ ] PDF/CDF computation
- [ ] Statistical tests
  - [ ] Hypothesis testing
  - [ ] ANOVA
  - [ ] Regression analysis
- [ ] Random number generation
  - [ ] Multiple RNG algorithms
  - [ ] Parallel RNG streams
  - [ ] Quasi-random sequences

### 3.3 Signal Processing
**Status**: Not implemented  
**Impact**: Low-Medium - Specialized use cases

#### Tasks:
- [ ] Fourier transforms
  - [ ] FFT implementation
  - [ ] Multi-dimensional FFT
  - [ ] Real-valued optimizations
- [ ] Wavelet transforms
  - [ ] DWT implementation
  - [ ] Common wavelet families
- [ ] Digital filters
  - [ ] FIR/IIR filters
  - [ ] Filter design methods

## Priority 4: Infrastructure & Tooling

### 4.1 Compilation Pipeline
**Status**: Basic stub implementation  
**Impact**: Medium - Performance improvements

#### Tasks:
- [ ] Native code generation
  - [ ] LLVM backend
  - [ ] C code generation
  - [ ] Assembly generation for critical paths
- [ ] JIT compilation
  - [ ] Expression compilation
  - [ ] Hot path detection
  - [ ] Adaptive optimization
- [ ] Expression optimization
  - [ ] Common subexpression elimination
  - [ ] Dead code elimination
  - [ ] Loop optimizations

### 4.2 Debugging & Profiling
**Status**: Minimal  
**Impact**: Medium - Developer experience

#### Tasks:
- [ ] Computation graph visualization
  - [ ] GraphViz export
  - [ ] Interactive visualization
  - [ ] Gradient flow visualization
- [ ] Performance profiling
  - [ ] Operation timing
  - [ ] Memory profiling
  - [ ] Bottleneck identification
- [ ] Debugging tools
  - [ ] Gradient checking
  - [ ] Numerical stability checks
  - [ ] NaN/Inf detection and tracking

### 4.3 Documentation & Testing
**Status**: Partial  
**Impact**: High - Code maintainability

#### Tasks:
- [ ] Comprehensive documentation
  - [ ] API reference completion
  - [ ] Tutorial notebooks
  - [ ] Performance guide
  - [ ] Architecture documentation
- [ ] Test coverage improvement
  - [ ] Property-based testing
  - [ ] Numerical accuracy tests
  - [ ] Performance benchmarks
  - [ ] Integration tests

## Known Bugs & Issues

### High Priority Bugs
- None currently (recent fixes addressed critical issues)

### Medium Priority Issues
1. **E-graph associativity**: Associative expressions don't achieve equivalence even with many iterations
2. **Memory tracking**: Could be more efficient, currently tracks at node granularity
3. **Broadcasting edge cases**: Some complex broadcasting scenarios may not handle gradients correctly

### Low Priority Issues
1. **Type inference**: Could be more sophisticated for symbolic expressions
2. **Error messages**: Could be more user-friendly with better context
3. **Pattern variables**: The pattern matching system could be more robust

## Recently Completed ✅

- Fixed DOT gradient computation for matrix operations
- Added comprehensive docstrings to main functions
- Refactored gradient rule registration with helper functions
- Fixed memory tracking implementation
- Optimized e-graph saturation performance (5x improvement)
- Fixed type errors in e-graph implementation
- Fixed undefined variable issues in rule application

## Technical Debt

1. **Code organization**: Some files are getting large (reverse-autodiff.lisp > 1000 lines)
2. **Naming consistency**: Mix of naming conventions (kebab-case vs camelCase in some places)
3. **Dead code**: Some stub implementations could be removed or clearly marked
4. **Test organization**: Tests could be better organized by functionality
5. **Performance**: Many operations allocate unnecessarily, could benefit from in-place operations

## Dependencies & Prerequisites

- **Foreign Function Interface**: Required for BLAS/LAPACK integration
- **GPU drivers**: Required for CUDA/OpenCL support
- **Build system updates**: May need updates to support native compilation
- **Additional libraries**: 
  - OpenBLAS or MKL for linear algebra
  - CUDA toolkit for GPU support
  - LLVM for native compilation

## Notes for Contributors

When implementing features from this backlog:

1. **Maintain backward compatibility** - Don't break existing APIs
2. **Add comprehensive tests** - Every new feature needs test coverage
3. **Document thoroughly** - Include docstrings and update relevant documentation
4. **Consider performance** - Profile before and after changes
5. **Follow existing patterns** - Maintain consistency with current code style
6. **Update this backlog** - Move completed items to the completed section

## Metrics for Success

- **Performance**: 10x improvement for large matrix operations
- **Test Coverage**: > 90% coverage for core functionality
- **Documentation**: All public APIs documented with examples
- **Reliability**: No critical bugs in production use
- **Usability**: Clear error messages and intuitive APIs
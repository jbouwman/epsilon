# Reverse-Mode Automatic Differentiation Implementation Plan

## Background Research

### What is Reverse-Mode Autodiff?

Reverse-mode automatic differentiation (backpropagation) computes gradients by:
1. **Forward Pass**: Evaluate the function, storing intermediate values
2. **Backward Pass**: Propagate derivatives backward from output to inputs

**Advantages:**
- Efficient for functions with many inputs, few outputs (f: ℝⁿ → ℝ)
- One backward pass computes gradients w.r.t. all inputs
- Foundation of modern deep learning frameworks

**Key Concepts:**
- **Computation Graph**: DAG of operations
- **Tape/Trace**: Record of operations during forward pass
- **Adjoints**: Accumulated derivatives (∂L/∂v for each node v)
- **VJP (Vector-Jacobian Product)**: Core operation for backward pass

### Mathematical Foundation

For a computation graph with nodes v₁, v₂, ..., vₙ:
- **Forward**: Compute values vᵢ = fᵢ(parents(vᵢ))
- **Backward**: Compute adjoints v̄ᵢ = Σⱼ∈children(i) v̄ⱼ · ∂fⱼ/∂vᵢ

Chain rule in reverse mode:
```
∂L/∂x = Σ over all paths p from x to L of (∏ derivatives along p)
```

### Implementation Strategies

#### 1. **Tape-Based (Dynamic)**
- Build computation graph during forward pass
- Store operations and intermediate values
- Walk tape backward to compute gradients
- Used by: PyTorch, TensorFlow (eager mode)

#### 2. **Source Transformation (Static)**
- Transform source code to include gradient computation
- Compile-time graph construction
- Used by: JAX, Julia (Zygote)

#### 3. **Dual Numbers Extended**
- Extend forward-mode with reverse accumulation
- Less common but elegant

### Design Decisions for Epsilon

Given Epsilon's architecture:
- **Choice: Tape-Based** - Fits with dynamic symbolic expressions
- **Tape Persistence**: Keep tape for multiple backward passes
- **Memory Strategy**: Checkpointing for large graphs
- **Integration**: Reuse existing symbolic differentiation for primitives

## Implementation Plan

### Phase 1: Core Infrastructure (2-3 days)

#### 1.1 Tape Data Structure
```lisp
(defstruct tape-node
  id           ; Unique node identifier
  op           ; Operation symbol
  value        ; Computed value
  inputs       ; List of (node-id . local-derivative) pairs
  adjoint      ; Accumulated gradient (initially 0)
  )

(defstruct tape
  nodes        ; Vector of tape-nodes
  input-map    ; Hash: var-name -> node-id
  output-id    ; Root node for backward pass
  )
```

#### 1.2 Forward Pass Recording
```lisp
(defun record-op (tape op args-with-values)
  "Record an operation on the tape"
  ...)

(defun trace-expr (expr bindings)
  "Trace expression evaluation, building tape"
  ...)
```

#### 1.3 Primitive Derivatives
```lisp
(defparameter *primitive-vjps*
  '((+ . add-vjp)
    (* . mul-vjp)
    (sin . sin-vjp)
    ...))

(defun add-vjp (adjoint x-val y-val)
  "VJP for addition: returns (∂L/∂x, ∂L/∂y)"
  (values adjoint adjoint))

(defun mul-vjp (adjoint x-val y-val)
  "VJP for multiplication"
  (values (* adjoint y-val) (* adjoint x-val)))
```

### Phase 2: Backward Pass (2 days)

#### 2.1 Adjoint Accumulation
```lisp
(defun backward-pass (tape)
  "Propagate gradients backward through tape"
  ;; Set output adjoint to 1
  ;; Walk nodes in reverse topological order
  ;; For each node, accumulate adjoints to inputs
  ...)

(defun accumulate-adjoint (tape node-id delta)
  "Add delta to node's adjoint"
  ...)
```

#### 2.2 Gradient Extraction
```lisp
(defun extract-gradients (tape var-names)
  "Extract gradients for specified variables"
  ...)
```

### Phase 3: Integration (1-2 days)

#### 3.1 Main API Functions
```lisp
(defun gradient (expr vars bindings)
  "Compute gradient of scalar expression"
  (let ((tape (trace-expr expr bindings)))
    (backward-pass tape)
    (extract-gradients tape vars)))

(defun grad (expr)
  "Return gradient function"
  (lambda (bindings)
    (gradient expr (free-vars expr) bindings)))
```

#### 3.2 Integration with Existing System
- Modify `evaluate` to optionally build tape
- Add tape-aware evaluation mode
- Ensure compatibility with symbolic differentiation

### Phase 4: Optimizations (2-3 days)

#### 4.1 Memory Management
```lisp
(defun checkpoint-tape (tape checkpoint-nodes)
  "Create checkpoints to trade computation for memory"
  ...)

(defun prune-tape (tape needed-vars)
  "Remove unnecessary nodes from tape"
  ...)
```

#### 4.2 Special Cases
- Detect and optimize linear operations
- Implement sparse gradient support
- Add zero-gradient pruning

### Phase 5: Advanced Features (3-4 days)

#### 5.1 Higher-Order Derivatives
```lisp
(defun hessian (expr vars bindings)
  "Compute Hessian matrix using mixed mode"
  ;; Forward-over-reverse or reverse-over-reverse
  ...)
```

#### 5.2 Vector-Jacobian Products
```lisp
(defun vjp (expr vars bindings vector)
  "Compute vector-Jacobian product"
  ...)

(defun jvp (expr vars bindings vector)
  "Compute Jacobian-vector product"
  ...)
```

#### 5.3 Custom Derivatives
```lisp
(defun register-custom-vjp (op vjp-fn)
  "Register custom VJP for user-defined operations"
  ...)

(defmacro with-custom-vjp (op vjp-fn &body body)
  "Locally override VJP for an operation"
  ...)
```

### Phase 6: Testing & Validation (2 days)

#### 6.1 Correctness Tests
- Compare with symbolic differentiation
- Test gradient of gradient (should match Hessian)
- Verify chain rule on complex expressions

#### 6.2 Performance Tests
- Memory usage vs expression complexity
- Comparison with forward mode
- Benchmark against other AD systems

#### 6.3 Integration Tests
- E-graph optimization + autodiff
- Broadcasting + autodiff
- Matrix operations + autodiff

## Implementation Priority & Timeline

### Week 1: Foundation
- **Days 1-2**: Core tape infrastructure (Phase 1)
- **Days 3-4**: Basic backward pass (Phase 2)
- **Day 5**: Integration and basic testing

### Week 2: Completion
- **Days 6-7**: Memory optimizations (Phase 4)
- **Days 8-9**: Advanced features (Phase 5)
- **Day 10**: Comprehensive testing (Phase 6)

## Key Design Decisions

### 1. Tape Persistence
**Decision**: Keep tape after backward pass
**Rationale**: Allows multiple backward passes, useful for Hessian

### 2. Symbolic vs Numeric
**Decision**: Numeric tape with symbolic primitive derivatives
**Rationale**: Reuse existing symbolic diff, efficient execution

### 3. Memory Strategy
**Decision**: Full tape by default, optional checkpointing
**Rationale**: Simple implementation first, optimize later

### 4. API Design
**Decision**: Similar to JAX/PyTorch style
**Rationale**: Familiar to users, proven design

## Success Criteria

### Must Have
- [ ] Gradient computation for all basic operations
- [ ] Correct gradients verified against symbolic diff
- [ ] Integration with existing evaluate function
- [ ] All 8 autodiff tests passing

### Should Have
- [ ] Memory-efficient for expressions with 1000+ nodes
- [ ] Custom VJP registration
- [ ] Basic Hessian computation
- [ ] Documentation with examples

### Nice to Have
- [ ] Checkpointing for very large graphs
- [ ] Sparse gradient support
- [ ] Parallel backward pass
- [ ] Visualization of computation graph

## Risks & Mitigations

### Risk 1: Memory Explosion
**Mitigation**: Implement checkpointing early, add memory limits

### Risk 2: Performance vs Forward Mode
**Mitigation**: Choose mode automatically based on dimensions

### Risk 3: Integration Complexity
**Mitigation**: Start with standalone implementation, integrate gradually

### Risk 4: Numeric Stability
**Mitigation**: Use symbolic derivatives for primitives, add tests

## Alternative Approaches Considered

### 1. Source Transformation
**Pros**: Can generate efficient code
**Cons**: Complex implementation, less dynamic

### 2. Operator Overloading
**Pros**: Transparent to user
**Cons**: Requires language support, performance overhead

### 3. Graph Rewriting
**Pros**: Can optimize before differentiation
**Cons**: Complex, may lose structure

## Conclusion

The tape-based reverse-mode autodiff implementation is the highest priority feature for the compute module. It enables efficient gradient computation essential for machine learning applications. The phased approach allows for incremental development with early validation of core functionality. The design integrates well with existing symbolic infrastructure while providing the performance benefits of reverse-mode accumulation.
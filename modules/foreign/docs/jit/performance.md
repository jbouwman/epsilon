# epsilon.foreign.jit Performance Guide

This document covers performance characteristics, benchmark results, and
optimization strategies for the JIT FFI module.

## Performance Overview

### Call Overhead Comparison

The JIT FFI module provides near-native call performance by generating specialized machine code at runtime.

| Method                 | Typical Overhead | Notes                            |
|------------------------|------------------|----------------------------------|
| Native C call          | ~2ns             | Baseline (function pointer call) |
| Static `alien-funcall` | ~5ns             | SBCL's optimized static FFI      |
| JIT specialized caller | ~8-12ns          | Via `make-jit-caller`            |
| JIT via `call-stub`    | ~20-30ns         | With `apply` overhead            |
| libffi dynamic         | ~200-300ns       | Full dynamic dispatch            |

### Why JIT is Fast

1. **Specialized Callers**: For each signature, a type-specific Lisp function is compiled
2. **No Runtime Dispatch**: Types are known at stub creation time
3. **Minimal Trampoline**: Generated stubs are 14-36 bytes
4. **Register Passing**: Arguments stay in registers, no stack marshalling

## Running Benchmarks

### Using the Benchmark Framework

```lisp
(require :epsilon.foreign.jit)
(require :epsilon.tool.benchmark)

;; Load benchmarks
(load "epsilon/modules/foreign-jit/benchmarks/jit-ffi-benchmarks.lisp")

;; Run quick test
(epsilon.tool.benchmark.jit-ffi:quick-performance-test)

;; Run all benchmarks
(epsilon.tool.benchmark.jit-ffi:run-all-benchmarks)
```

### Legacy Benchmarks

```lisp
(epsilon.foreign.jit.bench:run-benchmarks)
```

## Benchmark Categories

### 1. Call Overhead

Measures pure call overhead for different invocation methods.

**Benchmarks:**
- `jit-ffi/call/static-getpid` - SBCL static alien baseline (0 args)
- `jit-ffi/call/jit-getpid` - JIT caller (0 args)
- `jit-ffi/call/static-abs` - SBCL static alien baseline (1 int arg)
- `jit-ffi/call/jit-abs` - JIT caller (1 int arg)
- `jit-ffi/call/static-sin` - SBCL static alien baseline (1 double arg)
- `jit-ffi/call/jit-sin` - JIT caller (1 double arg)

**Target Performance:**
- JIT should be within 2x of static alien-funcall
- Both should be under 20ns per call

### 2. Stub Compilation

Measures time to compile new stubs.

**Benchmarks:**
- `jit-ffi/compile/cold-int` - Compile int->int stub (cold)
- `jit-ffi/compile/cold-double` - Compile double->double stub (cold)
- `jit-ffi/compile/cached-lookup` - Retrieve cached stub

**Target Performance:**
- Cold compilation: < 1ms
- Cached lookup: < 1us

### 3. Memory Usage

Measures memory consumption of stubs.

**Benchmarks:**
- `jit-ffi/memory/stub-region-alloc` - Allocate executable region
- `jit-ffi/memory/stub-size-int` - Size of int->int stub

**Typical Sizes:**
- ARM64 stub: 36 bytes
- x86-64 stub: 14 bytes
- Default region: 64KB (holds ~1800-4500 stubs)

### 4. Callback Overhead

Measures C-to-Lisp callback performance.

**Target Performance:**
- Callback invocation: < 100ns
- Round-trip (Lisp->C->Lisp): < 200ns

## Performance Budgets

The following budgets are enforced in CI:

```lisp
;; Call overhead (nanoseconds)
jit-ffi/call/static-getpid  :max-time 10ns
jit-ffi/call/jit-getpid     :max-time 20ns
jit-ffi/call/static-abs     :max-time 10ns
jit-ffi/call/jit-abs        :max-time 20ns
jit-ffi/call/static-sin     :max-time 10ns
jit-ffi/call/jit-sin        :max-time 20ns

;; Compilation (milliseconds)
jit-ffi/compile/cold-int    :max-time 1ms
jit-ffi/compile/cached      :max-time 1us
```

## Optimization Strategies

### 1. Cache JIT Callers

Create callers once, reuse many times:

```lisp
;; Good: O(1) amortized
(defvar *sin-fn*
  (jit:make-jit-caller
    (sb-sys:find-foreign-symbol-address "sin")
    :double '(:double)))

(defun fast-sin (x)
  (funcall *sin-fn* x))

;; Bad: O(n) - creates stub each time
(defun slow-sin (x)
  (funcall (jit:make-jit-caller
             (sb-sys:find-foreign-symbol-address "sin")
             :double '(:double))
           x))
```

### 2. Use Correct Types

Type mismatches cause extra conversions:

```lisp
;; Good: Matching types
(funcall sin-fn 1.0d0)  ; double literal

;; Slower: Type conversion
(funcall sin-fn 1.0)    ; single-float -> double
(funcall sin-fn 1)      ; integer -> double
```

### 3. Avoid call-stub in Hot Paths

The returned function is faster than `call-stub`:

```lisp
;; Fast: Direct funcall
(let ((fn (jit:make-jit-caller addr :int '(:int))))
  (dotimes (i 1000000)
    (funcall fn i)))

;; Slower: Through call-stub
(let ((stub (jit:compile-stub addr :int '(:int))))
  (dotimes (i 1000000)
    (jit:call-stub stub i)))  ; Extra apply overhead
```

### 4. Batch Similar Operations

For many similar calls, structure code to maximize cache hits:

```lisp
;; Good: All sin calls use same stub
(let ((sin-fn (jit:make-jit-caller sin-addr :double '(:double)))
      (cos-fn (jit:make-jit-caller cos-addr :double '(:double))))
  (loop for angle in angles
        collect (complex (funcall cos-fn angle)
                         (funcall sin-fn angle))))
```

### 5. Pre-warm Before Timing

For accurate benchmarks, warm up the cache:

```lisp
;; Warm up
(dotimes (i 10000)
  (funcall my-jit-fn arg))

;; Then measure
(time (dotimes (i 1000000)
        (funcall my-jit-fn arg)))
```

## Platform-Specific Notes

### ARM64 (Apple Silicon)

- Uses x16 as scratch register (per ARM64 ABI)
- Handles W^X via `sb-vm::jit-memcpy`
- MAP_JIT flag required for hardened runtime
- Memory barrier after code emission

### x86-64 (Linux/macOS)

- Uses r11 as scratch register
- Simpler code generation (14 bytes vs 36)
- No special memory protection handling needed

## Profiling Tips

### Using SBCL's Profiler

```lisp
(require :sb-sprof)

(sb-sprof:with-profiling (:report :flat)
  (dotimes (i 1000000)
    (funcall my-jit-fn arg)))
```

### Checking Cache Efficiency

```lisp
;; Before operations
(jit:clear-stub-cache)

;; Run code...

;; Check stats
(jit:stub-cache-stats)
; => (:STUB-COUNT 5 :REGION-SIZE 65536 :REGION-USED 180)

;; High stub-count with high region-used = good caching
;; Low stub-count with high region-used = large stubs
```

## Comparison with Other FFI Methods

### vs. CFFI

CFFI uses libffi for dynamic calls:
- CFFI: ~250ns per call
- JIT: ~10ns per call
- Speedup: ~25x

### vs. SBCL alien-funcall

Static alien-funcall is slightly faster:
- Static: ~5ns
- JIT: ~10ns

But JIT provides runtime flexibility without runtime overhead.

### vs. Direct C

Native C function pointer call:
- C: ~2ns
- JIT: ~10ns

Most overhead is Lisp<->C boundary crossing, not the stub itself.

## Future Optimizations

Potential improvements being considered:

1. **Inline small stubs**: For very hot paths, inline stub code
2. **Profile-guided optimization**: Track call frequencies
3. **Batch compilation**: Compile multiple stubs together
4. **SIMD support**: Vector argument passing

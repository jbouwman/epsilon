# Epsilon Foreign Function Interface Architecture

## Overview

The Epsilon FFI system provides high-performance foreign function calls with multiple optimization levels. The architecture follows a modular design where each component is independent and can be used standalone.

## Core Modules

### epsilon.foreign
The main FFI module providing core functionality:
- `shared-call` - Dynamic FFI calls with runtime type resolution
- `shared-call-fast` - Optimized calls using compiled trampolines  
- `lib-open`, `lib-close`, `lib-function` - Library management
- `foreign-alloc`, `foreign-free` - Memory management
- `convert-to-foreign`, `convert-from-foreign` - Type conversion

### epsilon.foreign.trampoline
Compiled function trampolines for ~2x performance improvement:
- Generates specialized machine code for each function signature
- Caches compiled trampolines for reuse
- Eliminates eval overhead from traditional FFI

### epsilon.foreign.marshalling
Automatic type marshalling and conversion:
- Type inference from C function signatures
- Enum definitions with keyword mappings
- Array and string conversion helpers
- Error handling with foreign-error conditions

### epsilon.foreign.struct  
C struct layout and manipulation:
- Struct field offset calculation
- Zero-copy struct access
- Union support
- Automatic struct parsing from headers

### epsilon.foreign.callback
Callback support through libffi integration:
- Create C-callable function pointers from Lisp functions
- Callback registry and lifecycle management
- Automatic type conversion for callback arguments

### epsilon.foreign.inline
Direct alien-funcall implementations for common functions:
- Standalone module with no epsilon.foreign dependency
- Provides %strlen, %memcpy, %getpid etc.
- Maximum performance for hot paths

### epsilon.foreign.memory-pool
Memory pooling for efficient allocations:
- Reduces allocation overhead
- Reusable memory blocks
- Smart pooling based on size
- Statistics and monitoring

### epsilon.foreign.batch
Batch operation optimization:
- Groups multiple FFI calls
- Improves cache locality
- Reduces per-call overhead

### epsilon.foreign.optimize
Compiler macro optimizations (loads after epsilon.foreign):
- Optimizes shared-call at compile time
- Generates direct alien-funcall for libc functions
- Can be enabled/disabled at runtime
- Provides hyper-optimized common functions

## Load Order and Dependencies

The module system automatically handles load order based on package dependencies:

1. **Independent modules** (no epsilon.foreign dependency):
   - trampoline
   - marshalling  
   - struct
   - callback
   - callback-impl
   - inline
   - memory-pool

2. **epsilon.foreign** loads after its dependencies via :local-nicknames

3. **Dependent modules** (reference epsilon.foreign):
   - batch (uses runtime symbol lookup to avoid circular deps)
   - optimize (loads after epsilon.foreign due to :local-nicknames)

## Performance Characteristics

### Optimization Levels

1. **Level 0: eval-based** (deprecated)
   - Original approach using eval
   - Baseline performance

2. **Level 1: Trampolines** 
   - ~2x faster than eval
   - Compiled function dispatch
   - Signature caching

3. **Level 2: Compiler Macros**
   - Compile-time optimization
   - Direct alien-funcall generation
   - Zero overhead for known signatures

4. **Level 3: Inline Functions**
   - Maximum performance
   - No FFI overhead
   - Direct machine code

### Benchmarks

- Trampoline vs eval: ~1.89x speedup
- Compiler macro optimization: Near-native performance for libc calls
- Memory pooling: Significant speedup for frequent allocations
- Batch operations: Reduced overhead for multiple calls

## Design Principles

1. **No circular dependencies** - Each module is independent
2. **No re-exports** - Each module exports only its own functions
3. **Automatic load ordering** - Module system handles dependencies
4. **Progressive optimization** - Multiple performance levels
5. **Clean separation** - Each module has a single responsibility

## Usage Examples

### Basic FFI Call
```lisp
(epsilon.foreign:shared-call '("strlen" "libc") :unsigned-long '(:string) "hello")
```

### With Optimization
```lisp
;; Compiler macro optimizes this at compile time
(epsilon.foreign:shared-call '("strlen" "libc") :unsigned-long '(:string) "hello")

;; Direct optimized function
(epsilon.foreign.optimize:%strlen-optimized "hello")
```

### Callbacks
```lisp
(epsilon.foreign.callback:with-callback (cb #'my-function :int '(:int :int))
  (c-function-expecting-callback cb))
```

### Memory Pooling
```lisp
(epsilon.foreign.memory-pool:with-pooled-memory (ptr 256)
  ;; Use ptr
  )
```

## Future Improvements

- Phase 6: Zero-copy arrays
- Phase 7: Automatic errno checking
- JIT compilation for dynamic signatures
- Platform-specific optimizations
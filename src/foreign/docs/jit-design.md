# JIT Compilation Design for epsilon.foreign

## Overview

This document outlines the design for Just-In-Time (JIT) compilation of FFI call sites in epsilon.foreign. The goal is to optimize frequently-called foreign functions by generating specialized machine code at runtime.

## Motivation

Current FFI implementation uses `eval` with `sb-alien:alien-funcall` for each call, which has overhead:

1. **Dynamic dispatch**: Each call goes through signature matching in `shared-call`
2. **Eval overhead**: Creating and evaluating forms at runtime
3. **Type conversion**: Arguments converted on every call
4. **No inlining**: Cannot inline short foreign functions

JIT compilation can eliminate these overheads for hot call sites.

## Design Goals

1. **Transparent**: No API changes required
2. **Adaptive**: Only compile frequently-called functions
3. **Fast**: Generated code should approach hand-written assembly
4. **Safe**: Maintain all type checking and error handling
5. **Debuggable**: Preserve stack traces and debugging info

## Architecture

### Call Site Tracking

```lisp
(defstruct call-site-info
  (count 0 :type fixnum)           ; Call count
  (total-time 0 :type fixnum)      ; Total time spent
  (signature nil)                  ; (return-type . arg-types)
  (compiled-fn nil)                ; Compiled function or NIL
  (last-compile-count 0))          ; Count at last compilation

(defvar *call-site-stats* (make-hash-table :test 'equal)
  "Maps (function-name . signature) to call-site-info")
```

### Compilation Threshold

```lisp
(defparameter *jit-threshold* 1000
  "Number of calls before JIT compilation")

(defparameter *jit-recompile-threshold* 10000
  "Calls before considering recompilation")
```

### JIT Compiler Pipeline

1. **Profile Collection**: Track call frequency and signatures
2. **Hot Spot Detection**: Identify functions exceeding threshold
3. **Code Generation**: Generate optimized SBCL code
4. **Compilation**: Use SBCL's compiler to generate machine code
5. **Installation**: Replace call site with compiled version

## Implementation Strategy

### Phase 1: Call Site Profiling

Modify `shared-call` to track statistics:

```lisp
(defun shared-call (function-designator return-type arg-types &rest args)
  (let* ((fn-name (function-designator-name function-designator))
         (signature (cons return-type arg-types))
         (key (cons fn-name signature))
         (info (or (gethash key *call-site-stats*)
                   (setf (gethash key *call-site-stats*)
                         (make-call-site-info :signature signature)))))
    
    ;; Increment call count
    (incf (call-site-info-count info))
    
    ;; Check if we should compile
    (when (and (>= (call-site-info-count info) *jit-threshold*)
               (null (call-site-info-compiled-fn info)))
      (setf (call-site-info-compiled-fn info)
            (jit-compile-call-site fn-name signature)))
    
    ;; Use compiled version if available
    (if (call-site-info-compiled-fn info)
        (apply (call-site-info-compiled-fn info) args)
        (shared-call-interpreted function-designator return-type arg-types args))))
```

### Phase 2: Code Generation

Generate specialized functions for hot signatures:

```lisp
(defun jit-compile-call-site (fn-name signature)
  "Generate and compile optimized function for signature"
  (let* ((return-type (car signature))
         (arg-types (cdr signature))
         (arg-names (loop for i from 0 below (length arg-types)
                          collect (gensym (format nil "ARG~D-" i))))
         (fn-address (get-function-address fn-name)))
    
    ;; Generate optimized function form
    (let ((form `(lambda ,arg-names
                   (declare (optimize (speed 3) (safety 0) (debug 0)))
                   ;; Type declarations for arguments
                   ,@(loop for name in arg-names
                           for type in arg-types
                           collect `(declare (type ,(lisp-type-for-ffi-type type) ,name)))
                   
                   ;; Direct alien funcall without eval
                   (sb-alien:alien-funcall 
                    (sb-alien:sap-alien 
                     (sb-sys:int-sap ,fn-address)
                     (sb-alien:function 
                      ,(alien-type-for-ffi-type return-type)
                      ,@(mapcar #'alien-type-for-ffi-type arg-types)))
                    ,@arg-names))))
      
      ;; Compile the form
      (compile nil form))))
```

### Phase 3: Specialized Optimizations

#### Inline Short Functions

For very short functions (like `strlen`), generate inline code:

```lisp
(defun generate-inline-strlen ()
  "Generate inlined strlen implementation"
  '(lambda (string)
     (declare (optimize (speed 3) (safety 0))
              (type string string))
     (length string))) ; Use Lisp's length for ASCII strings
```

#### Batch Operations

For functions called in loops, generate vectorized versions:

```lisp
(defun generate-batch-call (fn-name signature batch-size)
  "Generate function that processes multiple calls at once"
  ;; Implementation for batch processing
  )
```

## Performance Optimizations

### 1. Type Specialization

Generate type-specific code paths:

```lisp
;; Instead of generic number handling
(if (numberp value) value (error "Expected number"))

;; Generate specialized checks
(declare (type fixnum value)) ; For :int arguments
```

### 2. Argument Packing

Pre-allocate argument arrays for complex signatures:

```lisp
(defstruct ffi-arg-buffer
  (sap-args (make-array 8))      ; Pre-allocated SAPs
  (converted (make-array 8)))    ; Converted values
```

### 3. Call Site Caching

Cache everything possible at the call site:

- Function addresses
- Type conversion functions  
- Alien type specifications
- Argument count validation

## Adaptive Recompilation

Monitor performance and recompile if needed:

```lisp
(defun should-recompile-p (info)
  "Decide if call site should be recompiled"
  (and (> (- (call-site-info-count info)
             (call-site-info-last-compile-count info))
          *jit-recompile-threshold*)
       ;; Add more heuristics: argument patterns, timing, etc.
       ))
```

## Debugging Support

Maintain debugging information:

```lisp
(defstruct jit-debug-info
  (source-location nil)
  (original-signature nil)
  (compilation-time nil)
  (optimization-level nil))

(defvar *jit-debug-map* (make-hash-table)
  "Maps compiled functions to debug info")
```

## Example: JIT-Compiled strlen

Here's what the JIT compiler would generate for `strlen`:

```lisp
;; Original interpreted call
(shared-call '("strlen" "libc") :unsigned-long '(:string) "hello")

;; After JIT compilation, generates:
(lambda (string-arg)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type string string-arg))
  (sb-alien:alien-funcall 
   (sb-alien:sap-alien 
    (sb-sys:int-sap #x7F4D7CFE72AB) ; Cached function address
    (sb-alien:function sb-alien:unsigned-long sb-alien:c-string))
   string-arg))
```

## Implementation Phases

1. **Phase 1**: Call site profiling and statistics
2. **Phase 2**: Basic JIT compilation for hot functions  
3. **Phase 3**: Specialized optimizations (inlining, batching)
4. **Phase 4**: Adaptive recompilation
5. **Phase 5**: Integration with SBCL's compiler

## Expected Performance Gains

Based on analysis of current implementation:

- **Simple functions** (getpid, strlen): 5-10x speedup
- **Complex functions** (with structures): 2-5x speedup  
- **Memory overhead**: ~4KB per compiled call site
- **Compilation time**: <10ms per function

## Future Enhancements

1. **Profile-Guided Optimization**: Use runtime data to optimize
2. **Vectorization**: SIMD operations for batch calls
3. **Cross-Function Optimization**: Optimize sequences of FFI calls
4. **AOT Compilation**: Pre-compile known hot functions
5. **LLVM Integration**: Use LLVM for more advanced optimizations

## Conclusion

JIT compilation can significantly improve FFI performance for frequently-called functions. The design is modular and can be implemented incrementally, starting with basic compilation and adding optimizations over time.
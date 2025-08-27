# Epsilon Foreign Core API Design

## Core API (Essential exports only)

### Function Definition & Calling (5 exports)
```lisp
defshared           ; Define foreign functions  
shared-call         ; Call foreign functions
lib-open            ; Open shared libraries
lib-close           ; Close shared libraries
lib-function        ; Get function pointers
```

### Memory Management (3 exports)
```lisp
foreign-alloc       ; Allocate foreign memory
foreign-free        ; Free foreign memory
with-foreign-memory ; RAII memory management
```

### Type Conversion (4 exports)
```lisp
convert-to-foreign   ; Lisp -> C conversion
convert-from-foreign ; C -> Lisp conversion
define-c-type        ; Define custom types
with-c-string        ; String conversion helper
```

### Structures (5 exports)
```lisp
define-c-struct      ; Define C structures
with-c-struct        ; Allocate structures
struct-ref           ; Access struct fields
(setf struct-ref)    ; Set struct fields
struct-size          ; Get structure size
```

### Callbacks (3 exports)
```lisp
defcallback          ; Define Lisp callbacks
with-callback        ; Temporary callbacks
callback-pointer     ; Get callback address
```

### Error Handling (2 exports)
```lisp
foreign-error        ; Error condition type
with-foreign-error-handler ; Error handling macro
```

**Total Core Exports: 22**

## Extension Modules

### epsilon.foreign.advanced
- Arrays and complex types
- Enums and unions
- Batch operations (defcfuns)

### epsilon.foreign.performance
- Trampolines and JIT compilation
- Call statistics and benchmarking
- Optimization configuration

### epsilon.foreign.discovery
- Auto-discovery mechanisms
- Clang integration
- Header parsing

### epsilon.foreign.platform
- Platform-specific helpers
- epoll/kqueue/iocp wrappers
- System-specific optimizations

## Migration Path

### Phase 1: Create New Package Structure
```lisp
;; epsilon.foreign - Core only (22 exports)
;; epsilon.foreign.advanced - Extended types
;; epsilon.foreign.performance - Optimization
;; epsilon.foreign.discovery - Auto-generation
```

### Phase 2: Deprecation Notices
```lisp
(defmacro deprecated-export (name &optional replacement)
  `(progn
     (export ',name)
     (defun ,name (&rest args)
       (warn "~A is deprecated. Use ~A instead."
             ',name ',(or replacement 'epsilon.foreign.advanced))
       (apply #',(intern (string name) :epsilon.foreign.internal) args))))
```

### Phase 3: Documentation
- Core API reference with examples
- Migration guide for existing code
- Extension module documentation

## Design Principles

1. **Minimal Surface**: Core API contains only essential functionality
2. **Clear Layers**: Strict separation between core and extensions
3. **Progressive Disclosure**: Advanced features in optional modules
4. **Backward Compatible**: Deprecation path for existing code
5. **Zero Cost**: Core operations have minimal overhead
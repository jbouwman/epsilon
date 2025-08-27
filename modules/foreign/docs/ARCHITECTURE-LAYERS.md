# Epsilon Foreign Architecture Layers

## Layer Architecture (CFFI-inspired)

### Layer 1: Backend (epsilon.foreign.backend)
**Files:** backend-sbcl.lisp, backend-interface.lisp
**Purpose:** Implementation-specific low-level operations

```lisp
;; backend-interface.lisp - Generic functions all backends must implement
(defgeneric backend-dlopen (name))
(defgeneric backend-dlsym (handle symbol))
(defgeneric backend-dlclose (handle))
(defgeneric backend-foreign-alloc (size))
(defgeneric backend-foreign-free (ptr))
(defgeneric backend-call-function (ptr return-type arg-types args))
```

### Layer 2: Core Runtime (epsilon.foreign.runtime)
**Files:** runtime.lisp, types.lisp, memory.lisp
**Purpose:** Platform-independent runtime services

- Type conversion tables
- Memory management
- Function resolution cache
- Error handling

### Layer 3: Frontend API (epsilon.foreign)
**Files:** api.lisp, macros.lisp
**Purpose:** User-facing declarative interface

- defshared macro
- with-* resource management macros
- High-level type definitions

### Layer 4: Extensions (epsilon.foreign.ext.*)
**Purpose:** Optional advanced features

- Performance optimizations
- Auto-discovery
- Platform-specific features

## File Reorganization Plan

### Current Structure (Problem)
```
foreign.lisp (1575 lines) - MONOLITHIC
├── libffi integration
├── type system
├── memory management
├── callbacks
├── trampolines
├── smart FFI
├── error handling
└── utilities
```

### Proposed Structure (Solution)
```
src/
├── core/
│   ├── package.lisp (22 exports)
│   ├── api.lisp (200 lines) - Public API
│   ├── macros.lisp (150 lines) - defshared, with-*
│   └── conditions.lisp (50 lines) - Error types
│
├── runtime/
│   ├── types.lisp (200 lines) - Type system
│   ├── memory.lisp (150 lines) - Memory management
│   ├── cache.lisp (100 lines) - Function cache
│   └── conversion.lisp (200 lines) - Type conversion
│
├── backend/
│   ├── interface.lisp (50 lines) - Generic protocol
│   ├── sbcl.lisp (300 lines) - SBCL implementation
│   ├── libffi.lisp (200 lines) - libffi backend
│   └── trampoline.lisp (291 lines) - Fast path
│
└── ext/
    ├── advanced/ - Complex types, arrays
    ├── performance/ - Optimization, benchmarking
    ├── discovery/ - Auto-generation, clang
    └── platform/ - OS-specific features
```

## Implementation Strategy

### Step 1: Create Backend Abstraction
```lisp
(defpackage epsilon.foreign.backend
  (:use :cl)
  (:export #:*current-backend*
           #:backend
           #:make-backend
           ;; ... protocol functions
           ))

(defclass backend () 
  ((name :initarg :name)
   (capabilities :initarg :capabilities)))

(defgeneric backend-call-function (backend ptr return-type arg-types args)
  (:documentation "Call a foreign function through this backend"))
```

### Step 2: Extract Type System
```lisp
(defpackage epsilon.foreign.types
  (:use :cl)
  (:local-nicknames (#:backend epsilon.foreign.backend))
  (:export #:define-foreign-type
           #:convert-to-foreign
           #:convert-from-foreign))

;; Table-driven type conversion
(defparameter *type-converters*
  (make-hash-table :test 'eq))

(defmacro define-type-converter (type &key to-foreign from-foreign size)
  `(setf (gethash ',type *type-converters*)
         (make-type-converter
           :to-foreign ,to-foreign
           :from-foreign ,from-foreign
           :size ,size)))
```

### Step 3: Simplify Call Path
```lisp
;; Single, optimized entry point
(defun shared-call (function-designator return-type arg-types &rest args)
  (let* ((signature (make-signature return-type arg-types))
         (backend (select-best-backend signature))
         (address (resolve-function function-designator)))
    (backend-call-function backend address return-type arg-types args)))

;; Smart backend selection
(defun select-best-backend (signature)
  (cond
    ;; Fast path for simple signatures
    ((simple-signature-p signature)
     *trampoline-backend*)
    ;; Complex signatures need libffi
    ((needs-libffi-p signature)
     *libffi-backend*)
    ;; Fallback
    (t *default-backend*)))
```

## Benefits

1. **Clarity**: Clear separation of concerns
2. **Maintainability**: No file > 500 lines
3. **Performance**: Optimized call paths
4. **Extensibility**: Easy to add new backends
5. **Testability**: Each layer can be tested independently
6. **Documentation**: Natural documentation boundaries
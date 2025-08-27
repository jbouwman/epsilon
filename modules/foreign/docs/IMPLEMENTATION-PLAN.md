# Epsilon Foreign Implementation Plan

## Executive Summary

Refactor epsilon.foreign from a 1575-line monolithic module into a clean, layered architecture with a minimal core API (22 exports) and optional extension modules.

## Timeline: 2 Weeks

### Week 1: Core Refactoring

#### Day 1-2: Backend Abstraction Layer
**Goal:** Create pluggable backend system

```lisp
;; src/backend/protocol.lisp
(defpackage epsilon.foreign.backend.protocol
  (:use :cl)
  (:export
   ;; Protocol
   #:backend
   #:backend-capabilities
   #:backend-call
   #:backend-alloc
   #:backend-free
   ;; Registry
   #:register-backend
   #:find-backend
   #:*default-backend*))

(defgeneric backend-call (backend address return-type arg-types args)
  (:documentation "Execute foreign function call through backend"))

(defgeneric backend-capabilities (backend)
  (:documentation "Return capability keywords: :callbacks :structs-by-value :varargs"))
```

```lisp
;; src/backend/sbcl-direct.lisp
(defclass sbcl-direct-backend (backend)
  ((capabilities :initform '(:basic :strings :pointers))))

(defmethod backend-call ((backend sbcl-direct-backend) 
                         address return-type arg-types args)
  ;; Generate optimized sb-alien call
  (let ((alien-form (generate-alien-form address return-type arg-types)))
    (apply alien-form args)))
```

#### Day 3-4: Core API Implementation
**Goal:** Implement minimal 22-function API

```lisp
;; src/core/api.lisp
(defpackage epsilon.foreign
  (:use :cl)
  (:local-nicknames 
   (#:backend epsilon.foreign.backend.protocol)
   (#:types epsilon.foreign.types))
  (:export
   ;; Core API only
   #:defshared #:shared-call
   #:lib-open #:lib-close #:lib-function
   #:foreign-alloc #:foreign-free #:with-foreign-memory
   #:convert-to-foreign #:convert-from-foreign
   #:define-c-type #:with-c-string
   #:define-c-struct #:with-c-struct 
   #:struct-ref #:struct-size
   #:defcallback #:with-callback #:callback-pointer
   #:foreign-error #:with-foreign-error-handler))

(defmacro defshared (name c-name lib return-type args &rest options)
  "Define a foreign function binding.
   
   Example:
     (defshared strlen \"strlen\" \"libc\" :size-t ((str :string))
       :documentation \"Get string length\")"
  (let ((backend (or (getf options :backend) '*default-backend*))
        (arg-names (mapcar #'first args))
        (arg-types (mapcar #'second args)))
    `(defun ,name ,arg-names
       ,@(when-let (doc (getf options :documentation))
           (list doc))
       (shared-call (list ',c-name ,lib) 
                    ,return-type ',arg-types
                    ,@arg-names))))
```

#### Day 5: Type System Refactoring
**Goal:** Table-driven type conversion

```lisp
;; src/runtime/types.lisp
(defpackage epsilon.foreign.types
  (:use :cl)
  (:export
   #:define-type-mapping
   #:convert-value
   #:type-size
   #:type-alignment))

(defstruct type-info
  name
  size
  alignment
  to-foreign
  from-foreign
  sbcl-type)

(defparameter *type-registry* (make-hash-table :test 'eq))

(defmacro define-primitive-type (name &key size align sbcl-type
                                       to-foreign from-foreign)
  `(setf (gethash ',name *type-registry*)
         (make-type-info
          :name ',name
          :size ,size
          :alignment ,align
          :sbcl-type ',sbcl-type
          :to-foreign ,to-foreign
          :from-foreign ,from-foreign)))

;; Define all primitive types
(define-primitive-type :int
  :size 4 :align 4
  :sbcl-type sb-alien:int
  :to-foreign #'identity
  :from-foreign #'identity)

(define-primitive-type :string  
  :size 8 :align 8
  :sbcl-type sb-alien:c-string
  :to-foreign (lambda (s) (sb-alien:make-alien-string s))
  :from-foreign #'identity)
```

### Week 2: Documentation & Optimization

#### Day 6-7: Comprehensive Documentation
**Goal:** 100% API documentation with examples

```lisp
;; Every exported function gets:
;; 1. Docstring
;; 2. Example usage
;; 3. See-also references

(defun shared-call (function-designator return-type arg-types &rest args)
  "Call a foreign function.
   
   Arguments:
     function-designator - Symbol, string, or (name library) list
     return-type - Return type keyword (:int :pointer :void etc)
     arg-types - List of argument type keywords
     args - Actual arguments to pass
   
   Returns:
     The foreign function's return value, converted to Lisp
   
   Examples:
     ;; Call strlen from libc
     (shared-call \"strlen\" :size-t '(:string) \"hello\")
     => 5
     
     ;; Call function from specific library  
     (shared-call '(\"SSL_new\" \"libssl\") :pointer '(:pointer) ctx)
   
   See Also:
     defshared - Macro for defining foreign functions
     lib-function - Get function pointer directly"
  ...)
```

#### Day 8-9: Performance Optimization
**Goal:** Single fast path for common cases

```lisp
;; src/runtime/fast-path.lisp
(defpackage epsilon.foreign.fastpath
  (:use :cl)
  (:export #:make-optimized-caller))

(defun make-optimized-caller (name address return-type arg-types)
  "Generate an optimized caller for a specific signature"
  (let ((key (list return-type arg-types)))
    (case key
      ;; Special-case common signatures
      (((:int ()))  ; int fn(void)
       (make-int-void-caller address))
      (((:pointer (:pointer))) ; void* fn(void*)
       (make-pointer-pointer-caller address))
      (((:size-t (:string))) ; size_t fn(const char*)
       (make-size-t-string-caller address))
      ;; Generic path
      (t (make-generic-caller address return-type arg-types)))))

;; Pre-compiled trampoline for int(void)
(defun make-int-void-caller (address)
  (let ((sap (sb-sys:int-sap address)))
    (lambda ()
      (sb-alien:alien-funcall
       (sb-alien:sap-alien sap
         (sb-alien:function sb-alien:int))))))
```

#### Day 10: Migration & Testing
**Goal:** Ensure backward compatibility

```lisp
;; src/compat/deprecated.lisp
(defpackage epsilon.foreign.compat
  (:use :cl :epsilon.foreign))

;; Generate compatibility wrappers
(defmacro generate-compat-wrapper (old-name new-name)
  `(progn
     (defun ,old-name (&rest args)
       (warn 'deprecation-warning
             :old-name ',old-name
             :new-name ',new-name
             :removal-version "2.0.0")
       (apply #',new-name args))
     (export ',old-name)))

;; Maintain old exports during transition
(generate-compat-wrapper shared-call-unified shared-call)
(generate-compat-wrapper shared-call-fast shared-call)
(generate-compat-wrapper shared-call-libffi shared-call)
```

## Success Metrics

1. **API Surface**: Reduce from 172 to 22 core exports
2. **Code Organization**: No file > 500 lines
3. **Documentation**: 100% coverage of public API
4. **Performance**: 2-3x speedup for common calls
5. **Test Coverage**: All tests still pass
6. **Memory Safety**: No leaks in error paths

## Risk Mitigation

1. **Backward Compatibility**: Deprecation warnings, not immediate removal
2. **Performance Regression**: Benchmark before/after each change
3. **Test Failures**: Run tests after each refactoring step
4. **User Impact**: Provide migration guide and examples

## Rollout Plan

### Phase 1: Internal Refactoring (Week 1)
- No external API changes
- All tests continue to pass
- Performance improvements visible

### Phase 2: API Transition (Week 2)  
- New clean API available
- Old API deprecated but functional
- Documentation and examples ready

### Phase 3: User Migration (Month 2)
- Users migrate to new API
- Support provided for migration
- Deprecation warnings active

### Phase 4: Cleanup (Month 3)
- Remove deprecated code
- Final optimization pass
- Release 2.0.0

## Next Steps

1. Create feature branch for refactoring
2. Set up benchmarking infrastructure
3. Begin backend abstraction implementation
4. Weekly progress reviews
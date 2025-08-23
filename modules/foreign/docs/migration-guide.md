# Migration Guide: epsilon.linux to epsilon.foreign

This guide shows how to migrate the epsilon.linux module from using direct sb-alien calls to the epsilon.foreign FFI module.

## Benefits of Migration

1. **Cleaner API**: The `defshared` macro provides a more readable syntax than `sb-alien:define-alien-routine`
2. **Platform Independence**: epsilon.foreign handles platform-specific library naming automatically
3. **Performance**: Function pointers are cached for better performance
4. **Consistency**: Unified error handling and memory management across all FFI calls
5. **Maintainability**: Easier to add new functions and maintain existing ones

## Step-by-Step Migration

### 1. Update Package Dependencies

Replace sb-alien references with epsilon.foreign:

```lisp
;; Before
(defpackage :epsilon.sys.epoll
  (:use :cl))

;; After  
(defpackage :epsilon.sys.epoll
  (:use :cl)
  (:local-nicknames
   (lib epsilon.foreign)))
```

### 2. Replace Alien Function Definitions

Convert `sb-alien:define-alien-routine` to `lib:defshared`:

```lisp
;; Before
(sb-alien:define-alien-routine ("epoll_create" %epoll-create) sb-alien:int
  (size sb-alien:int))

;; After
(lib:defshared %epoll-create "epoll_create" "libc" :int
  (size :int)
  :documentation "Create epoll file descriptor")
```

### 3. Update Memory Allocation

Replace `sb-alien:with-alien` with `lib:with-foreign-memory`:

```lisp
;; Before
(sb-alien:with-alien ((event-buf (sb-alien:array sb-alien:char 16)))
  (let ((sap (sb-alien:alien-sap event-buf)))
    ;; use sap
    ))

;; After
(lib:with-foreign-memory ((event-buf 16))
  ;; event-buf is already a SAP
  ;; use event-buf directly
  )
```

### 4. Structure Handling

For structures, continue using manual packing/unpacking with SAP operations:

```lisp
;; Pack epoll_event structure
(defun pack-epoll-event (event buffer offset)
  (let ((ptr (sb-sys:sap+ buffer offset)))
    (setf (sb-sys:sap-ref-32 ptr 0) (epoll-event-events event))
    (setf (sb-sys:sap-ref-64 ptr 8) (epoll-event-data event))))
```

### 5. Type Conversions

epsilon.foreign handles common type conversions automatically:

- `:int`, `:long`, `:short` - Numeric types
- `:pointer` - System area pointers
- `:string` - Automatic string conversion
- `:void` - For functions with no return value

## Complete Example: epoll Module

### Original (sb-alien)

```lisp
(defpackage :epsilon.sys.epoll
  (:use :cl)
  (:export #:epoll-create #:epoll-ctl #:epoll-wait))

(in-package :epsilon.sys.epoll)

(sb-alien:define-alien-routine ("epoll_create1" %epoll-create1) sb-alien:int
  (flags sb-alien:int))

(sb-alien:define-alien-routine ("epoll_ctl" %epoll-ctl) sb-alien:int
  (epfd sb-alien:int)
  (op sb-alien:int)
  (fd sb-alien:int)
  (event sb-alien:system-area-pointer))

(defun epoll-create1 (flags)
  (let ((epfd (%epoll-create1 flags)))
    (when (= epfd -1)
      (error "Failed to create epoll"))
    epfd))

(defun epoll-ctl (epfd op fd event)
  (sb-alien:with-alien ((event-buf (sb-alien:array sb-alien:char 16)))
    (let ((sap (sb-alien:alien-sap event-buf)))
      (pack-event event sap)
      (%epoll-ctl epfd op fd sap))))
```

### Migrated (epsilon.foreign)

```lisp
(defpackage :epsilon.sys.epoll
  (:use :cl)
  (:local-nicknames (lib epsilon.foreign))
  (:export #:epoll-create #:epoll-ctl #:epoll-wait))

(in-package :epsilon.sys.epoll)

(lib:defshared %epoll-create1 "epoll_create1" "libc" :int
  (flags :int))

(lib:defshared %epoll-ctl "epoll_ctl" "libc" :int
  (epfd :int) (op :int) (fd :int) (event :pointer))

(defun epoll-create1 (flags)
  (let ((epfd (%epoll-create1 flags)))
    (when (= epfd -1)
      (error "Failed to create epoll"))
    epfd))

(defun epoll-ctl (epfd op fd event)
  (lib:with-foreign-memory ((event-buf 16))
    (pack-event event event-buf)
    (%epoll-ctl epfd op fd event-buf)))
```

## Testing the Migration

1. Create parallel implementations (e.g., `epoll-v2.lisp`)
2. Write  tests comparing old and new implementations
3. Benchmark performance to ensure no regression
4. Gradually replace usages of the old module

## Common Patterns

### Pattern 1: Simple System Calls

```lisp
;; Before
(sb-alien:define-alien-routine ("close" %close) sb-alien:int
  (fd sb-alien:int))

;; After
(lib:defshared %close "close" "libc" :int
  (fd :int))
```

### Pattern 2: Functions with Buffers

```lisp
;; Before
(sb-alien:define-alien-routine ("read" %read) sb-alien:long
  (fd sb-alien:int)
  (buf sb-alien:system-area-pointer)
  (count sb-alien:unsigned-long))

;; After
(lib:defshared %read "read" "libc" :long
  (fd :int) (buf :pointer) (count :unsigned-long))
```

### Pattern 3: String Arguments

```lisp
;; Before
(sb-alien:define-alien-routine ("getenv" %getenv) sb-alien:c-string
  (name sb-alien:c-string))

;; After
(lib:defshared %getenv "getenv" "libc" :pointer
  (name :string))
```

## Performance Considerations

- epsilon.foreign caches function pointers after first lookup
- Memory allocation patterns remain the same
- Type conversions are optimized for common cases
- Overall performance should be equivalent or better

## Troubleshooting

### Missing Function Signatures

If you encounter "Function signature not yet implemented", add the signature to `shared-call` in `foreign.lisp`:

```lisp
;; Add new signature pattern
((and (eq return-type :your-type) (equal arg-types '(:arg1 :arg2)))
 (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                               args arg-types)))
   (eval `(sb-alien:alien-funcall 
           (sb-alien:sap-alien 
            (sb-sys:int-sap ,function-address)
            (sb-alien:function sb-alien:your-type sb-alien:arg1 sb-alien:arg2))
           ,@converted-args))))
```

### Platform-Specific Libraries

epsilon.foreign automatically handles platform differences:

- Linux: "libc" → "libc.so.6"
- macOS: "libc" → "/usr/lib/libSystem.B.dylib"
- Windows: "msvcrt" → "msvcrt.dll" (planned)

## Next Steps

1. Identify all sb-alien usage in your module
2. Create a parallel implementation using epsilon.foreign
3. Write tests to verify correctness
4. Benchmark performance
5. Replace the original implementation
6. Remove sb-alien dependencies

The migration improves code clarity while maintaining full compatibility with existing functionality.
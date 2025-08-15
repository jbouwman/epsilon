# Epsilon Foreign Function Interface

## Overview

Epsilon provides a Foreign Function Interface (FFI) system for calling C libraries and creating C-callable callbacks from Lisp functions.

## Architecture

### Core Components

1. **Compiled Trampolines** - Replace eval-based calls with compiled functions
2. **Automatic Marshalling** - Type inference and conversion for common C functions  
3. **Struct Support** - Access to C structures with layout discovery
4. **Callback System** - C-callable function pointers via libffi integration

### Type System

The FFI supports C type mapping:

```lisp
:void :int :long :float :double :pointer :string :bool
:char :unsigned-char :short :unsigned-short 
:unsigned-int :unsigned-long :size
```

Composite types include structs, unions, arrays, and function pointers.

## Basic Usage

### Function Calls

```lisp
;; Simple function call
(shared-call '("strlen" "libc") :unsigned-long '(:string) "hello")
;; => 5

;; Define reusable binding
(defshared my-strlen "strlen" "libc" :unsigned-long (s :string))
(my-strlen "world")
;; => 5
```

### Library Management

```lisp
;; Open library
(lib-open "libssl")

;; Close library  
(lib-close handle)

;; Search paths
(setf *library-search-paths* '("/usr/lib" "/opt/lib"))
```

## Memory Management

### Basic Allocation

```lisp
;; Allocate foreign memory
(let ((ptr (foreign-alloc :int :count 10)))
  (unwind-protect 
      (use-foreign-memory ptr)
    (foreign-free ptr)))

;; Scoped allocation
(with-foreign-memory ((ptr :int :count 10))
  (use-foreign-memory ptr))
```

### Array Handling

```lisp
;; Pin Lisp array for zero-copy access
(let ((array (make-array 1000 :element-type '(unsigned-byte 8))))
  (with-pinned-array (ptr array)
    (shared-call '("process_bytes" "libc") :void 
                 '(:pointer :unsigned-long) ptr 1000)))
```

## Struct Support

### Definition and Layout

```lisp
;; Define C-compatible struct
(define-c-struct timespec
  (sec :long)
  (nsec :long))

;; Automatic layout discovery from C header
(define-c-struct-auto stat-struct 
  "struct stat { dev_t st_dev; ino_t st_ino; /* ... */ };")
```

### Usage

```lisp
;; Allocate and use struct
(with-c-struct (ts timespec)
  (setf (struct-ref ts 'sec) 123456789)
  (setf (struct-ref ts 'nsec) 500000000)
  (shared-call '("nanosleep" "libc") :int '(:pointer :pointer) 
               (struct-pointer ts) (sb-sys:int-sap 0)))
```

## Callback System

### Basic Callbacks

```lisp
;; Create C-callable function pointer
(make-callback (lambda (x) (* x 2)) :int '(:int))
;; => #<SB-SYS:SYSTEM-AREA-POINTER {1002F5C000}>

;; Use with C library function
(let* ((array #(3 1 4 1 5 9 2 6))
       (compare-fn (lambda (a-ptr b-ptr)
                     (let ((a (sb-alien:deref (sb-alien:cast a-ptr (* sb-alien:int)) 0))
                           (b (sb-alien:deref (sb-alien:cast b-ptr (* sb-alien:int)) 0)))
                       (cond ((< a b) -1)
                             ((> a b) 1)
                             (t 0)))))
       (callback-ptr (make-callback compare-fn :int '(:pointer :pointer))))
  (sb-sys:with-pinned-objects (array)
    (shared-call '("qsort" "libc") :void
                 '(:pointer :unsigned-long :unsigned-long :pointer)
                 (sb-sys:vector-sap array) 8 4 callback-ptr))
  array)
;; => #(1 1 2 3 4 5 6 9)
```

### Convenience Macros

```lisp
;; Define named callback
(defcallback my-comparator :int ((a :pointer) (b :pointer))
  (- (deref-int a) (deref-int b)))

;; Scoped callback usage
(with-callback (cmp (lambda (x y) (compare x y)) :int '(:int :int))
  (use-callback cmp))
```

## libffi Integration

### Architecture

The callback system uses libffi when available, falling back to SBCL infrastructure:

```
make-callback()
├── libffi available? 
├── YES: libffi C extension
│   ├── Real C-callable function pointers
│   ├── ffi_closure_alloc() + ffi_prep_closure_loc()
│   └── Cross-platform support
└── NO: SBCL fallback
    ├── Infrastructure testing
    └── Graceful degradation
```

### Dependencies

- **libffi library** (optional)
- **GCC or compatible C compiler** (for building extension)
- **SBCL with alien support**

### Installation

```bash
# Install libffi
apt install libffi-dev        # Ubuntu/Debian
brew install libffi           # macOS
dnf install libffi-devel      # Fedora

# Build extension (automatic if libffi available)
cd modules/foreign/c && make
```

## Performance Characteristics

### FFI Calls
- **Trampoline overhead**: ~50ns per call
- **Type conversion**: ~5-20ns per argument
- **Memory allocation**: Uses pooling where applicable

### Callbacks  
- **Creation time**: ~100μs (libffi) vs instantaneous (SBCL fallback)
- **Invocation overhead**: ~50-200ns (libffi) vs dispatch overhead (fallback)
- **Memory usage**: ~4KB per callback (libffi) vs minimal (fallback)

## Error Handling

### Foreign Errors

```lisp
(define-condition foreign-error (error)
  ((code :initarg :code :reader foreign-error-code)
   (function :initarg :function :reader foreign-error-function)))
```

### Automatic Checking

```lisp
;; Check errno after system calls
(defshared-with-errno open "open" "libc" :int 
  (path :string) (flags :int) (mode :int))
```

## Type Marshalling

### Automatic Conversion

The system includes automatic marshalling for common patterns:

```lisp
;; String handling
(shared-call '("getenv" "libc") :pointer '(:string) "PATH")

;; Array passing
(with-pinned-array (ptr byte-array)
  (shared-call '("write" "libc") :long 
               '(:int :pointer :unsigned-long) 1 ptr (length byte-array)))

;; Struct passing  
(with-c-struct (addr sockaddr-in)
  (shared-call '("bind" "libc") :int 
               '(:int :pointer :unsigned-int) 
               sock (struct-pointer addr) (struct-layout-size addr)))
```

### Custom Converters

```lisp
(defmethod convert-to-foreign ((value pathname) (type (eql :string)))
  (namestring value))

(defmethod convert-from-foreign ((value sb-sys:system-area-pointer) 
                                 (type (eql :string)))
  (unless (sb-alien:null-alien value)
    (sb-alien:c-string-to-string value)))
```

## Advanced Features

### Signature Registry

```lisp
;; Register function signature for reuse
(register-signature 'strlen :unsigned-long '(:string))

;; Get cached trampoline
(get-or-create-trampoline :unsigned-long '(:string))
```

### Memory Pools

```lisp
;; Use memory pools for repeated allocations
(with-pooled-memory ((ptr :int :count 100))
  (use-temporary-memory ptr))
```

### Batch Operations

```lisp
;; Optimize multiple related calls
(with-foreign-batch
  (call-1 ...)
  (call-2 ...)
  (call-3 ...))
```

## Integration Examples

### OpenSSL

```lisp
(defshared ssl-library-init "SSL_library_init" "libssl" :int)
(defshared ssl-ctx-new "SSL_CTX_new" "libssl" :pointer (method :pointer))
(defshared tls-client-method "TLS_client_method" "libssl" :pointer)

(ssl-library-init)
(let ((ctx (ssl-ctx-new (tls-client-method))))
  (use-ssl-context ctx))
```

### SQLite

```lisp
(defshared sqlite3-open "sqlite3_open" "libsqlite3" :int 
  (filename :string) (ppdb :pointer))

(with-foreign-memory ((db-ptr :pointer))
  (let ((result (sqlite3-open "test.db" db-ptr)))
    (when (zerop result)
      (use-database (sb-alien:deref db-ptr 0)))))
```

## Testing

The FFI system includes tests covering:

- Basic function calls and type conversions
- Struct definition and access patterns  
- Callback creation and invocation
- Memory management and cleanup
- Error handling and edge cases
- Performance benchmarks
- Library integration

## Implementation Notes

### SBCL Integration

The system builds on SBCL's `sb-alien` interface while providing higher-level abstractions. Key SBCL features used:

- `alien-funcall` for C function calls
- `system-area-pointer` for memory addresses
- `with-pinned-objects` for zero-copy array access
- `load-shared-object` for dynamic library loading

### Platform Support

The FFI system works on all SBCL-supported platforms:

- **Linux**: x86-64, ARM64
- **macOS**: x86-64, Apple Silicon  
- **Windows**: x86-64 (with appropriate toolchain)
- **Other**: Any platform with libffi support

### Thread Safety

All FFI operations are thread-safe:

- Library loading uses global locks
- Signature cache uses thread-safe hash tables
- Callback registry includes mutex protection
- Memory allocation is properly synchronized

## Migration Guide

### From sb-alien

```lisp
;; Old sb-alien code
(sb-alien:define-alien-routine "strlen" sb-alien:int
  (str sb-alien:c-string))

;; New epsilon.foreign code  
(defshared strlen "strlen" "libc" :unsigned-long (str :string))
```

### From CFFI

```lisp
;; CFFI style
(cffi:defcfun "strlen" :int (str :string))

;; Epsilon style
(defshared strlen "strlen" "libc" :unsigned-long (str :string))
```

The Epsilon FFI provides functionality similar to CFFI with integration into the Epsilon module system.
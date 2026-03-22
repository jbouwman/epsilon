# Auto-Binding Patterns for FFI

This document describes when to use automatic vs manual FFI binding generation.

## Overview

The `epsilon.foreign.auto-binding` module provides tools for automatically generating FFI bindings from C headers using libclang. This enables "Julia-style" FFI where function signatures are discovered at load time.

## Decision Matrix

| Criteria | Auto-Binding | Manual Binding |
|----------|--------------|----------------|
| Simple C functions (primitives) | Yes | - |
| Macro-based "functions" | - | Yes |
| Opaque pointer types | Yes | - |
| Complex memory ownership | - | Yes |
| Error condition integration | - | Yes |
| String parameter handling | - | Yes |
| Callback functions | - | Yes |
| Stable well-documented API | Yes | - |
| Rapidly evolving API | Yes | - |
| Performance-critical paths | JIT | Static |

## When to Use Auto-Binding

Use auto-binding when:

1. **Simple Function Signatures**: Functions with primitive types (int, long, double, pointer)
2. **Opaque Handles**: APIs using opaque pointer types (e.g., `PGconn*`, `SSL_CTX*`)
3. **Stable APIs**: Well-documented libraries with stable interfaces
4. **Bulk Binding**: When you need to bind many similar functions quickly
5. **Header-Driven Development**: When the header file is the source of truth

Example use case: Binding a library with 50+ similar functions where most just take and return primitive types or pointers.

## When to Use Manual Binding

Use manual binding when:

1. **Macros**: Many C libraries expose functionality through macros that appear as functions but aren't visible to libclang
   ```c
   // OpenSSL example - SSL_set_tlsext_host_name is a macro!
   #define SSL_set_tlsext_host_name(s,name) \
     SSL_ctrl((s),SSL_CTRL_SET_TLSEXT_HOSTNAME,TLSEXT_NAMETYPE_host_name,(name))
   ```

2. **String Handling**: Functions taking C strings often need Lisp string marshalling:
   ```lisp
   ;; Manual wrapper for string conversion
   (defun pq-connectdb (conninfo)
     (let ((c-str (sb-alien:make-alien-string conninfo)))
       (unwind-protect
           (pq-connectdb-raw (sb-alien:alien-sap c-str))
         (sb-alien:free-alien c-str))))
   ```

3. **Error Integration**: When you need to translate C error codes to Lisp conditions
4. **Memory Management**: Complex ownership patterns requiring explicit alloc/free pairing
5. **Callbacks**: Lisp callbacks need special handling via `sb-alien:with-alien-callable`

## Usage Examples

### Basic Library Binding

```lisp
(deflib :libpq
  :headers ("/usr/include/libpq-fe.h")
  :prefix "PQ"
  :exclude ("PQprint" "PQdisplayTuples")  ; deprecated functions
  :manual (("PQconnectdb" . (defun pq-connectdb (conninfo)
                              "Connect with string conversion"
                              (let ((c-str (sb-alien:make-alien-string conninfo)))
                                (unwind-protect
                                    (pq-connectdb-raw (sb-alien:alien-sap c-str))
                                  (sb-alien:free-alien c-str))))))
  :documentation "PostgreSQL client library bindings")
```

### Exploring a Library API

```lisp
;; List all functions in a header
(describe-library-api "/usr/include/libpq-fe.h" :prefix "PQ")

;; Generate a single binding
(generate-binding-for-function "PQstatus" "/usr/include/libpq-fe.h" :libpq)
;; => (ffi:defshared pq-status "PQstatus" :libpq :int (arg0 :pointer))
```

### Header Caching

Header parsing is cached for performance:

```lisp
;; First call parses the header
(list-header-functions "/usr/include/libpq-fe.h" :prefix "PQ")

;; Subsequent calls use the cache (until file is modified)
(list-header-functions "/usr/include/libpq-fe.h" :prefix "PQ")

;; Clear cache if needed
(clear-header-cache)
```

## Hybrid Approach

For complex libraries like OpenSSL, use a hybrid approach:

1. **Auto-bind pure functions**: EVP functions, BIO operations
2. **Manual bind macros**: SSL_set_tlsext_host_name, etc.
3. **Manual wrap error handling**: Custom condition signaling
4. **Manual wrap memory**: Explicit resource management

Example structure:
```
my-crypto-bindings/
  ffi-auto.lisp       ; Auto-generated bindings
  ffi-manual.lisp     ; Manual wrappers for macros/strings
  conditions.lisp     ; Error condition definitions
  high-level.lisp     ; User-facing API
```

## API Reference

### deflib

```lisp
(deflib library-name &key headers prefix exclude manual include-paths defines documentation)
```

Define FFI bindings for a C library using automatic header parsing.

- `library-name` - Keyword naming the library (e.g., :libpq)
- `headers` - List of header files to parse
- `prefix` - Only bind functions starting with this prefix
- `exclude` - List of function names to exclude from auto-binding
- `manual` - Alist of (c-name . wrapper-form) for manual treatment
- `include-paths` - Additional include paths for header parsing
- `defines` - Preprocessor defines for header parsing
- `documentation` - Documentation string

### generate-bindings-from-header

```lisp
(generate-bindings-from-header header-path library
                               &key prefix exclude include-paths defines manual-wrappers)
```

Generate binding forms for all functions in a header. Returns a list of `defshared` forms.

### list-header-functions

```lisp
(list-header-functions header-path &key include-paths defines prefix)
```

List all functions declared in a header file. Returns plists with `:name` and `:kind`.

### describe-library-api

```lisp
(describe-library-api header-path &key prefix include-paths defines)
```

Print a description of the C API for planning which functions need manual wrappers.

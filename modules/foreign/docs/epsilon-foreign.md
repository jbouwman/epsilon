# epsilon.foreign - Foreign Function Interface

The `epsilon.foreign` module provides a  Foreign Function Interface (FFI) for Common Lisp, allowing seamless integration with C libraries and system calls.

## Overview

The FFI module consists of two main components:

1. **epsilon.foreign** - Core FFI functionality for loading libraries and calling C functions
2. **epsilon.clang** - C language parser for analyzing headers and extracting type information

## Features

- Dynamic library loading with platform-specific path resolution
- Type-safe function calling with automatic conversions
- Memory management utilities
- Structure and union support
- Array handling
- String conversions between Lisp and C
- Error handling for missing libraries and symbols
- Performance-optimized calling conventions

## Quick Start

```lisp
;; Load the module
(epsilon.module:load-module "epsilon.foreign")

;; Define a foreign function
(lib:defshared strlen "strlen" "libc" :unsigned-long (str :string)
  :documentation "Get length of C string")

;; Call the function
(strlen "Hello, World!")  ; => 13
```

## API Reference

### Library Management

#### lib:lib-open
```lisp
(lib-open library-name &key local paths)
```
Opens a shared library and returns a handle. The library name is automatically converted to platform-specific format (e.g., "libc" becomes "libc.so.6" on Linux).

#### lib:lib-close
```lisp
(lib-close library-handle)
```
Closes a previously opened library.

#### lib:lib-function
```lisp
(lib-function library-handle function-name)
```
Returns a pointer to a function in the library, or NIL if not found.

### Function Definition

#### lib:defshared
```lisp
(defshared lisp-name c-name library return-type &rest args &key documentation)
```
Defines a Lisp function that calls a C function. Arguments are specified as (name type) pairs.

Example:
```lisp
(lib:defshared my-printf "printf" "libc" :int 
  (format :string)
  :documentation "Calls C printf function")
```

### Memory Management

#### lib:foreign-alloc
```lisp
(foreign-alloc type-or-size &key count initial-element initial-contents finalize)
```
Allocates foreign memory and returns a system area pointer.

#### lib:foreign-free
```lisp
(foreign-free pointer)
```
Explicitly frees foreign memory.

#### lib:with-foreign-memory
```lisp
(with-foreign-memory bindings &body body)
```
Allocates memory for the duration of body, automatically freeing on exit.

Example:
```lisp
(lib:with-foreign-memory ((buffer 1024))
  ;; Use buffer here
  )
```

### Type System

Supported primitive types:
- `:char`, `:unsigned-char`
- `:short`, `:unsigned-short`
- `:int`, `:unsigned-int`
- `:long`, `:unsigned-long`
- `:float`, `:double`
- `:pointer`
- `:string`
- `:void`

### Platform Support

The module automatically handles platform differences:

- **Linux**: `.so` libraries with version suffixes
- **macOS**: `.dylib` libraries, system libraries in `/usr/lib`
- **Windows**: `.dll` libraries (planned)

## Examples

### Basic Function Calls

```lisp
;; Zero-argument function
(lib:defshared getpid "getpid" "libc" :int)
(getpid)  ; => 12345

;; String manipulation
(lib:defshared strcpy "strcpy" "libc" :pointer 
  (dest :pointer) (src :string))
```

### Working with Structures

```lisp
;; Get current time using clock_gettime
(lib:defshared clock-gettime "clock_gettime" "libc" :int 
  (clockid :int) (tp :pointer))

(let ((timespec (lib:foreign-alloc 16))) ; timespec is 16 bytes
  (unwind-protect
       (when (zerop (clock-gettime 0 timespec)) ; CLOCK_REALTIME = 0
         (let ((seconds (sb-sys:sap-ref-64 timespec 0))
               (nanos (sb-sys:sap-ref-64 timespec 8)))
           (format t "Time: ~D.~9,'0D~%" seconds nanos)))
    (lib:foreign-free timespec)))
```

### Memory Operations

```lisp
;; Allocate and initialize memory
(lib:defshared memset "memset" "libc" :pointer
  (s :pointer) (c :int) (n :unsigned-long))

(let ((buffer (lib:foreign-alloc 256)))
  (unwind-protect
       (progn
         (memset buffer 0 256)  ; Clear buffer
         ;; Use buffer...
         )
    (lib:foreign-free buffer)))
```

### Environment Variables

```lisp
(lib:defshared getenv "getenv" "libc" :pointer (name :string))
(lib:defshared setenv "setenv" "libc" :int 
  (name :string) (value :string) (overwrite :int))

;; Set environment variable
(setenv "MY_VAR" "my_value" 1)

;; Get environment variable
(let ((value-ptr (getenv "MY_VAR")))
  (when (not (sb-sys:sap= value-ptr (sb-sys:int-sap 0)))
    ;; Convert C string to Lisp
    (with-output-to-string (s)
      (loop for i from 0
            for byte = (sb-sys:sap-ref-8 value-ptr i)
            until (zerop byte)
            do (write-char (code-char byte) s)))))
```

## Performance Considerations

The epsilon.foreign FFI is designed for performance:

1. **Function caching**: Function pointers are cached after first lookup
2. **Optimized signatures**: Common function signatures have specialized implementations
3. **Minimal overhead**: Direct alien function calls without intermediate wrappers
4. **Zero-copy operations**: Pointer passing avoids unnecessary copying

Benchmark results show competitive performance with SBCL's native alien interface.

## Error Handling

The FFI provides  error handling:

```lisp
;; Missing library
(handler-case
    (lib:lib-open "nonexistent-library")
  (error (e)
    (format t "Library error: ~A~%" e)))

;; Missing function
(let* ((handle (lib:lib-open "libc"))
       (fn (lib:lib-function handle "nonexistent_function")))
  (if fn
      (format t "Found function~%")
      (format t "Function not found~%")))
```

## Extending the FFI

### Adding New Function Signatures

To support new function signatures, add a clause to `shared-call` in `foreign.lisp`:

```lisp
;; Example: int fn(double, double)
((and (eq return-type :int) (equal arg-types '(:double :double)))
 (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                               args arg-types)))
   (eval `(sb-alien:alien-funcall 
           (sb-alien:sap-alien 
            (sb-sys:int-sap ,function-address)
            (sb-alien:function sb-alien:int sb-alien:double sb-alien:double))
           ,@converted-args))))
```

### Platform-Specific Code

Add platform-specific library name handling in `platform-library-name`:

```lisp
#+windows
(cond
  ((string-suffix-p ".dll" name) name)
  ((string= name "msvcrt") "msvcrt.dll")
  (t (concatenate 'string name ".dll")))
```

## Integration with epsilon.linux

The epsilon.foreign module is designed to replace platform-specific FFI code. For example, epsilon.linux can be updated to use epsilon.foreign instead of direct sb-alien calls:

```lisp
;; Before (using sb-alien)
(sb-alien:define-alien-routine ("epoll_create" %epoll-create) sb-alien:int
  (size sb-alien:int))

;; After (using epsilon.foreign)
(lib:defshared epoll-create "epoll_create" "libc" :int (size :int))
```

## Future Enhancements

Planned features include:

1. **Callback support**: Allow Lisp functions to be passed as C callbacks
2. **Structure generation**: Automatic struct definitions from C headers
3. **JIT compilation**: Runtime optimization of frequently-called functions
4. **Type inference**: Automatic type detection from C headers
5. **Windows support**: Full Windows DLL support
6. **Documentation generation**: Extract documentation from C headers

## C Parser (epsilon.clang)

The C language parser provides tools for analyzing C headers:

```lisp
;; Parse C declarations
(clang:parse "int foo(double x);")

;; Extract type information
(clang:parse-header "/usr/include/time.h" 
                    :types '("timespec" "timeval"))
```

This enables automatic FFI binding generation from existing C headers.
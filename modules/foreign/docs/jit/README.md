# epsilon.foreign.jit

JIT-compiled FFI stubs for zero-overhead foreign function calls.

## Overview

This module provides Julia-style FFI where you can call C functions with near-native performance. It generates native machine code at runtime to eliminate the overhead of dynamic dispatch typically seen in FFI systems like libffi.

### Key Features

- **JIT-compiled stubs**: Native ARM64 and x86-64 code generation
- **Specialized callers**: Type-specific compiled callers eliminate runtime marshalling
- **Automatic signature discovery**: Parse C headers with libclang to auto-detect types
- **Callbacks**: Lisp functions callable from C code
- **Near-native performance**: 5-10ns call overhead vs 200-300ns for libffi

## Quick Start

```lisp
;; Load the module
(require :epsilon.foreign.jit)

;; Create a JIT caller for the C sin() function
(let* ((sin-addr (sb-sys:find-foreign-symbol-address "sin"))
       (fast-sin (jit:make-jit-caller sin-addr :double '(:double))))
  ;; Call it like any Lisp function
  (funcall fast-sin 1.0d0))  ; => 0.8414709848078965d0
```

### Auto-discovery with libclang

```lisp
;; Let libclang discover the signature from headers
(clang:defauto-jit fast-sin "sin" "math.h")
(fast-sin 1.0d0)

;; Or for one-off calls
(clang:auto-jit "strlen" "string.h" "Hello")  ; => 5
```

### Callbacks

```lisp
;; Create a callback callable from C
(cb:defcallback my-comparator :int ((a :int) (b :int))
  (- a b))

;; Get the callback pointer for passing to C
(cb:callback-pointer 'my-comparator)
```

## Packages

| Package | Description |
|---------|-------------|
| `epsilon.foreign.jit` | Core JIT stub generation and calling |
| `epsilon.foreign.jit.callback` | Callback support (Lisp callable from C) |
| `epsilon.foreign.jit.clang` | libclang integration for auto-discovery |

## Supported Types

| Type Keyword | C Type | Size |
|--------------|--------|------|
| `:void` | void | - |
| `:char` | char | 1 |
| `:uchar` | unsigned char | 1 |
| `:short` | short | 2 |
| `:ushort` | unsigned short | 2 |
| `:int` | int | 4 |
| `:uint` | unsigned int | 4 |
| `:long` | long | 8 |
| `:ulong` | unsigned long | 8 |
| `:float` | float | 4 |
| `:double` | double | 8 |
| `:pointer` | void* | 8 |

## Performance

Typical call overhead on modern hardware:

| Method | Overhead | Notes |
|--------|----------|-------|
| Static `alien-funcall` | ~5ns | SBCL's optimized static path |
| JIT `make-jit-caller` | ~10ns | Near-native via specialized caller |
| JIT via `call-stub` | ~20ns | With apply overhead |
| libffi dynamic | ~250ns | Full dynamic dispatch |

Run benchmarks:
```lisp
(require :epsilon.foreign.jit)
(epsilon.foreign.jit.bench:run-benchmarks)
```

## Platform Support

| Platform | Status |
|----------|--------|
| ARM64 (Apple Silicon) | Fully supported |
| x86-64 (Linux/macOS) | Fully supported |
| Other | Not supported |

Check support at runtime:
```lisp
(jit:platform-supported-p)  ; => T or NIL
(jit:*current-platform*)    ; => :ARM64 or :X86-64
```

## API Reference

### Core JIT Functions

#### `make-jit-caller fn-addr return-type arg-types`

Create a JIT-compiled caller function.

```lisp
(let ((caller (jit:make-jit-caller addr :double '(:double :double))))
  (funcall caller 1.0d0 2.0d0))
```

#### `get-or-create-stub fn-addr return-type arg-types`

Get a cached stub or create a new one.

#### `clear-stub-cache`

Clear all cached stubs and free memory.

#### `stub-cache-stats`

Return statistics about stub cache usage.

### Auto-discovery

#### `(clang:defauto-jit name c-name header &key library)`

Define a function with auto-discovered signature.

```lisp
(clang:defauto-jit my-sqrt "sqrt" "math.h")
```

#### `(clang:auto-jit c-name header &rest args)`

One-shot call with auto-discovery.

#### `(clang:discover-signature function-name header-path)`

Discover a function's signature from a header file.

### Callbacks

#### `(cb:defcallback name return-type params &body body)`

Define a named callback.

```lisp
(cb:defcallback compare :int ((a :int) (b :int))
  (- a b))
```

#### `(cb:make-jit-callback fn return-type arg-types)`

Create a callback from a lambda.

#### `(cb:callback-pointer name)`

Get the SAP for a named callback.

## Implementation Details

### Stub Generation

The JIT generates minimal trampoline code that:

1. Loads the target function address into a scratch register
2. Jumps to the target
3. Returns the result

On ARM64:
- Uses x16 as scratch register
- Handles W^X via `sb-vm::jit-memcpy`
- 36 bytes per stub

On x86-64:
- Uses r11 as scratch register
- 14 bytes per stub

### Specialized Callers

For each stub, a specialized Lisp function is compiled that:
- Avoids runtime type dispatch
- Eliminates argument marshalling overhead
- Uses direct `alien-funcall` with known types

```lisp
;; Generated for (make-jit-caller addr :double '(:double))
(lambda (a0)
  (declare (optimize (speed 3) (safety 0)))
  (sb-alien:alien-funcall
    (sb-alien:sap-alien (sb-sys:int-sap STUB-ADDR)
      (function sb-alien:double sb-alien:double))
    a0))
```

## Dependencies

- `epsilon.library` - Shared library loading
- `epsilon.foreign` - Base FFI utilities
- `epsilon.foreign.libclang` - For auto-discovery (optional)

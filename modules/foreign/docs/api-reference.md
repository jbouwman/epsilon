# FFI API Reference

This document provides a complete API reference for epsilon's Foreign Function Interface.

## Core Module: epsilon.foreign

### defshared

```lisp
(defshared name c-name library return-type &rest params)
```

Define a shared library function binding.

- `name` - Lisp function name (symbol)
- `c-name` - C function name (string)
- `library` - Library keyword (e.g., `:libc`, `:libm`)
- `return-type` - Return type keyword
- `params` - Parameter specifications as `(name type)` pairs

Example:
```lisp
(defshared c-strlen "strlen" :libc :size-t (s :pointer))
```

### call

```lisp
(call c-name return-type &rest args)
```

Call a C function by name with JIT signature discovery.

### define-foreign-struct

```lisp
(define-foreign-struct name &rest fields)
```

Define a foreign struct type for use with FFI.

### with-foreign-struct

```lisp
(with-foreign-struct (var struct-type &key initial-values) &body body)
```

Allocate and pin a foreign struct for the duration of body.

### foreign-array

```lisp
(foreign-array element-type dimensions &key initial-contents)
```

Create a foreign-compatible array.

---

## Binding IR Module: epsilon.foreign.binding-ir

### grovel-to-file

```lisp
(grovel-to-file header-paths output-path &key prefix include-paths defines)
```

Parse C headers and write BIR to a file.

- `header-paths` - Header file path(s) to parse
- `output-path` - Output `.bir.lisp` file path
- `prefix` - Only include declarations with this prefix
- `include-paths` - Additional include directories
- `defines` - Preprocessor definitions

### grovel-to-ir

```lisp
(grovel-to-ir header-paths &key prefix include-paths defines)
```

Parse headers and return a `binding-ir` structure.

### load-binding-ir

```lisp
(load-binding-ir path)
```

Load and validate a BIR file. Returns `binding-ir` structure.

### define-library-from-ir

```lisp
(define-library-from-ir library-name bir-path)
```

Macro to load BIR at compile time and generate all bindings.

### write-binding-ir / read-binding-ir

```lisp
(write-binding-ir ir stream)
(read-binding-ir stream)
```

Serialize/deserialize BIR to/from streams.

### validate-binding-ir

```lisp
(validate-binding-ir ir)
```

Validate a BIR structure. Returns `t` or signals an error.

### bir-compatible-p

```lisp
(bir-compatible-p ir)
```

Check if BIR version is compatible with current system.

---

## Signatures Module: epsilon.foreign.signatures

### discover-signature-auto

```lisp
(discover-signature-auto function-name &key library force-refresh)
```

Automatically discover a function's signature using libclang.

- `function-name` - C function name (string)
- `library` - Optional library hint
- `force-refresh` - Bypass cache if true

Returns: `(values return-type param-types)` or `nil`

### detect-function-headers

```lisp
(detect-function-headers function-name)
```

Detect which headers likely contain a function declaration.

Returns: List of header names (e.g., `("string.h")`)

### clear-all-caches

```lisp
(clear-all-caches)
```

Clear all signature and header caches.

---

## Grovel Module: epsilon.foreign.grovel

### grovel-header

```lisp
(grovel-header header-name &key include-paths defines)
```

Parse a header and extract all type information.

### grovel-struct

```lisp
(grovel-struct struct-name header-name &key include-paths defines)
```

Extract a specific struct definition.

### grovel-function

```lisp
(grovel-function function-name header-name &key include-paths defines)
```

Extract a specific function signature.

### grovel-enum

```lisp
(grovel-enum enum-name header-name &key include-paths defines)
```

Extract a specific enum definition.

### grovel-typedef

```lisp
(grovel-typedef typedef-name header-name &key include-paths defines)
```

Extract a specific typedef definition.

---

## Auto-Binding Module: epsilon.foreign.auto-binding

### deflib

```lisp
(deflib library-name &key headers prefix exclude manual include-paths defines)
```

Define FFI bindings for a C library using automatic header parsing.

See [Auto-Binding Patterns](auto-binding.md) for detailed usage.

### generate-bindings-from-header

```lisp
(generate-bindings-from-header header-path library &key prefix exclude)
```

Generate binding forms for all functions in a header.

### list-header-functions

```lisp
(list-header-functions header-path &key include-paths defines prefix)
```

List all functions declared in a header file.

### describe-library-api

```lisp
(describe-library-api header-path &key prefix)
```

Print a description of the C API for planning bindings.

---

## Type Keywords

| Keyword | C Type | Size |
|---------|--------|------|
| `:void` | void | 0 |
| `:char` | char | 1 |
| `:unsigned-char` | unsigned char | 1 |
| `:short` | short | 2 |
| `:unsigned-short` | unsigned short | 2 |
| `:int` | int | 4 |
| `:unsigned-int` | unsigned int | 4 |
| `:long` | long | 8* |
| `:unsigned-long` | unsigned long | 8* |
| `:long-long` | long long | 8 |
| `:float` | float | 4 |
| `:double` | double | 8 |
| `:pointer` | void* | 8* |
| `:size-t` | size_t | 8* |
| `:ssize-t` | ssize_t | 8* |

*Platform-dependent (shown for 64-bit)

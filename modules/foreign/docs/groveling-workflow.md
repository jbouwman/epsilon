# Header Groveling Workflow

This document describes when and how header groveling occurs in epsilon's FFI system.

## Overview

"Groveling" is the process of parsing C headers to extract type information (structs, functions, enums, typedefs). Epsilon uses libclang for accurate parsing.

## When Groveling Occurs

| Phase | Trigger | Action | Output |
|-------|---------|--------|--------|
| **Development** | `grovel-to-file` | Parse headers | `.bir.lisp` file |
| **Build** | `define-library-from-ir` | Load BIR, generate forms | `defshared` in fasl |
| **Runtime** | First FFI call (JIT) | JIT compile stub | Cached stub |
| **Fallback** | No BIR available | Runtime grovel | Slower first call |

## Recommended Workflow

### 1. Development Time: Generate BIR

```lisp
(epsilon.foreign.binding-ir:grovel-to-file
  "/usr/include/libfoo.h"
  "bindings/libfoo.bir.lisp"
  :prefix "foo_"
  :include-paths '("/usr/local/include"))
```

This parses headers once and saves the result. Commit the `.bir.lisp` file to version control.

### 2. Build Time: Load BIR

```lisp
(epsilon.foreign.binding-ir:define-library-from-ir
  :libfoo "bindings/libfoo.bir.lisp")
```

This loads the pre-parsed information and generates `defshared` forms at compile time.

### 3. Runtime: Use Bindings

```lisp
(foo-init)
(foo-process data)
```

No parsing occurs at runtime - stubs are JIT-compiled on first call.

## API Reference

### grovel-to-file

```lisp
(grovel-to-file header-paths output-path &key prefix include-paths defines)
```

Parse headers and write BIR to file.

- `header-paths` - Single path or list of header file paths
- `output-path` - Where to write the `.bir.lisp` file
- `prefix` - Only include declarations starting with this prefix
- `include-paths` - Additional `-I` directories for the preprocessor
- `defines` - Preprocessor definitions as alist `(("DEBUG" . "1"))`

### grovel-to-ir

```lisp
(grovel-to-ir header-paths &key prefix include-paths defines)
```

Parse headers and return a `binding-ir` structure (without writing to file).

### load-binding-ir

```lisp
(load-binding-ir path)
```

Load and validate a BIR file. Returns a `binding-ir` structure.

### define-library-from-ir

```lisp
(define-library-from-ir library-name bir-path)
```

Macro that loads BIR at compile time and defines all bindings.

## Targeted Groveling

For specific types, use targeted functions:

```lisp
;; Single struct
(epsilon.foreign.grovel:grovel-struct "my_struct" "header.h")

;; Single function
(epsilon.foreign.grovel:grovel-function "my_func" "header.h")

;; Single enum
(epsilon.foreign.grovel:grovel-enum "my_enum" "header.h")

;; Single typedef
(epsilon.foreign.grovel:grovel-typedef "my_type" "header.h")
```

## Caching

Header parsing results are cached:

- Cache key: `(header-path . modification-time)`
- Cache invalidates when header file is modified
- Clear manually with `(epsilon.foreign.signatures:clear-all-caches)`

## Troubleshooting

### libclang not available

```
Error: libclang not available for header groveling
```

Ensure libclang is installed and `libclang.so` is in the library path.

### Header not found

```
Error: Failed to parse header: /path/to/header.h
```

Check include paths. Use `:include-paths` to add directories.

### Missing declarations

If expected functions are missing from BIR:
1. Check the `:prefix` filter
2. Verify the function is declared (not a macro)
3. Check for conditional compilation (`#ifdef`)

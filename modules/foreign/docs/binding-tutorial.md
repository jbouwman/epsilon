# Binding a C Library

This tutorial walks through binding a C library using epsilon's FFI system.

## Quick Start

The fastest way to bind a C library:

```lisp
;; 1. Generate BIR file from headers (run once during development)
(epsilon.foreign.binding-ir:grovel-to-file
  "/usr/include/mylib.h"
  "bindings/mylib.bir.lisp"
  :prefix "mylib_")

;; 2. Load bindings at compile time
(epsilon.foreign.binding-ir:define-library-from-ir
  :mylib "bindings/mylib.bir.lisp")

;; 3. Use the functions
(mylib-init)
(mylib-do-something 42)
```

## Workflow Options

### Option A: Compile-Time Groveling (Recommended)

Parse headers once, save results to `.bir.lisp` file, load at compile time.

```lisp
;; Development time: generate BIR
(grovel-to-file "/usr/include/libpq-fe.h"
                "bindings/libpq.bir.lisp"
                :prefix "PQ")

;; Build time: use BIR
(define-library-from-ir :libpq "bindings/libpq.bir.lisp")
```

**Pros**: Fast builds, no runtime libclang dependency
**Cons**: Must regenerate BIR when headers change

### Option B: Runtime Auto-Binding

Parse headers at load time using `deflib`:

```lisp
(deflib :libpq
  :headers ("/usr/include/libpq-fe.h")
  :prefix "PQ")
```

**Pros**: Always up-to-date with system headers
**Cons**: Slower load, requires libclang at runtime

### Option C: JIT Discovery

Discover signatures on first call:

```lisp
;; Signature discovered automatically
(ffi:call "strlen" :pointer "hello")
```

**Pros**: Zero setup
**Cons**: First call is slow, limited to common functions

## Step-by-Step: Binding libpq

### 1. Explore the API

```lisp
;; List available functions
(epsilon.foreign.auto-binding:describe-library-api
  "/usr/include/libpq-fe.h"
  :prefix "PQ")
```

### 2. Generate BIR

```lisp
(epsilon.foreign.binding-ir:grovel-to-file
  "/usr/include/libpq-fe.h"
  "bindings/libpq.bir.lisp"
  :prefix "PQ"
  :include-paths '("/usr/include/postgresql"))
```

### 3. Inspect the BIR File

The generated file is human-readable:

```lisp
(:binding-ir
 :version 1
 :functions
 ((:name "PQconnectdb" :return-type :pointer :params (...))
  (:name "PQfinish" :return-type :void :params (...))
  ...))
```

### 4. Load and Use

```lisp
;; In your library definition
(define-library-from-ir :libpq "bindings/libpq.bir.lisp")

;; Use the bindings
(let ((conn (pq-connectdb "host=localhost dbname=test")))
  (unwind-protect
      (format t "Status: ~A~%" (pq-status conn))
    (pq-finish conn)))
```

## Handling Special Cases

### String Parameters

C strings need explicit conversion:

```lisp
(defun my-pq-connectdb (conninfo)
  "Wrapper with automatic string conversion."
  (sb-alien:with-alien-string (c-str conninfo)
    (pq-connectdb-raw (sb-alien:alien-sap c-str))))
```

### Macros

C macros aren't visible to libclang. Bind manually:

```lisp
;; SSL_set_tlsext_host_name is a macro wrapping SSL_ctrl
(defun ssl-set-tlsext-host-name (ssl name)
  (ssl-ctrl ssl +ssl-ctrl-set-tlsext-hostname+
            +tlsext-nametype-host-name+ name))
```

### Callbacks

Use `with-alien-callable` for callbacks:

```lisp
(sb-alien:with-alien-callable
    ((callback :int ((data :pointer))
       (process-data data)
       0))
  (register-callback lib (sb-alien:alien-sap callback)))
```

## See Also

- [Auto-Binding Patterns](auto-binding.md) - Decision matrix for auto vs manual
- [BIR Format](bir-format.md) - BIR file specification
- [Groveling Workflow](groveling-workflow.md) - Detailed groveling guide

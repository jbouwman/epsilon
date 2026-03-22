# epsilon.foreign.jit User Guide

This guide covers practical usage of the JIT FFI module for calling C functions from Lisp with near-native performance.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Basic FFI Calls](#basic-ffi-calls)
3. [Auto-Discovery with libclang](#auto-discovery-with-libclang)
4. [Callbacks](#callbacks)
5. [Advanced Topics](#advanced-topics)
6. [Troubleshooting](#troubleshooting)

---

## Getting Started

### Loading the Module

```lisp
;; Load the core JIT module
(require :epsilon.foreign.jit)

;; For auto-discovery (optional, requires libclang)
(require :epsilon.foreign.jit.clang)

;; For callbacks
(require :epsilon.foreign.jit.callback)
```

### Checking Platform Support

```lisp
(jit:platform-supported-p)  ; => T on ARM64 and x86-64
(jit:*current-platform*)    ; => :ARM64 or :X86-64
```

### Your First JIT Call

```lisp
;; Get the address of the C abs() function
(defvar *abs-addr* (sb-sys:find-foreign-symbol-address "abs"))

;; Create a JIT caller
(defvar *fast-abs* (jit:make-jit-caller *abs-addr* :int '(:int)))

;; Use it
(funcall *fast-abs* -42)  ; => 42
```

---

## Basic FFI Calls

### Creating JIT Callers

The core function is `make-jit-caller`:

```lisp
(jit:make-jit-caller fn-addr return-type arg-types)
```

- `fn-addr`: Integer address of the C function
- `return-type`: Keyword like `:int`, `:double`, `:pointer`
- `arg-types`: List of argument type keywords

### Integer Functions

```lisp
;; int abs(int n)
(let* ((addr (sb-sys:find-foreign-symbol-address "abs"))
       (abs-fn (jit:make-jit-caller addr :int '(:int))))
  (funcall abs-fn -100))  ; => 100

;; pid_t getpid(void)
(let* ((addr (sb-sys:find-foreign-symbol-address "getpid"))
       (getpid-fn (jit:make-jit-caller addr :int '())))
  (funcall getpid-fn))  ; => your PID
```

### Floating Point Functions

```lisp
;; double sin(double x)
(let* ((addr (sb-sys:find-foreign-symbol-address "sin"))
       (sin-fn (jit:make-jit-caller addr :double '(:double))))
  (funcall sin-fn 1.0d0))  ; => 0.8414709848078965d0

;; double pow(double x, double y)
(let* ((addr (sb-sys:find-foreign-symbol-address "pow"))
       (pow-fn (jit:make-jit-caller addr :double '(:double :double))))
  (funcall pow-fn 2.0d0 10.0d0))  ; => 1024.0d0
```

### Pointer Functions

```lisp
;; void* malloc(size_t size)
(let* ((addr (sb-sys:find-foreign-symbol-address "malloc"))
       (malloc-fn (jit:make-jit-caller addr :pointer '(:ulong))))
  (let ((ptr (funcall malloc-fn 1024)))
    ;; ptr is an integer address
    (format t "Allocated at: ~X~%" ptr)))
```

### Caching Behavior

JIT callers are cached by signature. The first call compiles the stub; subsequent calls reuse it:

```lisp
;; Check cache stats
(jit:stub-cache-stats)
; => (:STUB-COUNT 5 :REGION-SIZE 65536 :REGION-USED 180)

;; Clear cache if needed
(jit:clear-stub-cache)
```

---

## Auto-Discovery with libclang

The `epsilon.foreign.jit.clang` package uses libclang to automatically discover function signatures from C headers.

### Prerequisites

libclang must be available:
- macOS: Comes with Xcode Command Line Tools
- Linux: `apt install libclang-dev` or equivalent

### Using defauto-jit

Define functions with automatic signature discovery:

```lisp
;; Discover and define in one step
(clang:defauto-jit my-sin "sin" "math.h")
(my-sin 1.0d0)  ; => 0.8414709848078965d0

;; With explicit library
(clang:defauto-jit my-strlen "strlen" "string.h" :library "c")
(my-strlen "hello")  ; => 5
```

### One-shot Calls

For single calls without defining a function:

```lisp
(clang:auto-jit "sqrt" "math.h" 2.0d0)  ; => 1.4142135623730951d0
```

### Discovering Signatures

Inspect what libclang finds:

```lisp
(clang:discover-signature "printf"
  "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/stdio.h")
; => (:NAME "printf" :RETURN-TYPE "int" :VARIADIC T
;     :PARAMS ((:NAME "format" :TYPE "const char *")))

(clang:describe-function "sin" "math.h")
; Prints detailed signature info
```

### Include Paths

Configure where to search for headers:

```lisp
;; Add a custom include path
(clang:add-include-path "/opt/custom/include")

;; Check default paths
clang:*default-include-paths*
```

---

## Callbacks

Callbacks allow C code to call back into Lisp.

### Defining Named Callbacks

```lisp
(cb:defcallback my-adder :int ((a :int) (b :int))
  (+ a b))

;; Get the pointer for passing to C
(cb:callback-pointer 'my-adder)  ; => #<SAP ...>
(cb:callback-pointer-int 'my-adder)  ; => integer address
```

### Creating Anonymous Callbacks

```lisp
(let ((callback (cb:make-jit-callback
                  (lambda (x) (* x 2))
                  :int '(:int))))
  ;; Use callback...
  (cb:callback-pointer callback))
```

### Callback with qsort Example

```lisp
;; Define a comparator for qsort
(cb:defcallback int-compare :int ((a :pointer) (b :pointer))
  (let ((va (sb-sys:sap-ref-32 (sb-sys:int-sap a) 0))
        (vb (sb-sys:sap-ref-32 (sb-sys:int-sap b) 0)))
    (- va vb)))

;; Call qsort with our callback
(let* ((qsort-addr (sb-sys:find-foreign-symbol-address "qsort"))
       (qsort-fn (jit:make-jit-caller
                   qsort-addr
                   :void
                   '(:pointer :ulong :ulong :pointer))))
  (funcall qsort-fn
           array-ptr
           array-length
           4  ; sizeof(int)
           (cb:callback-pointer-int 'int-compare)))
```

### Callback Lifecycle

```lisp
;; List all registered callbacks
(cb:list-callbacks)

;; Get a callback by name
(cb:get-callback 'my-adder)

;; Clean up a specific callback
(cb:free-jit-callback (cb:get-callback 'my-adder))

;; Clear all callbacks
(cb:clear-callbacks)
```

---

## Advanced Topics

### Debugging Stubs

Inspect generated machine code:

```lisp
(let* ((addr (sb-sys:find-foreign-symbol-address "abs"))
       (stub (jit:compile-stub addr :int '(:int))))
  (jit:disassemble-stub stub))
```

Output:
```
Stub at 104857600, 36 bytes:
  Platform: ARM64
  Signature: (:INT :INT)
  Code:
FF 43 00 D1 FE 07 00 F9 ...
```

### Memory Management

The JIT allocates executable memory in regions:

```lisp
;; Check region usage
(jit:stub-cache-stats)

;; Allocate custom region (advanced)
(let ((region (jit:make-executable-memory 4096)))
  (unwind-protect
      ;; Use region...
      nil
    (jit:free-executable-memory region)))
```

### Performance Tips

1. **Cache callers**: Create the JIT caller once, reuse many times
2. **Use specialized types**: `:double` is faster than generic `:pointer`
3. **Avoid call-stub**: Use the returned function directly
4. **Batch operations**: Minimize JIT creation in tight loops

```lisp
;; Good: Create once, use many times
(let ((sin-fn (jit:make-jit-caller addr :double '(:double))))
  (dotimes (i 1000000)
    (funcall sin-fn (random 1.0d0))))

;; Bad: Create in loop
(dotimes (i 1000000)
  (let ((sin-fn (jit:make-jit-caller addr :double '(:double))))
    (funcall sin-fn (random 1.0d0))))
```

---

## Troubleshooting

### "JIT compilation not supported"

Check platform:
```lisp
(jit:*current-platform*)  ; Should be :ARM64 or :X86-64
```

### "mmap failed to allocate executable memory"

System may restrict executable memory. On macOS with hardened runtime, ensure the application is properly signed.

### libclang not available

```lisp
(epsilon.foreign.libclang:libclang-available-p)
```

If NIL:
- macOS: Install Xcode Command Line Tools
- Linux: Install libclang-dev

### Wrong results from C function

1. Check type matching:
```lisp
;; Wrong: using :int for size_t
(jit:make-jit-caller addr :int '(:int))  ; size_t is :ulong

;; Correct:
(jit:make-jit-caller addr :ulong '(:ulong))
```

2. Check pointer handling:
```lisp
;; Pointers should use :pointer type
;; Strings need special handling via c-string conversion
```

### Callback crashes

1. Ensure types match exactly
2. Check that callback is still registered (not garbage collected)
3. Use `with-jit-callback` for temporary callbacks

```lisp
(cb:with-jit-callback (ptr (lambda (x) (* x 2)) :int '(:int))
  ;; ptr is valid only within this scope
  (call-c-function-with-callback ptr))
```

---

## Next Steps

- [API Reference](api-reference.md) - Complete API documentation
- [Performance Guide](performance.md) - Benchmark results and optimization
- [Internals](internals.md) - Implementation details

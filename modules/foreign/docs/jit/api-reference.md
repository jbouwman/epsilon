# epsilon.foreign.jit API Reference

Complete API documentation for the JIT FFI module.

---

## Package: epsilon.foreign.jit

Core JIT stub generation and invocation.

### Variables

#### `*current-platform*`

Current CPU architecture. One of `:arm64`, `:x86-64`, or `:unknown`.

```lisp
jit:*current-platform*  ; => :ARM64
```

#### `*stub-cache*`

Hash table caching compiled stubs by signature.

---

### Functions

#### `platform-supported-p`

```lisp
(platform-supported-p) => boolean
```

Check if JIT compilation is supported on this platform.

**Returns:** `T` for ARM64 and x86-64, `NIL` otherwise.

---

#### `make-jit-caller`

```lisp
(make-jit-caller fn-addr return-type arg-types) => function
```

Create a JIT-compiled function caller.

**Parameters:**
- `fn-addr` - Integer address of the foreign function
- `return-type` - Keyword specifying return type (`:int`, `:double`, etc.)
- `arg-types` - List of argument type keywords

**Returns:** A compiled Lisp function that calls the foreign function.

**Example:**
```lisp
(let ((sin-fn (jit:make-jit-caller
                (sb-sys:find-foreign-symbol-address "sin")
                :double '(:double))))
  (funcall sin-fn 1.0d0))
```

---

#### `get-or-create-stub`

```lisp
(get-or-create-stub fn-addr return-type arg-types) => jit-stub
```

Get a cached stub or create a new one for the given signature.

**Parameters:** Same as `make-jit-caller`.

**Returns:** A `jit-stub` structure.

---

#### `compile-stub`

```lisp
(compile-stub fn-addr return-type arg-types) => jit-stub
```

Compile a new call stub (bypasses cache).

**Parameters:** Same as `make-jit-caller`.

**Returns:** A newly compiled `jit-stub` structure.

---

#### `call-stub`

```lisp
(call-stub stub &rest args) => result
```

Call a JIT stub with arguments.

**Parameters:**
- `stub` - A `jit-stub` structure
- `args` - Arguments to pass to the foreign function

**Returns:** The foreign function's return value.

**Note:** Prefer using the function returned by `make-jit-caller` for better performance.

---

#### `clear-stub-cache`

```lisp
(clear-stub-cache) => nil
```

Clear the stub cache and free executable memory.

---

#### `stub-cache-stats`

```lisp
(stub-cache-stats) => plist
```

Return statistics about the stub cache.

**Returns:** Property list with keys:
- `:stub-count` - Number of cached stubs
- `:region-size` - Total executable region size in bytes
- `:region-used` - Used bytes in the region

---

#### `generate-call-stub`

```lisp
(generate-call-stub fn-addr return-type arg-types) => byte-vector
```

Generate machine code for a call stub.

**Returns:** Vector of bytes containing native code.

---

#### `disassemble-stub`

```lisp
(disassemble-stub stub &optional stream) => nil
```

Print a hex dump of the stub's machine code.

**Parameters:**
- `stub` - A `jit-stub` structure
- `stream` - Output stream (default: `*standard-output*`)

---

### Memory Management

#### `make-executable-memory`

```lisp
(make-executable-memory size) => executable-region
```

Allocate SIZE bytes of executable memory.

**Returns:** An `executable-region` structure.

---

#### `free-executable-memory`

```lisp
(free-executable-memory region) => nil
```

Free an executable memory region.

---

### Type Keywords

| Keyword | C Type | Size (bytes) |
|---------|--------|--------------|
| `:void` | void | 0 |
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
| `:size-t` | size_t | 8 |
| `:ssize-t` | ssize_t | 8 |

---

## Package: epsilon.foreign.jit.callback

Callback support for Lisp functions callable from C.

### Macros

#### `defcallback`

```lisp
(defcallback name return-type ((param1 type1) ...) &body body)
```

Define a named callback function.

**Parameters:**
- `name` - Symbol naming the callback
- `return-type` - Type keyword for return value
- `params` - List of (name type) pairs
- `body` - Lisp code for the callback

**Example:**
```lisp
(cb:defcallback my-compare :int ((a :int) (b :int))
  (- a b))
```

---

#### `with-jit-callback`

```lisp
(with-jit-callback (ptr-var fn return-type arg-types) &body body)
```

Create a temporary callback, bind its pointer, and clean up after.

**Parameters:**
- `ptr-var` - Variable to bind the callback pointer to
- `fn` - Lambda or function for the callback
- `return-type` - Return type keyword
- `arg-types` - List of argument type keywords

---

### Functions

#### `make-jit-callback`

```lisp
(make-jit-callback fn return-type arg-types &key name) => jit-callback
```

Create a callback from a Lisp function.

**Parameters:**
- `fn` - Lisp function to wrap
- `return-type` - Type keyword for return value
- `arg-types` - List of argument type keywords
- `name` - Optional name for registry

**Returns:** A `jit-callback` structure.

---

#### `free-jit-callback`

```lisp
(free-jit-callback callback) => nil
```

Free a callback and remove from registry.

---

#### `callback-pointer`

```lisp
(callback-pointer name-or-callback) => system-area-pointer
```

Get the SAP (System Area Pointer) for a callback.

**Parameters:**
- `name-or-callback` - Symbol name or jit-callback structure

---

#### `callback-pointer-int`

```lisp
(callback-pointer-int name-or-callback) => integer
```

Get the callback pointer as an integer address.

---

#### `call-callback`

```lisp
(call-callback callback &rest args) => result
```

Call a callback from Lisp (for testing).

---

### Registry Functions

#### `register-callback`

```lisp
(register-callback callback) => callback
```

Register a callback in the global registry.

---

#### `unregister-callback`

```lisp
(unregister-callback callback-or-id) => boolean
```

Remove a callback from the registry.

---

#### `get-callback`

```lisp
(get-callback name) => jit-callback or nil
```

Look up a callback by name.

---

#### `list-callbacks`

```lisp
(list-callbacks) => list
```

Return a list of all registered callback names.

---

#### `clear-callbacks`

```lisp
(clear-callbacks) => nil
```

Remove all registered callbacks.

---

## Package: epsilon.foreign.jit.clang

libclang integration for automatic signature discovery.

### Variables

#### `*sdk-include-path*`

Path to macOS SDK headers. Default: `/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include`

#### `*default-include-paths*`

List of default include paths for header search.

#### `*signature-cache*`

Hash table caching discovered signatures.

---

### Macros

#### `defauto-jit`

```lisp
(defauto-jit name c-name header &key library)
```

Define a function with automatically discovered signature.

**Parameters:**
- `name` - Lisp symbol for the function
- `c-name` - C function name (string)
- `header` - Header file containing declaration
- `library` - Optional library name

**Example:**
```lisp
(clang:defauto-jit my-sin "sin" "math.h")
```

---

### Functions

#### `auto-jit`

```lisp
(auto-jit c-name header &rest args) => result
```

One-shot JIT call with auto-discovered signature.

**Parameters:**
- `c-name` - C function name (string)
- `header` - Header file containing declaration
- `args` - Arguments to pass

**Example:**
```lisp
(clang:auto-jit "sqrt" "math.h" 2.0d0)  ; => 1.414...
```

---

#### `make-auto-jit-caller`

```lisp
(make-auto-jit-caller c-name header &key library) => function
```

Create a JIT caller with auto-discovered signature.

---

#### `discover-signature`

```lisp
(discover-signature function-name header-path) => plist or nil
```

Discover a function's signature from a header file.

**Returns:** Property list with keys:
- `:name` - Function name
- `:return-type` - Return type string
- `:return-type-kind` - Type kind keyword
- `:params` - List of parameter plists
- `:variadic` - T if function is variadic

---

#### `signature-to-jit-types`

```lisp
(signature-to-jit-types signature) => (values return-type arg-types)
```

Convert a libclang signature to JIT type keywords.

---

#### `add-include-path`

```lisp
(add-include-path path) => list
```

Add a path to the include search paths.

---

#### `find-header`

```lisp
(find-header header-name &optional additional-paths) => string or nil
```

Find a header file in the include paths.

---

#### `clear-signature-cache`

```lisp
(clear-signature-cache) => nil
```

Clear the signature cache.

---

#### `describe-function`

```lisp
(describe-function function-name header) => nil
```

Print detailed information about a C function's signature.

---

#### `list-functions`

```lisp
(list-functions header &key filter) => list
```

List all functions declared in a header.

---

## Structures

### jit-stub

```lisp
(defstruct jit-stub
  address     ; SAP to stub code
  signature   ; (return-type . arg-types)
  size        ; Code size in bytes
  region      ; Parent executable-region
  caller)     ; Compiled specialized caller function
```

### jit-callback

```lisp
(defstruct jit-callback
  id            ; Unique integer ID
  name          ; Optional symbol name
  function      ; Lisp function
  return-type   ; Type keyword
  arg-types     ; List of type keywords
  pointer       ; SAP to callback entry point
  alien-callback) ; Underlying SBCL callback
```

### executable-region

```lisp
(defstruct executable-region
  address   ; SAP to region start
  size      ; Total size in bytes
  used)     ; Used bytes
```

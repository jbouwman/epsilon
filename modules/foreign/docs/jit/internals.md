# epsilon.foreign.jit Internals

This document describes the internal implementation details of the JIT FFI module for developers who want to understand, modify, or extend the code.

## Architecture Overview

```
+------------------+     +------------------+     +------------------+
|  User Code       |     |  JIT Stub Cache  |     |  Executable      |
|                  |---->|                  |---->|  Memory Region   |
|  (make-jit-caller)     |  (hash table)    |     |  (mmap RWX)      |
+------------------+     +------------------+     +------------------+
                                |
                                v
                         +------------------+
                         |  Native Stub     |
                         |  (ARM64/x86-64)  |
                         +------------------+
                                |
                                v
                         +------------------+
                         |  Target C        |
                         |  Function        |
                         +------------------+
```

## Code Organization

```
src/
  jit.lisp              - Core JIT infrastructure
  callback-support.lisp - C-to-Lisp callbacks
  clang-integration.lisp - libclang signature discovery
  array-support.lisp    - Array/buffer passing utilities
  struct-support.lisp   - Struct handling (planned)
  integration.lisp      - High-level integration utilities
```

## Core Components

### 1. Executable Memory Management

The JIT allocates executable memory using `mmap` with `PROT_READ | PROT_WRITE | PROT_EXEC` permissions.

```lisp
;; Memory region structure
(defstruct executable-region
  (address nil :type (or null sb-sys:system-area-pointer))
  (size 0 :type fixnum)
  (used 0 :type fixnum))
```

**Platform-specific handling:**

- **ARM64 (macOS)**: Requires `MAP_JIT` flag for hardened runtime. Uses `sb-vm::jit-memcpy` which internally calls `pthread_jit_write_protect_np` to handle W^X (Write XOR Execute) restrictions.

- **x86-64**: Standard RWX memory, no special handling needed.

### 2. Stub Generation

Stubs are small pieces of machine code that:
1. Load the target function address into a register
2. Jump to that address
3. Return the result

**ARM64 Stub Layout (40 bytes):**

```
Offset  Instruction          Purpose
------  -----------          -------
0x00    SUB SP, SP, #16      Prologue: allocate stack frame
0x04    STR LR, [SP, #8]     Save link register
0x08    MOVZ X16, #imm16     Load address bits 0-15
0x0C    MOVK X16, #imm16, LSL #16   Load address bits 16-31
0x10    MOVK X16, #imm16, LSL #32   Load address bits 32-47
0x14    MOVK X16, #imm16, LSL #48   Load address bits 48-63
0x18    BLR X16              Call target function
0x1C    LDR LR, [SP, #8]     Restore link register
0x20    ADD SP, SP, #16      Epilogue: deallocate stack frame
0x24    RET                  Return to caller
```

**x86-64 Stub Layout (14 bytes):**

```
Offset  Bytes               Instruction      Purpose
------  -----               -----------      -------
0x00    49 BB imm64         MOV R11, imm64   Load 64-bit address
0x0A    41 FF D3            CALL R11         Call target function
0x0D    C3                  RET              Return
```

### 3. Stub Cache

Stubs are cached by signature (return type + argument types) to avoid recompilation:

```lisp
(defvar *stub-cache* (make-hash-table :test 'equal))

;; Key format: (fn-addr return-type . arg-types)
;; Example: (140735820554720 :double :double)
```

### 4. Compiled Caller Generation

`make-jit-caller` creates a specialized Lisp function that:
1. Gets or creates a stub for the signature
2. Compiles a closure that calls the stub with proper type conversion

```lisp
(defun make-jit-caller (fn-addr return-type arg-types)
  (let ((stub (get-or-create-stub fn-addr return-type arg-types)))
    ;; Compile a specialized caller function
    (compile nil
      `(lambda (&rest args)
         (apply #'call-stub ,stub args)))))
```

The actual implementation is more sophisticated, using SBCL's `alien-funcall` for type conversion.

## Register Usage

### ARM64 (Apple Silicon)

| Register | Usage |
|----------|-------|
| X0-X7    | Integer/pointer arguments |
| D0-D7    | Floating-point arguments |
| X16      | Scratch (holds target address) |
| X17      | Scratch (available) |
| LR (X30) | Link register (return address) |
| SP       | Stack pointer |

Arguments beyond 8 integers or 8 floats go on the stack.

### x86-64 (System V ABI)

| Register | Usage |
|----------|-------|
| RDI, RSI, RDX, RCX, R8, R9 | Integer arguments |
| XMM0-XMM7 | Floating-point arguments |
| RAX | Return value (integer) |
| XMM0 | Return value (float) |
| R11 | Scratch (holds target address) |

## Type Conversion

The module maps Lisp values to/from C types:

```lisp
;; Type mapping table
(defparameter *type-alien-map*
  '((:void . sb-alien:void)
    (:char . sb-alien:char)
    (:uchar . sb-alien:unsigned-char)
    (:short . sb-alien:short)
    (:ushort . sb-alien:unsigned-short)
    (:int . sb-alien:int)
    (:uint . sb-alien:unsigned-int)
    (:long . sb-alien:long)
    (:ulong . sb-alien:unsigned-long)
    (:float . sb-alien:float)
    (:double . sb-alien:double)
    (:pointer . sb-alien:unsigned-long)))
```

Note: `:pointer` uses `unsigned-long` to allow passing integer addresses directly without SAP conversion.

## Callback Implementation

Callbacks wrap SBCL's `alien-lambda` with additional infrastructure:

```lisp
(defstruct jit-callback
  (id nil :type (or null integer))
  (name nil :type (or null symbol))
  (function nil :type (or null function))
  (return-type nil :type keyword)
  (arg-types nil :type list)
  (pointer nil :type (or null sb-sys:system-area-pointer))
  (alien-callback nil))  ; Underlying SBCL callback
```

The callback registry prevents callbacks from being garbage collected while in use.

## Array Pinning Implementation

Array pinning uses SBCL's `with-pinned-objects` to prevent GC from moving arrays during FFI calls:

```lisp
(defmacro with-pinned-array ((ptr-var array &key element-type) &body body)
  `(let ((coerced-array (coerce-to-simple-array ,array ,element-type)))
     (sb-sys:with-pinned-objects (coerced-array)
       (let ((,ptr-var (sb-sys:sap-int
                        (sb-sys:vector-sap coerced-array))))
         ,@body))))
```

Key points:
- Arrays must be simple arrays (no displaced arrays)
- Element types must match C expectations
- Pointers are only valid within the `with-pinned-objects` scope

## libclang Integration

The clang integration uses libclang's C API to parse headers and extract signatures:

```
+-------------+     +-------------+     +-------------+
|  Header     |---->|  libclang   |---->|  Signature  |
|  File       |     |  Parsing    |     |  Cache      |
+-------------+     +-------------+     +-------------+
```

Key functions:
- `clang_createIndex` - Create translation unit index
- `clang_parseTranslationUnit` - Parse header
- `clang_visitChildren` - Walk AST to find functions
- `clang_getCursorKind` - Check if cursor is function declaration
- `clang_getCursorType` - Get function type

## Performance Characteristics

### Call Overhead Breakdown

| Component | Approximate Overhead |
|-----------|---------------------|
| Lisp function dispatch | ~2ns |
| Type checking/conversion | ~3-5ns |
| Native stub execution | ~2ns |
| **Total JIT call** | **~8-12ns** |
| **libffi call** | **~200-300ns** |

### Memory Usage

- **Stub code**: 14-40 bytes per signature
- **Stub metadata**: ~80 bytes per stub (Lisp structure)
- **Default region**: 64KB (holds ~1600-4500 stubs)
- **Cache entry**: ~32 bytes per entry

## Thread Safety

Current implementation is **not fully thread-safe**:

- Stub cache uses a standard hash table (not lock-free)
- Concurrent stub compilation may cause issues
- Callbacks are registered globally

For multi-threaded use:
1. Pre-create all stubs before spawning threads
2. Use a mutex around `make-jit-caller` if creating at runtime
3. Clear cache only when no threads are using JIT

## Debugging

### Disassembly

```lisp
(jit:disassemble-stub stub)
;; Output:
;; Stub at 104857600, 40 bytes:
;;   Platform: ARM64
;;   Signature: (:DOUBLE :DOUBLE)
;;   Code:
;;   FF 43 00 D1 FE 07 00 F9 ...
```

### Cache Inspection

```lisp
(jit:stub-cache-stats)
;; => (:STUB-COUNT 5 :REGION-SIZE 65536 :REGION-USED 200)

;; Internal: examine cache contents
epsilon.foreign.jit::*stub-cache*
```

### Memory Inspection

```lisp
;; Read bytes from executable region
(let ((region (jit::executable-region-address stub)))
  (dotimes (i 40)
    (format t "~2,'0X " (sb-sys:sap-ref-8 region i))))
```

## Extending the Module

### Adding New Type Support

1. Add to `*type-alien-map*` in jit.lisp
2. Add to `*c-to-lisp-element-types*` and `*element-sizes*` in array-support.lisp
3. Update `read-c-array-element` and `write-c-array-element` for array access

### Adding New Platform

1. Add platform detection in `*current-platform*`
2. Implement `generate-<platform>-stub` function
3. Update `generate-call-stub` to dispatch
4. Handle platform-specific memory protection

### Adding Struct Support

See struct-support.lisp for the planned implementation. Key considerations:
- Struct layout must match C ABI
- By-value returns require stack allocation
- Nested structs increase complexity

## Known Limitations

1. **Maximum 8 arguments per type**: ARM64 ABI limits register passing
2. **No variadic functions**: Requires special ABI handling
3. **No by-value struct returns**: Currently pointer-only
4. **No automatic memory management**: User must free callbacks
5. **No Windows support**: Only Unix-like systems

## Future Directions

1. **Variadic support**: Generate specialized stubs for common cases
2. **Struct by-value**: Use stack allocation with proper alignment
3. **Thread-safe cache**: Use lock-free hash table or per-thread caches
4. **JIT hot-patching**: Update stubs without recompilation
5. **Profile-guided optimization**: Track hot paths for inlining

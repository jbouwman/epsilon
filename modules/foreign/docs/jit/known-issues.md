# Known Issues: JIT FFI Module

## Memory Fault in deflibrary-exports-functions Test (FIXED)

### Summary

~~The `deflibrary-exports-functions` test in `epsilon.foreign.jit.library.test` intermittently fails with an unhandled memory fault when run as part of the consolidated `epsilon.foreign` module test suite.~~

**STATUS: FIXED** - This issue has been resolved by restoring SBCL's signal handlers after libclang calls.

### Root Cause (Identified)

The root cause was identified and fixed. LLVM/libclang installs its own signal handlers (SIGSEGV, SIGBUS, SIGFPE) when any libclang function is called. These handlers override SBCL's signal handlers which are critical for:
- GC write barriers (SIGSEGV)
- Memory protection (SIGBUS)
- Floating-point exceptions (SIGFPE)

When GC runs after a libclang call, SBCL's write barrier mechanism fails because the signal goes to LLVM's handler instead of SBCL's, causing memory corruption in the immobile space allocator.

### Fix Applied

The vendored SBCL fork (jbouwman/sbcl PR #22) now preserves signal handlers
established by foreign libraries at load time. LLVM's handlers chain correctly
to SBCL's handlers, so no manual save/restore is needed. The
`with-sbcl-signal-handlers` macro in `epsilon.foreign.libclang` is now a no-op.

### Original Error Details (Historical)

```
ERROR: deflibrary-exports-functions (epsilon.foreign.jit.library.test)
----------------------------------------------------------------------
Unhandled memory fault at #x3E90016D5EB.

Traceback:
0: ("bogus stack frame")
1: (SB-VM::ALLOC-IMMOBILE-FIXEDOBJ 0 395138321220)
2: (SB-VM::%ALLOC-IMMOBILE-SYMBOL "TEST-LIB-EXPORTS")
3: (SB-KERNEL:%MAKE-SYMBOL 1 "TEST-LIB-EXPORTS")
```

### Verification

After the fix, all 308 tests in `epsilon.foreign` pass consistently:
```bash
./epsilon --test epsilon.foreign  # 308 tests, 0 failures, 0 errors
```

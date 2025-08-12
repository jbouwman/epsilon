# Epsilon libffi C Extension

This directory contains the C extension that provides callback functionality for Epsilon's FFI system using libffi.

## Building

```bash
make
```

This creates `libepsilon-libffi.so` which is loaded automatically by the Epsilon foreign module when available.

## Requirements

- libffi development headers (`libffi-dev` on Ubuntu, `libffi-devel` on Fedora)
- GCC or compatible C compiler
- Standard C library headers

## Architecture

The extension provides a bridge between Lisp callback functions and C-callable function pointers:

1. **Callback Creation**: `epsilon_create_callback()` uses libffi's closure API to create C-callable function pointers
2. **Type System**: Maps Epsilon type constants to libffi types 
3. **Registry**: Thread-safe callback management with cleanup
4. **Error Handling**: Error reporting and graceful fallback

## API Functions

- `epsilon_create_callback()` - Create callback from Lisp function
- `epsilon_get_callback_pointer()` - Get C function pointer
- `epsilon_destroy_callback()` - Clean up callback resources
- `epsilon_cleanup_all_callbacks()` - Clean up all callbacks
- `epsilon_get_callback_count()` - Get active callback count
- `epsilon_libffi_test()` - Basic functionality test

## Integration

The extension is automatically detected and used by `epsilon.foreign` when available. If libffi is unavailable, the system gracefully falls back to infrastructure-only mode.

No changes to existing Epsilon code are required - callbacks use C-callable function pointers when the extension is present.
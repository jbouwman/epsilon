# Vendored SBCL Binaries

This directory contains pre-built SBCL binaries from https://github.com/jbouwman/sbcl,
a fork with extended C-call convention support required for Epsilon's FFI.

## Features

The vendored SBCL includes:
- **Struct-by-value returns**: Functions can return C structs by value
- **Struct-by-value callback parameters**: Callbacks can accept C structs by value

These features are required for libclang bindings (`clang_visitChildren`, etc.).

## Building

Use the build script to rebuild the vendored SBCL:

```bash
# Rebuild for current platform
./build.sh

# Rebuild from a specific branch
./build.sh --branch feature-branch

# Clean build (removes cached source)
./build.sh --clean
```

The script auto-detects the platform:
- **Linux**: Builds in a Debian container for portable binaries (standard interpreter paths)
- **macOS**: Builds natively on the host

## Directory Structure

```
sbcl/
  build.sh                # Build script (auto-detects platform)
  README.md               # This file
  linux-x86_64/           # Linux x86_64 binaries
    sbcl                  # SBCL executable
    sbcl.core             # SBCL core image
    VERSION               # Build info and git commit hash
    contrib/              # SBCL contributed modules
  darwin-arm64/           # macOS Apple Silicon binaries
    sbcl
    sbcl.core
    VERSION
    contrib/
```

## Runtime

The epsilon launcher sets `SBCL_HOME` to the appropriate platform directory so SBCL
can locate contribs at runtime when `(require :sb-posix)` is called.

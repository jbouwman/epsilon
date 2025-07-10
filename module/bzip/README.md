# BZip2 Module for Epsilon

This module provides BZip2 decompression support for Epsilon, extracted from the core codec module for better modularity.

## Status

**Current Implementation**: Partial
- ✅ Basic structure and API defined
- ✅ State management structures
- ⚠️  Decompression state machine (placeholder)
- ❌ Compression support (not implemented)

## API

### Main Functions

```lisp
;; Create a decompressing stream
(epsilon.lib.bzip:make-decompressing-stream :bzip2 input-stream)

;; Direct decompression (low-level)
(epsilon.lib.bzip:decompress output-array state input-array)
```

### Conditions

- `invalid-bzip2-data` - Signaled when invalid bzip2 data is encountered
- `bzip2-randomized-blocks-unimplemented` - Signaled for unsupported randomized blocks

## Usage

```lisp
(defpackage :my-app
  (:use :cl)
  (:local-nicknames
   (:bzip :epsilon.lib.bzip)))

(in-package :my-app)

;; Decompress a bzip2 file
(with-open-file (input "data.bz2" :element-type '(unsigned-byte 8))
  (let ((stream (bzip:make-decompressing-stream :bzip2 input)))
    ;; Read decompressed data from stream
    ...))
```

## Implementation Notes

The BZip2 decompression uses a state machine approach with the following main phases:

1. **Header parsing** - Validates BZ2 file signature and block size
2. **Block processing** - Processes compressed blocks
3. **Burrows-Wheeler Transform reversal** - Undoes the BWT
4. **Run-length decoding** - Final decompression step
5. **CRC validation** - Verifies data integrity

## TODO

1. Complete the state machine implementation (`%bzip2-state-machine`)
2. Add proper error handling and recovery
3. Implement streaming support
4. Add compression support
5. Optimize performance
6. Add comprehensive test suite with real bzip2 files

## Dependencies

- `epsilon.core` - For type definitions and checksum support

## Migration from Core

This module was extracted from `epsilon.lib.codec` to:
- Reduce core module size
- Allow independent versioning
- Enable optional loading (not all users need bzip2)
- Simplify maintenance

To migrate existing code:
1. Add dependency on `epsilon.bzip` module
2. Change package references from `epsilon.lib.codec` to `epsilon.lib.bzip`
3. The API remains the same
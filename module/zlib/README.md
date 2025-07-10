# ZLIB Module for Epsilon

This module provides ZLIB compression/decompression support for Epsilon, implementing RFC 1950 ZLIB format with header processing, Adler-32 validation, and proper integration with DEFLATE decompression. Extracted from the core codec module for modularity.

## Features

- **Complete ZLIB Format Support**: Full RFC 1950 ZLIB format implementation
- **Header Processing**: Complete ZLIB header parsing and validation
- **Adler-32 Validation**: Data integrity verification using Adler-32 checksums
- **DEFLATE Integration**: Seamless integration with the inflate module
- **Dictionary Support**: Support for preset dictionaries (FDICT)
- **Stream Interface**: Support for streaming compression/decompression
- **Error Handling**: Comprehensive error conditions for invalid data

## API

### Main Functions

```lisp
;; Decompress ZLIB data
(epsilon.lib.zlib:zlib-decode input-array output-array)

;; Compress data to ZLIB format (TODO: not yet implemented)
(epsilon.lib.zlib:zlib-encode input-array output-array)

;; Create decompressing stream
(epsilon.lib.zlib:make-zlib-decompressing-stream input-stream)

;; Create compressing stream (TODO: not yet implemented)
(epsilon.lib.zlib:make-zlib-compressing-stream output-stream)
```

### Header Processing

```lisp
;; Create ZLIB header
(make-instance 'epsilon.lib.zlib:zlib-header 
               :cmf #x78   ; compression method 8, window size 32K
               :flags #x9C) ; standard flags

;; Parse header bytes
(epsilon.lib.zlib:zlib-compression-method cmf-byte)
(epsilon.lib.zlib:zlib-compression-info cmf-byte)
(epsilon.lib.zlib:zlib-flag-fcheck flag-byte)
(epsilon.lib.zlib:zlib-flag-flevel flag-byte)
```

### Error Conditions

- `zlib-error` - Base error class
- `invalid-zlib-header-error` - Invalid ZLIB header format

## Usage

```lisp
(defpackage :my-app
  (:use :cl)
  (:local-nicknames
   (:zlib :epsilon.lib.zlib)))

(in-package :my-app)

;; Decompress ZLIB data
(let ((input (make-array 1024 :element-type '(unsigned-byte 8)))
      (output (make-array 4096 :element-type '(unsigned-byte 8))))
  (multiple-value-bind (bytes-read bytes-written)
      (zlib:zlib-decode input output)
    (format t "Read ~D bytes, wrote ~D bytes~%" bytes-read bytes-written)))

;; Stream-based decompression
(with-open-file (input "data.zlib" :element-type '(unsigned-byte 8))
  (let ((stream (zlib:make-zlib-decompressing-stream input)))
    (loop for byte = (read-byte stream nil nil)
          while byte
          do (write-byte byte *standard-output*))))
```

## Implementation Status

- ✅ ZLIB header parsing (CMF, FLG bytes)
- ✅ Header checksum validation (CMF*256 + FLG) mod 31 = 0
- ✅ Dictionary support (FDICT handling)
- ✅ DEFLATE decompression integration
- ✅ Adler-32 checksum validation
- ✅ Stream interface for decompression
- ✅ Error handling and validation
- ❌ ZLIB compression (not yet implemented)
- ❌ Stream interface for compression (not yet implemented)

## ZLIB Format Structure

The ZLIB format consists of:

1. **Header**: 2 bytes
   - CMF (Compression Method and flags):
     - Bits 0-3: Compression method (8 = DEFLATE)
     - Bits 4-7: Compression info (window size)
   - FLG (Flags):
     - Bits 0-4: FCHECK (header checksum)
     - Bit 5: FDICT (preset dictionary present)
     - Bits 6-7: FLEVEL (compression level)

2. **Optional Dictionary**: 4 bytes (if FDICT is set)
   - Adler-32 checksum of dictionary

3. **Compressed Data**: DEFLATE-compressed blocks

4. **Trailer**: 4 bytes
   - Adler-32 checksum of uncompressed data

## Header Validation

The ZLIB header is validated using the formula:
```
(CMF * 256 + FLG) mod 31 = 0
```

This ensures the header is valid and not corrupted.

## Constants

### Compression Methods
- `+zlib-compression-method+` (8) - DEFLATE compression

### Flags
- `+zlib-flag-fdict+` (5) - Dictionary flag bit position

### Compression Levels
- `+zlib-flevel-fastest+` (0) - Fastest compression
- `+zlib-flevel-fast+` (1) - Fast compression
- `+zlib-flevel-default+` (2) - Default compression
- `+zlib-flevel-maximum+` (3) - Maximum compression

### Magic Number
- `+zlib-magic+` (#x789C) - Standard ZLIB header

## Dependencies

- `epsilon.core` - Core types and utilities
- `epsilon.inflate` - DEFLATE decompression
- `epsilon.lib.checksum.adler-32` - Adler-32 checksum support

## Migration from Core

This module was extracted from `epsilon.lib.codec` to:
- Reduce core module size
- Allow independent versioning and maintenance
- Enable optional loading (not all users need ZLIB support)
- Provide cleaner module boundaries

### Backward Compatibility

The core codec module provides a compatibility shim that:
- Automatically loads the ZLIB module when needed
- Provides the same API as before
- Maintains all existing function signatures
- Ensures zero-impact migration for existing code

## Testing

Run tests with:
```lisp
(epsilon.lib.zlib.tests:run-zlib-tests)
```

## Performance

The ZLIB implementation is optimized for:
- **Header Parsing**: Efficient 2-byte header processing
- **Memory Usage**: Minimal memory allocation during decompression
- **Integration**: Seamless integration with inflate module
- **Checksum**: Fast Adler-32 validation

## Standards Compliance

- **RFC 1950**: ZLIB compressed data format specification
- **RFC 1951**: DEFLATE compressed data format (via inflate module)
- **Adler-32**: Standard Adler-32 checksum algorithm
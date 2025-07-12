# GZIP Module for Epsilon

This module provides GZIP compression/decompression support for Epsilon, implementing RFC 1952 GZIP format with header processing, CRC-32 validation, and proper integration with DEFLATE decompression. Extracted from the core codec module for modularity.

## Features

- **Complete GZIP Format Support**: Full RFC 1952 GZIP format implementation
- **Header Processing**: Complete GZIP header parsing and validation
- **CRC-32 Validation**: Data integrity verification using CRC-32 checksums
- **DEFLATE Integration**: Direct integration with the inflate module
- **Stream Interface**: Support for streaming compression/decompression
- **Error Handling**: Complete error detection for invalid data

## API

### Main Functions

```lisp
;; Decompress GZIP data
(epsilon.lib.gzip:gzip-decode input-array output-array)

;; Compress data to GZIP format (TODO: not yet implemented)
(epsilon.lib.gzip:gzip-encode input-array output-array)

;; Create decompressing stream
(epsilon.lib.gzip:make-gzip-decompressing-stream input-stream)

;; Create compressing stream (TODO: not yet implemented)
(epsilon.lib.gzip:make-gzip-compressing-stream output-stream)
```

### Header Processing

```lisp
;; Create GZIP header
(make-instance 'epsilon.lib.gzip:gzip-header 
               :compression-method epsilon.lib.gzip:+gzip-deflate-method+
               :flags 0)

;; Access header fields
(epsilon.lib.gzip:flags header)
(epsilon.lib.gzip:filename header)
(epsilon.lib.gzip:mtime header)
(epsilon.lib.gzip:comment header)
```

### Error Conditions

- `gzip-error` - Base error class
- `invalid-gzip-header-error` - Invalid GZIP header format

## Usage

```lisp
(defpackage :my-app
  (:use :cl)
  (:local-nicknames
   (:gzip :epsilon.lib.gzip)))

(in-package :my-app)

;; Decompress GZIP file
(with-open-file (input "file.gz" :element-type '(unsigned-byte 8))
  (with-open-file (output "file.txt" :direction :output 
                                     :element-type '(unsigned-byte 8))
    (let ((stream (gzip:make-gzip-decompressing-stream input)))
      (loop for byte = (read-byte stream nil nil)
            while byte
            do (write-byte byte output)))))

;; Direct decompression
(let ((input (make-array 1024 :element-type '(unsigned-byte 8)))
      (output (make-array 4096 :element-type '(unsigned-byte 8))))
  (multiple-value-bind (bytes-read bytes-written)
      (gzip:gzip-decode input output)
    (format t "Read ~D bytes, wrote ~D bytes~%" bytes-read bytes-written)))
```

## Implementation Status

- ✅ GZIP header parsing (ID1, ID2, CM, FLG, MTIME, XFL, OS)
- ✅ Optional header fields (filename, comment, extra data)
- ✅ Header CRC16 validation
- ✅ DEFLATE decompression integration
- ✅ CRC-32 data validation
- ✅ ISIZE validation
- ✅ Stream interface for decompression
- ✅ Error handling and validation
- ❌ GZIP compression (not yet implemented)
- ❌ Stream interface for compression (not yet implemented)

## GZIP Format Structure

The GZIP format consists of:

1. **Header**: 10 bytes minimum
   - ID1, ID2: Magic number (0x1F, 0x8B)
   - CM: Compression method (8 = DEFLATE)
   - FLG: Flags byte
   - MTIME: Modification time (4 bytes)
   - XFL: Extra flags
   - OS: Operating system

2. **Optional Fields** (if flags indicate presence):
   - Extra field (XLEN + data)
   - Filename (zero-terminated)
   - Comment (zero-terminated)
   - Header CRC16

3. **Compressed Data**: DEFLATE-compressed blocks

4. **Trailer**: 8 bytes
   - CRC32: Data checksum (4 bytes)
   - ISIZE: Uncompressed size (4 bytes)

## Dependencies

- `epsilon.core` - Core types and utilities
- `epsilon.inflate` - DEFLATE decompression
- `epsilon.lib.checksum.crc-32` - CRC-32 checksum support

## Migration from Core

This module was extracted from `epsilon.lib.codec` to:
- Reduce core module size
- Allow independent versioning and maintenance
- Enable optional loading (not all users need GZIP support)
- Provide cleaner module boundaries

### Backward Compatibility

The core codec module provides a compatibility shim that:
- Automatically loads the GZIP module when needed
- Provides the same API as before
- Maintains all existing function signatures
- Ensures zero-impact migration for existing code

## Testing

Run tests with:
```lisp
(epsilon.lib.gzip.tests:run-gzip-tests)
```

## Performance

The GZIP implementation is optimized for:
- **Header Parsing**: Efficient byte-by-byte header processing
- **Memory Usage**: Minimal memory allocation during decompression
- **Integration**: Direct integration with inflate module
- **Error Detection**: Fast validation and error reporting

## Standards Compliance

- **RFC 1952**: GZIP file format specification
- **RFC 1951**: DEFLATE compressed data format (via inflate module)
- **CRC-32**: Standard CRC-32 checksum algorithm
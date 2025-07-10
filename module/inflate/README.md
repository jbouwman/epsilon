# Inflate Module for Epsilon

This module provides DEFLATE/Inflate decompression support for Epsilon, implementing RFC 1951 DEFLATE decompression with huffman decoding and sliding window support. Extracted from the core codec module for modularity.

## Features

- **Complete DEFLATE Implementation**: Full RFC 1951 DEFLATE decompression
- **Huffman Decoding**: Both fixed and dynamic huffman tables
- **Sliding Window**: 32KB sliding window for LZ77 back-references
- **Block Types**: Support for uncompressed, fixed huffman, and dynamic huffman blocks
- **Stream Interface**: Gray stream support for streaming decompression
- **Error Handling**: Comprehensive error conditions for invalid data

## API

### Main Functions

```lisp
;; Create a decompression state
(epsilon.lib.inflate:make-inflate-state :deflate)   ; Raw DEFLATE
(epsilon.lib.inflate:make-inflate-state :gzip)      ; GZIP format
(epsilon.lib.inflate:make-inflate-state :zlib)      ; ZLIB format

;; Decompress data
(epsilon.lib.inflate:inflate-decompress state input-array output-array)

;; Create a decompressing stream
(epsilon.lib.inflate:make-decompressing-stream :deflate input-stream)
```

### Huffman Decoding

```lisp
;; Construct huffman decode table
(epsilon.lib.inflate:construct-huffman-decode-table code-lengths)

;; Access pre-built fixed tables
epsilon.lib.inflate:*fixed-literal/length-table*
epsilon.lib.inflate:*fixed-distance-table*
```

### Error Conditions

The module defines several error conditions for handling invalid data:

- `deflate-error` - Base error class
- `invalid-deflate-block` - Invalid block type
- `invalid-huffman-code` - Invalid huffman code
- `reserved-block-type-error` - Reserved block type encountered
- `unassigned-huffman-code-error` - Unassigned huffman code
- `invalid-checksum-error` - Checksum mismatch
- `invalid-gzip-header-error` - Invalid GZIP header
- `invalid-zlib-header-error` - Invalid ZLIB header

## Usage

```lisp
(defpackage :my-app
  (:use :cl)
  (:local-nicknames
   (:inflate :epsilon.lib.inflate)))

(in-package :my-app)

;; Basic decompression
(let ((state (inflate:make-inflate-state :deflate))
      (input (make-array 1024 :element-type '(unsigned-byte 8)))
      (output (make-array 4096 :element-type '(unsigned-byte 8))))
  (multiple-value-bind (bytes-read bytes-written)
      (inflate:inflate-decompress state input output)
    (format t "Read ~D bytes, wrote ~D bytes~%" bytes-read bytes-written)))

;; Stream-based decompression
(with-open-file (input "compressed.dat" :element-type '(unsigned-byte 8))
  (let ((stream (inflate:make-decompressing-stream :deflate input)))
    (loop for byte = (read-byte stream nil nil)
          while byte
          do (write-byte byte *standard-output*))))
```

## Implementation Status

- ✅ Core DEFLATE decompression state machine
- ✅ Huffman decoding (fixed and dynamic tables)
- ✅ Sliding window management
- ✅ All block types (uncompressed, fixed, dynamic)
- ✅ Error handling and validation
- ✅ Stream interface
- ⚠️ GZIP header processing (partial - stubs for full implementation)
- ⚠️ ZLIB header processing (partial - stubs for full implementation)
- ❌ Compression support (not implemented)

## Architecture

The inflate module implements a state machine approach to DEFLATE decompression:

1. **Block Type Processing**: Determines block type (uncompressed, fixed huffman, dynamic huffman)
2. **Huffman Decoding**: Decodes huffman-encoded symbols
3. **LZ77 Processing**: Handles length/distance pairs for back-references
4. **Sliding Window**: Maintains 32KB window for back-references
5. **Output Management**: Manages output buffer and checksum computation

### Key Components

- **inflate-state**: Main decompression state structure
- **huffman-decode-table**: Huffman decoding tables
- **sliding-window**: 32KB sliding window for LZ77
- **State Machine**: Complete decompression state machine with proper error handling

## Dependencies

- `epsilon.core` - Core types and utilities
- `epsilon.lib.checksum.adler-32` - Adler-32 checksum (for ZLIB)
- `epsilon.lib.checksum.crc-32` - CRC-32 checksum (for GZIP)

## Migration from Core

This module was extracted from `epsilon.lib.codec` to:
- Reduce core module size (removed ~1,200 lines)
- Allow independent versioning and maintenance
- Enable optional loading (not all users need inflate support)
- Provide cleaner module boundaries

### Backward Compatibility

The core codec module provides a compatibility shim (`codec-inflate-shim.lisp`) that:
- Automatically loads the inflate module when needed
- Provides the same API as before
- Maintains all existing function signatures
- Ensures zero-impact migration for existing code

## Testing

The module includes comprehensive tests covering:
- Basic functionality and state creation
- Huffman table construction
- Error condition handling
- Stream interface
- Backward compatibility
- Helper functions

Run tests with:
```lisp
(epsilon.lib.inflate.tests:run-inflate-tests)
```

## Performance

The inflate implementation is optimized for:
- **Memory Efficiency**: Minimal memory allocation during decompression
- **Speed**: Optimized huffman decoding and sliding window operations
- **Streaming**: Efficient buffer management for large files
- **Error Recovery**: Fast error detection and reporting

## Standards Compliance

- **RFC 1951**: DEFLATE Compressed Data Format Specification
- **RFC 1952**: GZIP file format specification (partial)
- **RFC 1950**: ZLIB Compressed Data Format Specification (partial)
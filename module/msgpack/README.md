# Epsilon MessagePack Module

A complete MessagePack binary serialization format implementation for Common Lisp.

## Features

- Full MessagePack specification support
- Efficient binary encoding/decoding
- Support for all MessagePack types:
  - Nil, boolean, integers, floats
  - Raw bytes (bin format)
  - Strings (str format)
  - Arrays
  - Maps
  - Extension types
- Streaming API support
- Optimized binary structure implementation

## Usage

```lisp
;; Load the module
(epsilon.lib.module:load-module "epsilon.msgpack")

;; Encode data to MessagePack
(msgpack:encode '(("name" . "John") ("age" . 30)))

;; Decode MessagePack data
(msgpack:decode msgpack-bytes)

;; Streaming encode
(with-open-file (stream "data.msgpack" :direction :output :element-type '(unsigned-byte 8))
  (msgpack:encode-stream data stream))

;; Streaming decode  
(with-open-file (stream "data.msgpack" :element-type '(unsigned-byte 8))
  (msgpack:decode-stream stream))
```

## Binary Structure Module

The module includes an optimized binary structure implementation (`msgpack-binary`) that provides:
- Fast binary data structures
- Efficient serialization
- Memory-efficient storage

## Performance

The implementation includes optimizations for:
- Direct binary manipulation
- Minimal allocation during decoding
- Efficient type dispatch
- Optimized integer encoding

## Dependencies

- epsilon.core
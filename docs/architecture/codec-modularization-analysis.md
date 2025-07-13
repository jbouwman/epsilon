# Codec Module Modularization Analysis

## Current State

The `epsilon.lib.codec` file is **3,335 lines** and contains several distinct compression/decompression formats that could be separated into independent modules.

## Identified Module Candidates

### 1. **Inflate/Deflate Module** (High Priority)
**Size**: ~1,000-1,500 lines
**Components**:
- DEFLATE/Inflate decompression
- Huffman decoding tables
- Fixed and dynamic block processing
- Window management
- Core decompression state machine

**Files to create**:
- `module/inflate/src/lib/inflate.lisp` - Core inflate implementation
- `module/inflate/src/lib/deflate.lisp` - Deflate compression (if implemented)

### 2. **GZip Module** (Medium Priority)
**Size**: ~300-500 lines
**Components**:
- GZip header parsing
- GZip-specific constants and flags
- CRC-32 validation
- GZip codec implementation

**Dependencies**: Would depend on inflate module for actual decompression

### 3. **ZLib Module** (Medium Priority)
**Size**: ~200-300 lines
**Components**:
- ZLib header parsing
- Adler-32 checksum validation
- ZLib codec implementation

**Dependencies**: Would depend on inflate module for actual decompression

### 4. **Bitstream Module** (Low Priority)
**Size**: ~200-400 lines
**Components**:
- Generic bitstream operations
- Bit reading/writing utilities
- Buffer management

**Benefits**: Could be reused by other binary processing modules

### 5. **Codec Protocol Module** (Low Priority)
**Size**: ~100-200 lines
**Components**:
- Generic encoder/decoder interfaces
- Common codec protocols
- Stream integration

## Proposed Module Structure

```
module/
├── inflate/           # DEFLATE/Inflate
│   ├── package.edn
│   ├── src/lib/
│   │   ├── inflate.lisp
│   │   └── huffman.lisp
│   └── tests/
├── gzip/              # GZip format
│   ├── package.edn
│   ├── src/lib/
│   │   └── gzip.lisp
│   └── tests/
├── zlib/              # ZLib format
│   ├── package.edn
│   ├── src/lib/
│   │   └── zlib.lisp
│   └── tests/
├── bzip/              # BZip2 format (already created)
└── core/
    └── src/lib/
        ├── codec.lisp      # Reduced to ~500 lines
        └── codec-shims.lisp # Compatibility layer
```

## Benefits of Modularization

### Size Reduction
- **Current**: `codec.lisp` = 3,335 lines
- **After**: `codec.lisp` = ~500 lines (85% reduction)
- **Modules**: 4-5 focused modules of 200-1,500 lines each

### Dependency Management
- **Optional Loading**: Applications only load needed compression formats
- **Independent Versioning**: Each format can evolve independently
- **Cleaner Dependencies**: Reduces coupling between formats

### Development Benefits
- **Maintainability**: Easier to understand and modify individual formats
- **Testing**: Format-specific test suites
- **Documentation**: Better focused documentation per format

## Implementation Priority

### Phase 1: High Impact (Recommended)
1. **Inflate Module** - Largest, most complex, widely used
2. **Codec Cleanup** - Remove inflate code from core

### Phase 2: Format-Specific
3. **GZip Module** - Popular format
4. **ZLib Module** - Common in libraries

### Phase 3: Infrastructure
5. **Bitstream Module** - Generic utilities
6. **Codec Protocol** - Generic interfaces

## Compatibility Strategy

1. **Shim Layer**: Create compatibility shims like we did for BZip2
2. **Gradual Migration**: Existing code continues to work
3. **Feature Flag**: `*modular-codecs*` to enable new behavior
4. **Documentation**: Clear migration guide

## File Size Breakdown (Estimated)

| Component | Current Lines | After Extraction |
|-----------|---------------|------------------|
| Core codec.lisp | 3,335 | ~500 |
| Inflate module | - | ~1,200 |
| GZip module | - | ~400 |
| ZLib module | - | ~300 |
| BZip module | - | ~800 |
| **Total** | **3,335** | **3,200** |

The slight reduction comes from eliminating duplication and improving organization.

## Recommendation

**Start with the Inflate module extraction** as it:
- Provides the biggest size reduction for codec.lisp
- Is used by both GZip and ZLib (natural dependency)
- Has well-defined boundaries
- Is the most complex part that would benefit from isolation
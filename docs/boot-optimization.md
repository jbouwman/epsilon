# Epsilon Boot Optimization

## Overview

The Epsilon boot system has been optimized to provide the fastest possible startup time by implementing multiple boot strategies:

1. **EPK Boot** (fastest) - Load from pre-built EPK package with combined FASL
2. **Cache Boot** (fast) - Load from local boot cache with combined FASL
3. **Build & Cache** (medium) - Compile sources and create cache for next boot
4. **Traditional Boot** (slowest) - Sequential compilation and loading

## Boot Performance

Typical boot times on modern hardware:

| Boot Method | Time | Description |
|------------|------|-------------|
| EPK Boot | ~0.1s | Pre-built package with combined FASL |
| Cache Boot | ~0.2s | Local cache with combined FASL |
| Build & Cache | ~2-3s | First run, creates cache |
| Traditional | ~3-4s | Sequential file compilation |

## Usage

### Quick Start

Use the optimized boot script:

```bash
# Normal boot (uses fastest available method)
sbcl --script scripts/epsilon-optimized.lisp

# Force rebuild cache
sbcl --script scripts/epsilon-optimized.lisp --rebuild

# Verbose output with timing
sbcl --script scripts/epsilon-optimized.lisp --verbose
```

### Integration with run.sh

Update your `run.sh` to use optimized boot:

```bash
#!/bin/bash
exec sbcl --script scripts/epsilon-optimized.lisp \
          --eval "(epsilon.tool.dev:main)" \
          --end-toplevel-options "$@"
```

## Boot Cache

The boot cache stores combined FASL files for rapid loading:

- **Location**: `~/.epsilon/boot-cache/`
- **Files**:
  - `epsilon.core.boot.fasl` - Combined FASL with all core modules
  - `epsilon.core.manifest` - Cache metadata and validation info

### Cache Validation

The cache is automatically invalidated when:
- Source files are modified (detected via content hash)
- SBCL version changes
- Epsilon version changes

### Manual Cache Management

```lisp
;; Clear cache for specific module
(epsilon.tool.fast-boot:clear-boot-cache "epsilon.core")

;; Clear all caches
(epsilon.tool.fast-boot:clear-boot-cache)

;; Force rebuild
(epsilon.tool.optimized-boot:rebuild-cache)
```

## EPK Integration

When an EPK (Epsilon Package) file is available, it provides the fastest boot:

### EPK Search Paths

1. `~/.epsilon/packages/`
2. `./target/packages/`
3. `./packages/`

### EPK File Naming

```
{module}-{version}-{platform}-{arch}.epk
```

Examples:
- `epsilon.core-1.0.0-darwin-arm64.epk`
- `epsilon.core-1.0.0-linux-x86_64.epk`

## Combined FASL Format

The boot system creates a single combined FASL file that includes:

1. All module source files in dependency order
2. Compiled with optimization settings
3. Concatenated into a single loadable file

Benefits:
- Single file I/O operation
- Reduced overhead vs. loading many files
- Better OS file caching
- Faster startup for large codebases

## Benchmarking

Run the boot benchmark to see performance on your system:

```bash
./scripts/benchmark-boot.sh
```

This will test:
1. Traditional boot (baseline)
2. Cached boot performance
3. EPK boot simulation
4. Detailed timing breakdown

## Implementation Details

### Boot Priority

The system checks boot methods in order of speed:

```lisp
(defun boot ()
  (cond
    ;; 1. Check for EPK file
    ((find-epk-file "epsilon.core")
     (load-from-epk))
    
    ;; 2. Check for valid cache
    ((cache-valid-p)
     (load-from-cache))
    
    ;; 3. Build and cache
    (t
     (bootstrap-and-cache))))
```

### Source Hash Calculation

The cache validation uses a content-based hash of:
- All source file timestamps
- File sizes
- Directory structure

This ensures cache invalidation on any source change.

### Error Recovery

If fast boot fails, the system automatically falls back to traditional boot:

```lisp
(handler-case
    (fast-boot)
  (error (e)
    (warn "Fast boot failed: ~A" e)
    (traditional-boot)))
```

## Future Optimizations

Planned improvements:

1. **Parallel Compilation** - Use multiple cores during cache creation
2. **Incremental Cache Updates** - Update only changed files
3. **Network EPK Repository** - Download pre-built packages
4. **Profile-Guided Optimization** - Optimize based on load patterns
5. **Memory-Mapped FASL** - Even faster loading via mmap

## Troubleshooting

### Cache Issues

If you experience boot problems:

```bash
# Clear all caches
rm -rf ~/.epsilon/boot-cache/

# Rebuild from scratch
sbcl --script scripts/epsilon-optimized.lisp --rebuild --verbose
```

### Performance Debugging

Enable verbose output to see timing breakdown:

```bash
sbcl --script scripts/epsilon-optimized.lisp --verbose
```

This shows:
- Which boot method was used
- Time for each phase
- Total boot time

### EPK Issues

If EPK boot fails:
- Check EPK file exists in search paths
- Verify platform/architecture match
- Ensure EPK contains `fasl/combined.fasl`
- Check file permissions
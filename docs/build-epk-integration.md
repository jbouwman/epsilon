# Build System and EPK Integration

This document describes the improved build system that better supports
package creation..

## Overview

The enhanced build system provides:
- Structured output directories that mirror EPK layout
- Tracking of loaded FASLs to avoid recompilation
- Clean separation of build artifacts
- Integration with the EPK packaging system

## Directory Structure

When building a module for EPK packaging, the following directory structure is created:

```
module/epsilon.core/
├── src/                    # Source files
├── target/
│   ├── lisp/              # Traditional build output (legacy)
│   ├── epsilon.core.fasl  # Concatenated FASL (legacy)
│   └── epk-staging/       # EPK staging area (new)
│       └── epsilon.core/
│           ├── module.fasl        # Standardized concatenated FASL (at root)
│           ├── META-INF/
│           │   └── MANIFEST.edn
│           ├── src/       # Copied source files
│           └── fasl/      # Individual compiled FASLs
│               ├── FASLS.edn      # FASL manifest
│               └── lib/
│                   ├── map.fasl
│                   ├── string.fasl
│                   └── ...
```

### Standardized EPK Structure

Every EPK file has the same internal structure:
- `module.fasl` - The concatenated FASL at the root (same name for all modules)
- `META-INF/MANIFEST.edn` - Package metadata
- `fasl/FASLS.edn` - Manifest describing the FASL contents
- `fasl/lib/*.fasl` - Individual compiled files (optional, for debugging)
- `src/` - Source files (optional)

### 1. Load Tracking

The system tracks which FASLs have been loaded in the current session:

```lisp
;; Global tracking of loaded FASLs
(defvar *global-load-tracker* (make-hash-table :test 'equal))

;; Check if a FASL needs recompilation
(defun needs-recompilation-p (source-path fasl-path)
  (or (not (probe-file fasl-path))
      (> (file-write-date source-path) (file-write-date fasl-path))
      (not (fasl-loaded-p fasl-path))))
```

### 2. Smart Compilation

Files are only recompiled when:
- The FASL doesn't exist
- The source is newer than the FASL
- The FASL hasn't been loaded in this session

### 3. EPK-Ready Output

The staging directory structure matches exactly what goes into an EPK:
- `META-INF/` - Package metadata
- `src/` - Source files (optional)
- `fasl/` - Compiled FASLs with proper directory structure

## Usage

### Basic Build

```lisp
;; Load the improvements
(load "scripts/build-improvements.lisp")

;; Build epsilon.core for EPK
(epsilon.build.improvements:build-for-epk 
  "epsilon.core"
  #p"/home/user/epsilon/module/core/")
```

### Integration with Existing Build

The existing build system can be enhanced to use these improvements:

```lisp
;; In epsilon.tool.build
(defun build-with-epk-support (module-name &key force)
  (let* ((module-dir (get-module-directory module-name))
         (build-context (epsilon.build.improvements:create-build-context 
                         module-name module-dir)))
    
    ;; Use improved compilation logic
    (if (and (not force) 
             (epsilon.build.improvements:fasl-loaded-p 
              (format nil "~A/combined.fasl" module-name)))
        (format t "Module ~A already loaded, skipping build~%" module-name)
        (epsilon.build.improvements:build-for-epk 
         module-name module-dir :clean force))))
```

## Creating EPKs from Build Output

Once a module is built with the EPK-oriented structure, creating an EPK is straightforward:

```lisp
;; The EPK staging directory is ready to be zipped
(let ((staging-dir "target/epk-staging/epsilon.core/")
      (epk-file "epsilon.core-1.0.0.epk"))
  (create-epk-from-directory staging-dir epk-file))
```

## Benefits

1. **Efficiency**: Avoids recompiling files that haven't changed and are already loaded
2. **Structure**: Output directory structure matches EPK format exactly
3. **Tracking**: Clear visibility into what's been compiled and loaded
4. **Integration**: Works alongside existing build system without breaking it

## Migration Path

To migrate existing modules:

1. Continue using existing `epsilon.tool.build` for day-to-day development
2. Use `build-for-epk` when preparing releases or creating packages
3. Gradually integrate load tracking into the main build system
4. Eventually merge the improvements into the core build tool

## Future Enhancements

1. **Dependency Tracking**: Track inter-file dependencies for smarter recompilation
2. **Parallel Compilation**: Compile independent files in parallel
3. **Incremental EPKs**: Only include changed files in update packages
4. **Build Cache**: Share compiled FASLs across projects
5. **Cross-Compilation**: Support building for different SBCL versions/platforms

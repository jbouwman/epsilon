# Epsilon Package Format (.epk)

## Overview

Epsilon packages are distributable, signed ZIP archives containing
compiled Lisp modules with all dependencies, similar to Python
wheels. The `.epk` (Epsilon Package) format enables efficient
distribution, installation, and verification of Epsilon modules.

## Package Structure

```
package-name-1.0.0-darwin-arm64.epk
├── META-INF/
│   ├── MANIFEST.edn          # Package metadata
│   ├── SIGNATURE             # Digital signature
│   ├── CHECKSUMS.sha256      # File checksums
│   └── DEPENDENCIES.edn      # Dependency tree
├── src/                      # Source code (for debugging/inspection)
│   └── lib/
│       └── *.lisp
├── fasl/                     # Compiled binaries
│   ├── individual/           # Per-file FASLs
│   │   └── lib/
│   │       └── *.fasl
│   └── combined.fasl         # Single concatenated FASL
├── native/                   # Platform-specific libraries
│   └── *.{so,dylib,dll}
├── docs/                     # Documentation
│   ├── README.md
│   └── api/
└── tests/                    # Test files (optional)
    └── *.lisp
```

## Metadata Format

### MANIFEST.edn
```edn
{:name "epsilon.path"
 :version "1.0.0"
 :description "Cross-platform filesystem path manipulation"
 :author "Jesse Bouwman"
 :license "MIT"
 :homepage "https://github.com/epsilon-org/epsilon"
 
 ;; Platform targeting
 :platform {:os :darwin
            :arch :arm64
            :lisp :sbcl
            :lisp-version "2.4.0"}
 
 ;; Package contents
 :provides ["epsilon.lib.path"]
 :main-package "epsilon.lib.path"
 
 ;; Build information
 :build {:timestamp "2024-01-15T10:30:00Z"
         :source-hash "sha256:abc123..."
         :builder "epsilon-build-1.0.0"}
 
 ;; Installation
 :install {:load-order ["combined.fasl"]
           :native-libs ["native/libpath.dylib"]
           :post-install nil}
           
 ;; Compatibility
 :compatibility {:min-epsilon-version "1.0.0"
                 :platforms #{:darwin :linux :windows}}}
```

### DEPENDENCIES.edn
```edn
{:direct {:epsilon.core "1.0.0"
          :epsilon.unicode "0.9.0"}
 :transitive {:epsilon.map "1.0.0"
              :epsilon.string "1.0.0"}
 :resolution-tree {:epsilon.core {:version "1.0.0"
                                  :hash "sha256:def456..."
                                  :source "registry://official"}}}
```

## Package Naming Convention

```
{name}-{version}-{platform}-{arch}.epk
```

Examples:
- `epsilon.path-1.0.0-darwin-arm64.epk`
- `epsilon.http-2.1.0-linux-x86_64.epk`
- `epsilon.crypto-1.5.0-windows-x86_64.epk`
- `mylib-0.1.0-universal-any.epk` (pure Lisp, no platform dependencies)

## Signature Format

Packages are signed using Ed25519:
```
SIGNATURE file contains:
{
  "algorithm": "ed25519",
  "public_key": "...",
  "signature": "...",
  "signed_hash": "sha256:...",
  "timestamp": "2024-01-15T10:30:00Z"
}
```

## Repository Index Format

Distribution repositories maintain an index for package discovery:

### index.json
```json
{
  "format_version": "1.0",
  "generated": "2024-01-15T10:30:00Z",
  "packages": {
    "epsilon.path": {
      "versions": {
        "1.0.0": {
          "platforms": {
            "darwin-arm64": {
              "filename": "epsilon.path-1.0.0-darwin-arm64.epk",
              "size": 1048576,
              "sha256": "abc123...",
              "dependencies": ["epsilon.core-1.0.0"]
            },
            "linux-x86_64": {
              "filename": "epsilon.path-1.0.0-linux-x86_64.epk",
              "size": 1024000,
              "sha256": "def456...",
              "dependencies": ["epsilon.core-1.0.0"]
            }
          }
        }
      },
      "metadata": {
        "description": "Cross-platform filesystem path manipulation",
        "homepage": "https://github.com/epsilon-org/epsilon",
        "license": "MIT"
      }
    }
  }
}
```

## Installation Process

1. **Download**: Fetch `.epk` file from repository
2. **Verify**: Check signature and checksums
3. **Extract**: Unpack to local package cache
4. **Install**: Load FASLs and register package
5. **Link**: Make package available to import system

## Build Process

1. **Compile**: Generate FASLs for target platforms
2. **Bundle**: Collect source, binaries, dependencies
3. **Sign**: Generate digital signature
4. **Package**: Create ZIP archive with metadata
5. **Upload**: Publish to distribution repository

## Security Model

- **Signatures**: All packages must be cryptographically signed
- **Checksums**: SHA-256 hashes for all files
- **Verification**: Automatic signature validation on install
- **Sandboxing**: Packages run in controlled environment
- **Audit Trail**: Installation logs and provenance tracking

## Compatibility Matrix

| Platform | Architecture | SBCL Version | Status |
|----------|--------------|--------------|--------|
| Darwin   | ARM64        | 2.4.0+       | ✓      |
| Darwin   | x86\_64      | 2.4.0+       | ✓      |
| Linux    | x86\_64      | 2.4.0+       | ✓      |
| Linux    | ARM64        | 2.4.0+       | ✓      |
| Windows  | x86\_64      | 2.4.0+       | x      |

## Future Extensions

- **Multi-Lisp Support**: CCL, ECL, ABCL packages
- **Delta Updates**: Incremental package updates
- **Dependency Bundling**: Fat packages with all dependencies
- **Native Compilation**: AOT compilation for performance
- **Package Overlays**: Development vs production variants

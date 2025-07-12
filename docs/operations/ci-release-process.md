# Epsilon CI and Release Process

## Overview

The Epsilon CI system provides automated testing, compilation, and release of modular EPK packages across multiple platforms.

## CI Pipeline Stages

### 1. Module Discovery and Validation
- **Purpose**: Discover all modules and extract version information
- **Outputs**: Module matrix for testing, core version, release tag
- **Duration**: ~1 minute

### 2. Platform Matrix Testing  
- **Purpose**: Test each module on each compatible platform
- **Matrix**: [Linux, macOS, Windows] × [all modules] 
- **Excludes**: Platform-specific modules on incompatible platforms
- **Features**:
  - Dependency resolution and building
  - Comprehensive test execution with JUnit output
  - Build artifact caching for performance
- **Duration**: ~10-20 minutes per platform

### 3. Integration Testing
- **Purpose**: Test module interactions and end-to-end workflows
- **Tests**:
  - Core module functionality
  - Module loading and interaction
  - Platform-specific modules
  - Data format pipelines
  - Foreign integration
- **Duration**: ~5-10 minutes per platform

### 4. EPK Compilation
- **Purpose**: Generate EPK packages for distribution
- **Triggered**: On main branch or releases
- **Outputs**: Platform-specific EPK files for all modules
- **Features**:
  - Combined core+essential packages for fast startup
  - Platform detection and compatibility
  - Manifest generation with metadata
- **Duration**: ~5-10 minutes per platform

### 5. Release Publishing
- **Purpose**: Publish EPKs to package repository
- **Triggered**: On GitHub releases only
- **Outputs**: Updated package repository with indexes
- **Features**:
  - Package repository management
  - Index generation for dependency resolution
  - Release notes and compatibility matrices
  - GitHub release artifact uploading

## Package Repository Structure

```
packages/
├── epsilon.core/
│   └── 1.0.0/
│       ├── epsilon.core-1.0.0-linux-x86_64.epk
│       ├── epsilon.core-1.0.0-darwin-x86_64.epk
│       └── epsilon.core-1.0.0-windows-x86_64.epk
├── epsilon.yaml/
│   └── 1.0.0/
│       └── epsilon.yaml-1.0.0-linux-x86_64.epk
index/
├── repository.json      # Repository metadata
└── packages.json        # Package index with dependencies
releases/
└── 1.0.0/
    ├── RELEASE_NOTES.md
    └── compatibility.json
```

## Usage

### Triggering CI
```bash
# Development testing
git push origin develop

# Production testing  
git push origin main

# Release
git tag v1.0.0
git push origin v1.0.0
gh release create v1.0.0 --title "Epsilon 1.0.0" --notes "Release notes"
```

### Installing Packages
```bash
# Add repository
epsilon-package repo add epsilon-packages https://github.com/epsilon-org/packages.git

# Install core
epsilon-package install epsilon.core@1.0.0

# Install modules
epsilon-package install epsilon.yaml@1.0.0
epsilon-package install epsilon.regex@1.0.0
```

### Manual EPK Generation
```bash
# Build all modules
./scripts/ci/build-all-modules.sh

# Generate EPKs
./scripts/ci/generate-epks.sh 1.0.0

# Setup release repo
./scripts/ci/setup-release-repo.sh v1.0.0

# Generate index
./scripts/ci/generate-release-index.sh 1.0.0

# Publish (requires RELEASE_TOKEN)
./scripts/ci/publish-release.sh v1.0.0
```

## Platform Support

### Supported Platforms
- **Linux**: x86_64, arm64 (Ubuntu 20.04+, CentOS 8+, Debian 11+)
- **macOS**: x86_64, arm64 (macOS 10.15+)
- **Windows**: x86_64 (Windows 10+)

### Platform-Specific Modules
- `linux` - Linux epoll networking
- `darwin` - macOS kqueue networking + TLS
- `windows` - Windows IOCP networking

## Configuration

### Environment Variables
- `EPSILON_RELEASE_REPO_URL` - Release repository URL
- `RELEASE_TOKEN` - GitHub token for publishing
- `CACHE_VERSION` - Build cache version for invalidation

### GitHub Secrets Required
- `RELEASE_TOKEN` - Personal access token with repo permissions

## Module Dependencies

```
epsilon.core (foundation)
├── Platform modules (linux/darwin/windows)
│   └── http (depends on platform networking)
│       └── lsp
├── Data modules
│   ├── epsilon.parsing (parser + lexer + json)
│   ├── epsilon.yaml
│   ├── epsilon.msgpack
│   └── epsilon.regex
├── Compression modules (independent)
│   ├── epsilon.bzip
│   ├── epsilon.gzip
│   ├── epsilon.zlib
│   └── epsilon.inflate
└── epsilon.foreign (depends on epsilon.parsing)
```

## Performance Optimizations

- **Build Caching**: Content-based hashing for incremental builds
- **Artifact Caching**: GitHub Actions cache for compiled modules
- **Combined EPKs**: Core+essential packages for fast startup
- **Platform Matrix**: Parallel execution across platforms
- **Dependency Building**: Smart dependency resolution

## Quality Assurance

- **Zero Tolerance**: All tests must pass for release
- **Platform Compatibility**: Automated testing across all platforms
- **Integration Testing**: End-to-end workflow validation
- **Performance Monitoring**: Benchmark execution tracking
- **Dependency Validation**: Module compatibility verification

## Release Approval Process

1. **Development**: Push to `develop` branch triggers full CI
2. **Staging**: Push to `main` branch triggers CI + EPK generation
3. **Release**: GitHub release creation triggers full publication
4. **Validation**: Automated verification of published packages
5. **Notification**: Success/failure notifications

This process ensures reliable, tested, and compatible Epsilon packages across all supported platforms.
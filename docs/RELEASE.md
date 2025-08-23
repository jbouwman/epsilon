# Epsilon Release Guide

This is a guide for creating, publishing, and managing Epsilon releases.

## Table of Contents

- [Overview](#overview)
- [Quick Start](#quick-start)
- [Release Types](#release-types)
- [Release Process](#release-process)
- [Version Management](#version-management)
- [Automation Scripts](#automation-scripts)
- [GitHub Actions](#github-actions)
- [Distribution](#distribution)
- [Installation](#installation)
- [Troubleshooting](#troubleshooting)

## Overview

Epsilon uses Git tags as the authoritative source for releases. Each
release includes pre-built binaries for Linux and macOS with the SBCL
runtime bundled.

- **Versioning**: Versioning follows MAJOR.MINOR.PATCH format (e.g., 0.11.0)
- **Git Tags**: Version tags trigger automated builds (e.g., v0.11.0)

## Quick Start

### Creating a Release

```bash
# 1. Ensure all changes are committed and pushed
git status

# 2. Run the automated release script
./scripts/release.sh 0.11.0

# 3. After release, update VERSION for next development cycle
./scripts/bump-version.sh --commit --push
```

### Installing a Release

```bash
# Users can install with one command
curl -sSL https://github.com/jbouwman/epsilon/releases/latest/download/install.sh | bash
```

## Release Types

### Stable Releases
- **Format**: `vMAJOR.MINOR.PATCH` (e.g., `v0.11.0`)
- **Branch**: Created from `main` or release branches
- **Process**: Full testing, documentation updates, changelog

### Pre-releases
- **Alpha**: `v0.11.0-alpha.1` - Early testing, API may change
- **Beta**: `v0.11.0-beta.1` - Feature complete, fixing bugs
- **RC**: `v0.11.0-rc.1` - Release candidate, final testing

### Development Builds
- **Version**: Content of VERSION file (e.g., `0.12.0-dev`)
- **Purpose**: Between releases for development work

## Release Process

### 1. Preparation

Before creating a release, ensure:

- [ ] All tests pass: `./scripts/test.sh`
- [ ] CHANGELOG.md updated with release notes
- [ ] Documentation updated for new features
- [ ] VERSION file shows correct version
- [ ] No uncommitted changes: `git status`

### 2. Create Release

The automated release script handles all validation and tagging:

```bash
# Create stable release
./scripts/release.sh 0.11.0

# Create pre-release
./scripts/release.sh 0.11.0-beta.1

# Preview without making changes
./scripts/release.sh --dry-run 0.11.0

# Force release from specific branch
./scripts/release.sh --branch release/v0.11 0.11.1
```

The script will:
1. Validate version format and git state
2. Check branch conventions
3. Run all tests
4. Verify CHANGELOG entry
5. Create annotated git tag
6. Push tag to trigger GitHub Actions

### 3. GitHub Actions Build

Once the tag is pushed, GitHub Actions automatically:

1. **Runs tests** on all platforms
2. **Builds releases** for each platform:
   - `epsilon-0.11.0-linux-x86_64.tar.gz`
   - `epsilon-0.11.0-macos-arm64.tar.gz`
   - `epsilon-0.11.0-macos-x86_64.tar.gz`
3. **Generates checksums** (SHA256) for each archive
4. **Creates GitHub Release** with all artifacts
5. **Publishes release notes** from CHANGELOG

### 4. Post-Release

After the release is published:

```bash
# Update VERSION for next development cycle
./scripts/bump-version.sh --commit --push

# Or manually specify version
./scripts/bump-version.sh --commit 0.12.0-dev
```

### 5. Release Branches (Optional)

For long-term support:

```bash
# Create release branch for maintenance
git checkout -b release/v0.11
git push origin release/v0.11

# Later, create patch releases from this branch
git checkout release/v0.11
./scripts/release.sh 0.11.1
```

## Version Management

### VERSION File

The `VERSION` file tracks the current development version:

- **During development**: `0.12.0-dev`
- **At release time**: `0.11.0`
- **After release**: Bump to next dev version

### Git Tags

Tags define official releases:

- **Format**: `v` prefix + version (e.g., `v0.11.0`)
- **Type**: Annotated tags with release metadata
- **Triggers**: GitHub Actions workflow on push

### Version Precedence

1. Git tags (for releases)
2. VERSION file (for development)
3. Environment variable `EPSILON_VERSION` (for overrides)

## Automation Scripts

### `scripts/release.sh`

Main release automation script:

```bash
# Options
--dry-run       # Preview without making changes
--force         # Skip confirmation prompts
--branch BRANCH # Release from specific branch

# Examples
./scripts/release.sh 0.11.0
./scripts/release.sh --dry-run 0.11.0-beta.1
./scripts/release.sh --branch release/v0.11 0.11.1
```

Features:
- Version format validation
- Git state verification
- Branch convention checks
- Automatic test execution
- CHANGELOG verification
- Tag creation and pushing
- Post-release instructions

### `scripts/bump-version.sh`

Version management helper:

```bash
# Options
--commit  # Create git commit
--push    # Push to origin (implies --commit)

# Examples
./scripts/bump-version.sh                    # Suggest next version
./scripts/bump-version.sh 0.12.0-dev        # Set specific version
./scripts/bump-version.sh minor             # Bump minor version
./scripts/bump-version.sh --commit patch    # Bump patch and commit
```

### `scripts/test.sh`

 test runner:

```bash
# Run all tests (required before release)
./scripts/test.sh
```

Executes:
- CLI smoke tests
- Module self-tests
- Epsilon executable verification

### `scripts/package.sh`

Platform-specific packaging (used by CI):

```bash
# Build for current platform
./scripts/package.sh

# Build for specific platform
./scripts/package.sh linux x86_64
./scripts/package.sh darwin arm64
```

## GitHub Actions

### Release Workflow (`.github/workflows/release.yml`)

**Triggers**: Git tags matching `v*.*.*`

**Process**:
1. Matrix build for all platforms
2. Test execution (must pass)
3. Runtime packaging
4. Archive creation with checksums
5. GitHub Release publication

**Platforms**:
- Ubuntu latest (Linux x86_64) - containerized
- macOS latest (Darwin ARM64)
- macOS 13 (Darwin x86_64)

### CI Workflow (`.github/workflows/ci.yml`)

**Triggers**: Push to main/develop, PRs to main

**Purpose**: Continuous testing on all platforms

## Distribution

### Release Artifacts

Each platform release contains:

```
epsilon-0.11.0-macos-arm64/
├── bin/
│   ├── epsilon       # Main executable
│   └── sbcl          # SBCL runtime
├── lib/
│   └── sbcl-libs/    # SBCL libraries
├── modules/          # All Epsilon modules
├── scripts/          # Utility scripts
├── CHANGELOG.md      # Release notes
├── LICENSE           # License file
├── README.md         # Documentation
└── VERSION           # Version file
```

### Archive Formats

- **Unix/Linux/macOS**: `.tar.gz` with SHA256 checksum
- **Windows**: Not yet supported (use WSL2)

### Installation Methods

#### 1. Automated Install Script

```bash
# Install latest release
curl -sSL https://github.com/jbouwman/epsilon/releases/latest/download/install.sh | bash

# Custom installation directory
EPSILON_HOME=/opt/epsilon ./install.sh
```

#### 2. Manual Download

```bash
# Download release
wget https://github.com/jbouwman/epsilon/releases/download/v0.11.0/epsilon-0.11.0-linux-x86_64.tar.gz

# Verify checksum
sha256sum -c epsilon-0.11.0-linux-x86_64.tar.gz.sha256

# Extract
tar -xzf epsilon-0.11.0-linux-x86_64.tar.gz

# Add to PATH
export PATH="$PWD/epsilon-0.11.0-linux-x86_64/bin:$PATH"
```

#### 3. From Source

```bash
# Clone repository
git clone https://github.com/jbouwman/epsilon.git
cd epsilon

# Build from source
make test
make release
```

### Uninstallation

```bash
# If installed via script
~/.epsilon/scripts/uninstall.sh

# Manual removal
rm -rf ~/.epsilon
rm -f ~/.local/bin/epsilon
```

## Installation

Users install Epsilon using the install script which:

1. **Detects platform** (Linux/macOS, x86_64/ARM64)
2. **Downloads** appropriate release from GitHub
3. **Extracts** to `~/.epsilon` (configurable)
4. **Creates symlink** in `~/.local/bin`
5. **Updates PATH** in shell configuration
6. **Verifies** installation

### System Requirements

- **Linux**: glibc 2.17+ (CentOS 7+, Ubuntu 14.04+)
- **macOS**: 10.14+ (Mojave or later)
- **Architecture**: x86_64 or ARM64
- **Disk Space**: ~200MB extracted
- **Memory**: ~500MB RAM minimum

## Troubleshooting

### Release Creation Issues

#### Tests Failing
```bash
# Run tests locally to reproduce
./scripts/test.sh

# Test specific module
./epsilon --test epsilon.core
```

#### Version Conflicts
- Ensure git tag matches intended version
- Check VERSION file is correct
- Verify no uncommitted changes

#### Tag Already Exists
```bash
# Delete local tag
git tag -d v0.11.0

# Delete remote tag (be careful!)
git push origin :refs/tags/v0.11.0
```

### Build Issues

#### GitHub Actions Failing
1. Check Actions tab for detailed logs
2. Review `.github/workflows/release.yml`
3. Test locally with: `make release`

#### Platform-Specific Failures
- **Linux**: Check container configuration
- **macOS**: Verify Xcode tools installed
- **Windows**: Not supported, use WSL2

### Installation Issues

#### "epsilon: command not found"
```bash
# Check PATH
echo $PATH

# Add to PATH manually
export PATH="$HOME/.local/bin:$PATH"

# Add to shell config
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

#### Permission Denied
```bash
# Fix permissions
chmod +x ~/.epsilon/bin/epsilon
chmod +x ~/.local/bin/epsilon
```

#### Wrong Architecture
- Verify platform detection: `uname -sm`
- Download correct release manually
- Set EPSILON_ARCH environment variable

## Best Practices

1. **Always run tests** before creating a release
2. **Update CHANGELOG** with user-facing changes
3. **Use semantic versioning** consistently
4. **Create release branches** for maintenance
5. **Tag from stable branches** (main or release/*)
6. **Document breaking changes** prominently
7. **Test installation** after release
8. **Monitor GitHub Actions** for build status

## Security

### Code Signing
- Future: macOS notarization support
- Future: GPG signatures for Linux packages

### Checksums
- SHA256 checksums for all archives
- Generated during build process
- Included in release artifacts

### Verification
```bash
# Verify checksum
sha256sum -c epsilon-0.11.0-linux-x86_64.tar.gz.sha256

# Verify installation
epsilon --version
epsilon --test epsilon.core
```

## Support

- **Issues**: https://github.com/jbouwman/epsilon/issues
- **Discussions**: https://github.com/jbouwman/epsilon/discussions
- **Documentation**: https://github.com/jbouwman/epsilon/wiki

---

*Last updated: 2024-08-17*

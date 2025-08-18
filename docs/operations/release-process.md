# Release Process

> **Note**: This document provides a quick reference. For the complete release guide, see [RELEASE.md](/RELEASE.md) in the project root.

## Overview

Epsilon follows semantic versioning with git tags as the authoritative version source for releases.

## Version Management

- **Git tags**: Define release versions (e.g., `v0.11.0`, `v0.11.0-beta.1`)
- **VERSION file**: Tracks development version between releases (e.g., `0.11.0-dev`)

## Release Types

### Stable Releases
- Format: `vMAJOR.MINOR.PATCH` (e.g., `v0.11.0`)
- Branch: Created from `main` or release branch
- Process: Full testing, documentation updates

### Pre-releases
- Alpha: `v0.11.0-alpha.1` - Early testing, API may change
- Beta: `v0.11.0-beta.1` - Feature complete, fixing bugs  
- RC: `v0.11.0-rc.1` - Release candidate, final testing

### Development Builds
- Automatic builds from main branch
- Version: Current VERSION file content + git SHA

## Release Workflow

### 1. Prepare Release

```bash
# Update CHANGELOG.md with release notes
# Ensure all tests pass
./scripts/test.sh

# For release branches
git checkout -b release/v0.11
```

### 2. Create Release (Automated)

```bash
# Use the automated release script
./scripts/release.sh 0.11.0

# Or for pre-releases
./scripts/release.sh 0.11.0-beta.1

# Preview without making changes
./scripts/release.sh --dry-run 0.11.0

# The script will:
# - Validate version format
# - Check git status and branch
# - Run all tests
# - Verify CHANGELOG entry
# - Create and push git tag
# - Trigger GitHub Actions for builds
```

### 3. Post-Release

```bash
# Update VERSION file for next development cycle
./scripts/bump-version.sh --commit --push

# Or manually specify version
./scripts/bump-version.sh --commit 0.12.0-dev
```

## Manual Release Build

To build a release locally:

```bash
# Build release for current platform
./epsilon --exec epsilon.release:generate -- v0.11.0

# Output will be in releases/epsilon-0.11.0-<platform>/
```

## Release Branch Strategy

### Long-term Support
- Create branch: `release/v0.11`
- Backports: Cherry-pick critical fixes
- Patch releases: `v0.11.1`, `v0.11.2`

### Pre-release Testing
```bash
# Create pre-release tags on release branch
git checkout release/v0.11
git tag -a v0.11.0-beta.1 -m "Beta 1 for v0.11.0"
git push origin v0.11.0-beta.1
```

## Platform Support

Each release builds automatically for:
- Linux x86_64
- macOS ARM64 (Apple Silicon)
- macOS x86_64 (Intel)

Each platform build includes:
- Standalone epsilon executable
- All core and platform-specific modules
- Documentation
- SHA256 checksum

## Verification

After release:
1. Check GitHub Actions for successful builds
2. Verify artifacts on GitHub Releases page
3. Test installation instructions
4. Update documentation if needed

## Troubleshooting

### Build Failures

If the release build fails:

1. Check the GitHub Actions logs
2. Test locally with:
   ```bash
   ./epsilon --exec epsilon.release:generate -- v0.11.0
   ```
3. Ensure all modules build correctly:
   ```bash
   ./epsilon --exec epsilon.release:selftest
   ```

### Version Conflicts

If you get version mismatch errors:
- Ensure git tag matches the version you're building
- Check that VERSION file is updated for development
- Verify no uncommitted changes

### Platform-Specific Issues

- **Linux**: Ensure SBCL is installed and in PATH
- **macOS**: May need to allow unsigned binaries in Security settings
- **Windows**: Not yet supported

## Release Scripts

The following scripts automate the release process:

### `scripts/release.sh`
Automated release creation with validation:
- Validates version format and git state
- Checks branch conventions
- Runs all tests
- Creates and pushes release tag
- Provides post-release instructions

Options:
- `--dry-run`: Preview without making changes
- `--force`: Skip confirmation prompts
- `--branch`: Release from specific branch

### `scripts/bump-version.sh`
Version management for development:
- Updates VERSION file
- Supports major/minor/patch increments
- Can auto-commit and push changes

Options:
- `--commit`: Create git commit
- `--push`: Push to origin (implies --commit)

### `scripts/test.sh`
Comprehensive test runner:
- Runs CLI smoke tests
- Executes module self-tests
- Validates epsilon executable

### `scripts/package.sh`
Platform-specific packaging:
- Builds runtime executable
- Creates platform metadata
- Used by CI/CD pipeline

### Release Build Scripts

These scripts handle the release build process and can be run both locally and in CI:

#### `scripts/run-tests.sh`
Comprehensive test runner:
```bash
# Run all tests
./scripts/run-tests.sh

# Run with JUnit output
./scripts/run-tests.sh junit target/test-results.xml
```

#### `scripts/generate-release.sh`
Generate release archive:
```bash
# Generate release for current platform (auto-detects platform/arch)
./scripts/generate-release.sh 0.11.0
```

#### `scripts/verify-release.sh`
Verify release archive works:
```bash
# Verify latest release
./scripts/verify-release.sh

# Verify specific archive
./scripts/verify-release.sh releases/epsilon-0.11.0-linux-x86_64.tar.gz
```

#### `scripts/orchestrate-release.sh`
Complete release pipeline:
```bash
# Full release build and verification (auto-detects platform/arch)
./scripts/orchestrate-release.sh 0.11.0
```

#### `scripts/setup.sh`
Environment setup:
```bash
# Local setup
./scripts/setup.sh

# Container setup (used in CI)
./scripts/setup.sh container-name
```

#### `scripts/get-version.sh`
Version extraction:
```bash
# Get current version
./scripts/get-version.sh
```

## Release Checklist

Before creating a release:

- [ ] All tests pass (`./scripts/test.sh`)
- [ ] CHANGELOG.md updated with release notes
- [ ] Documentation updated for new features
- [ ] VERSION file shows correct version
- [ ] No uncommitted changes in working directory
- [ ] Previous releases tested for upgrade path
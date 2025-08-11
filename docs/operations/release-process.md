# Release Process

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
./epsilon --exec epsilon.release:selftest

# For release branches
git checkout -b release/v0.11
```

### 2. Create Release

```bash
# Tag the release
git tag -a v0.11.0 -m "Release v0.11.0"
git push origin v0.11.0

# GitHub Actions will automatically:
# - Run all tests
# - Build platform-specific releases
# - Create GitHub release with artifacts
```

### 3. Post-Release

```bash
# Update VERSION file to next development version
echo "0.12.0-dev" > VERSION
git commit -am "Bump version to 0.12.0-dev"
git push
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

## Release Checklist

Before creating a release:

- [ ] All tests pass (`./epsilon --exec epsilon.release:selftest`)
- [ ] CHANGELOG.md updated with release notes
- [ ] Documentation updated for new features
- [ ] VERSION file shows correct development version
- [ ] No uncommitted changes in working directory
- [ ] Previous releases tested for upgrade path
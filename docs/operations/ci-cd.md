# CI/CD Process

Guide to Epsilon's continuous integration and testing workflows. For release process, see [Release Process](release-process.md).

## Overview

Epsilon uses GitHub Actions for:
- Automated testing on every push and PR
- Multi-platform builds (Linux, macOS)
- Versioned releases with semantic versioning
- Documentation deployment
- Container image maintenance

## GitHub Workflows

### 1. Continuous Integration (`ci.yml`)

**Purpose**: Test all packages on every code change

**Triggers**:
- Push to `main` or `develop` branches
- Pull requests to `main`

**Features**:
- Tests ALL epsilon packages
- Runs on Linux (containerized) and macOS
- Publishes detailed test reports
- Shows test coverage summary
- Uses artifact caching for speed

**Test Matrix**:
- **OS**: Ubuntu (containerized), macOS latest
- **SBCL**: Latest stable version
- **Packages**: All packages tested by default

### 2. Release Workflow (`release.yml`)

**Purpose**: Build and publish official releases

**Triggers**:
- Semantic version tags (`v*.*.*`)
- GitHub release creation

**Process**:
1. Run all tests (must pass)
2. Build platform-specific executables
3. Create distribution archives
4. Generate SHA256 checksums
5. Publish to GitHub releases

**Platforms**:
- Linux x86_64
- macOS ARM64 (M1/M2)
- macOS x86_64 (Intel)
- Windows (planned)

### 3. Documentation (`docs.yml`)

**Purpose**: Deploy documentation to GitHub Pages

**Triggers**:
- Release publication
- Manual dispatch

**Process**:
- Builds MkDocs site
- Deploys to gh-pages branch
- Available at project GitHub Pages URL

### 4. Container Images (`build-image.yml`)

**Purpose**: Maintain CI/CD container infrastructure

**Triggers**:
- Changes to `.docker/**` files
- Manual dispatch

**Images Built**:
- `epsilon-ci`: Linux CI environment with SBCL and dependencies
- Platform-specific development containers

## Testing

### Running Tests Locally

```bash
# Test all packages (matches CI behavior)
./scripts/ci-test-all.sh

# Test specific module
./epsilon test epsilon.http

# Test with JUnit output
./epsilon test all --format junit --file results.xml

# Test with pattern matching
./epsilon test --module epsilon.core --test 'parse-*'
```

### Test Discovery

The test system automatically discovers tests by:
1. Finding all `module.lisp` files
2. Loading test files from `tests/` directories
3. Running all functions defined with `deftest`

### Test Output Formats

- **Shell** (default): Human-readable console output
- **JUnit**: XML format for CI integration
- **REPL**: Interactive development format

### Caching

CI uses intelligent caching to speed up builds:

**Cache Key**: Based on OS + Lisp file hashes
```
build-v1-${{ runner.os }}-${{ hashFiles('src/**/*.lisp') }}
```

**Cached Paths**:
- `src/*/target` - Compiled FASL files
- `.epsilon-cache` - Build metadata

## Release Workflow

For detailed information about creating releases, version management, and release strategies, see the [Release Process documentation](release-process.md).

### Quick Reference

- **Create Release**: Tag with semantic version (e.g., `v0.11.0`)
- **Pre-releases**: Use suffixes like `-alpha.1`, `-beta.1`, `-rc.1`
- **Artifacts**: Automatic builds for Linux x86_64, macOS ARM64, and macOS x86_64
- **Checksums**: SHA256 checksums generated for all release archives

## Platform-Specific Notes

### Linux
- Uses containerized builds for consistency
- Based on Ubuntu with SBCL pre-installed
- Supports x86_64 architecture

### macOS
- Builds on latest macOS runners
- Separate builds for Intel (x86_64) and Apple Silicon (ARM64)
- Universal binary planned for future

### Windows
- Support planned but not yet implemented
- Will use MSYS2 or similar environment

## Security

### Code Signing
- macOS builds are notarized (when certificates configured)
- Linux packages include GPG signatures (when configured)

### Checksums
- All releases include SHA256 checksums
- Checksums generated during build process
- Verified in download instructions

### Secrets Management
- Signing certificates stored in GitHub Secrets
- Never commit sensitive data
- Use environment variables for credentials

## Troubleshooting

### Common CI Failures

1. **Test failures**:
   ```bash
   # Reproduce locally
   ./epsilon test MODULE_NAME
   ```

2. **Cache issues**:
   - Increment `CACHE_VERSION` in workflow
   - Or manually clear cache in GitHub UI

3. **Platform-specific failures**:
   ```bash
   # Test in Docker (Linux)
   docker run -it ghcr.io/jbouwman/epsilon/epsilon-ci:latest
   
   # Test on macOS
   # Requires local macOS machine
   ```

### Debug Mode

Enable verbose output:
```bash
# Local testing with debug
EPSILON_DEBUG=1 ./epsilon test

# In CI, check workflow logs
# Raw logs available in Actions tab
```

### Local CI Simulation

Using [act](https://github.com/nektos/act):
```bash
# Install act
brew install act  # or appropriate method

# Run CI locally
act push -W .github/workflows/ci.yml

# Run specific job
act -j test
```

## Best Practices

1. **Keep workflows simple** - Each workflow has one clear purpose
2. **Test locally first** - Catch issues before CI
3. **Use caching wisely** - Balance speed vs correctness
4. **Monitor CI status** - Set up notifications for failures
5. **Document changes** - Update this guide when changing workflows

## Metrics and Monitoring

### Build Status
- Badge in README shows current status
- GitHub Actions tab shows history
- Branch protection requires passing tests

### Performance Metrics
- Workflow run times tracked in Actions
- Cache hit rates visible in logs
- Test execution times in reports

### Notifications
- Email notifications for failures (configurable)
- Slack/Discord webhooks supported
- GitHub status checks on PRs

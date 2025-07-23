# CI and Release Process

Automated testing, building, and releasing for Epsilon.

## CI Pipeline

GitHub Actions workflow runs on every push and pull request.

### Test Matrix

Tests run on:
- **OS**: Ubuntu 22.04, macOS 12, macOS 13 (arm64)
- **SBCL**: 2.3.0+ 
- **Modules**: All non-platform-specific modules

Platform-specific modules tested only on their target OS.

### Workflow Stages

1. **Setup** (~1 min)
   - Checkout code
   - Install SBCL
   - Cache dependencies

2. **Build** (~5 min)
   - Discover modules
   - Build in dependency order
   - Generate FASL files

3. **Test** (~10 min)
   - Run module tests
   - Generate JUnit XML
   - Upload test results

4. **Package** (on main/tags only)
   - Build core image
   - Create distribution
   - Generate checksums

### Configuration

`.github/workflows/ci.yml`:
```yaml
name: CI
on: [push, pull_request]

jobs:
  test:
    strategy:
      matrix:
        os: [ubuntu-22.04, macos-12]
        exclude:
          - os: ubuntu-22.04
            module: darwin
          - os: macos-12  
            module: linux
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v3
      - uses: 40ants/setup-lisp@v2
      - run: ./epsilon test
```

## Release Process

Releases are triggered by version tags.

### Version Tagging

```bash
# Update version in package.edn
git commit -am "Release v1.2.3"
git tag v1.2.3
git push origin main v1.2.3
```

### Release Workflow

1. **Build Distributions** (~20 min)
   - Linux x86_64
   - macOS x86_64
   - macOS arm64

2. **Create Release**
   - Generate changelog
   - Upload artifacts
   - Create GitHub release

3. **Publish**
   - Update installation script
   - Notify package managers

### Release Files

Each release includes:
```
epsilon-1.2.3-linux-x86_64.tar.gz
epsilon-1.2.3-linux-x86_64.tar.gz.sha256
epsilon-1.2.3-macos-x86_64.tar.gz
epsilon-1.2.3-macos-x86_64.tar.gz.sha256
epsilon-1.2.3-macos-arm64.tar.gz
epsilon-1.2.3-macos-arm64.tar.gz.sha256
```

## Module Testing

### Test Discovery

Tests are discovered from `package.edn`:
```edn
{
  "name" "epsilon.json"
  "tests" ["tests"]
}
```

Test files match `*-tests.lisp` pattern.

### Test Execution

```bash
# Test single module
./epsilon test --module epsilon.json

# Test with coverage
./epsilon test --module epsilon.json --coverage

# Test with specific pattern
./epsilon test --module epsilon.json --test parse-*
```

### Test Output

JUnit XML format for CI integration:
```xml
<testsuites>
  <testsuite name="epsilon.json.tests" tests="42" failures="0">
    <testcase name="parse-empty-object" time="0.001"/>
    <testcase name="parse-nested-arrays" time="0.002"/>
  </testsuite>
</testsuites>
```

## Caching

Build artifacts are cached to speed up CI.

### Cache Keys

```yaml
key: ${{ runner.os }}-build-${{ hashFiles('**/package.edn') }}
restore-keys: |
  ${{ runner.os }}-build-
  ${{ runner.os }}-
```

### Cached Paths

- `~/.cache/epsilon/` - Build cache
- `module/*/target/` - Compiled FASLs

Cache invalidated when:
- `package.edn` files change
- Dependencies update
- Cache version bumped

## Platform Builds

### Linux

Built on Ubuntu 22.04 for glibc compatibility:
```yaml
runs-on: ubuntu-22.04
steps:
  - run: |
      sudo apt-get update
      sudo apt-get install -y sbcl
```

### macOS

Universal binaries for Intel and Apple Silicon:
```yaml
strategy:
  matrix:
    include:
      - os: macos-12
        arch: x86_64
      - os: macos-13-xlarge
        arch: arm64
```

### Cross-Compilation

Not supported. Each platform built natively.

## Security

### Code Signing

macOS distributions are signed and notarized:
```bash
codesign --deep --force --sign "Developer ID" epsilon
xcrun notarytool submit epsilon-*.tar.gz --wait
```

### Checksums

SHA-256 checksums for all artifacts:
```bash
shasum -a 256 epsilon-*.tar.gz > checksums.txt
```

### Secrets

Required GitHub secrets:
- `APPLE_DEVELOPER_ID` - Code signing certificate
- `APPLE_ID` - Notarization account
- `APPLE_APP_PASSWORD` - App-specific password

## Monitoring

### Build Status

Badge in README:
```markdown
[![CI](https://github.com/jbouwman/epsilon/actions/workflows/ci.yml/badge.svg)](https://github.com/jbouwman/epsilon/actions/workflows/ci.yml)
```

### Notifications

On failure:
- GitHub UI shows ❌
- PR checks block merge
- Optional: Slack/email alerts

### Metrics

Track in CI:
- Build time
- Test count
- Test duration  
- Cache hit rate
- Distribution size

## Troubleshooting

### CI Failures

**"Module not found"**
- Check module registration
- Verify `package.edn` syntax

**"Tests timeout"**
- Default timeout: 10 minutes
- Increase with `timeout-minutes: 20`

**"Cache miss"**
- Check cache key changes
- Manual cache clear in UI

### Local Reproduction

```bash
# Run CI locally with act
act -j test -P ubuntu-22.04=ubuntu:22.04

# Match CI environment
docker run -it ubuntu:22.04
# Install SBCL and run tests
```

### Debug Logging

```yaml
- name: Debug build
  run: |
    ./epsilon --verbose test
  env:
    EPSILON_DEBUG: 1
```

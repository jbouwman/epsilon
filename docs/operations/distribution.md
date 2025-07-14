# Distribution

Epsilon distributes as a standalone SBCL runtime with Epsilon libraries preloaded.

## Installation

### Quick Install

```bash
curl -sSL https://raw.githubusercontent.com/jbouwman/epsilon/main/scripts/install.sh | bash
```

This script:
1. Detects platform (Linux/macOS x86_64/arm64)
2. Downloads appropriate release from GitHub
3. Extracts to `~/.epsilon/`
4. Updates PATH in shell profile

### Manual Install

Download from [releases](https://github.com/jbouwman/epsilon/releases):
- `epsilon-linux-x86_64.tar.gz`
- `epsilon-macos-x86_64.tar.gz`  
- `epsilon-macos-arm64.tar.gz`

Extract and add to PATH:
```bash
tar -xzf epsilon-*.tar.gz
sudo mv epsilon /usr/local/bin/
```

## Usage

The `epsilon` command is SBCL with Epsilon preloaded:

```bash
# REPL
epsilon

# Run file
epsilon --load myapp.lisp

# Evaluate code
epsilon --eval "(epsilon.lib.json:encode '((foo . 1)))" --quit
```

## Building Distributions

### Prerequisites

- SBCL 2.0.0+
- POSIX shell
- tar, gzip

### Build Process

```bash
# Build core image with Epsilon
sbcl --load scripts/build-core.lisp

# Create distribution
./scripts/build-runtime.sh
```

This produces:
```
target/
├── epsilon-core              # SBCL core with Epsilon
├── epsilon                   # Wrapper script
└── epsilon-linux-x86_64.tar.gz
```

### Core Image Creation

`scripts/build-core.lisp`:
```lisp
;; Load Epsilon
(load "scripts/boot.lisp")
(epsilon.tool.boot:boot)

;; Save core image
(sb-ext:save-lisp-and-die 
  "target/epsilon-core"
  :toplevel #'epsilon-toplevel
  :executable nil
  :compression t)
```

The core image includes:
- All Epsilon core modules
- Compiled FASL files
- Package definitions
- No user code

### Wrapper Script

`epsilon` wrapper handles platform differences:
```bash
#!/bin/sh
SCRIPT_DIR=$(dirname "$0")
exec sbcl --core "$SCRIPT_DIR/epsilon-core" "$@"
```

## Release Process

### GitHub Actions

On tag push:
1. Build on each platform (Linux, macOS)
2. Create core images
3. Package distributions
4. Upload to GitHub release

`.github/workflows/release.yml`:
```yaml
on:
  push:
    tags:
      - 'v*'

jobs:
  release:
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v3
      - name: Install SBCL
        uses: 40ants/setup-lisp@v2
        with:
          ql-dist: ultralisp
      - name: Build distribution
        run: ./scripts/build-runtime.sh
      - name: Upload release
        uses: softprops/action-gh-release@v1
        with:
          files: target/epsilon-*.tar.gz
```

### Version Management

Version from `module/core/package.edn`:
```edn
{
  "name" "epsilon.core"
  "version" "1.0.0"
  ...
}
```

Tag matches version: `git tag v1.0.0`

## Platform Support

### Linux
- glibc 2.17+ (CentOS 7+, Ubuntu 14.04+)
- x86_64 architecture
- No additional dependencies

### macOS  
- macOS 10.14+ (Mojave)
- x86_64 and arm64 (Apple Silicon)
- Signed and notarized binaries

### Windows
- Not currently supported
- WSL2 recommended

## Distribution Contents

```
epsilon-linux-x86_64.tar.gz
├── epsilon              # Wrapper script (755)
├── epsilon-core         # SBCL core image
├── README.md            # Basic usage
└── LICENSE              # License file
```

Size: ~50-70MB compressed, ~150-200MB extracted

## Deployment

### System Package

For system-wide installation:
```bash
# Extract to system location
sudo tar -C /opt -xzf epsilon-*.tar.gz
sudo ln -s /opt/epsilon/epsilon /usr/local/bin/epsilon
```

### Container Image

Dockerfile:
```dockerfile
FROM ubuntu:22.04
RUN apt-get update && apt-get install -y curl
RUN curl -sSL https://raw.githubusercontent.com/jbouwman/epsilon/main/scripts/install.sh | bash
ENV PATH="/root/.epsilon/bin:${PATH}"
CMD ["epsilon"]
```

### Application Bundling

Include Epsilon with your application:
```
myapp/
├── epsilon/             # Epsilon runtime
├── src/                 # Your application
└── run.sh               # ./epsilon/epsilon --load src/main.lisp
```

## Troubleshooting

### Common Issues

**"epsilon-core not found"**
- Ensure wrapper and core are in same directory
- Check file permissions

**"Cannot allocate memory"**
- Increase ulimits: `ulimit -s unlimited`
- Check available RAM (needs ~500MB)

**Performance**
- Core image loads faster than source
- First run may be slower (filesystem cache)
- Use `--dynamic-space-size` for large heaps

### Verification

```bash
# Check version
epsilon --eval "(format t \"~A~%\" (epsilon:version))" --quit

# Test library loading  
epsilon --eval "(epsilon.lib.json:encode '((test . ok)))" --quit

# Run tests
epsilon --load scripts/test-distribution.lisp
```
# Epsilon Distribution System

Epsilon provides a distributable SBCL runtime with the Epsilon library preloaded, enabling curl-install deployment and usage as a modern Lisp development environment.

## Installation

### Quick Install (Recommended)

```bash
curl -sSL https://raw.githubusercontent.com/jbouwman/epsilon/main/scripts/install.sh | bash
```

### Manual Installation

1. Download the appropriate release for your platform from [GitHub Releases](https://github.com/jbouwman/epsilon/releases)
2. Extract the archive to your desired location
3. Add the `epsilon` binary to your PATH

Available platforms:
- `epsilon-macos-arm64.tar.gz` - macOS Apple Silicon
- `epsilon-macos-x86_64.tar.gz` - macOS Intel
- `epsilon-linux-x86_64.tar.gz` - Linux x86_64

## Usage

The `epsilon` command provides a complete SBCL runtime with Epsilon preloaded:

```bash
# Interactive REPL
epsilon

# Evaluate expressions
epsilon --eval "(format t \"Hello, Epsilon!\")" --eval "(sb-ext:quit)"

# Use Epsilon libraries
epsilon --eval "(format t \"Map: ~A~%\" (epsilon.lib.map:make-map :a 1 :b 2))" --eval "(sb-ext:quit)"

# Run Lisp files
epsilon --load my-program.lisp
```

## Development Workflow

For development projects using Epsilon:

1. Install Epsilon runtime using the installation script
2. Create your project structure
3. Use Epsilon's built-in tools for development:
   - `epsilon.tool.build` for dependency-tracked builds
   - `epsilon.tool.test` for testing
   - `epsilon.tool.benchmark` for performance measurement

## Building from Source

### Prerequisites

- SBCL (Steel Bank Common Lisp)
- Git
- Standard Unix tools (tar, gzip)

### Build Process

```bash
# Clone the repository
git clone https://github.com/jbouwman/epsilon.git
cd epsilon

# Build runtime package
chmod +x scripts/build-runtime.sh
./scripts/build-runtime.sh

# Test the built runtime
mkdir test-runtime
cd test-runtime
tar -xzf ../target/epsilon-*.tar.gz
./epsilon --eval "(format t \"Build test successful!\")" --eval "(sb-ext:quit)"
```

### Build Outputs

- `target/epsilon-core` - SBCL core image with Epsilon preloaded
- `target/epsilon-{platform}-{arch}.tar.gz` - Complete runtime package
- `target/dist/` - Extracted runtime components

## Architecture

The distribution system consists of:

1. **Core Image**: SBCL core with Epsilon library preloaded
2. **Runtime Package**: Tarball containing:
   - SBCL executable
   - Epsilon core image
   - Wrapper script (`epsilon`)
3. **Installation Script**: Detects platform and installs from GitHub releases
4. **GitHub Actions**: Automated building and releasing for multiple platforms

## Continuous Integration

The project uses GitHub Actions for:
- **CI**: Testing on every push/PR
- **Release**: Building and publishing runtime packages on version tags

To create a new release:
1. Tag the commit: `git tag v1.0.0`
2. Push the tag: `git push origin v1.0.0`
3. GitHub Actions will automatically build and release packages

## Project Structure

```
epsilon/
├── scripts/
│   ├── build-core.lisp      # Core image creation
│   ├── build-runtime.sh     # Runtime packaging
│   └── install.sh           # Installation script
├── .github/workflows/
│   ├── ci.yml              # Continuous integration
│   └── release.yml         # Release automation
└── module/core/            # Epsilon library source
```

## Advantages

This distribution approach provides:

1. **Zero Dependencies**: No need for Quicklisp or ASDF
2. **Fast Startup**: Preloaded libraries eliminate compilation time
3. **Consistent Environment**: Same runtime across development and deployment
4. **Modern Tooling**: Built-in build system, testing, and benchmarking
5. **Easy Distribution**: Single curl command installation
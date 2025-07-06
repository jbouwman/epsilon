# Installation

Epsilon requires SBCL 2.0.0 or later.

## Quick Install

```bash
curl -sSL https://github.com/jbouwman/epsilon/releases/latest/download/install.sh | bash
```

This installs Epsilon to `~/.epsilon` and adds it to your PATH.

## Manual Installation

### From Source

```bash
# Clone repository
git clone https://github.com/jbouwman/epsilon.git
cd epsilon

# Build
./make build

# Install (optional)
./make install PREFIX=/usr/local
```

### Docker

```bash
docker pull ghcr.io/jbouwman/epsilon:latest
docker run -it ghcr.io/jbouwman/epsilon:latest
```

## Platform Notes

### macOS
- Requires macOS 10.12 or later
- Install SBCL via Homebrew: `brew install sbcl`

### Linux
- Tested on Ubuntu 20.04+, Debian 10+, CentOS 8+
- Install SBCL from package manager or build from source

### Windows
- Currently experimental
- Use WSL2 for best compatibility

## Verification

```bash
# Check installation
epsilon --version

# Run REPL
epsilon
```

## Uninstallation

```bash
# If installed via script
rm -rf ~/.epsilon
# Remove from PATH in ~/.bashrc or ~/.zshrc

# If installed manually
make uninstall
```
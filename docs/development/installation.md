# Installation

Epsilon provides pre-built binaries for Linux and macOS. Each release includes SBCL runtime and all core modules.

## Quick Install

```bash
# Install latest release
curl -sSL https://github.com/jbouwman/epsilon/releases/latest/download/install.sh | bash

# Or download the script and run it
wget https://github.com/jbouwman/epsilon/releases/latest/download/install.sh
chmod +x install.sh
./install.sh
```

This installs Epsilon to `~/.epsilon` and creates a symlink in `~/.local/bin`.

## Manual Installation

### From Source

```bash
# Clone repository
git clone https://github.com/jbouwman/epsilon.git
cd epsilon

# Build documentation
./scripts/build.sh

# Install epsilon
./scripts/install.sh
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
# If installed via install script
~/.epsilon/scripts/uninstall.sh

# Or manually
rm -rf ~/.epsilon
rm -f ~/.local/bin/epsilon

# Remove PATH entry from ~/.bashrc or ~/.zshrc if added
```
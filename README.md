# Epsilon

[![CI - Multi-Platform Build](https://github.com/jbouwman/epsilon/actions/workflows/ci.yml/badge.svg)](https://github.com/jbouwman/epsilon/actions/workflows/ci.yml)

Epsilon is a Lisp programming environment built using SBCL that
provides functional data structures and some encoding, cryptographic
hashing and network programming capabilities.

## Installation

### Quick Install (Binary)

Install the latest release:

```bash
curl -sSL https://raw.githubusercontent.com/jbouwman/epsilon/main/install.sh | bash
```

### Install from Source

For development or when binary releases aren't available (requires SBCL):

```bash
curl -sSL https://raw.githubusercontent.com/jbouwman/epsilon/main/install.sh | bash -s -- --source
```

### Install Specific Version

```bash
# Binary release
curl -sSL https://raw.githubusercontent.com/jbouwman/epsilon/main/install.sh | bash -s -- --version 0.11.0

# From source
curl -sSL https://raw.githubusercontent.com/jbouwman/epsilon/main/install.sh | bash -s -- --source --version 0.11.0
```

### Verify Installation

```bash
epsilon --version
```

### Uninstall

```bash
curl -sSL https://raw.githubusercontent.com/jbouwman/epsilon/main/install.sh | bash -s -- --uninstall
```

For more installation options, run `install.sh --help`.

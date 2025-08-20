#!/usr/bin/env bash
#
# Minimal CI Setup Script
# Only handles platform-specific dependency installation
# All other logic moved to epsilon.release module
#

set -euo pipefail

# Detect OS
OS="$(uname -s)"
ARCH="$(uname -m)"

echo "CI Setup for $OS $ARCH"

# Install dependencies based on platform
case "$OS" in
    Linux)
        # Assume Ubuntu/Debian in CI containers
        if command -v apt-get >/dev/null 2>&1; then
            echo "Installing Linux dependencies..."
            apt-get update
            apt-get install -y sbcl libffi-dev libssl-dev build-essential git tar gzip
        fi
        
        # Configure git for CI environment if needed
        if [ -n "${GITHUB_WORKSPACE:-}" ]; then
            git config --global --add safe.directory "$GITHUB_WORKSPACE"
        fi
        ;;
        
    Darwin)
        echo "Installing macOS dependencies..."
        # Install SBCL if not present
        if ! command -v sbcl >/dev/null 2>&1; then
            brew install sbcl
        fi
        
        # Install other dependencies
        brew list libffi >/dev/null 2>&1 || brew install libffi
        brew list openssl@3 >/dev/null 2>&1 || brew install openssl@3
        
        # Link OpenSSL
        brew link openssl@3 --force 2>/dev/null || true
        ;;
        
    *)
        echo "Warning: Unknown OS $OS - skipping dependency installation"
        ;;
esac

# Build libffi extension
if [ -d "modules/foreign/c" ]; then
    echo "Building libffi extension..."
    cd modules/foreign/c
    make clean
    make
    cd - >/dev/null
fi

echo "CI setup complete"
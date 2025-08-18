#!/usr/bin/env bash
#
# Release Generation Script
# Generates installable release archive for current platform
# Assumes tests have already passed
#

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

error() { echo -e "${RED}✗ $1${NC}" >&2; }
success() { echo -e "${GREEN}✓ $1${NC}"; }
info() { echo -e "${BLUE}ℹ $1${NC}"; }

# Auto-detect platform and architecture
detect_platform() {
    local platform=$(uname -s | tr '[:upper:]' '[:lower:]')
    case "$platform" in
        darwin) platform="macos" ;;
        linux) platform="linux" ;;
        *) 
            error "Unsupported platform: $platform"
            exit 1
            ;;
    esac
    echo "$platform"
}

detect_arch() {
    local arch=$(uname -m)
    case "$arch" in
        x86_64|amd64) arch="x86_64" ;;
        arm64|aarch64) arch="arm64" ;;
        *)
            error "Unsupported architecture: $arch"
            exit 1
            ;;
    esac
    echo "$arch"
}

# Parse arguments - only VERSION is required
VERSION="${1:-}"

if [ -z "$VERSION" ]; then
    error "Usage: $0 VERSION"
    echo "Example: $0 0.11.0"
    exit 1
fi

# Auto-detect platform and architecture
PLATFORM=$(detect_platform)
ARCH=$(detect_arch)

echo "========================================="
echo "   Release Generation"
echo "========================================="
echo ""
info "Platform: $PLATFORM (auto-detected)"
info "Architecture: $ARCH (auto-detected)"
info "Version: $VERSION"
echo ""

# Generate distribution package
# The epsilon.release module will be loaded automatically by --exec
info "Generating distribution package..."
if ! ./epsilon --exec epsilon.release:generate -- "$VERSION"; then
    error "Failed to generate distribution package"
    exit 1
fi

# List generated artifacts
echo ""
info "Generated artifacts:"
find releases -type f \( -name "*.tar.gz" -o -name "*.sha256" \) | while read -r file; do
    echo "  - $(basename "$file") ($(du -h "$file" | cut -f1))"
done

echo ""
success "Release generation complete!"
#!/bin/bash
#
# Epsilon Installation Script
#
# This script installs Epsilon (a value-added SBCL runtime) from GitHub releases
# Usage: curl -sSL https://raw.githubusercontent.com/USER/REPO/main/scripts/install.sh | bash
#

set -e

# Configuration
GITHUB_REPO="jbouwman/epsilon"  # Update with actual repo
INSTALL_DIR="${EPSILON_HOME:-$HOME/.epsilon}"
BINARY_DIR="${EPSILON_BIN:-$HOME/.local/bin}"
GITHUB_API_URL="https://api.github.com/repos/$GITHUB_REPO/releases/latest"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Platform detection
detect_platform() {
    local platform=$(uname -s | tr '[:upper:]' '[:lower:]')
    local arch=$(uname -m)
    
    case "$platform" in
        darwin)
            platform="macos"
            ;;
        linux)
            platform="linux"
            ;;
        *)
            echo -e "${RED}Error: Unsupported platform: $platform${NC}"
            exit 1
            ;;
    esac
    
    case "$arch" in
        x86_64)
            arch="x86_64"
            ;;
        arm64|aarch64)
            arch="arm64"
            ;;
        *)
            echo -e "${RED}Error: Unsupported architecture: $arch${NC}"
            exit 1
            ;;
    esac
    
    echo "${platform}-${arch}"
}

# Get latest release info
get_latest_release() {
    local platform_arch="$1"
    local release_info
    
    echo -e "${YELLOW}Fetching latest release information...${NC}"
    
    if command -v curl >/dev/null 2>&1; then
        release_info=$(curl -s "$GITHUB_API_URL")
    elif command -v wget >/dev/null 2>&1; then
        release_info=$(wget -qO- "$GITHUB_API_URL")
    else
        echo -e "${RED}Error: curl or wget is required${NC}"
        exit 1
    fi
    
    # Extract download URL for the platform
    local download_url
    download_url=$(echo "$release_info" | grep -o "\"browser_download_url\":[[:space:]]*\"[^\"]*epsilon-${platform_arch}\.tar\.gz\"" | sed 's/.*"browser_download_url":[[:space:]]*"\([^"]*\)".*/\1/')
    
    if [ -z "$download_url" ]; then
        echo -e "${RED}Error: Could not find release for platform: $platform_arch${NC}"
        echo -e "${YELLOW}Available releases:${NC}"
        echo "$release_info" | grep -o '"name":[[:space:]]*"[^"]*"' | sed 's/.*"name":[[:space:]]*"\([^"]*\)".*/  \1/'
        exit 1
    fi
    
    echo "$download_url"
}

# Download and extract
download_and_extract() {
    local download_url="$1"
    local platform_arch="$2"
    
    echo -e "${YELLOW}Downloading Epsilon runtime...${NC}"
    
    # Create directories
    mkdir -p "$INSTALL_DIR"
    mkdir -p "$BINARY_DIR"
    
    # Download
    local temp_file="/tmp/epsilon-${platform_arch}.tar.gz"
    if command -v curl >/dev/null 2>&1; then
        curl -L "$download_url" -o "$temp_file"
    else
        wget -O "$temp_file" "$download_url"
    fi
    
    # Extract
    echo -e "${YELLOW}Installing to $INSTALL_DIR...${NC}"
    tar -xzf "$temp_file" -C "$INSTALL_DIR"
    
    # Create symlink
    ln -sf "$INSTALL_DIR/epsilon" "$BINARY_DIR/epsilon"
    
    # Cleanup
    rm -f "$temp_file"
    
    echo -e "${GREEN}Epsilon installed successfully!${NC}"
}

# Verify installation
verify_installation() {
    echo -e "${YELLOW}Verifying installation...${NC}"
    
    if "$BINARY_DIR/epsilon" --eval "(format t \"Epsilon ~A installed successfully!~%\" (lisp-implementation-version))" --eval "(sb-ext:quit)" 2>/dev/null; then
        echo -e "${GREEN}Installation verified!${NC}"
    else
        echo -e "${RED}Installation verification failed${NC}"
        exit 1
    fi
}

# Show usage information
show_usage() {
    echo -e "${GREEN}Epsilon Installation Complete!${NC}"
    echo
    echo "Installation directory: $INSTALL_DIR"
    echo "Binary location: $BINARY_DIR/epsilon"
    echo
    echo "Usage:"
    echo "  epsilon --eval \"(format t \\\"Hello, Epsilon!\\\")\" --eval \"(sb-ext:quit)\""
    echo "  epsilon [sbcl-options...]"
    echo
    echo -e "${YELLOW}Note: Make sure $BINARY_DIR is in your PATH${NC}"
    echo "Add this to your shell profile if needed:"
    echo "  export PATH=\"$BINARY_DIR:\$PATH\""
}

# Main installation flow
main() {
    echo -e "${GREEN}Installing Epsilon - A Value-Added SBCL Runtime${NC}"
    echo
    
    # Detect platform
    local platform_arch
    platform_arch=$(detect_platform)
    echo -e "${YELLOW}Detected platform: $platform_arch${NC}"
    
    # Get latest release
    local download_url
    download_url=$(get_latest_release "$platform_arch")
    echo -e "${YELLOW}Download URL: $download_url${NC}"
    
    # Download and install
    download_and_extract "$download_url" "$platform_arch"
    
    # Verify
    verify_installation
    
    # Show usage
    show_usage
}

# Run main function
main "$@"
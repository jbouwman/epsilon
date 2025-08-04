#!/bin/bash
#
# Epsilon Installation Script
#
# This script installs Epsilon (a Common Lisp programming environment) from GitHub releases
# Usage: curl -sSL https://raw.githubusercontent.com/USER/REPO/main/scripts/install.sh | bash
#

set -e

# Configuration
GITHUB_REPO="jbouwman/epsilon"  # Update with actual repo
INSTALL_DIR="${EPSILON_HOME:-$HOME/.epsilon}"
BINARY_DIR="${EPSILON_BIN:-$HOME/.local/bin}"
GITHUB_API_URL="https://api.github.com/repos/$GITHUB_REPO/releases/latest"

# Remove colors - plain text output only

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
        mingw*|msys*|cygwin*)
            platform="windows"
            ;;
        *)
            echo "Error: Unsupported platform: $platform"
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
            echo "Error: Unsupported architecture: $arch"
            exit 1
            ;;
    esac
    
    echo "${platform}-${arch}"
}

# Get latest release info
get_latest_release() {
    local platform_arch="$1"
    local release_info
    
    
    if command -v curl >/dev/null 2>&1; then
        release_info=$(curl -s "$GITHUB_API_URL")
    elif command -v wget >/dev/null 2>&1; then
        release_info=$(wget -qO- "$GITHUB_API_URL")
    else
        echo "Error: curl or wget is required"
        exit 1
    fi
    
    # Extract download URL for the platform
    local download_url
    local file_extension
    
    if [ "$platform_arch" != "${platform_arch#*windows}" ]; then
        file_extension="zip"
    else
        file_extension="tar.gz"
    fi
    
    download_url=$(echo "$release_info" | grep -o "\"browser_download_url\":[[:space:]]*\"[^\"]*epsilon-${platform_arch}\.${file_extension}\"" | sed 's/.*"browser_download_url":[[:space:]]*"\([^"]*\)".*/\1/')
    
    if [ -z "$download_url" ]; then
        echo "Error: Could not find release for platform: $platform_arch"
        echo "Available releases:"
        echo "$release_info" | grep -o '"name":[[:space:]]*"[^"]*"' | sed 's/.*"name":[[:space:]]*"\([^"]*\)".*/  \1/'
        exit 1
    fi
    
    echo "$download_url"
}

# Download and extract
download_and_extract() {
    local download_url="$1"
    local platform_arch="$2"
    
    echo "Downloading Epsilon runtime..."
    
    # Create directories
    mkdir -p "$INSTALL_DIR"
    mkdir -p "$BINARY_DIR"
    
    # Download
    local file_extension
    if [ "$platform_arch" != "${platform_arch#*windows}" ]; then
        file_extension="zip"
    else
        file_extension="tar.gz"
    fi
    
    local temp_file="/tmp/epsilon-${platform_arch}.${file_extension}"
    if command -v curl >/dev/null 2>&1; then
        curl -L "$download_url" -o "$temp_file"
    else
        wget -O "$temp_file" "$download_url"
    fi
    
    # Extract
    echo "Installing to $INSTALL_DIR..."
    if [ "$file_extension" = "zip" ]; then
        unzip "$temp_file" -d "$INSTALL_DIR"
    else
        tar -xzf "$temp_file" -C "$INSTALL_DIR"
    fi
    
    # Create symlink
    if [ "$platform_arch" != "${platform_arch#*windows}" ]; then
        # Windows: create batch file wrapper
        ln -sf "$INSTALL_DIR/epsilon.exe" "$BINARY_DIR/epsilon.exe" 2>/dev/null || \
        cp "$INSTALL_DIR/epsilon.exe" "$BINARY_DIR/epsilon.exe"
    else
        # Unix: create symlink
        ln -sf "$INSTALL_DIR/epsilon" "$BINARY_DIR/epsilon"
    fi
    
    # Cleanup
    rm -f "$temp_file"
    
    echo "Epsilon installed successfully!"
}

# Verify installation
verify_installation() {
    echo "Verifying installation..."
    
    local epsilon_cmd="$BINARY_DIR/epsilon"
    if [ "$platform_arch" != "${platform_arch#*windows}" ]; then
        epsilon_cmd="$BINARY_DIR/epsilon.exe"
    fi
    
    if "$epsilon_cmd" --eval "(format t \"Epsilon ~A installed successfully!~%\" (lisp-implementation-version))" --eval "(sb-ext:quit)" 2>/dev/null; then
        echo "Installation verified!"
    else
        echo "Installation verification failed"
        exit 1
    fi
}

# Show usage information
show_usage() {
    echo "Epsilon Installation Complete!"
    echo
    echo "Installation directory: $INSTALL_DIR"
    echo "Binary location: $BINARY_DIR/epsilon"
    echo
    echo "Usage:"
    echo "  epsilon --eval \"(format t \\\"Hello, Epsilon!\\\")\" --eval \"(sb-ext:quit)\""
    echo "  epsilon [sbcl-options...]"
    echo
    echo "Note: Make sure $BINARY_DIR is in your PATH"
    echo "Add this to your shell profile if needed:"
    echo "  export PATH=\"$BINARY_DIR:\$PATH\""
}

# Main installation flow
main() {
    echo "Installing Epsilon - A Common Lisp Programming Environment"
    echo
    
    # Detect platform
    local platform_arch
    platform_arch=$(detect_platform)
    echo "Detected platform: $platform_arch"
    
    # Get latest release
    local download_url
    download_url=$(get_latest_release "$platform_arch")
    echo "Download URL: $download_url"
    
    # Download and install
    download_and_extract "$download_url" "$platform_arch"
    
    # Verify
    verify_installation
    
    # Show usage
    show_usage
}

# Run main function
main "$@"
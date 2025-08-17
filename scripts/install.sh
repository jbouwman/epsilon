#!/usr/bin/env bash
#
# Epsilon Installation Script
#
# This script installs Epsilon from GitHub releases
# Usage: curl -sSL https://github.com/jbouwman/epsilon/releases/latest/download/install.sh | bash
#

set -euo pipefail

# Configuration
GITHUB_REPO="jbouwman/epsilon"
INSTALL_DIR="${EPSILON_HOME:-$HOME/.epsilon}"
BINARY_DIR="${EPSILON_BIN:-$HOME/.local/bin}"
GITHUB_API_URL="https://api.github.com/repos/$GITHUB_REPO/releases/latest"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Functions for colored output
error() { echo -e "${RED}✗ $1${NC}" >&2; }
success() { echo -e "${GREEN}✓ $1${NC}"; }
warning() { echo -e "${YELLOW}⚠ $1${NC}"; }
info() { echo -e "${BLUE}ℹ $1${NC}"; }

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
            error "Windows is not yet supported"
            echo "Please use WSL2 for Windows installation"
            exit 1
            ;;
        *)
            error "Unsupported platform: $platform"
            exit 1
            ;;
    esac
    
    case "$arch" in
        x86_64|amd64)
            arch="x86_64"
            ;;
        arm64|aarch64)
            arch="arm64"
            ;;
        *)
            error "Unsupported architecture: $arch"
            exit 1
            ;;
    esac
    
    echo "${platform}-${arch}"
}

# Check for required tools
check_requirements() {
    local missing_tools=()
    
    if ! command -v curl >/dev/null 2>&1 && ! command -v wget >/dev/null 2>&1; then
        missing_tools+=("curl or wget")
    fi
    
    if ! command -v tar >/dev/null 2>&1; then
        missing_tools+=("tar")
    fi
    
    if [ ${#missing_tools[@]} -gt 0 ]; then
        error "Missing required tools: ${missing_tools[*]}"
        echo "Please install them and try again."
        exit 1
    fi
}

# Get latest release info
get_latest_release() {
    local platform_arch="$1"
    local release_info
    
    info "Fetching latest release information..."
    
    if command -v curl >/dev/null 2>&1; then
        release_info=$(curl -sSL "$GITHUB_API_URL")
    else
        release_info=$(wget -qO- "$GITHUB_API_URL")
    fi
    
    # Check if we got valid JSON
    if ! echo "$release_info" | grep -q '"tag_name"'; then
        error "Failed to fetch release information from GitHub"
        echo "Response: $release_info"
        exit 1
    fi
    
    # Extract version
    local version=$(echo "$release_info" | grep -o '"tag_name":[[:space:]]*"[^"]*"' | sed 's/.*"tag_name":[[:space:]]*"v\?\([^"]*\)".*/\1/')
    info "Latest version: $version"
    
    # Extract download URL for the platform
    local download_url
    download_url=$(echo "$release_info" | grep -o "\"browser_download_url\":[[:space:]]*\"[^\"]*epsilon-[^\"]*${platform_arch}\.tar\.gz\"" | sed 's/.*"browser_download_url":[[:space:]]*"\([^"]*\)".*/\1/' | head -1)
    
    if [ -z "$download_url" ]; then
        error "Could not find release for platform: $platform_arch"
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
    
    info "Downloading Epsilon..."
    
    # Create temp directory
    local temp_dir=$(mktemp -d)
    trap "rm -rf $temp_dir" EXIT
    
    # Download
    local archive_file="$temp_dir/epsilon.tar.gz"
    if command -v curl >/dev/null 2>&1; then
        curl -sSL "$download_url" -o "$archive_file"
    else
        wget -qO "$archive_file" "$download_url"
    fi
    
    success "Download complete"
    
    # Extract to temp directory first
    info "Extracting archive..."
    tar -xzf "$archive_file" -C "$temp_dir"
    
    # Find the extracted directory (should be epsilon-VERSION-PLATFORM)
    local extracted_dir=$(find "$temp_dir" -maxdepth 1 -type d -name "epsilon-*" | head -1)
    if [ -z "$extracted_dir" ]; then
        error "Failed to find extracted directory"
        exit 1
    fi
    
    # Clean up existing installation
    if [ -d "$INSTALL_DIR" ]; then
        warning "Existing installation found at $INSTALL_DIR"
        read -p "Remove existing installation? (y/N) " -n 1 -r < /dev/tty
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            rm -rf "$INSTALL_DIR"
        else
            error "Installation cancelled"
            exit 1
        fi
    fi
    
    # Move to installation directory
    mkdir -p "$(dirname "$INSTALL_DIR")"
    mv "$extracted_dir" "$INSTALL_DIR"
    
    success "Extracted to $INSTALL_DIR"
}

# Create symlinks
create_symlinks() {
    info "Creating symlinks..."
    
    # Create binary directory if it doesn't exist
    mkdir -p "$BINARY_DIR"
    
    # Create symlink to epsilon
    local epsilon_bin="$INSTALL_DIR/bin/epsilon"
    local symlink_path="$BINARY_DIR/epsilon"
    
    if [ ! -f "$epsilon_bin" ]; then
        error "Epsilon binary not found at $epsilon_bin"
        exit 1
    fi
    
    # Remove existing symlink if present
    [ -L "$symlink_path" ] && rm "$symlink_path"
    
    # Create new symlink
    ln -sf "$epsilon_bin" "$symlink_path"
    
    success "Created symlink at $symlink_path"
}

# Update shell configuration
update_shell_config() {
    info "Checking PATH configuration..."
    
    # Check if binary directory is in PATH
    if [[ ":$PATH:" != *":$BINARY_DIR:"* ]]; then
        warning "$BINARY_DIR is not in your PATH"
        
        # Detect shell configuration file
        local shell_config=""
        if [ -n "${BASH_VERSION:-}" ]; then
            shell_config="$HOME/.bashrc"
        elif [ -n "${ZSH_VERSION:-}" ]; then
            shell_config="$HOME/.zshrc"
        else
            shell_config="$HOME/.profile"
        fi
        
        echo ""
        echo "Add the following to your $shell_config:"
        echo ""
        echo "  export PATH=\"$BINARY_DIR:\$PATH\""
        echo ""
        echo "Then reload your shell configuration:"
        echo "  source $shell_config"
    else
        success "PATH is correctly configured"
    fi
}

# Verify installation
verify_installation() {
    info "Verifying installation..."
    
    local epsilon_cmd="$BINARY_DIR/epsilon"
    
    if [ ! -x "$epsilon_cmd" ]; then
        error "Epsilon executable not found or not executable at $epsilon_cmd"
        exit 1
    fi
    
    # Test epsilon with a simple command
    if "$epsilon_cmd" --eval "(+ 1 2)" >/dev/null 2>&1; then
        success "Installation verified successfully!"
        
        # Show version
        local version=$("$epsilon_cmd" --version 2>&1 | head -1 || echo "unknown")
        info "Installed version: $version"
    else
        error "Installation verification failed"
        echo "Try running: $epsilon_cmd --help"
        exit 1
    fi
}

# Show usage information
show_usage() {
    echo ""
    echo "========================================="
    echo "   Epsilon Installation Complete!"
    echo "========================================="
    echo ""
    success "Installation directory: $INSTALL_DIR"
    success "Binary location: $BINARY_DIR/epsilon"
    echo ""
    echo "Quick test:"
    echo "  epsilon --eval '(format t \"Hello from Epsilon!~%\")'"
    echo ""
    echo "Start REPL:"
    echo "  epsilon"
    echo ""
    echo "List modules:"
    echo "  epsilon --modules"
    echo ""
    echo "Run tests:"
    echo "  epsilon --test epsilon.core"
    echo ""
    echo "For more information:"
    echo "  epsilon --help"
    echo "  https://github.com/$GITHUB_REPO"
}

# Main installation flow
main() {
    echo "========================================="
    echo "   Epsilon Installation"
    echo "========================================="
    echo ""
    
    # Check requirements
    check_requirements
    
    # Detect platform
    local platform_arch
    platform_arch=$(detect_platform)
    success "Detected platform: $platform_arch"
    
    # Get latest release
    local download_url
    download_url=$(get_latest_release "$platform_arch")
    success "Found release: $(basename "$download_url")"
    
    # Download and install
    download_and_extract "$download_url" "$platform_arch"
    
    # Create symlinks
    create_symlinks
    
    # Update PATH if needed
    update_shell_config
    
    # Verify
    verify_installation
    
    # Show usage
    show_usage
}

# Handle errors
trap 'error "Installation failed on line $LINENO"' ERR

# Run main function
main "$@"
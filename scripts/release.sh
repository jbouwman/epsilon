#!/usr/bin/env bash
#
# Epsilon Release Script
#
# Creates an installable package for the current build platform containing:
# - The epsilon binary/wrapper
# - Complete source repository with prebuilt platform-compatible packages
# - A local copy of SBCL and its core image
#

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
DEFAULT_OUTPUT_DIR="$PROJECT_ROOT/releases"

# Default values
VERSION=""
OUTPUT_DIR="$DEFAULT_OUTPUT_DIR"

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

# Get version from VERSION file, git tag, or default
get_version() {
    local version=""
    
    # Try VERSION file first
    if [[ -f "$PROJECT_ROOT/VERSION" ]]; then
        version=$(cat "$PROJECT_ROOT/VERSION" | tr -d '\n\r ')
    fi
    
    # Try git tag if no VERSION file
    if [[ -z "$version" ]] && command -v git >/dev/null 2>&1; then
        if git rev-parse --git-dir >/dev/null 2>&1; then
            version=$(git describe --tags --abbrev=0 2>/dev/null || echo "")
        fi
    fi
    
    # Default version
    if [[ -z "$version" ]]; then
        version="dev-$(date +%Y%m%d)"
    fi
    
    echo "$version"
}

# Show usage information
show_usage() {
    echo "Usage: $0 [OPTIONS]"
    echo
    echo "Creates an installable Epsilon package for the current platform."
    echo
    echo "Options:"
    echo "  --version VERSION    Specify release version (default: from VERSION file or git)"
    echo "  --output DIR         Output directory for release archive (default: releases/)"
    echo "  --help              Show this help message"
    echo
    echo "Examples:"
    echo "  $0                           # Create release with default version"
    echo "  $0 --version 1.2.3          # Create release with specific version"
    echo "  $0 --output ~/my-releases/   # Output to custom directory"
}

# Parse command line arguments
parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --version)
                VERSION="$2"
                shift 2
                ;;
            --output)
                OUTPUT_DIR="$2"
                shift 2
                ;;
            --help)
                show_usage
                exit 0
                ;;
            *)
                echo -e "${RED}Error: Unknown option: $1${NC}"
                show_usage
                exit 1
                ;;
        esac
    done
}

# Verify prerequisites
verify_prerequisites() {
    echo -e "${BLUE}Verifying prerequisites...${NC}"
    
    # Check if we're in the right directory
    if [[ ! -f "$PROJECT_ROOT/epsilon" ]]; then
        echo -e "${RED}Error: epsilon binary not found in $PROJECT_ROOT${NC}"
        echo "Please run this script from the Epsilon project root."
        exit 1
    fi
    
    # Check if SBCL is available
    if ! command -v sbcl >/dev/null 2>&1; then
        echo -e "${RED}Error: SBCL not found in PATH${NC}"
        echo "SBCL is required to create the release."
        exit 1
    fi
    
    # Verify epsilon works
    if ! "$PROJECT_ROOT/epsilon" --eval "(format t \"OK\")" --eval "(sb-ext:quit)" >/dev/null 2>&1; then
        echo -e "${RED}Error: epsilon command failed${NC}"
        echo "Please ensure Epsilon is working correctly before creating a release."
        exit 1
    fi
    
    echo -e "${GREEN}Prerequisites verified${NC}"
}

# Build all platform-compatible packages
build_packages() {
    echo -e "${BLUE}Building all platform-compatible packages...${NC}"
    
    cd "$PROJECT_ROOT"
    
    # Get list of packages to build for this platform
    local platform_arch
    platform_arch=$(detect_platform)
    local packages
    packages=($(get_platform_packages "$platform_arch"))
    
    # Build each package
    for package in "${packages[@]}"; do
        if [[ -d "src/$package" ]]; then
            echo "Building epsilon.$package..."
            if ! ./epsilon build "epsilon.$package"; then
                echo -e "${YELLOW}Warning: Failed to build epsilon.$package (may not exist for this platform)${NC}"
            fi
        fi
    done
    
    echo -e "${GREEN}Package build completed${NC}"
}

# Get platform-specific packages to include
get_platform_packages() {
    local platform_arch="$1"
    local platform="${platform_arch%-*}"
    
    # Base packages (always included)
    local packages=(
        "core"
        "test" 
        "foreign"
        "json"
        "yaml"
        "msgpack"
        "http"
        "crypto"
        "regex"
        "xml"
        "parsing"
        "format"
        "web"
        "websocket"
        "tls"
        "net"
        "monitor"
        "benchmark"
        "coverage"
        "catalog"
        "lsp"
    )
    
    # Add platform-specific packages
    case "$platform" in
        linux)
            packages+=("linux")
            ;;
        macos)
            packages+=("darwin")
            ;;
        windows)
            packages+=("windows")
            ;;
    esac
    
    printf '%s\n' "${packages[@]}"
}

# Create SBCL bundle
create_sbcl_bundle() {
    local release_dir="$1"
    
    echo -e "${BLUE}Creating SBCL bundle...${NC}"
    
    # Find SBCL binary and info
    local sbcl_bin=$(which sbcl)
    local sbcl_home=$(sbcl --noinform --no-sysinit --no-userinit --eval "(princ (sb-ext:posix-getenv \"SBCL_HOME\"))" --eval "(sb-ext:quit)" 2>/dev/null || echo "")
    
    if [[ -z "$sbcl_home" ]]; then
        # Try to find SBCL_HOME from the binary location
        sbcl_home=$(dirname "$sbcl_bin")/../lib/sbcl
        if [[ ! -d "$sbcl_home" ]]; then
            sbcl_home=$(dirname "$sbcl_bin")/../share/sbcl
        fi
    fi
    
    echo "SBCL binary: $sbcl_bin"
    echo "SBCL home: $sbcl_home"
    
    # Create directories
    mkdir -p "$release_dir/bin"
    mkdir -p "$release_dir/lib/sbcl-libs"
    
    # Copy SBCL binary
    cp "$sbcl_bin" "$release_dir/bin/"
    
    # Copy SBCL libraries if they exist
    if [[ -d "$sbcl_home" ]]; then
        cp -r "$sbcl_home"/* "$release_dir/lib/sbcl-libs/"
    fi
    
    # Create custom Epsilon core image
    echo -e "${BLUE}Creating Epsilon core image...${NC}"
    cd "$PROJECT_ROOT"
    
    # Create core build script
    cat > /tmp/build-core.lisp << 'EOF'
(load "scripts/boot.lisp")
(boot)
(epsilon.tool.build:build "epsilon.core")
(sb-ext:save-lisp-and-die "epsilon.core" :executable t :compression t)
EOF
    
    # Build the core
    if ! sbcl --noinform --no-sysinit --no-userinit --load /tmp/build-core.lisp; then
        echo -e "${YELLOW}Warning: Failed to create custom core, using default SBCL${NC}"
    else
        mv epsilon.core "$release_dir/lib/"
        echo -e "${GREEN}Custom Epsilon core created${NC}"
    fi
    
    # Cleanup
    rm -f /tmp/build-core.lisp
}

# Copy source tree and prebuilt packages
copy_source_and_packages() {
    local release_dir="$1"
    local platform_arch="$2"
    
    echo -e "${BLUE}Copying source tree and prebuilt packages...${NC}"
    
    # Copy entire source tree
    cp -r "$PROJECT_ROOT/src" "$release_dir/"
    
    # Copy scripts
    cp -r "$PROJECT_ROOT/scripts" "$release_dir/"
    
    # Copy documentation and other files
    cp "$PROJECT_ROOT/README.md" "$release_dir/" 2>/dev/null || true
    cp "$PROJECT_ROOT/LICENSE" "$release_dir/" 2>/dev/null || true
    cp "$PROJECT_ROOT/VERSION" "$release_dir/" 2>/dev/null || true
    
    echo -e "${GREEN}Source tree and packages copied${NC}"
}

# Create relocatable epsilon wrapper
create_epsilon_wrapper() {
    local release_dir="$1"
    local platform_arch="$2"
    local platform="${platform_arch%-*}"
    
    echo -e "${BLUE}Creating epsilon wrapper...${NC}"
    
    if [[ "$platform" == "windows" ]]; then
        # Create Windows batch file
        cat > "$release_dir/bin/epsilon.bat" << 'EOF'
@echo off
setlocal

REM Get the directory where this script is located
set "EPSILON_HOME=%~dp0.."
set "EPSILON_USER=%CD%"

REM Set SBCL_HOME to our bundled libraries
set "SBCL_HOME=%EPSILON_HOME%\lib\sbcl-libs"

REM Check if custom core exists
if exist "%EPSILON_HOME%\lib\epsilon.core" (
    set "SBCL_CORE=--core %EPSILON_HOME%\lib\epsilon.core"
) else (
    set "SBCL_CORE="
)

REM Run SBCL with Epsilon
cd /d "%EPSILON_HOME%"
"%EPSILON_HOME%\bin\sbcl.exe" %SBCL_CORE% --noinform --no-sysinit --no-userinit --disable-debugger --quit --load "%EPSILON_HOME%\scripts\epsilon.lisp" --eval "(epsilon.main:cli-run)" -- %*
EOF
    else
        # Create Unix shell script
        cat > "$release_dir/bin/epsilon" << 'EOF'
#!/usr/bin/env bash
#
# Epsilon command-line interface (Bundled Release)
#

set -eu

# Get the directory where this script is located
EPSILON_HOME="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
export EPSILON_USER="$PWD"

# Set SBCL_HOME to our bundled libraries
export SBCL_HOME="$EPSILON_HOME/lib/sbcl-libs"

# Epsilon boot script
EPSILON_BOOT="$EPSILON_HOME/scripts/epsilon.lisp"

# Check if custom core exists
if [[ -f "$EPSILON_HOME/lib/epsilon.core" ]]; then
    SBCL_CORE="--core $EPSILON_HOME/lib/epsilon.core"
else
    SBCL_CORE=""
fi

# Parse debug and rebuild flags
DEBUG_MODE="false"
declare -a FILTERED_ARGS=()
for arg in "$@"; do
    case "$arg" in
        --debug)
            DEBUG_MODE="true"
            ;;
        *)
            FILTERED_ARGS+=("$arg")
            ;;
    esac
done

SBCL_RUNTIME_ARGS="--noinform"
if [[ "$DEBUG_MODE" == "true" ]]; then
    SBCL_TOPLEVEL_ARGS="--no-sysinit --no-userinit --quit"
else
    SBCL_TOPLEVEL_ARGS="--no-sysinit --no-userinit --disable-debugger --quit"
fi

cd "$EPSILON_HOME"
if [[ ${#FILTERED_ARGS[@]} -eq 0 ]]; then
    exec "$EPSILON_HOME/bin/sbcl" $SBCL_CORE $SBCL_RUNTIME_ARGS \
         $SBCL_TOPLEVEL_ARGS \
         --load "$EPSILON_BOOT" \
         --eval "(epsilon.main:cli-run)" \
         --
else
    exec "$EPSILON_HOME/bin/sbcl" $SBCL_CORE $SBCL_RUNTIME_ARGS \
         $SBCL_TOPLEVEL_ARGS \
         --load "$EPSILON_BOOT" \
         --eval "(epsilon.main:cli-run)" \
         -- "${FILTERED_ARGS[@]}"
fi
EOF
        chmod +x "$release_dir/bin/epsilon"
    fi
    
    echo -e "${GREEN}Epsilon wrapper created${NC}"
}

# Create installation instructions
create_install_instructions() {
    local release_dir="$1"
    local platform_arch="$2"
    local release_name="$3"
    
    echo -e "${BLUE}Creating installation instructions...${NC}"
    
    cat > "$release_dir/INSTALL.txt" << EOF
Epsilon ${VERSION} Installation Instructions
==========================================

This is a self-contained Epsilon release for ${platform_arch}.

Installation:
1. Extract this archive to your desired location
2. Add the bin/ directory to your PATH, or create a symlink to bin/epsilon

Example installation to /opt/epsilon:
    sudo tar -xzf ${release_name}.tar.gz -C /opt/
    sudo ln -sf /opt/${release_name}/bin/epsilon /usr/local/bin/epsilon

Or install to your home directory:
    tar -xzf ${release_name}.tar.gz -C ~/
    echo 'export PATH="\$HOME/${release_name}/bin:\$PATH"' >> ~/.bashrc

Usage:
    epsilon --eval "(format t \"Hello, Epsilon!\")" --eval "(sb-ext:quit)"
    epsilon build epsilon.core
    epsilon test epsilon.core

This release includes:
- Complete Epsilon source code
- Prebuilt packages for ${platform_arch}
- Bundled SBCL runtime
- All development tools

For more information, see README.md or visit:
https://github.com/jbouwman/epsilon
EOF
    
    echo -e "${GREEN}Installation instructions created${NC}"
}

# Create release archive
create_archive() {
    local release_dir="$1"
    local release_name="$2"
    local platform_arch="$3"
    local platform="${platform_arch%-*}"
    
    echo -e "${BLUE}Creating release archive...${NC}"
    
    cd "$(dirname "$release_dir")"
    
    if [[ "$platform" == "windows" ]]; then
        # Create ZIP archive for Windows
        local archive_name="${release_name}.zip"
        if command -v zip >/dev/null 2>&1; then
            zip -r "$archive_name" "$(basename "$release_dir")" -x "*/.*"
        else
            echo -e "${RED}Error: zip command not found${NC}"
            exit 1
        fi
    else
        # Create tar.gz archive for Unix
        local archive_name="${release_name}.tar.gz"
        tar -czf "$archive_name" --exclude="*/.*" "$(basename "$release_dir")"
    fi
    
    # Create checksum
    sha256sum "$archive_name" > "${archive_name}.sha256"
    
    echo -e "${GREEN}Release archive created: $archive_name${NC}"
    echo -e "${GREEN}Checksum created: ${archive_name}.sha256${NC}"
    
    # Show final information
    local size=$(du -h "$archive_name" | cut -f1)
    echo
    echo -e "${GREEN}Release Complete!${NC}"
    echo "===================="
    echo "Archive: $archive_name"
    echo "Size: $size"
    echo "Location: $(pwd)/$archive_name"
}

# Main function
main() {
    echo -e "${GREEN}Epsilon Release Builder${NC}"
    echo "=============================="
    echo
    
    # Parse arguments
    parse_args "$@"
    
    # Verify prerequisites
    verify_prerequisites
    
    # Detect platform
    local platform_arch
    platform_arch=$(detect_platform)
    echo -e "${BLUE}Building for platform: $platform_arch${NC}"
    
    # Get version
    if [[ -z "$VERSION" ]]; then
        VERSION=$(get_version)
    fi
    echo -e "${BLUE}Release version: $VERSION${NC}"
    
    # Create release name
    local release_name="epsilon-${VERSION}-${platform_arch}"
    echo -e "${BLUE}Release name: $release_name${NC}"
    echo
    
    # Create output directory
    mkdir -p "$OUTPUT_DIR"
    local release_dir="$OUTPUT_DIR/$release_name"
    
    # Clean up any existing release directory
    if [[ -d "$release_dir" ]]; then
        echo -e "${YELLOW}Removing existing release directory...${NC}"
        rm -rf "$release_dir"
    fi
    
    # Create release directory
    mkdir -p "$release_dir"
    
    # Build packages
    build_packages
    
    # Create SBCL bundle
    create_sbcl_bundle "$release_dir"
    
    # Copy source and packages
    copy_source_and_packages "$release_dir" "$platform_arch"
    
    # Create wrapper
    create_epsilon_wrapper "$release_dir" "$platform_arch"
    
    # Create installation instructions
    create_install_instructions "$release_dir" "$platform_arch" "$release_name"
    
    # Create archive
    create_archive "$release_dir" "$release_name" "$platform_arch"
}

# Run main function with all arguments
main "$@"
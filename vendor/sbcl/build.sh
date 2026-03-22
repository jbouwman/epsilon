#!/usr/bin/env bash
#
# build.sh - Rebuild vendored SBCL from jbouwman/sbcl fork
#
# This script builds SBCL from the jbouwman/sbcl fork which includes
# extended C-call convention support (struct-by-value returns and callbacks).
#
# Usage:
#   ./epsilon/vendor/sbcl/build.sh [--clean] [--branch BRANCH]
#
# Options:
#   --clean     Remove existing SBCL source and clone fresh
#   --branch    Branch to build (default: master)
#
# Platform behavior:
#   - Linux: Builds in a Debian container to ensure standard interpreter paths
#            (avoids NixOS-specific dynamic linker paths)
#   - macOS: Builds natively on the host
#
# The script will:
#   1. Clone/update jbouwman/sbcl repository
#   2. Build SBCL with --fancy and --with-sb-core-compression
#   3. Build required contribs (sb-posix, sb-rotate-byte)
#   4. Copy binaries to <platform>/ subdirectory
#   5. Update VERSION file with git commit hash

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENDOR_DIR="$SCRIPT_DIR"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
BUILD_DIR="$REPO_ROOT/.sbcl-build"
SBCL_REPO="https://github.com/jbouwman/sbcl.git"

# Default options
CLEAN=false
BRANCH="master"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --clean)
            CLEAN=true
            shift
            ;;
        --branch)
            BRANCH="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 [--clean] [--branch BRANCH]"
            echo ""
            echo "Options:"
            echo "  --clean     Remove existing SBCL source and clone fresh"
            echo "  --branch    Branch to build (default: master)"
            echo ""
            echo "Platform behavior:"
            echo "  Linux:  Builds in Debian container (portable binary)"
            echo "  macOS:  Builds natively on host"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Detect platform
detect_platform() {
    local os=$(uname -s)
    local arch=$(uname -m)

    case "$os" in
        Linux)
            case "$arch" in
                x86_64)
                    echo "linux-x86_64"
                    ;;
                *)
                    echo "Unsupported Linux architecture: $arch" >&2
                    exit 1
                    ;;
            esac
            ;;
        Darwin)
            case "$arch" in
                arm64)
                    echo "darwin-arm64"
                    ;;
                x86_64)
                    echo "darwin-x86_64"
                    ;;
                *)
                    echo "Unsupported macOS architecture: $arch" >&2
                    exit 1
                    ;;
            esac
            ;;
        *)
            echo "Unsupported operating system: $os" >&2
            exit 1
            ;;
    esac
}

# Build SBCL for Linux using a Debian container (ensures portable binary)
build_linux_container() {
    local platform="linux-x86_64"
    local target_dir="$VENDOR_DIR/$platform"

    # Detect container runtime
    local container_cmd=""
    if command -v podman &> /dev/null; then
        container_cmd="podman"
    elif command -v docker &> /dev/null; then
        container_cmd="docker"
    else
        echo "Error: Neither podman nor docker found" >&2
        echo "Container build is required for portable Linux binaries" >&2
        exit 1
    fi

    echo "Container runtime: $container_cmd"
    echo ""

    # Create temporary directory for build output
    local build_output=$(mktemp -d)
    trap "rm -rf $build_output" EXIT

    echo "Building SBCL in Debian container..."

    # Run build in Debian container
    $container_cmd run --rm \
        -v "$build_output:/output" \
        -e BRANCH="$BRANCH" \
        debian:bookworm bash -c '
set -ex

# Install build dependencies (excluding sbcl - we download a recent binary)
apt-get update
apt-get install -y --no-install-recommends \
    git \
    make \
    gcc \
    g++ \
    zlib1g-dev \
    libzstd-dev \
    ca-certificates \
    curl \
    bzip2

# Download recent SBCL binary for bootstrapping (Debian sbcl is too old for v2.6+ sources)
# The packaged sbcl (~v2.2) lacks IN-OUTER-ALIEN-STACK-CLEANUP-CONTEXT-P needed by newer SBCL
echo "Downloading SBCL 2.4.11 for bootstrap..."
curl -L -o /tmp/sbcl.tar.bz2 \
    "https://downloads.sourceforge.net/project/sbcl/sbcl/2.4.11/sbcl-2.4.11-x86-64-linux-binary.tar.bz2"
cd /tmp && tar xf sbcl.tar.bz2
cd /tmp/sbcl-2.4.11-x86-64-linux
INSTALL_ROOT=/usr/local sh install.sh
cd /tmp
rm -rf sbcl.tar.bz2 sbcl-2.4.11-x86-64-linux

# Clone repository
cd /tmp
git clone '"$SBCL_REPO"' sbcl
cd sbcl
git checkout "$BRANCH"

COMMIT=$(git rev-parse HEAD)
SHORT_HASH=$(git rev-parse --short HEAD)
COMMIT_COUNT=$(git rev-list --count HEAD)

# Generate version
VERSION="2.6.0.${COMMIT_COUNT}.${SHORT_HASH}.epsilon"
echo "\"$VERSION\"" > version.lisp-expr
echo "Building version: $VERSION"

# Build SBCL
sh make.sh --fancy --with-sb-core-compression

# Copy output
cp src/runtime/sbcl /output/
cp output/sbcl.core /output/

# Copy all contribs (--fancy builds them all)
mkdir -p /output/contrib
if [ -d "obj/sbcl-home/contrib" ]; then
    cp obj/sbcl-home/contrib/*.fasl /output/contrib/ 2>/dev/null || true
    cp obj/sbcl-home/contrib/*.asd /output/contrib/ 2>/dev/null || true
    echo "Copied contribs:"
    ls /output/contrib/ | head -20
fi

# Write version info
cat > /output/VERSION << EOF
# SBCL vendored build
# Built: $(date -u +"%Y-%m-%d %H:%M:%S UTC")
# Platform: linux-x86_64
# Branch: $BRANCH
# Commit: $COMMIT
$COMMIT
EOF

# Verify interpreter
echo "Binary interpreter:"
readelf -l /output/sbcl | grep interpreter || true

echo "Container build complete!"
'

    echo ""
    echo "Installing to vendor directory..."

    # Copy from build output to vendor directory
    mkdir -p "$target_dir/contrib"
    cp "$build_output/sbcl" "$target_dir/"
    cp "$build_output/sbcl.core" "$target_dir/"
    cp "$build_output/VERSION" "$target_dir/"

    if [ -d "$build_output/contrib" ]; then
        cp -r "$build_output/contrib/"* "$target_dir/contrib/" 2>/dev/null || true
    fi

    # Verify installation
    echo ""
    echo "Verifying installation..."
    echo "Binary interpreter:"
    readelf -l "$target_dir/sbcl" | grep interpreter || file "$target_dir/sbcl"

    echo ""
    echo "Installed files:"
    ls -la "$target_dir/"

    echo ""
    echo "Testing SBCL..."
    export SBCL_HOME="$target_dir"
    "$target_dir/sbcl" --version || echo "Note: SBCL may not run on this host if interpreter differs"
}

# Check for required tools (native build)
check_requirements() {
    local missing=()

    if ! command -v git &> /dev/null; then
        missing+=("git")
    fi

    if ! command -v sbcl &> /dev/null; then
        # Check if we have a vendored SBCL we can use as bootstrap
        local platform=$(detect_platform)
        if [[ ! -x "$VENDOR_DIR/$platform/sbcl" ]]; then
            missing+=("sbcl (needed as bootstrap compiler)")
        fi
    fi

    if [[ ${#missing[@]} -gt 0 ]]; then
        echo "Missing required tools: ${missing[*]}" >&2
        exit 1
    fi
}

# Get SBCL for bootstrapping (native build)
get_bootstrap_sbcl() {
    if command -v sbcl &> /dev/null; then
        echo "sbcl"
    else
        local platform=$(detect_platform)
        echo "$VENDOR_DIR/$platform/sbcl"
    fi
}

# Clone or update repository (native build)
setup_repository() {
    if [[ "$CLEAN" == "true" ]] && [[ -d "$BUILD_DIR" ]]; then
        echo "Removing existing build directory..."
        rm -rf "$BUILD_DIR"
    fi

    if [[ ! -d "$BUILD_DIR" ]]; then
        echo "Cloning $SBCL_REPO..."
        git clone "$SBCL_REPO" "$BUILD_DIR"
    else
        echo "Updating existing repository..."
        cd "$BUILD_DIR"
        git fetch origin
    fi

    cd "$BUILD_DIR"
    echo "Checking out branch: $BRANCH"
    git checkout "$BRANCH"
    git pull origin "$BRANCH" || true

    echo "Current commit: $(git rev-parse HEAD)"
}

# Generate version.lisp-expr with epsilon suffix (native build)
generate_version() {
    cd "$BUILD_DIR"

    local short_hash=$(git rev-parse --short HEAD)
    local commit_count=$(git rev-list --count HEAD)

    local version="2.6.0.${commit_count}.${short_hash}.epsilon"

    echo "Generating version: $version"
    echo "\"$version\"" > version.lisp-expr
}

# Build SBCL natively
build_sbcl_native() {
    cd "$BUILD_DIR"

    local bootstrap_sbcl=$(get_bootstrap_sbcl)
    echo "Using bootstrap SBCL: $bootstrap_sbcl"

    # Clean previous build
    if [[ -f "output/sbcl.core" ]]; then
        echo "Cleaning previous build..."
        sh clean.sh || true
    fi

    echo "Building SBCL..."
    echo "  Options: --fancy --with-sb-core-compression"

    # Set SBCL_HOME for bootstrap if using vendored
    if [[ "$bootstrap_sbcl" != "sbcl" ]]; then
        local platform=$(detect_platform)
        export SBCL_HOME="$VENDOR_DIR/$platform"
    fi

    sh make.sh --fancy --with-sb-core-compression

    echo "SBCL build complete!"
}

# Install to vendor directory (native build)
install_vendor() {
    local platform=$(detect_platform)
    local target_dir="$VENDOR_DIR/$platform"

    echo "Installing to $target_dir..."

    # Create target directory
    mkdir -p "$target_dir/contrib"

    # Copy main binaries
    cp "$BUILD_DIR/src/runtime/sbcl" "$target_dir/"
    cp "$BUILD_DIR/output/sbcl.core" "$target_dir/"

    # Find and copy all contribs from obj/sbcl-home/contrib (where --fancy puts them)
    local contrib_src="$BUILD_DIR/obj/sbcl-home/contrib"

    echo "Looking for contribs in $contrib_src..."
    if [[ -d "$contrib_src" ]]; then
        echo "Found contrib directory"

        # Copy all .fasl and .asd files (same as container build)
        cp "$contrib_src"/*.fasl "$target_dir/contrib/" 2>/dev/null || true
        cp "$contrib_src"/*.asd "$target_dir/contrib/" 2>/dev/null || true

        echo "Copied contribs:"
        ls "$target_dir/contrib/" | wc -l | xargs echo "  Total files:"
    else
        echo "ERROR: Contrib directory not found at $contrib_src"
        find "$BUILD_DIR" -name "sb-posix.fasl" -o -name "sb-posix--system.fasl" 2>/dev/null | head -5
    fi

    # Update VERSION file
    cd "$BUILD_DIR"
    local commit=$(git rev-parse HEAD)
    local branch=$(git rev-parse --abbrev-ref HEAD)
    local date=$(date -u +"%Y-%m-%d %H:%M:%S UTC")

    cat > "$target_dir/VERSION" << EOF
# SBCL vendored build
# Built: $date
# Platform: $platform
# Branch: $branch
# Commit: $commit
$commit
EOF

    echo "Installation complete!"
    echo ""
    echo "Installed files:"
    ls -la "$target_dir/"
    echo ""
    echo "Contribs:"
    ls -la "$target_dir/contrib/" 2>/dev/null || echo "  (none)"
}

# Verify installation (native build)
verify_install() {
    local platform=$(detect_platform)
    local sbcl="$VENDOR_DIR/$platform/sbcl"

    echo "Verifying installation..."

    if [[ ! -x "$sbcl" ]]; then
        echo "ERROR: SBCL executable not found or not executable" >&2
        exit 1
    fi

    export SBCL_HOME="$VENDOR_DIR/$platform"
    local version=$("$sbcl" --version 2>&1)
    echo "SBCL version: $version"

    # Test struct-by-value callback support
    echo "Testing struct-by-value callback support..."
    local test_result=$("$sbcl" --noinform --non-interactive --eval '
(handler-case
    (progn
      (sb-alien:define-alien-type nil
        (sb-alien:struct test-struct
          (x sb-alien:int)
          (y sb-alien:int)))
      (sb-alien:with-alien-callable
          ((callback sb-alien:int
               ((s (sb-alien:struct test-struct)))
             (sb-alien:slot s (quote x))))
        (format t "CALLBACK_SUPPORT_OK~%")))
  (error (e)
    (format t "CALLBACK_SUPPORT_FAILED: ~A~%" e)))
' 2>&1)

    if echo "$test_result" | grep -q "CALLBACK_SUPPORT_OK"; then
        echo "Struct-by-value callback support: OK"
    else
        echo "WARNING: Struct-by-value callback test inconclusive"
        echo "Test output: $test_result"
    fi
}

# Build natively on macOS
build_macos_native() {
    check_requirements
    setup_repository
    generate_version
    build_sbcl_native
    install_vendor
    verify_install
}

# Main
main() {
    echo "========================================"
    echo "SBCL Rebuild Script for Epsilon"
    echo "========================================"
    echo ""

    local platform=$(detect_platform)
    echo "Platform: $platform"
    echo "Branch: $BRANCH"
    echo ""

    case "$platform" in
        linux-*)
            echo "Using container build for portable Linux binary..."
            echo ""
            build_linux_container
            ;;
        darwin-*)
            echo "Using native build for macOS..."
            echo "Build dir: $BUILD_DIR"
            echo "Vendor dir: $VENDOR_DIR"
            echo ""
            build_macos_native
            ;;
    esac

    echo ""
    echo "========================================"
    echo "SBCL rebuild complete!"
    echo "========================================"
    echo ""
    echo "To test with epsilon:"
    echo "  ./epsilon/epsilon --version"
    echo ""
    echo "To commit the updated binary:"
    echo "  git add epsilon/vendor/sbcl/$platform/"
    echo "  git commit -m 'Rebuild SBCL $platform'"
}

main "$@"

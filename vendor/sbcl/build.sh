#!/usr/bin/env bash
#
# build.sh - Rebuild vendored SBCL from jbouwman/sbcl fork
#
# This script builds SBCL from the jbouwman/sbcl fork. The fork is
# maintained as a staging ground for SBCL features Epsilon needs that
# are not in upstream SBCL — currently, an in-progress green threads
# (fibers) implementation.
#
# Usage:
#   ./epsilon/vendor/sbcl/build.sh [--clean] [--branch BRANCH]
#
# Options:
#   --clean     Remove existing SBCL source and clone fresh
#   --branch    Branch to build (default: master)
#
# Platform behavior:
#   - Linux: Builds natively on the host (glibc-linked). On NixOS, the dev
#            shell keeps zstd/zlib reachable at runtime via LD_LIBRARY_PATH.
#   - macOS: Builds natively on the host
#
# The script will:
#   1. Clone/update jbouwman/sbcl repository
#   2. Build SBCL with --fancy, --with-sb-core-compression, --with-sb-fiber
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
            echo "  Linux:  Builds natively on host (glibc-linked)"
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
                aarch64|arm64)
                    echo "linux-arm64"
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

    local version="2.6.4.${commit_count}.${short_hash}.epsilon"

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
    echo "  Options: --fancy --with-sb-core-compression --with-sb-fiber"

    # Make the bootstrap sbcl resolvable via PATH + SBCL_HOME, since
    # make-config.sh invokes `sbcl` directly rather than honoring $SBCL.
    if [[ "$bootstrap_sbcl" != "sbcl" ]]; then
        local platform=$(detect_platform)
        export SBCL_HOME="$VENDOR_DIR/$platform"
        export PATH="$(dirname "$bootstrap_sbcl"):$PATH"
    fi

    sh make.sh --fancy --with-sb-core-compression --with-sb-fiber

    echo "SBCL build complete!"
}

# Install to vendor directory (native build)
install_vendor() {
    local platform=$(detect_platform)
    local target_dir="$VENDOR_DIR/$platform"

    echo "Installing to $target_dir..."

    # Create target directory
    mkdir -p "$target_dir/contrib"

    # Copy main binaries. Unlink first so a running copy of the old sbcl
    # (e.g. a system-level epsilon service) doesn't trigger ETXTBUSY.
    rm -f "$target_dir/sbcl" "$target_dir/sbcl.core"
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

# Build natively on Linux. Produces a glibc-linked binary; on NixOS,
# LD_LIBRARY_PATH from the dev shell keeps zstd/zlib reachable at runtime.
build_linux_native() {
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
            echo "Using native build for $platform..."
            echo "Build dir: $BUILD_DIR"
            echo "Vendor dir: $VENDOR_DIR"
            echo ""
            build_linux_native
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
    echo "The per-platform directory is gitignored; do not commit it."
    echo "Under Nix, 'nix develop' provides this SBCL automatically"
    echo "via the sbcl-epsilon derivation."
}

main "$@"

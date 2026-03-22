#!/usr/bin/env bash
#
# build.sh - Build all vendored libraries for Epsilon
#
# Builds all vendored shared libraries in a single pass.
# All Linux builds use Debian Bookworm to match the runtime container
# images (glibc 2.36).
#
# Usage:
#   ./epsilon/vendor/build.sh [--clean]
#
# Options:
#   --clean     Remove existing build artifacts and start fresh
#
# Platform behavior:
#   - Linux: Builds in a Debian Bookworm container (matches runtime images)
#   - macOS: Builds natively with system tools
#
# Libraries built:
#   OpenSSL  3.5.5   (LTS, supported until 2030)
#   xxHash   v0.8.3
#   BLAKE3   1.8.3

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
BUILD_DIR="$REPO_ROOT/.vendor-build"
OUTPUT_DIR="$REPO_ROOT/epsilon/vendor/lib"

OPENSSL_VERSION="openssl-3.5.5"
XXHASH_VERSION="v0.8.3"
BLAKE3_VERSION="1.8.3"
OPENSSL_REPO="https://github.com/openssl/openssl.git"
XXHASH_REPO="https://github.com/Cyan4973/xxHash.git"
BLAKE3_REPO="https://github.com/BLAKE3-team/BLAKE3.git"

# Default options
CLEAN=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --clean)
            CLEAN=true
            shift
            ;;
        -h|--help)
            echo "Usage: $0 [--clean]"
            echo ""
            echo "Builds all vendored shared libraries for Epsilon:"
            echo "  OpenSSL  ${OPENSSL_VERSION}"
            echo "  xxHash   ${XXHASH_VERSION}"
            echo "  BLAKE3   ${BLAKE3_VERSION}"
            echo ""
            echo "Options:"
            echo "  --clean     Remove existing build artifacts and start fresh"
            echo ""
            echo "Platform behavior:"
            echo "  Linux:  Builds in Debian Bookworm container (matches runtime images)"
            echo "  macOS:  Builds natively"
            echo ""
            echo "Output: epsilon/vendor/lib/{platform}/"
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

# Build all libraries in a Debian Bookworm container (matches runtime images)
build_linux_container() {
    local platform="linux-x86_64"
    local target_dir="$OUTPUT_DIR/$platform"

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

    echo "Building all vendored libraries in Debian Bookworm container..."

    $container_cmd run --rm \
        -v "$build_output:/output" \
        -e OPENSSL_VERSION="$OPENSSL_VERSION" \
        -e XXHASH_VERSION="$XXHASH_VERSION" \
        -e BLAKE3_VERSION="$BLAKE3_VERSION" \
        \
        debian:bookworm bash -c '
set -ex

# Install build dependencies
apt-get update
apt-get install -y --no-install-recommends \
    git \
    make \
    gcc \
    g++ \
    cmake \
    autoconf \
    automake \
    libtool \
    pkg-config \
    ca-certificates

INSTALL_PREFIX=/tmp/vendor-dist

# ---- 1. Build OpenSSL ----
cd /tmp
git clone --depth 1 --branch "$OPENSSL_VERSION" https://github.com/openssl/openssl.git
cd openssl
./Configure \
    --prefix="$INSTALL_PREFIX" \
    --libdir=lib \
    shared \
    no-tests \
    no-docs
make -j$(nproc)
make install_sw

OPENSSL_COMMIT=$(git rev-parse HEAD)

# ---- 2. Build xxHash ----
cd /tmp
git clone --depth 1 --branch "$XXHASH_VERSION" https://github.com/Cyan4973/xxHash.git
cd xxHash
make -j$(nproc) lib
make install PREFIX="$INSTALL_PREFIX"

XXHASH_COMMIT=$(git rev-parse HEAD)

# ---- 3. Build BLAKE3 ----
cd /tmp
git clone --depth 1 --branch "$BLAKE3_VERSION" https://github.com/BLAKE3-team/BLAKE3.git
cd BLAKE3/c
mkdir build && cd build
cmake .. \
    -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
    -DCMAKE_BUILD_TYPE=Release \
    -DBUILD_SHARED_LIBS=ON
make -j$(nproc)
make install

BLAKE3_COMMIT=$(git -C /tmp/BLAKE3 rev-parse HEAD)

# ---- Copy libraries to output ----
# OpenSSL
cp -a "$INSTALL_PREFIX"/lib/libcrypto.so* /output/
cp -a "$INSTALL_PREFIX"/lib/libssl.so* /output/

# xxHash
cp -a "$INSTALL_PREFIX"/lib/libxxhash.so* /output/

# BLAKE3
cp -a "$INSTALL_PREFIX"/lib/libblake3.so* /output/

# Write version metadata
cat > /output/VERSION << EOF
# Epsilon vendored libraries
# Built: $(date -u +"%Y-%m-%d %H:%M:%S UTC")
# Platform: linux-x86_64
# Container: debian:bookworm
#
# OpenSSL ${OPENSSL_VERSION} (${OPENSSL_COMMIT})
# xxHash ${XXHASH_VERSION} (${XXHASH_COMMIT})
# BLAKE3 ${BLAKE3_VERSION} (${BLAKE3_COMMIT})
openssl=${OPENSSL_VERSION}
xxhash=${XXHASH_VERSION}
blake3=${BLAKE3_VERSION}
EOF

echo ""
echo "Built libraries:"
ls -la /output/

echo ""
echo "Library dependencies:"
for lib in /output/*.so*; do
    if [ -f "$lib" ] && [ ! -L "$lib" ]; then
        echo "--- $(basename "$lib") ---"
        ldd "$lib" 2>/dev/null || true
    fi
done

echo ""
echo "Container build complete!"
'

    echo ""
    echo "Installing to $target_dir..."
    mkdir -p "$target_dir"

    # Copy all .so files and symlinks
    cp -P "$build_output"/libcrypto.so* "$target_dir/"
    cp -P "$build_output"/libssl.so* "$target_dir/"
    cp -P "$build_output"/libxxhash.so* "$target_dir/"
    cp -P "$build_output"/libblake3.so* "$target_dir/"
    cp "$build_output/VERSION" "$target_dir/vendor-VERSION"

    echo ""
    echo "Installed libraries:"
    ls -la "$target_dir"/lib*.so*

    echo ""
    echo "Version info:"
    cat "$target_dir/vendor-VERSION"
}

# Build natively on macOS
build_macos_native() {
    local platform=$(detect_platform)
    local target_dir="$OUTPUT_DIR/$platform"

    if [[ "$CLEAN" == "true" ]] && [[ -d "$BUILD_DIR" ]]; then
        echo "Removing existing build directory..."
        rm -rf "$BUILD_DIR"
    fi

    mkdir -p "$BUILD_DIR"

    local install_prefix="$BUILD_DIR/vendor-dist"

    # Detect architecture for OpenSSL Configure target
    local arch=$(uname -m)
    local openssl_target=""
    case "$arch" in
        arm64)
            openssl_target="darwin64-arm64-cc"
            ;;
        x86_64)
            openssl_target="darwin64-x86_64-cc"
            ;;
    esac

    # ---- 1. Build OpenSSL ----
    echo ""
    echo "Building OpenSSL ${OPENSSL_VERSION}..."
    if [[ ! -d "$BUILD_DIR/openssl" ]]; then
        git clone --depth 1 --branch "$OPENSSL_VERSION" "$OPENSSL_REPO" "$BUILD_DIR/openssl"
    fi
    cd "$BUILD_DIR/openssl"
    ./Configure \
        "$openssl_target" \
        --prefix="$install_prefix" \
        --libdir=lib \
        shared \
        no-tests \
        no-docs
    make -j$(sysctl -n hw.ncpu)
    make install_sw

    local openssl_commit=$(git rev-parse HEAD)

    # ---- 2. Build xxHash ----
    echo ""
    echo "Building xxHash ${XXHASH_VERSION}..."
    if [[ ! -d "$BUILD_DIR/xxHash" ]]; then
        git clone --depth 1 --branch "$XXHASH_VERSION" "$XXHASH_REPO" "$BUILD_DIR/xxHash"
    fi
    cd "$BUILD_DIR/xxHash"
    make -j$(sysctl -n hw.ncpu) lib
    make install PREFIX="$install_prefix"

    local xxhash_commit=$(git rev-parse HEAD)

    # ---- 3. Build BLAKE3 ----
    echo ""
    echo "Building BLAKE3 ${BLAKE3_VERSION}..."
    if [[ ! -d "$BUILD_DIR/BLAKE3" ]]; then
        git clone --depth 1 --branch "$BLAKE3_VERSION" "$BLAKE3_REPO" "$BUILD_DIR/BLAKE3"
    fi
    cd "$BUILD_DIR/BLAKE3/c"
    mkdir -p build && cd build
    cmake .. \
        -DCMAKE_INSTALL_PREFIX="$install_prefix" \
        -DCMAKE_BUILD_TYPE=Release \
        -DBUILD_SHARED_LIBS=ON
    make -j$(sysctl -n hw.ncpu)
    make install

    local blake3_commit=$(git -C "$BUILD_DIR/BLAKE3" rev-parse HEAD)

    # ---- Install to output directory ----
    echo ""
    echo "Installing to $target_dir..."
    mkdir -p "$target_dir"

    cp -P "$install_prefix/lib"/libcrypto*.dylib "$target_dir/"
    cp -P "$install_prefix/lib"/libssl*.dylib "$target_dir/"
    cp -P "$install_prefix/lib"/libxxhash*.dylib "$target_dir/"
    cp -P "$install_prefix/lib"/libblake3*.dylib "$target_dir/"

    # Fix install names to use @rpath
    for lib in "$target_dir"/*.dylib; do
        [ -L "$lib" ] && continue
        install_name_tool -id "@rpath/$(basename "$lib")" "$lib" 2>/dev/null || true
    done

    # Write version metadata
    local date=$(date -u +"%Y-%m-%d %H:%M:%S UTC")
    cat > "$target_dir/vendor-VERSION" << EOF
# Epsilon vendored libraries
# Built: $date
# Platform: $platform
#
# OpenSSL ${OPENSSL_VERSION} (${openssl_commit})
# xxHash ${XXHASH_VERSION} (${xxhash_commit})
# BLAKE3 ${BLAKE3_VERSION} (${blake3_commit})
openssl=${OPENSSL_VERSION}
xxhash=${XXHASH_VERSION}
blake3=${BLAKE3_VERSION}
EOF

    echo ""
    echo "Installed libraries:"
    ls -la "$target_dir"/lib*.dylib

    echo ""
    echo "Version info:"
    cat "$target_dir/vendor-VERSION"
}

# Main
main() {
    echo "========================================"
    echo "Vendored Library Build for Epsilon"
    echo "========================================"
    echo ""
    echo "  OpenSSL: ${OPENSSL_VERSION}"
    echo "  xxHash:  ${XXHASH_VERSION}"
    echo "  BLAKE3:  ${BLAKE3_VERSION}"
    echo ""

    local platform=$(detect_platform)
    echo "Platform: $platform"
    echo ""

    case "$platform" in
        linux-*)
            echo "Using Debian Bookworm container build..."
            echo ""
            build_linux_container
            ;;
        darwin-*)
            echo "Using native build for macOS..."
            echo ""
            build_macos_native
            ;;
    esac

    echo ""
    echo "========================================"
    echo "Vendored library build complete!"
    echo "========================================"
    echo ""
    echo "Libraries installed to:"
    echo "  $OUTPUT_DIR/$platform/"
    echo ""
    echo "To commit the vendored libraries:"
    echo "  git add epsilon/vendor/lib/$platform/"
    echo "  git commit -m 'Vendor libs ($platform)'"
}

main "$@"

#!/bin/bash
#
# Build script for creating Epsilon runtime distribution
#
# This script builds a complete Epsilon runtime package that includes:
# - SBCL core image with Epsilon preloaded
# - Wrapper script for running Epsilon
# - Platform-specific packaging
#

set -e

# Configuration
EPSILON_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TARGET_DIR="$EPSILON_DIR/target"
DIST_DIR="$TARGET_DIR/dist"
PLATFORM=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)

# Platform-specific configurations
case "$PLATFORM" in
    darwin)
        PLATFORM_NAME="macos"
        SBCL_EXECUTABLE="$(which sbcl)"
        EPSILON_WRAPPER="epsilon"
        ;;
    linux)
        PLATFORM_NAME="linux"
        SBCL_EXECUTABLE="$(which sbcl)"
        EPSILON_WRAPPER="epsilon"
        ;;
    *)
        echo "Unsupported platform: $PLATFORM"
        echo "Use build-runtime-windows.sh for Windows builds"
        exit 1
        ;;
esac

echo "Building Epsilon runtime for $PLATFORM_NAME-$ARCH..."
echo "Platform detected: $PLATFORM"
echo "SBCL executable: $SBCL_EXECUTABLE"
echo "Working directory: $(pwd)"

# Create distribution directory
mkdir -p "$DIST_DIR"

# Build core image
echo "Building SBCL core image with Epsilon..."
cd "$EPSILON_DIR"

# Create target directory first
mkdir -p "$TARGET_DIR"

# Build core image - use relative paths that work across all platforms
echo "Building SBCL core image..."
echo "Current directory: $(pwd)"
echo "Target directory: $TARGET_DIR"

# Ensure we're in the right directory
cd "$EPSILON_DIR"

# Use a simpler approach - let SBCL handle the path resolution
sbcl --noinform \
     --non-interactive \
     --no-sysinit \
     --no-userinit \
     --load "module/core/src/tool/boot.lisp" \
     --eval "(epsilon.tool.boot:boot)" \
     --eval "(sb-ext:save-lisp-and-die \"target/epsilon-core\" :executable nil :save-runtime-options t :compression t)"

# Create standalone SBCL runtime
echo "Creating standalone SBCL runtime..."

# Detect SBCL installation directory
SBCL_PATH="$(which sbcl)"
if [ -L "$SBCL_PATH" ]; then
    # Follow symlink to find actual SBCL installation
    SBCL_REAL_PATH="$(readlink "$SBCL_PATH")"
    if [[ "$SBCL_REAL_PATH" != /* ]]; then
        # Relative symlink
        SBCL_REAL_PATH="$(dirname "$SBCL_PATH")/$SBCL_REAL_PATH"
    fi
    SBCL_PATH="$SBCL_REAL_PATH"
fi

# Find SBCL installation root
SBCL_BIN_DIR="$(dirname "$SBCL_PATH")"
SBCL_ROOT="$(dirname "$SBCL_BIN_DIR")"

echo "SBCL installation root: $SBCL_ROOT"

# Copy SBCL runtime components (Unix only)
cp "$SBCL_PATH" "$DIST_DIR/sbcl"
chmod +x "$DIST_DIR/sbcl"

# Copy SBCL core and runtime files
if [ -f "$SBCL_ROOT/lib/sbcl/sbcl.core" ]; then
    mkdir -p "$DIST_DIR/lib/sbcl"
    cp "$SBCL_ROOT/lib/sbcl/sbcl.core" "$DIST_DIR/lib/sbcl/"
    if [ -f "$SBCL_ROOT/lib/sbcl/sbclrc" ]; then
        cp "$SBCL_ROOT/lib/sbcl/sbclrc" "$DIST_DIR/lib/sbcl/"
    fi
fi

# For homebrew installations, also copy the runtime
if [ -f "$SBCL_ROOT/share/sbcl/src/runtime/sbcl" ]; then
    mkdir -p "$DIST_DIR/share/sbcl/src/runtime"
    cp "$SBCL_ROOT/share/sbcl/src/runtime/sbcl" "$DIST_DIR/share/sbcl/src/runtime/"
fi

# Create epsilon wrapper script (Unix only)
cat > "$DIST_DIR/epsilon" << 'EOF'
#!/bin/bash
#
# Epsilon runtime wrapper
#
# This script provides a convenient way to run Epsilon with the preloaded core
#

EPSILON_HOME="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CORE_IMAGE="$EPSILON_HOME/epsilon-core"
SBCL_CORE="$EPSILON_HOME/lib/sbcl/sbcl.core"

# Check if core image exists
if [ ! -f "$CORE_IMAGE" ]; then
    echo "Error: Epsilon core image not found at $CORE_IMAGE"
    exit 1
fi

# Set SBCL_HOME to use embedded runtime
export SBCL_HOME="$EPSILON_HOME/lib/sbcl"

# Run embedded SBCL with Epsilon core
exec "$EPSILON_HOME/sbcl" --core "$CORE_IMAGE" "$@"
EOF
chmod +x "$DIST_DIR/epsilon"

# Copy core image and source code to distribution
echo "Checking for core image at: $TARGET_DIR/epsilon-core"
if [ -f "$TARGET_DIR/epsilon-core" ]; then
    echo "Core image found, copying to distribution..."
    cp "$TARGET_DIR/epsilon-core" "$DIST_DIR/"
    rm -f "$TARGET_DIR/epsilon-core"
else
    echo "ERROR: Core image not found at $TARGET_DIR/epsilon-core"
    echo "Listing target directory contents:"
    ls -la "$TARGET_DIR/" 2>/dev/null || echo "Target directory does not exist or cannot be listed"
    echo "Listing current directory contents:"
    ls -la . 2>/dev/null
    echo "Checking if core image exists in current directory:"
    ls -la epsilon-core 2>/dev/null || echo "No epsilon-core in current directory"
    exit 1
fi

# Include source code for navigation
echo "Including source code for navigation..."
cp -r "$EPSILON_DIR/module/" "$DIST_DIR/module/"

# Copy essential project files
if [ -f "$EPSILON_DIR/CLAUDE.md" ]; then
    cp "$EPSILON_DIR/CLAUDE.md" "$DIST_DIR/"
fi
if [ -f "$EPSILON_DIR/README.md" ]; then
    cp "$EPSILON_DIR/README.md" "$DIST_DIR/"
fi
if [ -f "$EPSILON_DIR/run.sh" ]; then
    cp "$EPSILON_DIR/run.sh" "$DIST_DIR/"
    chmod +x "$DIST_DIR/run.sh"
fi

# Verify distribution contents
echo "Distribution directory contents:"
ls -la "$DIST_DIR/"

# Create package archive
PACKAGE_NAME="epsilon-$PLATFORM_NAME-$ARCH"
PACKAGE_FILE="$TARGET_DIR/$PACKAGE_NAME.tar.gz"
echo "Creating package: $PACKAGE_FILE"
cd "$TARGET_DIR"
tar -czf "$PACKAGE_NAME.tar.gz" -C dist .
echo "Epsilon runtime package created: $PACKAGE_FILE"
echo "Package contents:"
tar -tzf "$PACKAGE_NAME.tar.gz"
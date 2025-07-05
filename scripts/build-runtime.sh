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
        ;;
    linux)
        PLATFORM_NAME="linux"
        SBCL_EXECUTABLE="$(which sbcl)"
        ;;
    *)
        echo "Unsupported platform: $PLATFORM"
        exit 1
        ;;
esac

echo "Building Epsilon runtime for $PLATFORM_NAME-$ARCH..."

# Create distribution directory
mkdir -p "$DIST_DIR"

# Build core image
echo "Building SBCL core image with Epsilon..."
cd "$EPSILON_DIR"
sbcl --noinform \
     --non-interactive \
     --no-sysinit \
     --no-userinit \
     --load "module/core/src/tool/boot.lisp" \
     --eval "(epsilon.tool.boot:boot)" \
     --eval "(sb-ext:save-lisp-and-die \"target/epsilon-core\" :executable nil :save-runtime-options t :compression t)"

# Copy SBCL executable
echo "Copying SBCL runtime..."
cp "$SBCL_EXECUTABLE" "$DIST_DIR/sbcl"

# Create epsilon wrapper script
cat > "$DIST_DIR/epsilon" << 'EOF'
#!/bin/bash
#
# Epsilon runtime wrapper
#
# This script provides a convenient way to run Epsilon with the preloaded core
#

EPSILON_HOME="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CORE_IMAGE="$EPSILON_HOME/epsilon-core"

# Check if core image exists
if [ ! -f "$CORE_IMAGE" ]; then
    echo "Error: Epsilon core image not found at $CORE_IMAGE"
    exit 1
fi

# Run SBCL with Epsilon core
exec "$EPSILON_HOME/sbcl" --core "$CORE_IMAGE" "$@"
EOF

chmod +x "$DIST_DIR/epsilon"

# Copy core image to distribution
mv "$TARGET_DIR/epsilon-core" "$DIST_DIR/"

# Create package archive
PACKAGE_NAME="epsilon-$PLATFORM_NAME-$ARCH"
PACKAGE_FILE="$TARGET_DIR/$PACKAGE_NAME.tar.gz"

echo "Creating package: $PACKAGE_FILE"
cd "$TARGET_DIR"
tar -czf "$PACKAGE_NAME.tar.gz" -C dist .

echo "Epsilon runtime package created: $PACKAGE_FILE"
echo "Package contents:"
tar -tzf "$PACKAGE_NAME.tar.gz"
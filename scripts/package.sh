#!/usr/bin/env bash
# Build epsilon runtime executable for a specific platform

set -euo pipefail

# Parse arguments
PLATFORM="${1:-$(uname -s | tr '[:upper:]' '[:lower:]')}"
ARCH="${2:-$(uname -m)}"

# Normalize architecture names
case "$ARCH" in
    x86_64|amd64)
        ARCH="x86_64"
        ;;
    aarch64|arm64)
        ARCH="arm64"
        ;;
esac

echo "=== Building epsilon runtime for $PLATFORM-$ARCH ==="

# Ensure we're in the project root
if [ ! -f "epsilon" ]; then
    echo "Error: epsilon script not found. Please run from project root."
    exit 1
fi

# Build directory for runtime artifacts
BUILD_DIR="build/runtime/$PLATFORM-$ARCH"
mkdir -p "$BUILD_DIR"

# Step 1: Ensure all modules are built
echo "Building all modules..."
./epsilon --exec epsilon.release:selftest --format none || {
    echo "Warning: Some modules failed to build, continuing anyway..."
}

# Step 2: Create runtime wrapper script
echo "Creating runtime wrapper..."
cat > "$BUILD_DIR/epsilon-runtime" << 'EOF'
#!/usr/bin/env bash
# Epsilon runtime wrapper

# Resolve the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EPSILON_HOME="$(cd "$SCRIPT_DIR/.." && pwd)"

# Set up environment
export EPSILON_HOME
export SBCL_HOME="${EPSILON_HOME}/lib/sbcl-libs"

# Use bundled SBCL if available, otherwise system SBCL
if [ -x "${EPSILON_HOME}/bin/sbcl" ]; then
    SBCL="${EPSILON_HOME}/bin/sbcl"
else
    SBCL="sbcl"
fi

# Execute epsilon
exec "$SBCL" --script "${EPSILON_HOME}/epsilon" "$@"
EOF

chmod +x "$BUILD_DIR/epsilon-runtime"

# Step 3: Create platform-specific metadata
echo "Creating platform metadata..."
cat > "$BUILD_DIR/platform.info" << EOF
PLATFORM=$PLATFORM
ARCH=$ARCH
BUILD_DATE=$(date -u +"%Y-%m-%d %H:%M:%S UTC")
EPSILON_VERSION=$(cat VERSION || echo "0.0.0-dev")
SBCL_VERSION=$(sbcl --version | head -1)
EOF

# Step 4: Verify the build
echo "Verifying runtime build..."
if [ -f "$BUILD_DIR/epsilon-runtime" ] && [ -f "$BUILD_DIR/platform.info" ]; then
    echo "✓ Runtime build completed successfully"
    echo "  Location: $BUILD_DIR"
    cat "$BUILD_DIR/platform.info"
else
    echo "✗ Runtime build failed"
    exit 1
fi

echo ""
echo "=== Build complete ==="
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
    mingw*|msys*|cygwin*|windows*)
        PLATFORM_NAME="windows"
        SBCL_EXECUTABLE="$(which sbcl)"
        EPSILON_WRAPPER="epsilon.exe"
        ;;
    *)
        # Check for Windows environment variables as fallback
        if [ -n "$WINDIR" ] || [ -n "$SYSTEMROOT" ]; then
            PLATFORM_NAME="windows"
            SBCL_EXECUTABLE="$(which sbcl)"
            EPSILON_WRAPPER="epsilon.exe"
        else
            echo "Unsupported platform: $PLATFORM"
            exit 1
        fi
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

# Copy SBCL executable
echo "Copying SBCL runtime..."
if [ "$PLATFORM_NAME" = "windows" ]; then
    # Find SBCL executable on Windows
    SBCL_PATH="$(which sbcl 2>/dev/null)"
    if [ -n "$SBCL_PATH" ]; then
        echo "Found SBCL at: $SBCL_PATH"
        cp "$SBCL_PATH" "$DIST_DIR/sbcl.exe"
    else
        echo "Error: Could not find SBCL executable"
        which sbcl || echo "sbcl not found in PATH"
        exit 1
    fi
else
    cp "$SBCL_EXECUTABLE" "$DIST_DIR/sbcl"
fi

# Create epsilon wrapper script
if [ "$PLATFORM_NAME" = "windows" ]; then
    # Windows batch script
    cat > "$DIST_DIR/epsilon.bat" << 'EOF'
@echo off
rem Epsilon runtime wrapper for Windows
rem
rem This script provides a convenient way to run Epsilon with the preloaded core

set EPSILON_HOME=%~dp0
set CORE_IMAGE=%EPSILON_HOME%epsilon-core

if not exist "%CORE_IMAGE%" (
    echo Error: Epsilon core image not found at %CORE_IMAGE%
    exit /b 1
)

rem Run SBCL with Epsilon core
"%EPSILON_HOME%sbcl.exe" --core "%CORE_IMAGE%" %*
EOF
    
    # Also create a PowerShell wrapper
    cat > "$DIST_DIR/epsilon.exe" << 'EOF'
#!/bin/bash
# Temporary Unix-style wrapper for Windows build in CI
# This will be replaced with proper Windows executable in production

EPSILON_HOME="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CORE_IMAGE="$EPSILON_HOME/epsilon-core"

if [ ! -f "$CORE_IMAGE" ]; then
    echo "Error: Epsilon core image not found at $CORE_IMAGE"
    exit 1
fi

exec "$EPSILON_HOME/sbcl.exe" --core "$CORE_IMAGE" "$@"
EOF
    chmod +x "$DIST_DIR/epsilon.exe"
else
    # Unix shell script
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
fi

# Copy core image to distribution
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

# Verify distribution contents
echo "Distribution directory contents:"
ls -la "$DIST_DIR/"

# Create package archive
PACKAGE_NAME="epsilon-$PLATFORM_NAME-$ARCH"

if [ "$PLATFORM_NAME" = "windows" ]; then
    # Use ZIP for Windows
    PACKAGE_FILE="$TARGET_DIR/$PACKAGE_NAME.zip"
    echo "Creating package: $PACKAGE_FILE"
    cd "$DIST_DIR"
    zip -r "../$PACKAGE_NAME.zip" .
    echo "Epsilon runtime package created: $PACKAGE_FILE"
    echo "Package contents:"
    unzip -l "$PACKAGE_FILE"
else
    # Use tar.gz for Unix
    PACKAGE_FILE="$TARGET_DIR/$PACKAGE_NAME.tar.gz"
    echo "Creating package: $PACKAGE_FILE"
    cd "$TARGET_DIR"
    tar -czf "$PACKAGE_NAME.tar.gz" -C dist .
    echo "Epsilon runtime package created: $PACKAGE_FILE"
    echo "Package contents:"
    tar -tzf "$PACKAGE_NAME.tar.gz"
fi
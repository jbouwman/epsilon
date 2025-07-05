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

# Create distribution directory
mkdir -p "$DIST_DIR"

# Build core image
echo "Building SBCL core image with Epsilon..."
cd "$EPSILON_DIR"

# Use relative paths that work across platforms
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
    # On Windows, SBCL might be sbcl.exe already
    if [ -f "$(which sbcl).exe" ]; then
        cp "$(which sbcl).exe" "$DIST_DIR/sbcl.exe"
    elif [ -f "$(which sbcl)" ]; then
        cp "$(which sbcl)" "$DIST_DIR/sbcl.exe"
    else
        echo "Error: Could not find SBCL executable"
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
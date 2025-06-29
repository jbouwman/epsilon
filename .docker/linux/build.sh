#!/bin/bash
# Linux build script for epsilon
set -e

PLATFORM=${1:-linux}
SBCL_VERSION=${2:-2.4.0}
ACTION=${3:-build}

echo "=== Epsilon Linux Build ==="
echo "Platform: $PLATFORM"
echo "SBCL Version: $SBCL_VERSION"
echo "Action: $ACTION"

# Switch SBCL version if needed
case $SBCL_VERSION in
    2.3.11)
        export PATH="/opt/sbcl-2.3.11/bin:$PATH"
        ;;
    2.4.0|*)
        export PATH="/usr/local/bin:$PATH"
        ;;
esac

echo "Using SBCL: $(which sbcl)"
sbcl --version

# Make run.sh executable
chmod +x run.sh

case $ACTION in
    build)
        echo "Building epsilon for platform: $PLATFORM"
        ./run.sh build --platform $PLATFORM
        ;;
    test)
        echo "Testing epsilon for platform: $PLATFORM"
        ./run.sh test --platform $PLATFORM
        ;;
    package)
        echo "Creating distribution package"
        ./run.sh build --platform $PLATFORM
        
        # Create dist directory
        mkdir -p dist/epsilon-linux-x64
        
        # Copy core files
        cp -r src-core dist/epsilon-linux-x64/
        if [ "$PLATFORM" = "linux" ]; then
            cp -r src-linux dist/epsilon-linux-x64/src-platform
            cp -r tests-linux dist/epsilon-linux-x64/tests-platform
            cp package-linux.yaml dist/epsilon-linux-x64/package.yaml
        fi
        cp -r tests-core dist/epsilon-linux-x64/
        cp package-core.yaml dist/epsilon-linux-x64/
        
        # Copy build artifacts
        if [ -d target ]; then
            cp -r target dist/epsilon-linux-x64/
        fi
        
        # Copy essential files
        cp README.md LICENSE CLAUDE.md run.sh dist/epsilon-linux-x64/
        
        # Create platform info
        cat > dist/epsilon-linux-x64/PLATFORM.txt << EOF
Platform: linux
SBCL Version: $(sbcl --version)
Build Date: $(date -u)
Architecture: x86-64
Container: epsilon-linux
EOF
        
        # Create archive
        cd dist
        tar -czf epsilon-linux-x64.tar.gz epsilon-linux-x64
        echo "Package created: dist/epsilon-linux-x64.tar.gz"
        ;;
    *)
        echo "Unknown action: $ACTION"
        echo "Usage: epsilon-build [platform] [sbcl-version] [build|test|package]"
        exit 1
        ;;
esac

echo "=== Build Complete ==="
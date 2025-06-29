#!/bin/bash
# Windows package creation script for epsilon
# Note: Creates Windows source packages, not native binaries
set -e

PLATFORM=${1:-windows}
SBCL_VERSION=${2:-2.4.0}
ACTION=${3:-package}

echo "=== Epsilon Windows Package Builder ==="
echo "Platform: $PLATFORM"
echo "SBCL Version: $SBCL_VERSION"
echo "Action: $ACTION"

echo "Using SBCL: $(which sbcl)"
sbcl --version

# Make run.sh executable
chmod +x run.sh

case $ACTION in
    build)
        echo "Building epsilon core (Windows-specific requires Windows)"
        ./run.sh build --platform core
        ;;
    test)
        echo "Testing epsilon core (Windows-specific requires Windows)"
        ./run.sh test --platform core
        ;;
    package)
        echo "Creating Windows distribution package"
        
        # Build core first
        ./run.sh build --platform core
        
        # Create dist directory
        mkdir -p dist/epsilon-windows-x64
        
        # Copy core files
        cp -r src-core dist/epsilon-windows-x64/
        cp -r src-windows dist/epsilon-windows-x64/src-platform
        cp -r tests-core dist/epsilon-windows-x64/
        cp -r tests-windows dist/epsilon-windows-x64/tests-platform
        
        # Copy package specifications
        cp package-core.yaml dist/epsilon-windows-x64/
        cp package-windows.yaml dist/epsilon-windows-x64/package.yaml
        
        # Copy build artifacts if they exist
        if [ -d target ]; then
            cp -r target dist/epsilon-windows-x64/
        fi
        
        # Copy essential files
        cp README.md LICENSE CLAUDE.md run.sh dist/epsilon-windows-x64/
        
        # Create Windows batch file for building
        cat > dist/epsilon-windows-x64/build.bat << 'EOF'
@echo off
echo Building epsilon for Windows...
sbcl --load src-core/tool/boot.lisp --eval "(epsilon.tool.boot:boot)" --eval "(epsilon.tool.dev:main)" build --platform windows
EOF
        
        # Create platform info
        cat > dist/epsilon-windows-x64/PLATFORM.txt << EOF
Platform: windows
SBCL Version: $SBCL_VERSION
Build Date: $(date -u)
Architecture: x86-64
Container: epsilon-windows
Note: Source package - requires Windows with SBCL for compilation
EOF
        
        # Create Windows-specific usage instructions
        cat > dist/epsilon-windows-x64/WINDOWS_USAGE.txt << EOF
Windows Epsilon Distribution
===========================

This package contains epsilon optimized for Windows with IOCP networking.

Installation on Windows:
1. Install SBCL for Windows from https://www.sbcl.org/
2. Extract this package
3. Run: build.bat (or .\run.sh build --platform windows in Git Bash)
4. Run tests: .\run.sh test --platform windows

Platform-specific modules:
- epsilon.sys.iocp: Native I/O Completion Ports interface
- epsilon.net: IOCP-optimized networking with Winsock2

The IOCP backend provides high-performance scalable networking on Windows.

Requirements:
- Windows 10/11 or Windows Server 2016+
- SBCL 2.4.0 or later
- Visual Studio Build Tools (for FFI compilation)
EOF
        
        # Create archive (ZIP for Windows)
        cd dist
        zip -r epsilon-windows-x64.zip epsilon-windows-x64
        echo "Package created: dist/epsilon-windows-x64.zip"
        ;;
    *)
        echo "Unknown action: $ACTION"
        echo "Usage: epsilon-build [platform] [sbcl-version] [build|test|package]"
        exit 1
        ;;
esac

echo "=== Package Creation Complete ==="
#!/usr/bin/env bash
#
# CI Release Build Script
# Builds a complete release for a specific platform
#

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

error() { echo -e "${RED}✗ $1${NC}" >&2; }
success() { echo -e "${GREEN}✓ $1${NC}"; }
warning() { echo -e "${YELLOW}⚠ $1${NC}"; }
info() { echo -e "${BLUE}ℹ $1${NC}"; }

# Parse arguments
PLATFORM="${1:-}"
ARCH="${2:-}"
VERSION="${3:-}"

if [ -z "$PLATFORM" ] || [ -z "$ARCH" ] || [ -z "$VERSION" ]; then
    error "Usage: $0 PLATFORM ARCH VERSION"
    echo "Example: $0 linux x86_64 0.11.0"
    exit 1
fi

echo "========================================="
echo "   CI Release Build"
echo "========================================="
echo ""
info "Platform: $PLATFORM"
info "Architecture: $ARCH"
info "Version: $VERSION"
echo ""

# 1. Setup environment based on platform
setup_environment() {
    info "Setting up build environment for $PLATFORM..."
    
    case "$PLATFORM" in
        linux)
            # Container setup should already be done by workflow
            export TMPDIR="${TMPDIR:-/tmp}"
            ;;
        darwin|macos)
            # macOS setup
            if ! command -v sbcl >/dev/null 2>&1; then
                warning "SBCL not found, installing via Homebrew..."
                brew install sbcl || {
                    error "Failed to install SBCL"
                    exit 1
                }
            fi
            export TMPDIR="${TMPDIR:-/tmp}"
            ;;
        *)
            error "Unsupported platform: $PLATFORM"
            exit 1
            ;;
    esac
    
    success "Environment setup complete"
}

# 2. Run all tests
run_tests() {
    info "Running all tests..."
    
    if ! ./scripts/test.sh; then
        error "Tests failed - aborting release build"
        exit 1
    fi
    
    success "All tests passed"
}

# 4. Generate distribution package
generate_distribution() {
    info "Generating distribution package..."
    
    # Create distribution archive using epsilon.release:generate
    if ! ./epsilon --exec epsilon.release:generate -- "$VERSION"; then
        error "Failed to generate distribution package"
        exit 1
    fi
    
    # List generated artifacts
    echo ""
    info "Generated artifacts:"
    find releases -type f -name "*.tar.gz" -o -name "*.sha256" | sort || {
        error "No release artifacts found"
        exit 1
    }
    
    success "Distribution package generated"
}

# 5. Test the generated release
test_release() {
    info "Testing generated release..."
    
    # Find the generated tarball
    local tarball=$(find releases -name "epsilon-*.tar.gz" | head -1)
    
    if [ ! -f "$tarball" ]; then
        error "No tarball found for testing"
        find releases -type f || true
        exit 1
    fi
    
    info "Testing tarball: $(basename "$tarball")"
    
    # Create temp directory for testing
    local test_dir=$(mktemp -d)
    trap "rm -rf $test_dir" EXIT
    
    # Extract and test
    if ! tar -xzf "$tarball" -C "$test_dir"; then
        error "Failed to extract tarball"
        exit 1
    fi
    
    local extracted_dir=$(find "$test_dir" -maxdepth 1 -type d -name "epsilon-*" | head -1)
    if [ -z "$extracted_dir" ]; then
        error "Could not find extracted directory"
        ls -la "$test_dir"
        exit 1
    fi
    
    info "Extracted to: $(basename "$extracted_dir")"
    
    # Test epsilon executable
    local epsilon_bin="$extracted_dir/bin/epsilon"
    
    if [ ! -x "$epsilon_bin" ]; then
        error "Epsilon executable not found or not executable"
        ls -la "$extracted_dir/bin/" || true
        exit 1
    fi
    
    # Basic functionality tests
    if ! "$epsilon_bin" --version >/dev/null 2>&1; then
        error "Version command failed"
        exit 1
    fi
    
    if ! "$epsilon_bin" --eval "(+ 1 2)" >/dev/null 2>&1; then
        error "Basic evaluation failed"
        exit 1
    fi
    
    if ! "$epsilon_bin" --modules >/dev/null 2>&1; then
        error "Module listing failed"
        exit 1
    fi
    
    success "Release testing complete"
}

# Main execution
main() {
    setup_environment
    run_tests
    generate_distribution
    test_release
    
    echo ""
    success "CI release build completed successfully!"
    echo ""
    info "Release artifacts:"
    find releases -type f \( -name "*.tar.gz" -o -name "*.sha256" \) -exec basename {} \; | sort
}

# Run main function
main "$@"

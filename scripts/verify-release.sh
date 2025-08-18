#!/usr/bin/env bash
#
# CI Release Verification Script
# Verifies that a generated release archive works correctly
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
info() { echo -e "${BLUE}ℹ $1${NC}"; }

# Parse arguments
TARBALL="${1:-}"

# If no tarball specified, find the most recent one
if [ -z "$TARBALL" ]; then
    TARBALL=$(find releases -name "epsilon-*.tar.gz" -type f -print0 2>/dev/null | xargs -0 ls -t | head -1)
    if [ -z "$TARBALL" ]; then
        error "No release archive found. Please specify a tarball or generate one first."
        echo "Usage: $0 [path/to/epsilon-VERSION.tar.gz]"
        exit 1
    fi
fi

if [ ! -f "$TARBALL" ]; then
    error "Release archive not found: $TARBALL"
    exit 1
fi

echo "========================================="
echo "   Release Verification"
echo "========================================="
echo ""
info "Testing: $(basename "$TARBALL")"
info "Size: $(du -h "$TARBALL" | cut -f1)"
echo ""

# Create temp directory for testing
TEST_DIR=$(mktemp -d)
trap "rm -rf $TEST_DIR" EXIT

# Extract archive
info "Extracting archive..."
if ! tar -xzf "$TARBALL" -C "$TEST_DIR"; then
    error "Failed to extract archive"
    exit 1
fi
success "Archive extracted"

# Find extracted directory
EXTRACTED_DIR=$(find "$TEST_DIR" -maxdepth 1 -type d -name "epsilon-*" | head -1)
if [ -z "$EXTRACTED_DIR" ]; then
    error "Could not find extracted directory"
    ls -la "$TEST_DIR"
    exit 1
fi

info "Extracted to: $(basename "$EXTRACTED_DIR")"

# Verify directory structure
info "Verifying directory structure..."
REQUIRED_DIRS="bin lib modules scripts"
for dir in $REQUIRED_DIRS; do
    if [ ! -d "$EXTRACTED_DIR/$dir" ]; then
        error "Missing required directory: $dir"
        exit 1
    fi
    echo "  ✓ $dir/"
done

# Verify key files
info "Verifying key files..."
REQUIRED_FILES="bin/epsilon bin/sbcl VERSION LICENSE README.md"
for file in $REQUIRED_FILES; do
    if [ ! -f "$EXTRACTED_DIR/$file" ]; then
        error "Missing required file: $file"
        exit 1
    fi
    echo "  ✓ $file"
done

# Test epsilon executable
EPSILON_BIN="$EXTRACTED_DIR/bin/epsilon"
info "Testing epsilon executable..."

if [ ! -x "$EPSILON_BIN" ]; then
    error "Epsilon binary is not executable"
    exit 1
fi

# Test 1: Version
echo -n "  Testing --version... "
if "$EPSILON_BIN" --version >/dev/null 2>&1; then
    echo "✓"
else
    echo "✗"
    error "Version command failed"
    exit 1
fi

# Test 2: Basic evaluation
echo -n "  Testing basic evaluation... "
RESULT=$("$EPSILON_BIN" --eval "(+ 1 2)" --eval "(format t \"~A\" *)" --eval "(quit)" 2>/dev/null || echo "FAILED")
if [ "$RESULT" = "3" ]; then
    echo "✓"
else
    echo "✗"
    error "Basic evaluation failed (expected '3', got '$RESULT')"
    exit 1
fi

# Test 3: Module listing
echo -n "  Testing module listing... "
if "$EPSILON_BIN" --modules 2>&1 | grep -q "epsilon.core"; then
    echo "✓"
else
    echo "✗"
    error "Module listing failed"
    exit 1
fi

# Test 4: Module loading
echo -n "  Testing module loading... "
if "$EPSILON_BIN" --module epsilon.json --eval "(quit)" >/dev/null 2>&1; then
    echo "✓"
else
    echo "✗"
    error "Module loading failed"
    exit 1
fi

echo ""
success "All verification tests passed!"
echo ""
info "Release archive is ready for distribution: $(basename "$TARBALL")"
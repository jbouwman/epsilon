#!/usr/bin/env bash
#
# CI Test Runner
# Runs all tests and reports results
# Independent of release generation
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
FORMAT="${1:-shell}"  # shell, junit, or quiet
OUTPUT_FILE="${2:-}"   # Optional output file for junit

echo "========================================="
echo "   CI Test Suite"
echo "========================================="
echo ""

# 1. Run smoke tests
info "Running CLI smoke tests..."
if ./scripts/smoke.sh; then
    success "CLI smoke tests passed"
else
    error "CLI smoke tests failed"
    exit 1
fi

echo ""

# 2. Run module self-tests
info "Running module self-tests..."
if [ "$FORMAT" = "junit" ] && [ -n "$OUTPUT_FILE" ]; then
    # JUnit format with output file
    if ./epsilon --exec epsilon.release:selftest --format junit --file "$OUTPUT_FILE"; then
        success "Module self-tests passed"
        info "JUnit report written to: $OUTPUT_FILE"
    else
        error "Module self-tests failed"
        exit 1
    fi
elif [ "$FORMAT" = "quiet" ]; then
    # Quiet mode - minimal output
    if ./epsilon --exec epsilon.release:selftest --format none >/dev/null 2>&1; then
        success "Module self-tests passed"
    else
        error "Module self-tests failed"
        exit 1
    fi
else
    # Default shell format
    if ./epsilon --exec epsilon.release:selftest; then
        success "Module self-tests passed"
    else
        error "Module self-tests failed"
        exit 1
    fi
fi

echo ""

# 3. Verify epsilon executable
info "Verifying epsilon executable..."
if ./epsilon --version >/dev/null 2>&1; then
    success "Epsilon executable verified"
else
    error "Epsilon executable verification failed"
    exit 1
fi

echo ""
echo "========================================="
success "All tests passed!"
echo "========================================="
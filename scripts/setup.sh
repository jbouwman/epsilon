#!/usr/bin/env bash
#
# CI Setup Script
# Handles platform-specific environment setup
#

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

error() { echo -e "${RED}✗ $1${NC}" >&2; }
success() { echo -e "${GREEN}✓ $1${NC}"; }
info() { echo -e "${BLUE}ℹ $1${NC}"; }

# Parse environment variables from GitHub Actions
RUNNER_OS="${RUNNER_OS:-}"
CONTAINER="${1:-}"  # Container name if running in container

info "Setting up CI environment..."
info "Runner OS: ${RUNNER_OS}"
info "Container: ${CONTAINER:-none}"

# Container setup for Linux
if [ -n "$CONTAINER" ]; then
    info "Setting up containerized environment..."
    
    # Update package lists and install dependencies
    apt-get update && apt-get install -y git tar gzip xz-utils
    
    # Configure git for container environment
    git config --global --add safe.directory "$(pwd)"
    
    success "Container setup complete"
fi

# macOS setup
if [ "$RUNNER_OS" = "macOS" ]; then
    info "Setting up macOS environment..."
    
    # Install SBCL via Homebrew
    if ! command -v sbcl >/dev/null 2>&1; then
        info "Installing SBCL..."
        brew install sbcl || {
            error "Failed to install SBCL"
            exit 1
        }
    else
        info "SBCL already installed"
    fi
    
    success "macOS setup complete"
fi

# Set TMPDIR for tar compatibility
export TMPDIR="${TMPDIR:-/tmp}"
info "TMPDIR set to: $TMPDIR"

# Show environment info
info "Environment summary:"
echo "  OS: $RUNNER_OS"
echo "  Shell: $SHELL"
echo "  SBCL: $(command -v sbcl || echo 'not found')"
echo "  Working directory: $(pwd)"
echo "  Git status: $(git status --porcelain | wc -l) modified files"

success "CI setup complete"
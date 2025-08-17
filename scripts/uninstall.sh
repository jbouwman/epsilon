#!/usr/bin/env bash
#
# Epsilon Uninstallation Script
#
# Removes Epsilon installation and symlinks
#

set -euo pipefail

# Configuration
INSTALL_DIR="${EPSILON_HOME:-$HOME/.epsilon}"
BINARY_DIR="${EPSILON_BIN:-$HOME/.local/bin}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Functions for colored output
error() { echo -e "${RED}✗ $1${NC}" >&2; }
success() { echo -e "${GREEN}✓ $1${NC}"; }
warning() { echo -e "${YELLOW}⚠ $1${NC}"; }
info() { echo -e "${BLUE}ℹ $1${NC}"; }

# Show what will be removed
show_removal_plan() {
    echo "========================================="
    echo "   Epsilon Uninstallation"
    echo "========================================="
    echo ""
    
    echo "The following will be removed:"
    echo ""
    
    if [ -d "$INSTALL_DIR" ]; then
        info "Installation directory: $INSTALL_DIR"
        echo "  Size: $(du -sh "$INSTALL_DIR" 2>/dev/null | cut -f1)"
    else
        warning "Installation directory not found: $INSTALL_DIR"
    fi
    
    if [ -L "$BINARY_DIR/epsilon" ] || [ -f "$BINARY_DIR/epsilon" ]; then
        info "Symlink: $BINARY_DIR/epsilon"
    else
        warning "Symlink not found: $BINARY_DIR/epsilon"
    fi
    
    echo ""
}

# Remove installation
remove_installation() {
    local removed_something=false
    
    # Remove installation directory
    if [ -d "$INSTALL_DIR" ]; then
        info "Removing installation directory..."
        rm -rf "$INSTALL_DIR"
        success "Removed $INSTALL_DIR"
        removed_something=true
    fi
    
    # Remove symlink
    if [ -L "$BINARY_DIR/epsilon" ] || [ -f "$BINARY_DIR/epsilon" ]; then
        info "Removing symlink..."
        rm -f "$BINARY_DIR/epsilon"
        success "Removed $BINARY_DIR/epsilon"
        removed_something=true
    fi
    
    if ! $removed_something; then
        warning "Nothing to remove - Epsilon may not be installed"
        exit 0
    fi
}

# Check for running processes
check_running_processes() {
    if pgrep -f "epsilon" >/dev/null 2>&1; then
        warning "Epsilon processes are running"
        echo "Please close all Epsilon processes before uninstalling"
        echo ""
        echo "Running processes:"
        pgrep -fl "epsilon" || true
        echo ""
        read -p "Continue anyway? (y/N) " -n 1 -r < /dev/tty
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            error "Uninstallation cancelled"
            exit 1
        fi
    fi
}

# Show post-uninstall message
show_post_uninstall() {
    echo ""
    echo "========================================="
    echo "   Uninstallation Complete"
    echo "========================================="
    echo ""
    success "Epsilon has been removed from your system"
    echo ""
    echo "Note: You may want to remove PATH entries from your shell configuration:"
    echo "  Check ~/.bashrc, ~/.zshrc, or ~/.profile for:"
    echo "  export PATH=\"$BINARY_DIR:\$PATH\""
    echo ""
    echo "To reinstall Epsilon:"
    echo "  curl -sSL https://github.com/jbouwman/epsilon/releases/latest/download/install.sh | bash"
    echo ""
    echo "Thank you for using Epsilon!"
}

# Main uninstallation flow
main() {
    # Show removal plan
    show_removal_plan
    
    # Check for running processes
    check_running_processes
    
    # Confirm
    echo "This action cannot be undone."
    read -p "Remove Epsilon? (y/N) " -n 1 -r < /dev/tty
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        warning "Uninstallation cancelled"
        exit 0
    fi
    
    echo ""
    
    # Remove installation
    remove_installation
    
    # Show post-uninstall message
    show_post_uninstall
}

# Handle command line arguments
if [ "${1:-}" = "--force" ] || [ "${1:-}" = "-f" ]; then
    # Force uninstall without prompts
    info "Force uninstall mode"
    remove_installation
    success "Epsilon has been removed"
elif [ "${1:-}" = "--help" ] || [ "${1:-}" = "-h" ]; then
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Uninstall Epsilon from your system"
    echo ""
    echo "OPTIONS:"
    echo "  -f, --force    Remove without confirmation"
    echo "  -h, --help     Show this help message"
    echo ""
    echo "Environment variables:"
    echo "  EPSILON_HOME   Installation directory (default: ~/.epsilon)"
    echo "  EPSILON_BIN    Binary directory (default: ~/.local/bin)"
    exit 0
else
    # Interactive uninstall
    main "$@"
fi
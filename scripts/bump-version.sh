#!/usr/bin/env bash
#
# Version bump script for Epsilon
# Updates VERSION file and creates commit
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

usage() {
    cat << EOF
Usage: $0 [OPTIONS] [VERSION]

Update the VERSION file for Epsilon development.

VERSION can be:
  - Explicit version (e.g., 0.12.0-dev)
  - 'major', 'minor', or 'patch' to auto-increment
  - If omitted, suggests next version based on current

OPTIONS:
    -c, --commit    Create git commit after updating
    -p, --push      Push commit to origin (implies --commit)
    -h, --help      Show this help message

EXAMPLES:
    $0                      # Suggest next version
    $0 0.12.0-dev          # Set specific version
    $0 minor               # Bump minor version
    $0 --commit patch      # Bump patch and commit
    $0 --push major        # Bump major, commit, and push

EOF
    exit 0
}

# Parse arguments
COMMIT=false
PUSH=false
VERSION_ARG=""

while [[ $# -gt 0 ]]; do
    case $1 in
        -c|--commit)
            COMMIT=true
            shift
            ;;
        -p|--push)
            PUSH=true
            COMMIT=true  # Push implies commit
            shift
            ;;
        -h|--help)
            usage
            ;;
        -*)
            error "Unknown option: $1"
            usage
            ;;
        *)
            VERSION_ARG="$1"
            shift
            ;;
    esac
done

# Get current version
if [ ! -f VERSION ]; then
    error "VERSION file not found"
    exit 1
fi

CURRENT_VERSION=$(cat VERSION)
info "Current version: $CURRENT_VERSION"

# Parse current version
if [[ "$CURRENT_VERSION" =~ ^([0-9]+)\.([0-9]+)\.([0-9]+)(-.*)?$ ]]; then
    MAJOR="${BASH_REMATCH[1]}"
    MINOR="${BASH_REMATCH[2]}"
    PATCH="${BASH_REMATCH[3]}"
    SUFFIX="${BASH_REMATCH[4]}"
else
    error "Cannot parse current version: $CURRENT_VERSION"
    exit 1
fi

# Determine new version
if [ -z "$VERSION_ARG" ]; then
    # No argument - suggest next version
    if [[ "$SUFFIX" == "-dev" ]]; then
        info "Currently in development mode"
        NEW_VERSION="$CURRENT_VERSION"
    else
        # Released version - suggest next minor dev
        NEW_MINOR=$((MINOR + 1))
        NEW_VERSION="$MAJOR.$NEW_MINOR.0-dev"
        info "Suggesting next development version after release"
    fi
elif [ "$VERSION_ARG" = "major" ]; then
    NEW_MAJOR=$((MAJOR + 1))
    NEW_VERSION="$NEW_MAJOR.0.0-dev"
elif [ "$VERSION_ARG" = "minor" ]; then
    NEW_MINOR=$((MINOR + 1))
    NEW_VERSION="$MAJOR.$NEW_MINOR.0-dev"
elif [ "$VERSION_ARG" = "patch" ]; then
    NEW_PATCH=$((PATCH + 1))
    NEW_VERSION="$MAJOR.$MINOR.$NEW_PATCH-dev"
else
    # Explicit version provided
    NEW_VERSION="$VERSION_ARG"
    
    # Validate format
    if ! [[ "$NEW_VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+(-.*)?$ ]]; then
        error "Invalid version format: $NEW_VERSION"
        echo "Expected: X.Y.Z or X.Y.Z-suffix"
        exit 1
    fi
fi

# Check if version is changing
if [ "$NEW_VERSION" = "$CURRENT_VERSION" ]; then
    warning "Version unchanged: $CURRENT_VERSION"
    exit 0
fi

# Show what will happen
echo ""
echo "Version Update:"
echo "  From: $CURRENT_VERSION"
echo "  To:   $NEW_VERSION"
echo ""

# Update VERSION file
echo "$NEW_VERSION" > VERSION
success "Updated VERSION file"

# Show the change
git diff VERSION

# Commit if requested
if $COMMIT; then
    echo ""
    info "Creating git commit..."
    git add VERSION
    git commit -m "Bump version to $NEW_VERSION"
    success "Created commit"
    
    # Push if requested
    if $PUSH; then
        echo ""
        info "Pushing to origin..."
        BRANCH=$(git branch --show-current)
        git push origin "$BRANCH"
        success "Pushed to origin/$BRANCH"
    fi
else
    echo ""
    echo "To commit this change:"
    echo "  git add VERSION"
    echo "  git commit -m 'Bump version to $NEW_VERSION'"
fi

echo ""
success "Version update complete!"
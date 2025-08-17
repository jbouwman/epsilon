#!/usr/bin/env bash
#
# Automated release script for Epsilon
# Handles version validation, tagging, and release workflow
#

set -euo pipefail

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

# Show usage
usage() {
    cat << EOF
Usage: $0 [OPTIONS] VERSION

Create and push a release tag for Epsilon.

VERSION should be in format: X.Y.Z[-prerelease]
Examples: 0.11.0, 0.11.0-beta.1, 0.11.0-rc.1

OPTIONS:
    -b, --branch BRANCH    Release from specific branch (default: current branch)
    -f, --force           Skip confirmation prompts
    -n, --dry-run         Show what would be done without making changes
    -h, --help            Show this help message

EXAMPLES:
    $0 0.11.0                    # Create stable release v0.11.0
    $0 0.11.0-beta.1            # Create beta release
    $0 --branch release/v0.11 0.11.1  # Create patch release from branch
    $0 --dry-run 0.12.0        # Preview release process

EOF
    exit 0
}

# Parse command line arguments
BRANCH=""
FORCE=false
DRY_RUN=false
VERSION=""

while [[ $# -gt 0 ]]; do
    case $1 in
        -b|--branch)
            BRANCH="$2"
            shift 2
            ;;
        -f|--force)
            FORCE=true
            shift
            ;;
        -n|--dry-run)
            DRY_RUN=true
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
            VERSION="$1"
            shift
            ;;
    esac
done

# Validate version was provided
if [ -z "$VERSION" ]; then
    error "Version is required"
    usage
fi

# Validate version format
if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+(-[a-z]+(\.[0-9]+)?)?$ ]]; then
    error "Invalid version format: $VERSION"
    echo "Expected format: X.Y.Z or X.Y.Z-prerelease.N"
    exit 1
fi

# Add 'v' prefix for git tag
TAG="v$VERSION"

# Get current branch if not specified
if [ -z "$BRANCH" ]; then
    BRANCH=$(git branch --show-current)
fi

echo "========================================="
echo "   Epsilon Release Process"
echo "========================================="
echo ""
info "Version:  $VERSION"
info "Tag:      $TAG"
info "Branch:   $BRANCH"
if $DRY_RUN; then
    warning "Mode:     DRY RUN (no changes will be made)"
fi
echo ""

# Function to run commands (respects dry-run mode)
run_cmd() {
    if $DRY_RUN; then
        echo "[DRY RUN] $*"
    else
        "$@"
    fi
}

# Check prerequisites
echo "Checking prerequisites..."

# 1. Check git is clean
if ! git diff --quiet || ! git diff --cached --quiet; then
    error "Working directory has uncommitted changes"
    git status --short
    exit 1
fi
success "Working directory is clean"

# 2. Check we're on the correct branch
CURRENT_BRANCH=$(git branch --show-current)
if [ "$CURRENT_BRANCH" != "$BRANCH" ]; then
    error "Not on expected branch"
    echo "  Current: $CURRENT_BRANCH"
    echo "  Expected: $BRANCH"
    exit 1
fi
success "On correct branch: $BRANCH"

# 3. Check branch is up to date with remote
git fetch origin "$BRANCH" --quiet
LOCAL=$(git rev-parse HEAD)
REMOTE=$(git rev-parse "origin/$BRANCH")
if [ "$LOCAL" != "$REMOTE" ]; then
    error "Branch is not up to date with origin"
    echo "  Run: git pull origin $BRANCH"
    exit 1
fi
success "Branch is up to date with origin"

# 4. Check tag doesn't already exist
if git tag -l "$TAG" | grep -q "$TAG"; then
    error "Tag $TAG already exists"
    exit 1
fi
success "Tag $TAG is available"

# 5. Validate VERSION file
VERSION_FILE=$(cat VERSION 2>/dev/null || echo "")
if [ -z "$VERSION_FILE" ]; then
    error "VERSION file not found"
    exit 1
fi

# Check VERSION file for development version or matching release
if [[ "$VERSION_FILE" == *"-dev" ]]; then
    BASE_VERSION="${VERSION_FILE%-dev}"
    # Extract major.minor from both versions
    RELEASE_MM=$(echo "$VERSION" | cut -d. -f1-2)
    FILE_MM=$(echo "$BASE_VERSION" | cut -d. -f1-2)
    
    if [ "$RELEASE_MM" != "$FILE_MM" ]; then
        warning "VERSION file ($VERSION_FILE) doesn't match release ($VERSION)"
        echo "  This is normal for patch releases from release branches"
    else
        success "VERSION file indicates development for this release series"
    fi
elif [ "$VERSION_FILE" != "$VERSION" ]; then
    warning "VERSION file ($VERSION_FILE) doesn't match release version ($VERSION)"
    if ! $FORCE; then
        read -p "Continue anyway? (y/N) " -n 1 -r < /dev/tty
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            exit 1
        fi
    fi
else
    success "VERSION file matches release version"
fi

# 6. Check branch naming convention
if [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.0$ ]]; then
    # Major/minor release - should be from main or release branch
    if [ "$BRANCH" != "main" ] && ! [[ "$BRANCH" =~ ^release/v[0-9]+\.[0-9]+$ ]]; then
        warning "Major/minor releases typically come from 'main' or 'release/vX.Y' branches"
        echo "  Current branch: $BRANCH"
        if ! $FORCE; then
            read -p "Continue anyway? (y/N) " -n 1 -r
            echo
            if [[ ! $REPLY =~ ^[Yy]$ ]]; then
                exit 1
            fi
        fi
    fi
elif [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[1-9][0-9]*$ ]]; then
    # Patch release - should be from release branch
    MAJOR_MINOR=$(echo "$VERSION" | cut -d. -f1-2)
    EXPECTED_BRANCH="release/v$MAJOR_MINOR"
    if [ "$BRANCH" != "$EXPECTED_BRANCH" ] && [ "$BRANCH" != "main" ]; then
        warning "Patch releases typically come from release branches"
        echo "  Expected: $EXPECTED_BRANCH"
        echo "  Current:  $BRANCH"
        if ! $FORCE; then
            read -p "Continue anyway? (y/N) " -n 1 -r
            echo
            if [[ ! $REPLY =~ ^[Yy]$ ]]; then
                exit 1
            fi
        fi
    fi
fi

# 7. Run tests
echo ""
echo "Running tests..."
if $DRY_RUN; then
    echo "[DRY RUN] Would run: ./scripts/test.sh"
else
    if ! ./scripts/test.sh; then
        error "Tests failed - aborting release"
        exit 1
    fi
fi
success "All tests passed"

# 8. Check CHANGELOG
echo ""
echo "Checking CHANGELOG..."
if ! grep -q "\[$VERSION\]" CHANGELOG.md 2>/dev/null; then
    warning "Version $VERSION not found in CHANGELOG.md"
    if ! $FORCE; then
        read -p "Continue without CHANGELOG entry? (y/N) " -n 1 -r < /dev/tty
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            error "Please update CHANGELOG.md before releasing"
            exit 1
        fi
    fi
else
    success "CHANGELOG.md contains entry for $VERSION"
fi

# Show release summary
echo ""
echo "========================================="
echo "   Release Summary"
echo "========================================="
echo ""
echo "  Version:     $VERSION"
echo "  Git Tag:     $TAG"
echo "  Branch:      $BRANCH"
echo "  Commit:      $(git rev-parse --short HEAD)"
echo ""

# Get release type
if [[ "$VERSION" =~ -alpha ]]; then
    RELEASE_TYPE="Alpha Release"
elif [[ "$VERSION" =~ -beta ]]; then
    RELEASE_TYPE="Beta Release"
elif [[ "$VERSION" =~ -rc ]]; then
    RELEASE_TYPE="Release Candidate"
else
    RELEASE_TYPE="Stable Release"
fi
echo "  Type:        $RELEASE_TYPE"
echo ""

# Confirm release
if ! $FORCE && ! $DRY_RUN; then
    echo "This will:"
    echo "  1. Create git tag $TAG"
    echo "  2. Push tag to origin"
    echo "  3. Trigger GitHub Actions to build and publish release"
    echo ""
    
    # Fix for read error in some terminal environments
    if [ -t 0 ]; then
        # Terminal is available
        read -p "Proceed with release? (y/N) " -n 1 -r < /dev/tty
        echo
    else
        # No terminal available, check for force flag or exit
        error "No terminal available for confirmation. Use --force to skip confirmation."
        exit 1
    fi
    
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        warning "Release cancelled"
        exit 0
    fi
fi

# Create and push tag
echo ""
echo "Creating release..."

# Create annotated tag
TAG_MESSAGE="Release $VERSION

Type: $RELEASE_TYPE
Branch: $BRANCH
Commit: $(git rev-parse HEAD)"

run_cmd git tag -a "$TAG" -m "$TAG_MESSAGE"
success "Created tag $TAG"

# Push tag to origin
run_cmd git push origin "$TAG"
success "Pushed tag to origin"

# Post-release actions
echo ""
echo "========================================="
echo "   Post-Release Steps"
echo "========================================="
echo ""

if ! $DRY_RUN; then
    success "Release $VERSION has been tagged and pushed!"
    echo ""
    echo "GitHub Actions will now:"
    echo "  • Run all tests on multiple platforms"
    echo "  • Build release artifacts for Linux and macOS"
    echo "  • Create GitHub Release with artifacts"
    echo ""
    echo "Monitor the build at:"
    echo "  https://github.com/jbouwman/epsilon/actions"
    echo ""
    
    # Suggest next version update
    if [[ "$VERSION" =~ ^([0-9]+)\.([0-9]+)\.([0-9]+) ]]; then
        MAJOR="${BASH_REMATCH[1]}"
        MINOR="${BASH_REMATCH[2]}"
        PATCH="${BASH_REMATCH[3]}"
        
        # Suggest next development version
        if [[ "$VERSION" =~ -[a-z] ]]; then
            # Pre-release - stay on same version
            NEXT_VERSION="$MAJOR.$MINOR.$PATCH-dev"
        else
            # Stable release - bump minor for next dev
            NEXT_MINOR=$((MINOR + 1))
            NEXT_VERSION="$MAJOR.$NEXT_MINOR.0-dev"
        fi
        
        echo "Suggested next steps:"
        echo ""
        echo "  1. Update VERSION file for next development cycle:"
        echo "     echo '$NEXT_VERSION' > VERSION"
        echo ""
        echo "  2. Commit and push the version bump:"
        echo "     git add VERSION"
        echo "     git commit -m 'Bump version to $NEXT_VERSION'"
        echo "     git push origin $BRANCH"
        echo ""
        
        if [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.0$ ]] && [ "$BRANCH" = "main" ]; then
            echo "  3. (Optional) Create a release branch for maintenance:"
            echo "     git checkout -b release/v$MAJOR.$MINOR"
            echo "     git push origin release/v$MAJOR.$MINOR"
        fi
    fi
else
    echo "[DRY RUN] Release process completed successfully!"
    echo ""
    echo "To perform the actual release, run without --dry-run:"
    echo "  $0 $VERSION"
fi

echo ""
success "Release process complete!"
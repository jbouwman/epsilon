#!/bin/bash
set -euo pipefail

# Publish release to package repository
# Usage: publish-release.sh <release-tag>

RELEASE_TAG="${1:-}"
if [[ -z "$RELEASE_TAG" ]]; then
    echo "Error: Release tag required" >&2
    echo "Usage: $0 <release-tag>" >&2
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EPSILON_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$EPSILON_ROOT"

RELEASE_REPO_DIR="target/release-repo"

if [[ ! -d "$RELEASE_REPO_DIR" ]]; then
    echo "Error: Release repository not found. Run setup-release-repo.sh first." >&2
    exit 1
fi

echo "Publishing release: $RELEASE_TAG"

cd "$RELEASE_REPO_DIR"

# Configure git if in CI environment
if [[ -n "${CI:-}" ]]; then
    git config user.name "Epsilon CI"
    git config user.email "ci@epsilon.org"
fi

# Add all changes
git add .

# Check if there are changes to commit
if git diff --staged --quiet; then
    echo "No changes to publish for release $RELEASE_TAG"
    exit 0
fi

# Commit changes
git commit -m "Release $RELEASE_TAG

- Added EPK packages for version $RELEASE_TAG
- Updated package index
- Generated release notes and compatibility matrix

Published by Epsilon CI on $(date -u +"%Y-%m-%d %H:%M:%S UTC")"

# Tag the release
git tag -a "v$RELEASE_TAG" -m "Epsilon release $RELEASE_TAG"

# Push to repository (if RELEASE_TOKEN is available)
if [[ -n "${RELEASE_TOKEN:-}" ]]; then
    echo "Pushing to release repository..."
    
    # Setup authentication for GitHub
    if [[ "$RELEASE_REPO_URL" == *"github.com"* ]]; then
        git remote set-url origin "https://x-access-token:$RELEASE_TOKEN@github.com/epsilon-org/packages.git"
    fi
    
    # Push branch and tags
    git push origin "release-$RELEASE_TAG"
    git push origin "v$RELEASE_TAG"
    
    echo "Release published successfully to repository!"
else
    echo "Warning: RELEASE_TOKEN not set, skipping repository push"
    echo "To manually publish:"
    echo "  cd $RELEASE_REPO_DIR"
    echo "  git push origin release-$RELEASE_TAG"
    echo "  git push origin v$RELEASE_TAG"
fi

# Generate release summary
echo "=== Release Summary ==="
echo "Release Tag: $RELEASE_TAG"
echo "Packages: $(find packages -name '*.epk' | wc -l)"
echo "Platforms: $(find packages -name '*.epk' | sed 's/.*-\([^-]*-[^-]*\)\.epk$/\1/' | sort -u | tr '\n' ' ')"
echo "Total Size: $(find packages -name '*.epk' -exec du -b {} + | awk '{sum+=$1} END {printf "%.2f MB\n", sum/1024/1024}')"
echo "Repository: $(pwd)"

# List all published packages
echo "=== Published Packages ==="
find packages -name '*.epk' | sort

echo "Release publication complete!"
#!/usr/bin/env bash
#
# CI Version Extraction Script
# Gets version from VERSION file or git tag
#

set -euo pipefail

# Extract version from VERSION file (preferred) or git tag (fallback)
get_version() {
    local version=""
    
    # First try VERSION file
    if [ -f VERSION ]; then
        version=$(cat VERSION | tr -d '\n\r')
        echo "Using version from VERSION file: $version" >&2
        echo "$version"
        return 0
    fi
    
    # Fallback: extract from GITHUB_REF if available
    if [ -n "${GITHUB_REF:-}" ]; then
        if echo "$GITHUB_REF" | grep -q '^refs/tags/v'; then
            version=$(echo "$GITHUB_REF" | sed 's|refs/tags/v||')
            echo "Extracted version from git tag: $version" >&2
            echo "$version"
            return 0
        fi
    fi
    
    # Last resort: try current git tag
    if command -v git >/dev/null 2>&1; then
        local git_tag=$(git describe --tags --exact-match 2>/dev/null | sed 's/^v//' || echo "")
        if [ -n "$git_tag" ]; then
            echo "Using current git tag: $git_tag" >&2
            echo "$git_tag"
            return 0
        fi
    fi
    
    # No version found
    echo "Could not determine version, using default" >&2
    echo "0.0.0-dev"
}

# If run directly, output the version
if [ "${BASH_SOURCE[0]}" = "${0}" ]; then
    get_version
fi
#!/bin/bash
set -euo pipefail

# Setup release repository for EPK distribution
# Usage: setup-release-repo.sh <release-tag>

RELEASE_TAG="${1:-}"
if [[ -z "$RELEASE_TAG" ]]; then
    echo "Error: Release tag required" >&2
    echo "Usage: $0 <release-tag>" >&2
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EPSILON_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Configuration
RELEASE_REPO_URL="${EPSILON_RELEASE_REPO_URL:-https://github.com/epsilon-org/packages.git}"
RELEASE_REPO_DIR="target/release-repo"

cd "$EPSILON_ROOT"

echo "Setting up release repository for tag: $RELEASE_TAG"
echo "Release repository: $RELEASE_REPO_URL"

# Clean and setup release repository
rm -rf "$RELEASE_REPO_DIR"
mkdir -p "$RELEASE_REPO_DIR"

# Clone or initialize release repository
if [[ "$RELEASE_REPO_URL" == "https://github.com/epsilon-org/packages.git" ]]; then
    # Clone existing repository
    git clone "$RELEASE_REPO_URL" "$RELEASE_REPO_DIR"
    cd "$RELEASE_REPO_DIR"
    
    # Create release branch if it doesn't exist
    git checkout -B "release-$RELEASE_TAG" || git checkout "release-$RELEASE_TAG"
else
    # Initialize new repository
    cd "$RELEASE_REPO_DIR"
    git init
    git config user.name "Epsilon CI"
    git config user.email "ci@epsilon.org"
fi

# Create repository structure
mkdir -p packages
mkdir -p index
mkdir -p releases

# Create repository metadata
cat > README.md << EOF
# Epsilon Package Repository

This repository contains pre-compiled EPK packages for the Epsilon Common Lisp library.

## Repository Structure

- \`packages/\` - EPK package files organized by version and platform
- \`index/\` - Package index files for dependency resolution
- \`releases/\` - Release metadata and changelogs

## Installation

### Using epsilon-package tool:
\`\`\`bash
# Add this repository
epsilon-package repo add epsilon-packages $RELEASE_REPO_URL

# Install core package
epsilon-package install epsilon.core@latest

# Install specific module
epsilon-package install epsilon.yaml@1.0.0
\`\`\`

### Manual Installation:
\`\`\`bash
# Download EPK file for your platform
wget $RELEASE_REPO_URL/packages/epsilon.core/1.0.0/epsilon.core-1.0.0-linux-x86_64.epk

# Extract and install
epsilon-package install-local epsilon.core-1.0.0-linux-x86_64.epk
\`\`\`

## Available Packages

See \`index/packages.json\` for a complete list of available packages and versions.

## Platform Support

- Linux (x86_64, arm64)
- macOS (x86_64, arm64)  
- Windows (x86_64)

## Latest Release

**Version:** $RELEASE_TAG
**Release Date:** $(date -u +"%Y-%m-%d")

See \`releases/$RELEASE_TAG/\` for detailed release information.
EOF

# Create package repository index structure
cat > index/repository.json << EOF
{
  "name": "epsilon-packages",
  "description": "Official Epsilon Common Lisp package repository",
  "version": "1.0.0",
  "url": "$RELEASE_REPO_URL",
  "api_version": "1.0",
  "supported_platforms": [
    "linux-x86_64",
    "linux-arm64",
    "darwin-x86_64", 
    "darwin-arm64",
    "windows-x86_64"
  ],
  "last_updated": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "packages_url": "index/packages.json",
  "releases_url": "releases/"
}
EOF

# Initialize empty package index (will be populated by generate-release-index.sh)
cat > index/packages.json << EOF
{
  "packages": {},
  "last_updated": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "total_packages": 0
}
EOF

# Create release directory for this version
mkdir -p "releases/$RELEASE_TAG"

echo "Release repository setup complete: $RELEASE_REPO_DIR"
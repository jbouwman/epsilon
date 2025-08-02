#!/bin/bash
# Generate version information from git tags and commit info

set -e

# Get version from git
TAG=$(git describe --tags --abbrev=0 2>/dev/null || echo "v0.0.0")
COMMIT=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
DIRTY=$(git diff --quiet && git diff --cached --quiet || echo "-dirty")
FULL_VERSION=$(git describe --tags --always --dirty 2>/dev/null || echo "${TAG}-${COMMIT}${DIRTY}")

# FIXME compare with VERSION file

# Extract semantic version from tag (remove 'v' prefix)
SEMVER=${TAG#v}

# Get current timestamp
BUILD_TIME=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Generate EDN file
cat > target/epsilon-version.edn << EOF
{:version "${SEMVER}"
 :tag "${TAG}"
 :commit "${COMMIT}"
 :full-version "${FULL_VERSION}"
 :build-time "${BUILD_TIME}"}
EOF

echo "Generated version info: ${FULL_VERSION}"

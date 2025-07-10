#!/bin/bash
set -euo pipefail

# Generate package index for release repository
# Usage: generate-release-index.sh <core-version>

CORE_VERSION="${1:-1.0.0}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EPSILON_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$EPSILON_ROOT"

RELEASE_REPO_DIR="target/release-repo"
EPK_DIR="target/epk"

if [[ ! -d "$RELEASE_REPO_DIR" ]]; then
    echo "Error: Release repository not found. Run setup-release-repo.sh first." >&2
    exit 1
fi

echo "Generating package index for version: $CORE_VERSION"

# Function to extract metadata from EPK file
extract_epk_metadata() {
    local epk_file="$1"
    local temp_dir="target/epk-metadata-temp"
    
    rm -rf "$temp_dir"
    mkdir -p "$temp_dir"
    
    # Extract EPK (ZIP file)
    unzip -q "$epk_file" -d "$temp_dir"
    
    # Read package metadata
    local package_file="$temp_dir/package.edn"
    local manifest_file="$temp_dir/MANIFEST.txt"
    
    local package_name=""
    local version=""
    local description=""
    local dependencies=""
    local platform=""
    local build_time=""
    
    if [[ -f "$package_file" ]]; then
        package_name=$(grep -o ':name[[:space:]]*"[^"]*"' "$package_file" | sed 's/:name[[:space:]]*"//' | sed 's/"//' || echo "")
        version=$(grep -o ':version[[:space:]]*"[^"]*"' "$package_file" | sed 's/:version[[:space:]]*"//' | sed 's/"//' || echo "$CORE_VERSION")
        description=$(grep -o ':description[[:space:]]*"[^"]*"' "$package_file" | sed 's/:description[[:space:]]*"//' | sed 's/"//' || echo "")
        dependencies=$(grep -o ':depends[[:space:]]*\[.*\]' "$package_file" | sed 's/:depends[[:space:]]*\[//' | sed 's/\]//' || echo "")
    fi
    
    if [[ -f "$manifest_file" ]]; then
        platform=$(grep "^Platform:" "$manifest_file" | cut -d' ' -f2 || echo "")
        build_time=$(grep "^Build-Time:" "$manifest_file" | cut -d' ' -f2 || echo "")
    fi
    
    # Generate JSON metadata
    cat << EOF
{
  "name": "$package_name",
  "version": "$version", 
  "description": "$description",
  "platform": "$platform",
  "dependencies": [$(echo "$dependencies" | sed 's/"//g' | sed 's/,/","/g' | sed 's/^/"/' | sed 's/$/"/' | grep -v '^""$' || echo '')],
  "build_time": "$build_time",
  "filename": "$(basename "$epk_file")",
  "size": $(stat -f%z "$epk_file" 2>/dev/null || stat -c%s "$epk_file"),
  "sha256": "$(sha256sum "$epk_file" | cut -d' ' -f1)"
}
EOF
    
    rm -rf "$temp_dir"
}

# Generate package index
echo "Processing EPK files..."

# Start building package index
PACKAGES_JSON="$RELEASE_REPO_DIR/index/packages.json"
TEMP_INDEX="target/temp-index.json"

cat > "$TEMP_INDEX" << EOF
{
  "packages": {
EOF

first_package=true

# Process all EPK files
for epk_file in "$EPK_DIR"/*.epk; do
    if [[ -f "$epk_file" ]]; then
        echo "Processing: $(basename "$epk_file")"
        
        # Extract package metadata
        metadata=$(extract_epk_metadata "$epk_file")
        package_name=$(echo "$metadata" | jq -r '.name')
        version=$(echo "$metadata" | jq -r '.version')
        platform=$(echo "$metadata" | jq -r '.platform')
        
        # Add comma separator for JSON
        if [[ "$first_package" == "false" ]]; then
            echo "," >> "$TEMP_INDEX"
        fi
        first_package=false
        
        # Add package entry to index
        cat >> "$TEMP_INDEX" << EOF
    "$package_name": {
      "latest": "$version",
      "versions": {
        "$version": {
          "platforms": {
            "$platform": $metadata
          }
        }
      }
    }
EOF
        
        # Copy EPK to release repository with organized structure
        package_dir="$RELEASE_REPO_DIR/packages/$package_name/$version"
        mkdir -p "$package_dir"
        cp "$epk_file" "$package_dir/"
        
        echo "Added package: $package_name@$version ($platform)"
    fi
done

# Finish package index
cat >> "$TEMP_INDEX" << EOF
  },
  "last_updated": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "total_packages": $(ls "$EPK_DIR"/*.epk 2>/dev/null | wc -l)
}
EOF

# Format and install package index
if command -v jq >/dev/null 2>&1; then
    jq . "$TEMP_INDEX" > "$PACKAGES_JSON"
else
    # Fallback if jq is not available
    cp "$TEMP_INDEX" "$PACKAGES_JSON"
fi

rm -f "$TEMP_INDEX"

# Generate release notes
RELEASE_NOTES="$RELEASE_REPO_DIR/releases/$CORE_VERSION/RELEASE_NOTES.md"
mkdir -p "$(dirname "$RELEASE_NOTES")"

cat > "$RELEASE_NOTES" << EOF
# Epsilon Release $CORE_VERSION

**Release Date:** $(date -u +"%Y-%m-%d")
**Build Time:** $(date -u +"%Y-%m-%dT%H:%M:%SZ")

## Package Summary

This release includes $(ls "$EPK_DIR"/*.epk 2>/dev/null | wc -l) packages across multiple platforms.

### Available Packages

EOF

# List all packages in release notes
for epk_file in "$EPK_DIR"/*.epk; do
    if [[ -f "$epk_file" ]]; then
        basename "$epk_file" .epk >> "$RELEASE_NOTES"
    fi
done

cat >> "$RELEASE_NOTES" << EOF

## Installation

### Core Package
\`\`\`bash
epsilon-package install epsilon.core@$CORE_VERSION
\`\`\`

### Platform-Specific Packages
\`\`\`bash
# Install platform networking support
epsilon-package install epsilon.linux@$CORE_VERSION    # Linux
epsilon-package install epsilon.darwin@$CORE_VERSION   # macOS
epsilon-package install epsilon.windows@$CORE_VERSION  # Windows
\`\`\`

### Optional Modules
\`\`\`bash
epsilon-package install epsilon.yaml@$CORE_VERSION
epsilon-package install epsilon.regex@$CORE_VERSION
epsilon-package install epsilon.msgpack@$CORE_VERSION
epsilon-package install epsilon.parsing@$CORE_VERSION
epsilon-package install epsilon.foreign@$CORE_VERSION
\`\`\`

## Compatibility

- **SBCL Version:** 2.3.0+
- **Platforms:** Linux, macOS, Windows
- **Architectures:** x86_64, arm64

## Changes in This Release

See individual module changelogs for detailed changes.
EOF

# Generate platform compatibility matrix
COMPATIBILITY_JSON="$RELEASE_REPO_DIR/releases/$CORE_VERSION/compatibility.json"
cat > "$COMPATIBILITY_JSON" << EOF
{
  "core_version": "$CORE_VERSION",
  "sbcl_minimum": "2.3.0",
  "platforms": {
    "linux": {
      "architectures": ["x86_64", "arm64"],
      "tested_distributions": ["Ubuntu 20.04+", "CentOS 8+", "Debian 11+"]
    },
    "darwin": {
      "architectures": ["x86_64", "arm64"],
      "minimum_version": "macOS 10.15"
    },
    "windows": {
      "architectures": ["x86_64"],
      "minimum_version": "Windows 10"
    }
  }
}
EOF

echo "Package index generation complete!"
echo "Packages indexed: $(jq '.total_packages' "$PACKAGES_JSON" 2>/dev/null || echo "unknown")"
echo "Release repository ready: $RELEASE_REPO_DIR"
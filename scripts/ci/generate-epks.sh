#!/bin/bash
set -euo pipefail

# Generate EPK packages for all built modules
# Usage: generate-epks.sh <core-version>

CORE_VERSION="${1:-1.0.0}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EPSILON_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$EPSILON_ROOT"

# Detect platform
case "$(uname -s)" in
    Linux*)     PLATFORM="linux";;
    Darwin*)    PLATFORM="darwin";;
    MINGW*|CYGWIN*|MSYS*) PLATFORM="windows";;
    *)          PLATFORM="unknown";;
esac

case "$(uname -m)" in
    x86_64)     ARCH="x86_64";;
    arm64|aarch64) ARCH="arm64";;
    *)          ARCH="unknown";;
esac

PLATFORM_ARCH="${PLATFORM}-${ARCH}"
EPK_DIR="target/epk"

echo "Generating EPKs for platform: $PLATFORM_ARCH"
echo "Core version: $CORE_VERSION"

# Create EPK output directory
mkdir -p "$EPK_DIR"

# Function to create EPK for a module
create_epk() {
    local module_name="$1"
    local module_dir="module/$module_name"
    
    if [[ ! -d "$module_dir" ]]; then
        echo "Warning: Module directory not found: $module_dir"
        return 1
    fi
    
    local package_file="$module_dir/package.edn"
    if [[ ! -f "$package_file" ]]; then
        echo "Warning: Package file not found: $package_file"
        return 1
    fi
    
    # Extract module version
    local module_version
    module_version=$(grep -o ':version[[:space:]]*"[^"]*"' "$package_file" | sed 's/:version[[:space:]]*"//' | sed 's/"//' || echo "$CORE_VERSION")
    
    # Extract module name from package file
    local package_name
    package_name=$(grep -o ':name[[:space:]]*"[^"]*"' "$package_file" | sed 's/:name[[:space:]]*"//' | sed 's/"//' || echo "$module_name")
    
    # Check if module is platform-specific
    local platform_restriction=""
    if grep -q ':platform' "$package_file"; then
        platform_restriction=$(grep -o ':platform[[:space:]]*"[^"]*"' "$package_file" | sed 's/:platform[[:space:]]*"//' | sed 's/"//')
        if [[ -n "$platform_restriction" && "$platform_restriction" != "$PLATFORM" ]]; then
            echo "Skipping $module_name: platform restriction ($platform_restriction != $PLATFORM)"
            return 0
        fi
    fi
    
    local epk_filename="${package_name}-${module_version}-${PLATFORM_ARCH}.epk"
    local epk_path="$EPK_DIR/$epk_filename"
    local temp_dir="target/epk-temp/$module_name"
    
    echo "Creating EPK: $epk_filename"
    
    # Create temporary directory for EPK contents
    rm -rf "$temp_dir"
    mkdir -p "$temp_dir"
    
    # Copy compiled FASL files
    if [[ -d "$module_dir/target/lisp" ]]; then
        cp -r "$module_dir/target/lisp" "$temp_dir/"
    else
        echo "Warning: No compiled FASL files found for $module_name"
        mkdir -p "$temp_dir/lisp"
    fi
    
    # Copy source files (for debugging/inspection)
    if [[ -d "$module_dir/src" ]]; then
        cp -r "$module_dir/src" "$temp_dir/"
    fi
    
    # Copy package metadata
    cp "$package_file" "$temp_dir/"
    
    # Create manifest
    cat > "$temp_dir/MANIFEST.txt" << EOF
Package: $package_name
Version: $module_version
Platform: $PLATFORM_ARCH
Build-Time: $(date -u +"%Y-%m-%dT%H:%M:%SZ")
Core-Version: $CORE_VERSION
EOF
    
    # Create EPK (ZIP file)
    (cd "$temp_dir" && zip -r "../../epk/$epk_filename" .)
    
    echo "Created EPK: $epk_path"
}

# Generate EPKs for all modules
echo "Discovering modules..."
for module_dir in module/*/; do
    if [[ -d "$module_dir" ]]; then
        module_name=$(basename "$module_dir")
        echo "Processing module: $module_name"
        create_epk "$module_name" || echo "Failed to create EPK for $module_name"
    fi
done

# Generate combined core+essential EPK
echo "Creating combined core EPK..."
create_combined_epk() {
    local epk_filename="epsilon-core-essential-${CORE_VERSION}-${PLATFORM_ARCH}.epk"
    local epk_path="$EPK_DIR/$epk_filename"
    local temp_dir="target/epk-temp/combined"
    
    rm -rf "$temp_dir"
    mkdir -p "$temp_dir"
    
    # Combine core + essential modules
    for essential_module in core parsing; do
        if [[ -d "module/$essential_module/target/lisp" ]]; then
            cp -r "module/$essential_module/target/lisp"/* "$temp_dir/" 2>/dev/null || true
        fi
    done
    
    # Create combined manifest
    cat > "$temp_dir/MANIFEST.txt" << EOF
Package: epsilon-core-essential
Version: $CORE_VERSION
Platform: $PLATFORM_ARCH
Build-Time: $(date -u +"%Y-%m-%dT%H:%M:%SZ")
Description: Combined core and essential modules for fast startup
Includes: epsilon.core, epsilon.parsing
EOF
    
    (cd "$temp_dir" && zip -r "../../epk/$epk_filename" .)
    echo "Created combined EPK: $epk_path"
}

create_combined_epk

echo "EPK generation complete. Generated packages:"
ls -la "$EPK_DIR"/*.epk
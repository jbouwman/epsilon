#!/bin/bash
set -euo pipefail

# Discover all modules and generate JSON array for CI matrix
# This script scans module/ directory for package.edn files and extracts module names

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EPSILON_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$EPSILON_ROOT"

# Find all package.edn files and extract module names
MODULES=()

for package_file in module/*/package.edn; do
    if [[ -f "$package_file" ]]; then
        # Extract module directory name
        module_dir=$(dirname "$package_file")
        module_name=$(basename "$module_dir")
        
        # Skip if package.edn is malformed
        if grep -q ":name" "$package_file"; then
            MODULES+=("$module_name")
        fi
    fi
done

# Generate JSON array for GitHub Actions matrix
printf '['
for i in "${!MODULES[@]}"; do
    if [[ $i -gt 0 ]]; then
        printf ','
    fi
    printf '"%s"' "${MODULES[$i]}"
done
printf ']'
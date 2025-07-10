#!/bin/bash
set -euo pipefail

# Extract version from module package.edn file
# Usage: get-version.sh <module-name>

MODULE_NAME="${1:-epsilon.core}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EPSILON_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$EPSILON_ROOT"

# Find the module's package.edn file
if [[ "$MODULE_NAME" == "epsilon.core" ]]; then
    PACKAGE_FILE="module/core/package.edn"
else
    # Handle module name to directory mapping
    case "$MODULE_NAME" in
        "epsilon."*) 
            # Remove epsilon. prefix and find matching directory
            DIR_NAME="${MODULE_NAME#epsilon.}"
            PACKAGE_FILE="module/$DIR_NAME/package.edn"
            ;;
        *)
            PACKAGE_FILE="module/$MODULE_NAME/package.edn"
            ;;
    esac
fi

if [[ ! -f "$PACKAGE_FILE" ]]; then
    echo "Error: Package file not found: $PACKAGE_FILE" >&2
    exit 1
fi

# Extract version from package.edn
# Look for :version "x.y.z" pattern
VERSION=$(grep -o ':version[[:space:]]*"[^"]*"' "$PACKAGE_FILE" | sed 's/:version[[:space:]]*"//' | sed 's/"//')

if [[ -z "$VERSION" ]]; then
    echo "Error: Version not found in $PACKAGE_FILE" >&2
    exit 1
fi

echo "$VERSION"
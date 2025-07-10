#!/bin/bash
set -euo pipefail

# Build dependency chain for a specific module
# Usage: build-dependencies.sh <module-name>

MODULE_NAME="${1:-}"
if [[ -z "$MODULE_NAME" ]]; then
    echo "Error: Module name required" >&2
    echo "Usage: $0 <module-name>" >&2
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EPSILON_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$EPSILON_ROOT"

echo "Building dependencies for module: $MODULE_NAME"

# Extract dependencies from package.edn
PACKAGE_FILE="module/$MODULE_NAME/package.edn"
if [[ ! -f "$PACKAGE_FILE" ]]; then
    echo "Warning: Package file not found: $PACKAGE_FILE"
    echo "Assuming no dependencies"
    exit 0
fi

# Parse dependencies (simplified - assumes dependencies are listed as strings)
DEPS=$(grep -o ':depends[[:space:]]*\[.*\]' "$PACKAGE_FILE" || echo "")

if [[ -n "$DEPS" ]]; then
    echo "Found dependencies: $DEPS"
    
    # Extract dependency module names (simplified parsing)
    # This handles common cases like ["epsilon.core"] or ["epsilon.core", "epsilon.parsing"]
    DEP_MODULES=$(echo "$DEPS" | sed 's/:depends[[:space:]]*\[//' | sed 's/\]//' | tr ',' '\n' | sed 's/[" ]//g' | grep -v '^$')
    
    for dep in $DEP_MODULES; do
        case "$dep" in
            "epsilon.core")
                echo "Building core module..."
                ./run.sh build --module core
                ;;
            "epsilon.parsing")
                echo "Building parsing module..."
                ./run.sh build --module parsing
                ;;
            "epsilon."*)
                # Extract module directory name
                dep_dir="${dep#epsilon.}"
                echo "Building module: $dep_dir"
                ./run.sh build --module "$dep_dir"
                ;;
            *)
                echo "Building module: $dep"
                ./run.sh build --module "$dep"
                ;;
        esac
    done
else
    echo "No dependencies found for module: $MODULE_NAME"
fi

echo "Dependencies built successfully for module: $MODULE_NAME"
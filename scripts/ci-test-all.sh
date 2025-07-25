#!/bin/bash
# CI test runner - discovers and runs all epsilon tests

set -e

echo "=== Running all epsilon tests ==="

# Clean any stale build artifacts
find src -type d -name target -exec rm -rf {} + 2>/dev/null || true

# Create target directory for test results
mkdir -p target

# Run tests for core modules
MODULES=(
    "epsilon.core"
    "epsilon.parsing"
    "epsilon.json" 
    "epsilon.yaml"
    "epsilon.msgpack"
    "epsilon.http"
    "epsilon.web"
    "epsilon.test"
)

TOTAL_TESTS=0
FAILED_MODULES=""

for module in "${MODULES[@]}"; do
    echo ""
    echo "=== Testing $module ==="
    
    # Generate test results in target directory with JUnit format
    TEST_FILE="target/TEST-${module}.xml"
    
    if ./epsilon test --package "$module" --format junit --file "$TEST_FILE"; then
        echo "✓ $module tests passed - results in $TEST_FILE"
    else
        echo "✗ $module tests failed - results in $TEST_FILE"
        FAILED_MODULES="$FAILED_MODULES $module"
    fi
done

echo ""
echo "=== Test Summary ==="

# List all generated test result files
echo "Generated test result files:"
for xml_file in target/TEST-*.xml; do
    if [ -f "$xml_file" ]; then
        echo "  $xml_file"
    fi
done

if [ -z "$FAILED_MODULES" ]; then
    echo "All tests passed!"
    exit 0
else
    echo "Failed modules:$FAILED_MODULES"
    exit 1
fi
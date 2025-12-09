#!/usr/bin/env bash
# CI test runner for epsilon

set -euo pipefail

echo "=== Running all tests before building release ==="
echo ""

# Track overall status
EXIT_CODE=0

# 1. Run smoke tests
echo "Running CLI smoke tests..."
if ./scripts/smoke.sh; then
    echo "✓ CLI smoke tests passed"
else
    echo "✗ CLI smoke tests failed"
    EXIT_CODE=1
fi

echo ""

# 2. Run self-test for all modules
echo "Running module self-tests..."
if ./epsilon --exec epsilon.release:selftest; then
    echo "✓ Module self-tests passed"
else
    echo "✗ Module self-tests failed"
    EXIT_CODE=1
fi

echo ""

# 3. Verify epsilon executable works
echo "Verifying epsilon executable..."
if ./epsilon --version > /dev/null 2>&1; then
    echo "✓ Epsilon executable verified"
else
    echo "✗ Epsilon executable verification failed"
    EXIT_CODE=1
fi

echo ""

# Summary
if [ $EXIT_CODE -eq 0 ]; then
    echo "=== All tests passed ==="
else
    echo "=== Some tests failed ==="
fi

exit $EXIT_CODE

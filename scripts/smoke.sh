#!/bin/bash
# CLI smoke test suite for epsilon

set -euo pipefail


# Track test results
PASSED=0
FAILED=0
SKIPPED=0

# Test output directory
TEST_DIR="/tmp/epsilon-cli-test-$$"
mkdir -p "$TEST_DIR"

# Clean up on exit
trap "rm -rf $TEST_DIR" EXIT

# Test function
run_test() {
    local name="$1"
    local cmd="$2"
    local expected_exit="${3:-0}"
    local grep_pattern="${4:-}"
    
    echo -n "Testing $name... "
    
    # Run command and capture output and exit code
    local exit_code=0
    local output_file="$TEST_DIR/output.txt"
    if eval "$cmd" > "$output_file" 2>&1; then
        exit_code=0
    else
        exit_code=$?
    fi
    
    # Check exit code
    if [ "$exit_code" -ne "$expected_exit" ]; then
        echo "FAILED (exit code: $exit_code, expected: $expected_exit)"
        echo "  Command: $cmd"
        echo "  Output:"
        cat "$output_file" | sed 's/^/    /'
        ((FAILED++))
        return 1
    fi
    
    # Check output pattern if provided
    if [ -n "$grep_pattern" ]; then
        if ! grep -q "$grep_pattern" "$output_file"; then
            echo "FAILED (pattern not found: $grep_pattern)"
            echo "  Command: $cmd"
            echo "  Output:"
            cat "$output_file" | sed 's/^/    /'
            ((FAILED++))
            return 1
        fi
    fi
    
    echo "PASSED"
    ((PASSED++))
    return 0
}

# Test cases
echo "=== Epsilon CLI Smoke Tests ==="
echo

# Basic flags
run_test "help flag" "./epsilon --help" 0 "Epsilon development environment"
run_test "version flag" "./epsilon --version" 0 "EPSILON"

# Package operations
run_test "list packages" "./epsilon --packages" 0 "epsilon.core"
run_test "list packages (finds epsilon.test)" "./epsilon --packages" 0 "epsilon.test"

# Build operations
run_test "load epsilon.json" "./epsilon --package epsilon.json --eval t" 0 "Successfully loaded epsilon.json"
run_test "load invalid package" "./epsilon --package nonexistent.package" 1 "Unknown package"

# Test operations
echo
echo "=== Testing --test flag ==="
run_test "test epsilon.core" "./epsilon --test epsilon.json" 0 "test"
run_test "test invalid package" "./epsilon --test nonexistent.package" 1 "Unknown"

# Eval operations
run_test "eval simple expression" "./epsilon --eval '(+ 1 2)'" 0 "3"
run_test "eval with package load" "./epsilon --package epsilon.core --eval '(epsilon.map:make-map)'" 0

# Path operations
TEST_PKG_DIR="$TEST_DIR/test-package"
mkdir -p "$TEST_PKG_DIR"
cat > "$TEST_PKG_DIR/package.lisp" << 'EOF'
(:name "test.package"
 :version "1.0.0"
 :author "Test"
 :description "Test package"
 :sources ("src")
 :dependencies ())
EOF
mkdir -p "$TEST_PKG_DIR/src"
echo "(defpackage test.package)" > "$TEST_PKG_DIR/src/main.lisp"

run_test "path to package" "./epsilon --path $TEST_PKG_DIR --packages" 0 "test.package"
run_test "path without package.lisp" "./epsilon --path /tmp --packages" 1 "No package.lisp found"

# Combined operations
run_test "build and eval" "./epsilon --build epsilon.core --eval '(format t \"built\")'" 0 "built"

# Exec operations (if main function exists)
# run_test "exec function" "./epsilon --exec epsilon.core:some-function" 0

# Debug and logging
run_test "debug flag" "./epsilon --debug --eval '(+ 1 1)'" 0 "2"
run_test "log flag" "./epsilon --log debug --eval '(+ 1 1)'" 0 "2"

# Summary
echo
echo "=== Test Summary ==="
echo "Passed: $PASSED"
echo "Failed: $FAILED"
echo "Skipped: $SKIPPED"
echo "Total: $((PASSED + FAILED + SKIPPED))"

if [ $FAILED -eq 0 ]; then
    echo
    echo "All tests passed!"
    exit 0
else
    echo
    echo "Some tests failed!"
    exit 1
fi

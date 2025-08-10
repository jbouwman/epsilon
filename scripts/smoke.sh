#!/usr/bin/env sh
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
        FAILED=$((FAILED + 1))
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
    PASSED=$((PASSED + 1))
    return 0
}

# Test cases
echo "=== Epsilon CLI Smoke Tests ==="
echo

# Basic flags
run_test "help flag" "./epsilon --help" 0 "Epsilon development environment"
run_test "version flag" "./epsilon --version" 0 "EPSILON"

# Module operations
run_test "list modules" "./epsilon --modules" 0 "epsilon.core"
run_test "list modules (finds epsilon.test)" "./epsilon --modules" 0 "epsilon.test"

# Build operations
run_test "load epsilon.json" "./epsilon --module epsilon.json --eval \'success" 0 "SUCCESS"
run_test "load invalid module" "./epsilon --module nonexistent.module" 1 "Unknown module"

# Test operations
echo
echo "=== Testing --test flag ==="
run_test "test epsilon.core" "./epsilon --test epsilon.json" 0 "test"
run_test "test invalid module" "./epsilon --test nonexistent.module" 1 "Unknown"

# Eval operations
run_test "eval simple expression" "./epsilon --eval '(+ 1 2)'" 0 "3"
run_test "eval with module load" "./epsilon --module epsilon.core --eval '(epsilon.map:make-map)'" 0

# Path operations
TEST_PKG_DIR="$TEST_DIR/test-module"
mkdir -p "$TEST_PKG_DIR"
cat > "$TEST_PKG_DIR/module.lisp" << 'EOF'
(:name "test.module"
 :version "1.0.0"
 :author "Test"
 :description "Test module"
 :sources ("src")
 :dependencies ())
EOF
mkdir -p "$TEST_PKG_DIR/src"
echo "(defpackage test.module)" > "$TEST_PKG_DIR/src/main.lisp"

run_test "path to module" "./epsilon --path $TEST_PKG_DIR --modules" 0 "test.module"
run_test "path without module.lisp" "./epsilon --path /tmp --modules" 0 "Found [0-9]* modules"

# Combined operations
run_test "module and eval" "./epsilon --module epsilon.core --eval '(format t \"loaded\")'" 0 "loaded"

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
    exit 0
else
    exit 1
fi

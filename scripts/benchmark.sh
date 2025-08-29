#!/usr/bin/env bash
# Epsilon Benchmark Runner Script
# Simple wrapper for running benchmarks

EPSILON_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
EPSILON="$EPSILON_ROOT/epsilon"
BENCHMARK_MODULE="epsilon.benchmark"
CONFIG_FILE="$EPSILON_ROOT/modules/benchmark/config/suites.lisp"
RUNNER_FILE="$EPSILON_ROOT/modules/benchmark/run.lisp"

# Default command
COMMAND="${1:-help}"

# Check if epsilon binary exists
if [ ! -f "$EPSILON" ]; then
    echo "Error: epsilon binary not found at $EPSILON"
    exit 1
fi

# Run the benchmark command
case "$COMMAND" in
    help|--help|-h)
        echo "Epsilon Benchmark Runner"
        echo ""
        echo "Usage: $0 [COMMAND]"
        echo ""
        echo "Commands:"
        echo "  help      - Show this help message"
        echo "  run       - Run default benchmark suites"
        echo "  all       - Run all benchmark suites"
        echo "  quick     - Run quick benchmarks (for CI)"
        echo "  baseline  - Save current results as baseline"
        echo "  compare   - Compare with saved baseline"
        echo "  list      - List available suites"
        echo "  <suite>   - Run specific suite by name"
        echo ""
        echo "Examples:"
        echo "  $0 run          # Run default suites"
        echo "  $0 quick        # Quick CI benchmarks"
        echo "  $0 baseline     # Save baselines"
        echo "  $0 compare      # Check for regressions"
        echo "  $0 core-operations  # Run specific suite"
        ;;
    *)
        # Run the benchmark command through epsilon
        exec "$EPSILON" \
            --module "$BENCHMARK_MODULE" \
            --load "$CONFIG_FILE" \
            --load "$RUNNER_FILE" \
            --eval "(epsilon.tool.benchmark.suites:main \"$COMMAND\")"
        ;;
esac
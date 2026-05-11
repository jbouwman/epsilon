#!/bin/sh
# Demonstrate parallel module compilation on a many-core box.
#
# Usage:  ./epsilon/modules/build/demo/build-and-load.sh [WORKERS]
#
# Default WORKERS = 72 (sized for the auk box). Pass an explicit count
# to override. Runs from the repo root.

set -eu

WORKERS="${1:-72}"
MAX_JOBS_PER_WORKER="${MAX_JOBS_PER_WORKER:-50}"
LOAD_TARGET="${LOAD_TARGET:-epsilon.http}"
# Each worker is its own SBCL image; give it enough heap to load
# hundreds of modules' worth of FASL state without GC thrash.
export EPSILON_HEAP_MB="${EPSILON_HEAP_MB:-2048}"

cd "$(dirname "$0")/../../../.."
EPSILON=./epsilon/epsilon
BUILD_DIR=epsilon/_build

if [ ! -x "$EPSILON" ]; then
    echo "epsilon binary not found at $EPSILON" >&2
    exit 1
fi

echo "=== Parallel build demo (workers=$WORKERS) ==="
echo "Repo: $(pwd)"
echo "Epsilon: $EPSILON"
echo

echo "--- Step 1: clean FASL cache ---"
rm -rf "$BUILD_DIR"
echo "Removed $BUILD_DIR"
echo

echo "--- Step 2: bootstrap epsilon.build (sequential) ---"
echo "Building enough core modules to run 'epsilon build'..."
TS=$(date +%s)
"$EPSILON" eval --module epsilon.build t > /tmp/epsilon-build-bootstrap.log 2>&1
BOOTSTRAP=$(($(date +%s) - TS))
echo "Bootstrap finished in ${BOOTSTRAP}s. Log: /tmp/epsilon-build-bootstrap.log"
echo

echo "--- Step 3: parallel build of every module (cold cache) ---"
TS=$(date +%s)
"$EPSILON" build --all \
    --workers "$WORKERS" \
    --max-jobs-per-worker "$MAX_JOBS_PER_WORKER" \
    2>&1 | tee /tmp/epsilon-build-run.log | tail -30
PARALLEL=$(($(date +%s) - TS))
echo
echo "Parallel build wall-clock: ${PARALLEL}s"
echo

echo "--- Step 4: verify load ($LOAD_TARGET) in a fresh image ---"
TS=$(date +%s)
"$EPSILON" eval --module "$LOAD_TARGET" t > /tmp/epsilon-build-load.log 2>&1
LOAD=$(($(date +%s) - TS))
echo "Loaded $LOAD_TARGET in ${LOAD}s (no recompiles expected)"
echo

echo "=== Summary ==="
printf "  bootstrap     %4ds\n" "$BOOTSTRAP"
printf "  parallel build %3ds  (workers=%d)\n" "$PARALLEL" "$WORKERS"
printf "  fresh load    %4ds  (target=%s)\n" "$LOAD" "$LOAD_TARGET"

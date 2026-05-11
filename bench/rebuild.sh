#!/usr/bin/env bash
# Measure incremental-rebuild times on the host this is run on.
# Used to back the paper's Results-section claim with a number that
# came from a real run rather than from the author's laptop.
#
# Scenarios
#   warm-null         : rebuild a module with no source change (cache hit only)
#   edit-leaf         : add a comment to a leaf module's source, rebuild
#   edit-wide         : add a comment to a widely-required module's source, rebuild
#   cold-leaf         : delete the leaf's build cache, rebuild from scratch
#
# All edits are comment-only and reverted on exit.
#
# Usage:
#   ./epsilon/bench/rebuild.sh            # measure once
#   N=3 ./epsilon/bench/rebuild.sh        # measure N times, report median

set -euo pipefail

# ROOT is the epsilon repo root: the directory containing this
# script's parent ("bench/"). Works whether epsilon is checked out
# standalone or as a subtree of a larger monorepo.
ROOT=$(cd "$(dirname "$0")/.." && pwd)
cd "$ROOT"

EPSILON=${EPSILON:-$ROOT/epsilon}
N=${N:-1}

# Module fixtures. Paths are relative to ROOT (the epsilon repo).
LEAF_MODULE=epsilon.uuid
LEAF_FILE=modules/uuid/src/uuid.lisp
LEAF_BUILD=_build/${LEAF_MODULE}

WIDE_MODULE=epsilon.json
WIDE_FILE=modules/json/src/epsilon/json.lisp
WIDE_BUILD=_build/${WIDE_MODULE}

# Module with a substantial transitive closure for the parallel-build
# scenarios -- pulls in crypto, ssl, encode, parse, etc.
PAR_MODULE=epsilon.http

# Target for the cascade scenarios: a module whose closure contains
# both LEAF_MODULE and WIDE_MODULE plus many modules that use them
# transitively. epsilon.web directly :requires both uuid and json and
# pulls in the rest of the web stack, so an upstream edit forces the
# build pool to recompile every invalidated module in the closure.
CASCADE_TARGET=epsilon.web

# --- helpers -------------------------------------------------------

mutate() {
    # Append a comment that does not change any symbol's ABI shape but
    # does change the source file's content hash, so the file's FASL key
    # will mismatch and the file will recompile. Downstream modules
    # whose key derivation only depends on this file's exported symbol
    # shapes should NOT recompile.
    local f=$1
    printf '\n;; bench-mutate %s\n' "$RANDOM" >> "$f"
}

restore() {
    git checkout -- "$LEAF_FILE" "$WIDE_FILE" 2>/dev/null || true
}

trap restore EXIT

time_one() {
    # Run a command, print wall-clock seconds to stdout.
    local label=$1; shift
    local start=$EPOCHREALTIME
    "$@" >/dev/null 2>&1 || true
    local end=$EPOCHREALTIME
    awk -v s="$start" -v e="$end" 'BEGIN{ printf "%.3f", e - s }'
}

# Median of N runs.
median_of() {
    local label=$1; shift
    local setup=$1; shift
    local cmd=("$@")
    local samples=()
    for ((i=0; i<N; i++)); do
        eval "$setup"
        samples+=("$(time_one "$label" "${cmd[@]}")")
    done
    printf '%s\n' "${samples[@]}" | sort -n | awk -v n="${#samples[@]}" '
        { v[NR] = $1 }
        END { if (n%2) print v[int(n/2)+1]; else printf "%.3f\n", (v[n/2]+v[n/2+1])/2 }
    '
}

# --- environment banner --------------------------------------------

echo "=== host ==="
echo "kernel:   $(uname -srm)"
echo "cpu:      $(awk -F: '/model name/{print $2; exit}' /proc/cpuinfo 2>/dev/null | sed 's/^ //' || sysctl -n machdep.cpu.brand_string 2>/dev/null || echo unknown)"
echo "cores:    $(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo unknown)"
echo "ram:      $(awk '/MemTotal:/{ printf "%.1f GiB\n", $2/1024/1024 }' /proc/meminfo 2>/dev/null || echo unknown)"
echo "epsilon:  $($EPSILON --version 2>&1 | head -1)"
echo "git head: $(git rev-parse --short HEAD)"
echo "samples:  $N per scenario (median reported)"
echo

# --- warmup --------------------------------------------------------

echo "warming caches..."
$EPSILON test "$LEAF_MODULE" >/dev/null 2>&1 || true
$EPSILON test "$WIDE_MODULE" >/dev/null 2>&1 || true
echo

# --- scenarios -----------------------------------------------------

printf '%-32s %10s\n' "scenario" "median (s)"
printf '%-32s %10s\n' "--------" "----------"

t=$(median_of "warm-null leaf"  "true" \
        $EPSILON test "$LEAF_MODULE")
printf '%-32s %10s\n' "warm-null  ($LEAF_MODULE)" "$t"

t=$(median_of "warm-null wide"  "true" \
        $EPSILON test "$WIDE_MODULE")
printf '%-32s %10s\n' "warm-null  ($WIDE_MODULE)" "$t"

t=$(median_of "edit-leaf"  "restore; mutate \"$LEAF_FILE\"" \
        $EPSILON test "$LEAF_MODULE")
printf '%-32s %10s\n' "edit-leaf  ($LEAF_MODULE)" "$t"
restore

t=$(median_of "edit-wide"  "restore; mutate \"$WIDE_FILE\"" \
        $EPSILON test "$WIDE_MODULE")
printf '%-32s %10s\n' "edit-wide  ($WIDE_MODULE)" "$t"
restore

t=$(median_of "cold-leaf"  "rm -rf \"$LEAF_BUILD\"" \
        $EPSILON test "$LEAF_MODULE")
printf '%-32s %10s\n' "cold-leaf  ($LEAF_MODULE)" "$t"

# Parallel-build scenarios. Each iteration rebuilds the closure of
# PAR_MODULE from scratch via the subprocess pool, with N workers.
# --force bypasses the FASL cache; we further rm the closure's _build
# directories in the setup hook so each sample is genuinely cold.
par_setup() {
    # Drop build artifacts for the target module and everything in its
    # transitive closure so --force has to rebuild every dep too.
    find _build -maxdepth 1 -type d \
        \( -name "$PAR_MODULE" \
           -o -name 'epsilon.crypto' \
           -o -name 'epsilon.ssl' \
           -o -name 'epsilon.encode' \
           -o -name 'epsilon.parse' \
           -o -name 'epsilon.url' \
           -o -name 'epsilon.regex' \
           -o -name 'epsilon.time' \) \
        -exec rm -rf {} + 2>/dev/null || true
}

for w in 1 4 8; do
    t=$(median_of "par-cold-w${w}" "par_setup" \
            $EPSILON build --force --workers "$w" "$PAR_MODULE")
    printf '%-32s %10s\n' "par-cold-w${w}  ($PAR_MODULE)" "$t"
done

# Cascade scenarios. The build pool only rebuilds what's invalidated,
# so editing an upstream then building the cascade target measures the
# actual closure of dependents that need to recompile.
echo "warming cascade target..."
$EPSILON build --force "$CASCADE_TARGET" >/dev/null 2>&1 || true

t=$(median_of "cascade-leaf" "restore; mutate \"$LEAF_FILE\"" \
        $EPSILON build "$CASCADE_TARGET")
printf '%-32s %10s\n' "cascade-leaf  ($CASCADE_TARGET)" "$t"
restore

t=$(median_of "cascade-wide" "restore; mutate \"$WIDE_FILE\"" \
        $EPSILON build "$CASCADE_TARGET")
printf '%-32s %10s\n' "cascade-wide  ($CASCADE_TARGET)" "$t"
restore

echo
echo "(edit-* scenarios use comment-only changes; only the edited file's"
echo " own FASL is invalidated.  par-cold-w* scenarios force-rebuild"
echo " $PAR_MODULE and its transitive closure on N workers.)"

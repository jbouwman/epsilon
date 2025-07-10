#!/bin/bash
# Epsilon optimized launcher with fast boot support

# Use the optimized boot script by default
BOOT_SCRIPT="${BOOT_SCRIPT:-scripts/epsilon-optimized.lisp}"

# Forward all arguments to SBCL with the dev tool
exec sbcl --script "$BOOT_SCRIPT" \
          --eval "(epsilon.tool.dev:main)" \
          --end-toplevel-options "$@"
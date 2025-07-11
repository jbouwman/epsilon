#!/bin/sh
#
# Script for booting Epsilon and running development steps
#

set -e

sbcl --noinform \
     --non-interactive \
     --no-sysinit \
     --no-userinit \
     --load "scripts/epsilon.lisp" \
     --eval "(epsilon.tool.dev:main)" \
     "$@"

#!/bin/sh
#
# Script for booting Epsilon and running development steps
#

sbcl --noinform \
     --non-interactive \
     --no-sysinit \
     --no-userinit \
     --load "scripts/boot.lisp" \
     --eval "(epsilon.tool.dev:main)" \
     "$@"

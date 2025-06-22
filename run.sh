#!/bin/sh
#
# Script for booting Epsilon and running development steps
#

set -e

sbcl --noinform \
     --non-interactive \
     --no-sysinit \
     --no-userinit \
     --load "src/tool/boot.lisp" \
     --eval "(epsilon.tool.boot:boot)" \
     --eval "(epsilon.tool.dev:main)" \
     "$@"

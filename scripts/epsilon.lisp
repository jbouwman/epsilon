;;;; Bootstrap a minimal build system, then build and load the
;;;; epsilon.core module, which includes all dev tools.

(load "scripts/boot.lisp")

(boot)

(epsilon.tool.build:build "epsilon.core")
